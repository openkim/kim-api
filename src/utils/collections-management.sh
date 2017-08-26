#!/bin/sh
#

#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2015, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

collections_info=###COLLECTIONS#INFO#UTILITY###
build_config=###BUILD#CONFIG###

# define usage function
usage () {
  printf "usage: model-info <command> [<args>]\n"
  printf "\n"
  printf "command is one of the following:\n"
  printf "  list                                          "
  printf "List installed kim-api models and/or model drivers\n"
  printf "  set-user-models-dir  <dir>                    "
  printf "Rewrite configuration file with provided directory\n"
  printf "  set-user-drivers-dir <dir>                    "
  printf "Rewrite configuration file with provided directory\n"
  printf "  install  <user | system> <item ID | OpenKIM>  "
  printf "Install model(s) and/or model driver(s) from openkim.org\n"
  printf "  remove   <item ID>                            "
  printf "Remove model(s) and/or model driver(s)\n"
}

check_config_file_and_create_if_empty () {
  local config_file_name=`${collections_info} config_file name`
  local drivers_dir=`${collections_info} config_file model_drivers`
  local models_dir=`${collections_info} config_file models`

  if test \! -f "${config_file_name}" -o x"" = x"${drivers_dir}" -o x"" = x"${models_dir}"; then
    printf "Missing or invalid kim-api configuration file.  Recreating file with default values!\n"
    drivers_dir="`printf "${config_file_name}" | sed -e 's|\(.*\)/[^/]*$|\1|'`/model_drivers"
    mkdir -p "${drivers_dir}"
    printf "model_drivers_dir = %s\n" "${drivers_dir}" >  "${config_file_name}"
    models_dir="`printf "${config_file_name}" | sed -e 's|\(.*\)/[^/]*$|\1|'`/models"
    mkdir -p "${models_dir}"
    printf "models_dir = %s\n" "${models_dir}"         >> "${config_file_name}"
  fi
}

rewrite_config_file_models_dir () {
  if test -d "$1"; then
     local config_file_name=`${collections_info} config_file name`
     local drivers_dir=`${collections_info} config_file model_drivers`
     local models_dir=`cd "$1" && pwd`

     printf "model_drivers_dir = %s\n" "${drivers_dir}" >  "${config_file_name}"
     printf "models_dir = %s\n" "${models_dir}"         >> "${config_file_name}"
  else
    printf "Directory '%s' does not exist. Aborting!\n" "$1"
    exit 1
  fi
}

rewrite_config_file_drivers_dir () {
  if test -d "$1"; then
    local config_file_name=`${collections_info} config_file name`
    local drivers_dir=`cd "$1" && pwd`
    local models_dir=`${collections_info} config_file models`

    printf "model_drivers_dir = %s\n" "${drivers_dir}" >  "${config_file_name}"
    printf "models_dir = %s\n" "${models_dir}"         >> "${config_file_name}"
  else
    printf "Directory '%s' does not exist. Aborting!\n" "$1"
  fi
}

get_build_install_item () {
  local install_collection="$1"
  local item_name="$2"
  local found_item=""
  local item_type=""

  # check for existing item
  if test x"__MD_" = x`printf "${item_name}" | sed 's/.*\(__MD_\).*/\1/'`; then
    found_item=`${collections_info} model_drivers find "${item_name}"`
    item_type="MD"
  elif test x"__MO_" = x`printf "${item_name}" | sed 's/.*\(__MO_\).*/\1/'`; then
    found_item=`${collections_info} models find "${item_name}"`
    item_type="MO"
  elif test x"__SM_" = x`printf "${item_name}" | sed 's/.*\(__SM_\).*/\1/'`; then
    found_item=`${collections_info} models find "${item_name}"`
    item_type="SM"
  elif test x"OpenKIM" = x"${item_name}"; then
    found_item=""
    item_type="OpenKIM"
  else
    found_item="UnknownItemType"
    item_type="Unknown"
  fi
  if test x"" != x"${found_item}"; then
    if test x"Unknown" = x"${item_type}"; then
      printf "Item '${item_name} of unknown type.  Aborting!\n"
      exit 1
    else
      printf "Item '${item_name}' already installed.\n"
      return 0
    fi
  fi

  # create private temporary directory
  if test x"" == x"${TMPDIR}"; then TMPDIR="/tmp"; fi
  local build_dir=`mktemp -d "${TMPDIR}/kim-api-build-XXXXXXXXXX"`
  if test $? -ne 0; then
    printf "Unable to create temporary directory. Aborting!\n"
    exit 1
  fi

  (  # subshell
    cd "${build_dir}"

    # setup kim-api config
    ${build_config} --makefile-kim-config > Makefile.KIM_Config

    # download item (and possibly its driver)
    if test x"OpenKIM" = x"${item_type}"; then
      local query='query={"type":"mo","kim-api-version":{"$regex":"^1\\."}}'
      query="${query}"'&fields={"kimcode":1, "kim-api-version":1}'
      query="${query}"'&database=obj'
      local list=`wget -q -O - --post-data="${query}" https://query.openkim.org/api \
                     | \
                     sed -e 's/\[//g' -e 's/\]//g' \
                     -e 's/{"kim-api-version": "\([0-9.]*\)", "kimcode": "\([^"]*\)"},*/\1:\2/g'`
      for model in ${list}; do \
        local minor=`printf "${model}" | sed -e 's/1\.\([^.:]*\).*/\1/'`
        local modname=`printf "${model}" | sed -e 's/.*://'`
        if test ${minor} -ge 6; then
          get_build_install_item "$install_collection" "${modname}"
        fi
      done
    elif test x"MD" = x"${item_type}"; then
      printf "*@downloading.......@%-50s\n" "${item_name}" | sed -e 's/ /./g' -e 's/@/ /g'
      if wget -q --content-disposition "https://openkim.org/download/${item_name}.tgz"; then
        tar zxvf "${item_name}.tgz" 2>&1 | sed -e 's/^/                /' &&
          rm -f "${item_name}.tgz" &&
          if test 0 -lt `grep -c MAKE_SYSTEM ${item_name}/Makefile`; then \
            printf "*** ERROR *** ${item_name} appears to be written for an older, incompatible, version of the KIM API.\n"
            exit 1
          fi
        cd ${item_name}
        make && make "install-${install_collection}"
        cd ..
      else
        printf "                Unable to download ${item_name} from https://openkim.org.  Check the KIM Item ID for errors.\n"
        exit 1
      fi
    elif test x"MO" = x"${item_type}"; then
      printf "*@downloading.......@%-50s\n" "${item_name}" | sed -e 's/ /./g' -e 's/@/ /g'
      if wget -q --content-disposition "https://openkim.org/download/${item_name}.tgz"; then
        tar zxvf "${item_name}.tgz" 2>&1 | sed -e 's/^/                /' &&
          rm -f "${item_name}.tgz" &&
          if test 0 -lt `grep -c MAKE_SYSTEM ${item_name}/Makefile`; then
            printf "*** ERROR *** ${item_name} appears to be written for an older, incompatible, version of the KIM API.\n";
            exit
          elif test x"ParameterizedModel" = x"`make -C \"${item_name}\" kim-item-type`"; then
            dvr="`make -C \"${item_name}\" model-driver-name`"
            if test x"" != x`${collections_info} model_drivers find "${dvr}"`; then
              printf "*@using installed driver.......@%-50s\n" "${dvr}" | sed -e 's/ /./g' -e 's/@/ /g'
              true
            else
              get_build_install_item "${install_collection}" "${dvr}"
            fi
          fi
        cd ${item_name}
        make && make "install-${install_collection}"
        cd ..
      else
        printf "                Unable to download ${item_name} from https://openkim.org.  Check the KIM Item ID for errors.\n"
        exit 1
      fi
    fi
  )  # exit subshell

  rm -rf "${build_dir}"
}

remove_item () {
  local item_name="$1"
  local found_item=""
  local item_type=""

  # check for existing item
  if test x"__MD_" = x`printf "${item_name}" | sed 's/.*\(__MD_\).*/\1/'`; then
    found_item=`${collections_info} model_drivers find "${item_name}"`
    item_type="model_drivers"
  elif test x"__MO_" = x`printf "${item_name}" | sed 's/.*\(__MO_\).*/\1/'`; then
    found_item=`${collections_info} models find "${item_name}"`
    item_type="models"
  elif test x"__SM_" = x`printf "${item_name}" | sed 's/.*\(__SM_\).*/\1/'`; then
    found_item=`${collections_info} models find "${item_name}"`
    item_type="models"
  else
    found_item=""
    item_type="Unknown"
  fi
  if test x"" = x"${found_item}"; then
    printf "Item not installed or of unknown type. Aborting!\n"
    exit 1
  fi


  local item_dir=`${collections_info} "${item_type}" find "${item_name}" | sed -e 's/^[^ ]* [^ ]* \([^ ]*\).*/\1/'`"/${item_name}"
  printf "Removing '%s'.\n" "${item_dir}"
  rm -rf "${item_dir}"
}

split_drivers_list_into_collections () {
  drivers_cwd_collection=""; number_drivers_cwd=0
  drivers_env_collection=""; number_drivers_env=0
  drivers_usr_collection=""; number_drivers_usr=0
  drivers_sys_collection=""; number_drivers_sys=0
    while read line; do
      local collection=`printf "$line" | sed -e 's/\([^ ]*\) .*/\1/'`
      local name=`printf "$line" | sed -e 's/[^ ]* \([^ ]*\) .*/\1/'`
      case $collection in
        "")
        # empty do nothing
        ;;
        CWD)
          number_drivers_cwd=`expr $number_drivers_cwd \+ 1`
          drivers_cwd_collection="${drivers_cwd_collection}\t${name}\n"
          ;;
        environment)
          number_drivers_env=`expr $number_drivers_env \+ 1`
          drivers_env_collection="${drivers_env_collection}\t${name}\n"
          ;;
        user)
          number_drivers_usr=`expr $number_drivers_usr \+ 1`
          drivers_usr_collection="${drivers_usr_collection}\t${name}\n"
          ;;
        system)
          number_drivers_sys=`expr $number_drivers_sys \+ 1`
          drivers_sys_collection="${drivers_sys_collection}\t${name}\n"
          ;;
        *)
          printf "Error unknown collection!\n"
          exit 1
          ;;
      esac
    done <<EOF
$1
EOF
}

split_models_list_into_collections () {
  models_cwd_collection=""; number_models_cwd=0
  models_env_collection=""; number_models_env=0
  models_usr_collection=""; number_models_usr=0
  models_sys_collection=""; number_models_sys=0
    while read line; do
      local collection=`printf "$line" | sed -e 's/\([^ ]*\) .*/\1/'`
      local name=`printf "$line" | sed -e 's/[^ ]* \([^ ]*\) .*/\1/'`
      case $collection in
        "")
        # empty do nothing
        ;;
        CWD)
          number_models_cwd=`expr $number_models_cwd \+ 1`
          models_cwd_collection="${models_cwd_collection}\t${name}\n"
          ;;
        environment)
          number_models_env=`expr $number_models_env \+ 1`
          models_env_collection="${models_env_collection}\t${name}\n"
          ;;
        user)
          number_models_usr=`expr $number_models_usr \+ 1`
          models_usr_collection="${models_usr_collection}\t${name}\n"
          ;;
        system)
          number_models_sys=`expr $number_models_sys \+ 1`
          models_sys_collection="${models_sys_collection}\t${name}\n"
          ;;
        *)
          printf "Error unknown collection!\n"
          exit 1
          ;;
      esac
    done <<EOF
$1
EOF
}

print_list_of_drivers_and_models () {
  config_env_name=`${collections_info} config_file env | sed -e 's/ .*//'`
  config_env=`${collections_info} config_file env | sed -e 's/^[^ ]* //'`
  if test x"" = x"${config_env}"; then config_env="--empty--"; fi
  drivers_env_name=`${collections_info} env env | sed -e 's/^[^ ]* //'`
  drivers_env=`${collections_info} env model_drivers`
  if test x"" = x"${drivers_env}"; then drivers_env="--empty--"; fi
  models_env_name=`${collections_info} env env | sed -e 's/ .*//'`
  models_env=`${collections_info} env models`
  if test x"" = x"${models_env}"; then models_env="--empty--"; fi

  printf "\n\n"
  printf "Knowledgebase of Interatomic Models (KIM)"
  printf -- "  ---  Model Collections Listing\n"
  printf -- "=%.0s" {1..79}; printf "\n"
  printf "\n"
  printf "kim-api library: \n\t%s\n" `${collections_info} system library`
  printf "\n"
  printf "kim-api configuration file:\n\t%s\n" \
         `${collections_info} config_file name`
  printf "\n\n"
  printf "Environment Variables:\n"
  printf -- "-%.0s" {1..79}; printf "\n"
  printf -- "${config_env_name}:\n"
  printf -- "\t${config_env}\n"
  printf "\n"
  printf -- "${drivers_env_name}:\n"
  printf -- "%s\n" "`printf -- "${drivers_env}" | sed -e 's/^/	/g'`"
  printf "\n"
  printf -- "${models_env_name}:\n"
  printf -- "%s\n" "`printf -- "${models_env}" | sed -e 's/^/	/g'`"
  printf "\n"
  printf -- "=%.0s" {1..79}; printf "\n"


  model_drivers_list=`${collections_info} model_drivers`
  split_drivers_list_into_collections "${model_drivers_list}"
  models_list=`${collections_info} models`
  split_models_list_into_collections "${models_list}"

  printf "\n\n\n"
  printf "Current Working Directory Collection\n"
  printf -- "-%.0s" {1..79}; printf "\n"
  printf "Drivers: %s\n" "${PWD}"
  if test $number_drivers_cwd -gt 0; then
    printf "${drivers_cwd_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n"

    printf "Models: %s\n" "${PWD}"
  if test $number_models_cwd -gt 0; then
    printf "${models_cwd_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n\n"

  printf "%-79s\n" "Environment Variable Collection"
  printf -- "-%.0s" {1..79}; printf "\n"
  printf "Drivers: "
  if test x"--empty--" = x"${drivers_env}"; then
    printf "%s" ${drivers_env}
  else
    printf "'%s' " ${drivers_env}
  fi
  printf "\n"
  if test $number_drivers_env -gt 0; then
    printf "${drivers_env_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n"
  printf "Models: "
  if test x"--empty--" = x"${models_env}"; then
    printf "%s" ${models_env}
  else
    printf "'%s' " ${models_env}
  fi
  printf "\n"
  if test $number_models_env -gt 0; then
    printf "${modelss_env_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n\n"

  drivers_usr=`${collections_info} config_file model_drivers`
  if test x"" = x"${drivers_usr}"; then drivers_usr="--empty--"; fi
  models_usr=`${collections_info} config_file models`
  if test x"" = x"${models_usr}"; then models_usr="--empty--"; fi
  printf "%-79s\n" "User Collection"
  printf -- "-%.0s" {1..79}; printf "\n"
  printf "Drivers: %s\n" "${drivers_usr}"
  if test $number_drivers_usr -gt 0; then
    printf "${drivers_usr_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n"
  printf "Models: %s\n" "${models_usr}"
  if test $number_models_usr -gt 0; then
    printf "${models_usr_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n\n"

  printf "%-79s\n" "System Collection"
  printf -- "-%.0s" {1..79}; printf "\n"
  printf "Drivers: %s\n" `${collections_info} system model_drivers`
  if test $number_drivers_sys -gt 0; then
    printf "${drivers_sys_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n\n"
  printf "Models: %s\n" `${collections_info} system models`
  if test $number_models_sys -gt 0; then
    printf "${models_sys_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n"
}


######## main script ########

# check that command is given
if test $# -lt 1; then
  usage
  exit 1
else
  command=$1
  case $command in
    list|set-user-drivers-dir|set-user-models-dir|install|remove)
    ;;
    *)
      printf "unknown command: %s\n\n" $command
      usage
      exit 1
  esac
fi

check_config_file_and_create_if_empty

case $command in
  list)
    print_list_of_drivers_and_models
    ;;
  set-user-models-dir)
    if test $# -lt 2; then
      usage
      exit 1
    else
      subcommand=$2
      rewrite_config_file_models_dir "$subcommand"
    fi
    ;;
  set-user-drivers-dir)
    if test $# -lt 3; then
      usage
      exit 1
    else
      subcommand=$2
      rewrite_config_file_drivers_dir "$subcommand"
    fi
    ;;
  install)
    if test $# -ne 3; then
      usage
      exit 1
    else
      subcommand=$2
      itemName=$3
      case $subcommand in
        user|system)
          get_build_install_item "${subcommand}" "${itemName}"
        ;;
        *)
          printf "unknown subcommand: %s\n\n" $subcommand
          usage
          ;;
      esac
    fi
    ;;
  remove)
    if test $# -lt 2; then
      usage
      exit 1
    else
      item_name=$2
      remove_item "${item_name}"
    fi
    ;;
esac
