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

make_command="make --no-print-directory"


# define usage function
usage () {
  printf "usage: $0 <command> [<args>]\n"
  printf "\n"
  printf "command is one of the following:\n"
  printf "  list\n"
  printf "  set-user-models-dir <dir>\n"
  printf "  set-user-drivers-dir <dir>\n"
  printf "  install <user | system [--sudo]> <item ID | OpenKIM>\n"
  printf "  remove [--sudo] <item ID>\n"
  printf "\n\n"

  printf "list:\n"
  printf "  List installed kim-api models and model drivers\n"
  printf "\n"
  printf "set-user-models-dir:\n"
  printf "  Rewrite configuration file with provided directory\n"
  printf "\n"
  printf "set-user-drivers-dir:\n"
  printf "  Rewrite configuration file with provided directory\n"
  printf "\n"
  printf "install:\n"
  printf "  Install model and/or model driver from openkim.org\n"
  printf "\n"
  printf "remove:\n"
  printf "  Remove model or model driver\n"
}

check_version_compatibility () {
  local version="$1"
  local major=`printf -- "${version}" | sed -e 's/\([^.}]*\).*/\1/'`
  local minor=`printf -- "${version}" | sed -e 's/[^.]*\.\([^.}]*\).*/\1/'`
  if test \! \( \( ${major} -eq 1 \) -a \( ${minor} -ge 6 \) \) ; then
    return 1
  else
    return 0
  fi
}

check_item_compatibility () {
  local item_name="$1"
  local query="query={\"kimcode\":\"${item_name}\"}"
  query="${query}"'&fields={"kim-api-version":1}'
  query="${query}"'&database=obj&history=on'
  local version=`wget -q -O - --post-data="${query}" https://query.openkim.org/api \
                 | \
                 sed -e 's/\[//g' -e 's/\]//g' \
                 -e 's/{"kim-api-version": "\([0-9.]*\)"/\1/g'`
  if test x"" = x"${version}"; then
    printf "*** ERROR *** ${item_name} not found.\n"
    return 1
  else
    if check_version_compatibility "${version}"; then
      return 0
    else
      printf "*** ERROR *** ${item_name} is not compatible with this version of the KIM API.\n"
      return 1
    fi
  fi
}

check_config_file () {
  local config_file_name=`${collections_info} config_file name`
  local drivers_dir=`${collections_info} config_file model_drivers`
  local models_dir=`${collections_info} config_file models`

  if test \! -f "${config_file_name}" -o x"" = x"${drivers_dir}" -o x"" = x"${models_dir}"; then
    printf "Invalid kim-api configuration file.\n"
    return 1
  fi
}

rewrite_config_file_models_dir () {
  if test -d "$1"; then
     local config_file_name=`${collections_info} config_file name`
     local drivers_dir=`${collections_info} config_file model_drivers`
     local models_dir=`cd "$1" && pwd`

     printf "model_drivers_dir = %s\n" "${drivers_dir}" >  "${config_file_name}" || return 1
     printf "models_dir = %s\n" "${models_dir}"         >> "${config_file_name}"
  else
    printf "Directory '%s' does not exist.\n" "$1"
    return 1
  fi
}

rewrite_config_file_drivers_dir () {
  if test -d "$1"; then
    local config_file_name=`${collections_info} config_file name`
    local drivers_dir=`cd "$1" && pwd`
    local models_dir=`${collections_info} config_file models`

    printf "model_drivers_dir = %s\n" "${drivers_dir}" >  "${config_file_name}" || return 1
    printf "models_dir = %s\n" "${models_dir}"         >> "${config_file_name}"
  else
    printf "Directory '%s' does not exist." "$1"
    return 1
  fi
}

get_build_install_item () {
  local install_collection="$1"
  local item_name="$2"
  local use_sudo="$3"
  local PASSWORD="$4"
  local found_item=""
  local item_type=""


  # check for existing item
  if test x"__MD_" = x`printf -- "${item_name}" | sed 's/.*\(__MD_\).*/\1/'`; then
    found_item="`${collections_info} model_drivers find "${item_name}"`"
    item_type="MD"
  elif test x"__MO_" = x`printf -- "${item_name}" | sed 's/.*\(__MO_\).*/\1/'`; then
    found_item="`${collections_info} models find "${item_name}"`"
    item_type="MO"
  elif test x"__SM_" = x`printf -- "${item_name}" | sed 's/.*\(__SM_\).*/\1/'`; then
    found_item="`${collections_info} models find "${item_name}"`"
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
      printf "Item '${item_name}' of unknown type.\n"
      return 1
    else
      local item_collection=`printf -- "${found_item}" | sed -e 's/ .*//'`
      printf "Item '${item_name}' already installed in collection '${item_collection}'.\n"
      if test x"${item_collection}" = x"${install_collection}"; then
        return 0
      else
        return 1
      fi
    fi
  fi

  # create private temporary directory
  if test x"" = x"${TMPDIR}"; then TMPDIR="/tmp"; fi
  local build_dir=`mktemp -d "${TMPDIR}/kim-api-v1-build-XXXXXXXXXX"`
  if test $? -ne 0; then
    printf "Unable to create temporary directory.\n"
    return 1;
  fi

  (  # subshell
    cd "${build_dir}" || return 1

    # setup kim-api config
    ${build_config} --makefile-kim-config > Makefile.KIM_Config || return 1

    # download item (and possibly its driver)
    if test x"OpenKIM" = x"${item_type}"; then
      local query='query={"type":"mo","kim-api-version":{"$regex":"^1\\."}}'
      query="${query}"'&fields={"kimcode":1, "kim-api-version":1}'
      query="${query}"'&database=obj'
      local list=`wget -q -O - --post-data="${query}" https://query.openkim.org/api \
                     | \
                     sed -e 's/\[//g' -e 's/\]//g' \
                     -e 's/{"kim-api-version": "\([0-9.]*\)", "kimcode": "\([^"]*\)"},*/\1:\2/g'`
      for version in ${list}; do \
        if check_version_compatibility "${version}"; then
          get_build_install_item "$install_collection" "${modname}" "${use_sudo}" "${PASSWORD}" || return 1
        fi
      done
    elif test x"MD" = x"${item_type}"; then
      if check_item_compatibility "${item_name}"; then
        printf "*@downloading.......@%-50s\n" "${item_name}" | sed -e 's/ /./g' -e 's/@/ /g'
        if wget -q --content-disposition "https://openkim.org/download/${item_name}.txz"; then
          tar Jxvf "${item_name}.txz" 2>&1 | sed -e 's/^/                /' &&
            rm -f "${item_name}.txz" &&
            if test 0 -lt `grep -c MAKE_SYSTEM ${item_name}/Makefile`; then \
              printf "*** ERROR *** ${item_name} appears to be written for an older, incompatible, version of the KIM API.\n"
              return 1
            fi
          cd ${item_name}
          ${make_command} && if test x"sudo-yes" = x"${use_sudo}"; then
            printf -- "${PASSWORD}\n" | sudo -k -S ${make_command} "install-${install_collection}" 2> /dev/null || return 1
          else
            ${make_command} "install-${install_collection}" || return 1
          fi
          cd ..
        else
          printf "                Unable to download ${item_name} from https://openkim.org.  Check the KIM Item ID for errors.\n"
          return 1
        fi
      else
        return 1
      fi
    elif test x"MO" = x"${item_type}"; then
      if check_item_compatibility "${item_name}"; then
        printf "*@downloading.......@%-50s\n" "${item_name}" | sed -e 's/ /./g' -e 's/@/ /g'
        if wget -q --content-disposition "https://openkim.org/download/${item_name}.txz"; then
          tar Jxvf "${item_name}.txz" 2>&1 | sed -e 's/^/                /' &&
            rm -f "${item_name}.txz" &&
            if test 0 -lt `grep -c MAKE_SYSTEM ${item_name}/Makefile`; then
              printf "*** ERROR *** ${item_name} appears to be written for an older, incompatible, version of the KIM API.\n";
              return 1
            elif test x"ParameterizedModel" = x"`${make_command} -C \"${item_name}\" kim-item-type`"; then
              dvr="`${make_command} -C \"${item_name}\" model-driver-name`"
              if test x"" != x"`${collections_info} model_drivers find "${dvr}"`"; then
                printf "*@using installed driver.......@%-50s\n" "${dvr}" | sed -e 's/ /./g' -e 's/@/ /g' || return 1
              else
                get_build_install_item "${install_collection}" "${dvr}" "${use_sudo}" "${PASSWORD}" || return 1
              fi
            fi
          cd ${item_name}
          ${make_command} && if test x"sudo-yes" = x"${use_sudo}"; then
            printf -- "${PASSWORD}\n" | sudo -k -S ${make_command} "install-${install_collection}" 2> /dev/null || return 1
          else
            ${make_command} "install-${install_collection}" || return 1
          fi
          cd ..
        else
          printf "                Unable to download ${item_name} from https://openkim.org.  Check the KIM Item ID for errors.\n"
          return 1
        fi
      else
        return 1
      fi
    fi
  )  || return 1  # exit subshell

  rm -rf "${build_dir}" || return 1
}

remove_item () {
  local item_name="$1"
  local use_sudo="$2"
  local PASSWORD="$3"
  local found_item=""
  local item_type=""

  # check for existing item
  found_item="`${collections_info} model_drivers find "${item_name}"`"
  if test x"" = x"${found_itme}"; then
    found_item="`${collections_info} models find "${item_name}"`"
    if test x"" = x"${found_item}"; then
      printf "Item not installed.\n"
      return 1
    else
      item_type="models"
    fi
  else
    item_type="model_drivers"
  fi

  local item_dir=`${collections_info} "${item_type}" find "${item_name}" | sed -e 's/^[^ ]* [^ ]* \([^ ]*\).*/\1/'`"/${item_name}"
  printf "Removing '%s'.\n" "${item_dir}"
  if test x"sudo-yes" = x"${use_sudo}"; then
    printf -- "${PASSWORD}\n" | sudo -k -S rm -rf "${item_dir}" 2> /dev/null || return 1
  else
    rm -rf "${item_dir}" || return 1
  fi
}

split_drivers_list_into_collections () {
  drivers_cwd_collection=""; number_drivers_cwd=0
  drivers_env_collection=""; number_drivers_env=0
  drivers_usr_collection=""; number_drivers_usr=0
  drivers_sys_collection=""; number_drivers_sys=0
    while read line; do
      local collection=`printf -- "$line" | sed -e 's/\([^ ]*\) .*/\1/'`
      local name=`printf -- "$line" | sed -e 's/[^ ]* \([^ ]*\) .*/\1/'`
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
      local collection=`printf -- "$line" | sed -e 's/\([^ ]*\) .*/\1/'`
      local name=`printf -- "$line" | sed -e 's/[^ ]* \([^ ]*\) .*/\1/'`
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

print_separator_line () {
  printf "%79s\n" " " | sed -e "s/ /$1/g"
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
  print_separator_line "="
  printf "\n"
  printf "kim-api library: \n\t%s\n" `${collections_info} system library`
  printf "\n"
  printf "kim-api configuration file:\n\t%s\n" \
         `${collections_info} config_file name`
  printf "\n\n"
  printf "Environment Variables:\n"
  print_separator_line "-"
  printf -- "${config_env_name}:\n"
  printf -- "\t${config_env}\n"
  printf "\n"
  printf -- "${drivers_env_name}:\n"
  printf -- "%s\n" "`printf -- "${drivers_env}" | sed -e 's/^/	/g'`"
  printf "\n"
  printf -- "${models_env_name}:\n"
  printf -- "%s\n" "`printf -- "${models_env}" | sed -e 's/^/	/g'`"
  printf "\n"
  print_separator_line "="


  model_drivers_list=`${collections_info} model_drivers`
  split_drivers_list_into_collections "${model_drivers_list}"
  models_list=`${collections_info} models`
  split_models_list_into_collections "${models_list}"

  printf "\n\n\n"
  printf "Current Working Directory Collection\n"
  print_separator_line "-"
  printf "Drivers: %s\n" "${PWD}"
  if test $number_drivers_cwd -gt 0; then
    printf "${drivers_cwd_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n"

    printf "Models: %s\n" "${PWD}"
  if test $number_models_cwd -gt 0; then
    printf -- "${models_cwd_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n\n"

  printf "Environment Variable Collection\n"
  print_separator_line "-"
  printf "Drivers: "
  if test x"--empty--" = x"${drivers_env}"; then
    printf "%s" ${drivers_env}
  else
    printf "'%s' " ${drivers_env}
  fi
  printf "\n"
  if test $number_drivers_env -gt 0; then
    printf -- "${drivers_env_collection}"
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
    printf -- "${models_env_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n\n"

  drivers_usr=`${collections_info} config_file model_drivers`
  if test x"" = x"${drivers_usr}"; then drivers_usr="--empty--"; fi
  models_usr=`${collections_info} config_file models`
  if test x"" = x"${models_usr}"; then models_usr="--empty--"; fi
  printf "User Collection\n"
  print_separator_line "-"
  printf "Drivers: %s\n" "${drivers_usr}"
  if test $number_drivers_usr -gt 0; then
    printf -- "${drivers_usr_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n"
  printf "Models: %s\n" "${models_usr}"
  if test $number_models_usr -gt 0; then
    printf -- "${models_usr_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n\n"

  printf "System Collection\n"
  print_separator_line "-"
  printf "Drivers: %s\n" `${collections_info} system model_drivers`
  if test $number_drivers_sys -gt 0; then
    printf -- "${drivers_sys_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n\n"
  printf "Models: %s\n" `${collections_info} system models`
  if test $number_models_sys -gt 0; then
    printf -- "${models_sys_collection}"
  else
    printf "\t--empty--\n"
  fi
  printf "\n"
}


get_password () {
  printf "Enter Password : "
  stty -echo
  trap 'stty echo' EXIT
  read PASSWORD
  stty echo
  trap - EXIT
  printf "\n"
  if ! (printf -- "${PASSWORD}\n" | \
          sudo -k -S printf "" > /dev/null 2>&1); then
    printf "Bad password.\n"
    return 1
  fi
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

if ! check_config_file; then
  printf "Aborting!\n"
  exit 1
fi

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
      if ! rewrite_config_file_models_dir "$subcommand"; then
        printf "\nAborting!\n"
        exit 1
      else
        printf "\nSuccess!\n."
      fi
    fi
    ;;
  set-user-drivers-dir)
    if test $# -lt 2; then
      usage
      exit 1
    else
      subcommand=$2
      if ! rewrite_config_file_drivers_dir "$subcommand"; then
        printf "\nAborting!\n"
        exit 1
      else
        printf "\nSuccess!\n"
      fi
    fi
    ;;
  install)
    if test $# -lt 3; then
      usage
      exit 1
    else
      subcommand=$2
      case $subcommand in
        user)
          item_name=$3
          if ! get_build_install_item "user" "${item_name}" "sudo-no" ""; then
            printf "\nAborting!\n"
            exit 1
          else
            printf "\nSuccess!\n"
          fi
          ;;
        system)
          PASSWORD=""
          if test x"--sudo" = x"$3"; then
            use_sudo="sudo-yes"
            if ! get_password; then
              printf "\nAborting!\n"
              exit 1
            fi
            item_name=$4
          else
            use_sudo="sudo-no"
            item_name=$3
          fi
          if ! get_build_install_item "system" "${item_name}" "${use_sudo}" "${PASSWORD}"; then
            printf "\nAborting!\n"
            exit 1
          else
            printf "\nSuccess!\n"
          fi
          ;;
        *)
          printf "unknown subcommand: %s\n\n" $subcommand
          usage
          exit 1
          ;;
      esac
    fi
    ;;
  remove)
    if test $# -lt 2; then
      usage
      exit 1
    else
      if test x"--sudo" = x"$2"; then
        use_sudo="sudo-yes"
        if ! get_password; then
          printf "\nAborting!\n"
          exit 1
        fi
        item_name=$3
      else
        use_sudo="sudo-no"
        item_name=$2
      fi
      if ! remove_item "${item_name}" "${use_sudo}" "${PASSWORD}"; then
        printf "\nAborting!\n"
        exit 1
      else
        printf "\nSuccess!\n"
      fi
    fi
    ;;
esac
