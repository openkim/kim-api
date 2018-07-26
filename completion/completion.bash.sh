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
# Copyright (c) 2018, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

_###SANITIZED#FULL#PACKAGE#NAME###_build_config()
{
  local cur stand_alone_opts combine_opts
  # NOTE: leading and trailing spaces are necessary for the below two vars.
  stand_alone_opts=" --makefile-kim-config --master-config --libexec-path"
  stand_alone_opts="${stand_alone_opts} --cc --cxx --fc --ld --objonlyflag"
  stand_alone_opts="${stand_alone_opts} --outputinflag --version --help "
  combine_opts=" --includes --cflags --cxxflags --ldflags"
  combine_opts="${combine_opts} --ldlibs --xlangldlibs --fnomainflag "

  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"

  if test ${COMP_CWORD} -eq 1; then
    COMPREPLY=( $(compgen -W "${stand_alone_opts} ${combine_opts}" \
                          -- "${cur}") )
    return 0
  elif printf "%s" "${stand_alone_opts}" | \
      grep -- "${COMP_WORDS[1]}" > /dev/null 2>&1; then
    return 0
  else
    for opt in "${COMP_WORDS[@]:1:${COMP_CWORD}-1}"; do
      combine_opts=`printf "%s" "${combine_opts}" | sed -e "s/ ${opt} / /"`
    done

    COMPREPLY=( $(compgen -W "${combine_opts}" -- "${cur}") )
    return 0
  fi

  return 1
}
complete -F _###SANITIZED#FULL#PACKAGE#NAME###_build_config ###FULL#PACKAGE#NAME###-build-config


_###SANITIZED#FULL#PACKAGE#NAME###_collections_management()
{
  local collections_info
  collections_info=###COLLECTIONS#INFO#UTILITY###

  local cur subcommands opts
  subcommands="list set-user-models-dir set-user-drivers-dir install reinstall"
  subcommands="${subcommands} remove remove-all"

  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"

  if test ${COMP_CWORD} -eq 1; then
    COMPREPLY=( $(compgen -W "${subcommands}" -- "${cur}") )
    return 0
  else
    case "${COMP_WORDS[1]}" in
      list)
        opts=""
        if test ${COMP_CWORD} -eq 2; then
          opts="--with-version --log"
        elif test ${COMP_CWORD} -eq 3 -a x"--with-version" = x"${COMP_WORDS[2]}"; then
          opts="--log"
        elif test ${COMP_CWORD} -eq 3 -a x"--log" = x"${COMP_WORDS[2]}"; then
          opts="--with-version"
        else
          return 1
        fi
        COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
        return 0
        ;;
      set-user-*)
        if test ${COMP_CWORD} -gt 2; then
          return 1
        else
          _cd
          return 0
        fi
        ;;
      install)
        if test ${COMP_CWORD} -eq 2; then
          COMPREPLY=( $(compgen -W "--force CWD environment user system" \
                                -- "${cur}") )
          return 0
        fi
        if test ${COMP_CWORD} -eq 3 -a x"${COMP_WORDS[2]}" = x"--force"; then
          COMPREPLY=( $(compgen -W "CWD environment user system" \
                                -- "${cur}") )
          return 0
        fi
        local collection_position
        if test x"${COMP_WORDS[2]}" = x"--force"; then
          collection_position=3
        else
          collection_position=2
        fi
        opts=""
        if test ${COMP_CWORD} -eq `expr ${collection_position} + 1` \
                -a x"system" = x"${COMP_WORDS[${collection_position}]}"; then
          opts="--sudo OpenKIM OpenKIM_with_history"
        elif test ${COMP_CWORD} -eq 3; then
          opts="OpenKIM OpenKIM_with_history"
        fi

        local query='query={"type":"mo","kim-api-version":{"$regex":"^###MAJOR#VERSION###\\."}}'
        query="${query}"'&fields={"kimcode":1}'
        query="${query}"'&database=obj&history=on'
        local list=`wget -q -O - --post-data="${query}" \
                    https://query.openkim.org/api | \
                    sed -e 's/\[//g' -e 's/\]//g' \
                    -e 's/{"kimcode": "\([^"]*\)"},*/\1/g'`
        opts="${opts} ${list}"

        _cd
        COMPREPLY=( "${COMPREPLY[@]}" $(compgen -W "${opts}" -- "${cur}") )
        return 0
        ;;
      reinstall|remove)
        opts=""
        if test ${COMP_CWORD} -eq 2; then
          opts="--interactive --sudo"
        elif test ${COMP_CWORD} -eq 3 -a x"--interactive" = x"${COMP_WORDS[2]}"; then
          opts="--sudo"
        elif test ${COMP_CWORD} -eq 3 -a x"--sudo" = x"${COMP_WORDS[2]}"; then
          opts="--interactive"
        fi
        local models drivers
        models=`${collections_info} models | sed -e 's|[^[:space:]]* \([^[:space:]]*\) .*|\1|'`
        drivers=`${collections_info} model_drivers | sed -e 's|[^[:space:]]* \([^[:space:]]*\) .*|\1|'`
        opts="${opts} ${models} ${drivers}"

        if test x"reinstall" = x"${COMP_WORDS[1]}"; then
          _cd
        fi
        COMPREPLY=( "${COMPREPLY[@]}" $(compgen -W "${opts}" -- "${cur}") )
        return 0
        ;;
      remove-all)
        opts=""
        if test ${COMP_CWORD} -eq 2; then
          opts="--interactive --sudo"
        elif test ${COMP_CWORD} -eq 3 -a x"--interactive" = x"${COMP_WORDS[2]}"; then
          opts="--sudo"
        elif test ${COMP_CWORD} -eq 3 -a x"--sudo" = x"${COMP_WORDS[2]}"; then
          opts="--interactive"
        fi
        COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
        return 0
        ;;
      *)
        return 1
        ;;
    esac
  fi
  return 1
}
complete -F _###SANITIZED#FULL#PACKAGE#NAME###_collections_management ###FULL#PACKAGE#NAME###-collections-management
