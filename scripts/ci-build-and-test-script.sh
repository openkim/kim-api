#!/bin/sh

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
# Copyright (c) 2013--2019, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#


# print environment
env

mkdir build
cd build
ccache -z
cmake -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX:=/usr/local} \
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE:=Debug} \
      ${SANITIZE:+-DKIM_API_ENABLE_SANITIZE=ON} \
      ${COVERAGE:+-DKIM_API_ENABLE_COVERAGE=ON} \
      ${IWYU:+-DCMAKE_CXX_INCLUDE_WHAT_YOU_USE=include-what-you-use} \
      ${IWYU:+-DCMAKE_C_INCLUDE_WHAT_YOU_USE=include-what-you-use} \
      .. \
  || exit $?
make -j2 || exit $?
ccache -s

make test || exit $?
if test x"${COVERAGE}" != x""; then
  curl -s https://codecov.io/bash | sed -e's/execdir/exec/g' > ${PWD}/codecov
  bash ${PWD}/codecov -x gcov-6 || exit $?
fi

# if test ${COVERAGE}; then
#   if test x"clang" = x"${CC}"; then
#     codecov -R ${PWD}/../../${TRAVIS_REPO_SLUG#*/} -F "${CC}" -x "llvm-cov gcov" 2>&1 | grep -v "arcs.*block"
#   else
#     codecov -R ${PWD}/../../${TRAVIS_REPO_SLUG#*/} -F "${CC}" 2>&1 | grep -v "arcs.*block";
#   fi
# fi

mkdir ${PWD}/destdir
make install DESTDIR=${PWD}/destdir || exit $?
rm -rf ${PWD}/destdir${INSTALL_PREFIX}/{bin,include,${LIB_DIR:=lib},libexec,share} || exit $?
rm -rf ${PWD}/destdir${BASH_COMPLETION_DIR:=${INSTALL_PREFIX}/etc} || exit $?
printf -- "--- remaining installed files ---\n" &&
  find ${PWD}/destdir &&
  printf -- "---------------------------------\n"
dir="${INSTALL_PREFIX}"
while test x"" != x"${dir}"; do
  printf "Removing dir: %s\n" "${PWD}/destdir${dir}"
  rmdir ${PWD}/destdir${dir} || exit $?
  dir=`printf -- "${dir}" | sed -e 's|\(.*\)/.*|\1|'`
done
rmdir ${PWD}/destdir || exit $?
cd ../
rm -rf build || exit $?

# now test collections-management and install
mkdir build
cd build
ccache -z
cmake -DCMAKE_INSTALL_PREFIX=${PWD}/test-install \
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} \
      -DKIM_API_BUILD_EXAMPLES=OFF \
      ${SANITIZE:+-DKIM_API_ENABLE_SANITIZE=ON} \
      ${COVERAGE:+-DKIM_API_ENABLE_COVERAGE=ON} \
      ${IWYU:+-DCMAKE_CXX_INCLUDE_WHAT_YOU_USE=include-what-you-use} \
      {$IWYU:+-DCMAKE_C_INCLUDE_WHAT_YOU_USE=include-what-you-use} \
      .. \
  || exit $?
make -j2 install || exit $?
ccache -s

source ${PWD}/test-install/bin/kim-api-activate || exit $?
kim-api-collections-management install system `find ../examples/model-drivers -mindepth 1 -maxdepth 1 -type d` &&
  kim-api-collections-management install system `find ../examples/portable-models -mindepth 1 -maxdepth 1 -type d` &&
  kim-api-collections-management install system `find ../examples/simulator-models -mindepth 1 -maxdepth 1 -type d` &&
  kim-api-collections-management list \
    || exit $?

mkdir "${PWD}/WORKSPACE"
../scripts/run-cmds-from-install IN_WORKSPACE "${PWD}/WORKSPACE" "${PWD}/.." || exit $?

# success
exit 0
