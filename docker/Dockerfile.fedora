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
#    Christoph Junghans
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api-2.0.2 package.
#


FROM fedora:latest

ARG COVERAGE
ARG SANITIZE
ARG CC
ARG CXX
ARG CXXFLAGS
ARG FC
ARG CMAKE_BUILD_TYPE

#for coverage
ARG CI
ARG TRAVIS
ARG TRAVIS_BRANCH
ARG TRAVIS_JOB_NUMBER
ARG TRAVIS_PULL_REQUEST
ARG TRAVIS_JOB_ID
ARG TRAVIS_TAG
ARG TRAVIS_REPO_SLUG
ARG TRAVIS_COMMIT
ARG TRAVIS_OS_NAME

RUN dnf -y install make cmake git gcc-c++ gcc-gfortran ccache wget vim-common \
                   findutils ${SANITIZE:+libasan}
RUN wget -O /usr/bin/codecov https://codecov.io/bash
RUN chmod +x /usr/bin/codecov

RUN useradd -m kim
USER kim
ENV PATH=/usr/lib64/ccache:${PATH}
ENV CCACHE_MAXSIZE=250M

COPY kim-api/ /home/kim/kim-api
RUN rm -rf /home/kim/.ccache
COPY ccache/ /home/kim/.ccache
USER root
RUN chown -R kim:kim /home/kim/kim-api /home/kim/.ccache
USER kim

RUN env

WORKDIR /home/kim/kim-api
RUN mkdir build
WORKDIR build
RUN ccache -z
RUN cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} \
          -DCMAKE_INSTALL_PREFIX=/usr \
          ${COVERAGE:+-DKIM_API_ENABLE_COVERAGE=ON} \
          ${SANITIZE:+-DKIM_API_ENABLE_SANITIZE=ON} \
  ..
RUN make -j2
RUN ccache -s
RUN make test
RUN make install DESTDIR=${PWD}/destdir && \
    rm -rf ${PWD}/destdir/usr/{bin,include,lib64,libexec,share} && \
    rm -rf ${PWD}/destdir/etc && \
    rmdir ${PWD}/destdir/usr && \
    rmdir ${PWD}/destdir
RUN if [ ${COVERAGE} ]; then \
  if [ ${CC} = clang ]; then \
    codecov -R ${PWD}/../../${TRAVIS_REPO_SLUG#*/} -F "${CC}" -x "llvm-cov gcov" 2>&1 | grep -v "arcs.*block"; \
  else \
    codecov -R ${PWD}/../../${TRAVIS_REPO_SLUG#*/} -F "${CC}" 2>&1 | grep -v "arcs.*block"; \
  fi; \
fi
USER root
RUN make install
USER kim
WORKDIR /home/kim/kim-api
RUN rm -rf build
RUN mkdir build
WORKDIR build
RUN cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} \
          -DCMAKE_INSTALL_PREFIX=${PWD}/test-install \
          -DKIM_API_BUILD_EXAMPLES=OFF \
          ${COVERAGE:+-DKIM_API_ENABLE_COVERAGE=ON} \
          ${SANITIZE:+-DKIM_API_ENABLE_SANITIZE=ON} \
  ..
RUN make -j2 install
RUN ccache -s
RUN source ./test-install/bin/kim-api-activate && \
    kim-api-collections-management install system `find ../examples/model-drivers -mindepth 1 -maxdepth 1 -type d` && \
    kim-api-collections-management install system `find ../examples/models -mindepth 1 -maxdepth 1 -type d` && \
    kim-api-collections-management install system `find ../examples/simulator-models -mindepth 1 -maxdepth 1 -type d` && \
    kim-api-collections-management list
