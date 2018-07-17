#!/bin/sh -v
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
# Copyright (c) 2018, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

PRMPT=''
REDIRECT_DEV_NULL=/dev/null

${PRMPT} cd ${HOME}
${PRMPT} mkdir kim-api-porting
${PRMPT} cd kim-api-porting

#//! [Setup kim-api v1 and v2]
git clone ~/unison-sync/KIM/git/kim-api kim-api-v1.9.8
#${PRMPT} wget https://s3.openkim.org/kim-api/kim-api-v1.9.8.txz >&${REDIRECT_DEV_NULL}
#${PRMPT} tar Jxf kim-api-v1.9.8.txz
#${PRMPT} rm kim-api-v1.9.8.txz
${PRMPT} cd kim-api-v1.9.8
git checkout -b devel origin/devel
#${PRMPT} ./configure --prefix=${HOME}/kim-api-porting/kim-api-installed
./configure --prefix=${HOME}/kim-api-porting/kim-api-installed --system-arch=32bit LDFLAGS+="-read_only_relocs suppress" CXX=g++-4.8 CC=gcc-4.8 FC=gfortran-4.8 >&${REDIRECT_DEV_NULL}
${PRMPT} make >&${REDIRECT_DEV_NULL}
${PRMPT} make install >&${REDIRECT_DEV_NULL}
${PRMPT} cd ..

git clone ~/unison-sync/KIM/git/kim-api kim-api-v2.0.0-beta.2
#${PRMPT} wget https://s3.openkim.org/kim-api/kim-api-v2.0.0-beta.2.txz >&${REDIRECT_DEV_NULL}
#${PRMPT} tar Jxf kim-api-v2.0.0-beta.2.txz
#${PRMPT} rm kim-api-v2.0.0-beta.2.txz
${PRMPT} cd kim-api-v2.0.0-beta.2
git checkout devel-v2
#${PRMPT} ./configure --prefix=${HOME}/kim-api-porting/kim-api-installed --log-maximum-level=KIM_LOG_VERBOSITY_DEBUG_
./configure --prefix=${HOME}/kim-api-porting/kim-api-installed --system-arch=32bit --log-maximum-level=KIM_LOG_VERBOSITY_DEBUG_ LDFLAGS+="-read_only_relocs suppress" CXX=g++-4.8 CC=gcc-4.8 FC=gfortran-4.8 >&${REDIRECT_DEV_NULL}
${PRMPT} make >&${REDIRECT_DEV_NULL}
${PRMPT} make install >&${REDIRECT_DEV_NULL}
${PRMPT} cd ..
#//! [Setup kim-api v1 and v2]

#//! [Activate kim-api]
${PRMPT} source ${HOME}/kim-api-porting/kim-api-installed/bin/kim-api-v1-activate
${PRMPT} source ${HOME}/kim-api-porting/kim-api-installed/bin/kim-api-v2-activate
#//! [Activate kim-api]

#//! [Copy porting examples]
${PRMPT} cp -r ./kim-api-v2.0.0-beta.2/docs/porting-content-from-v1-to-v2-examples v1-v2-porting
#//! [Copy porting examples]

#//! [Install step0 models]
${PRMPT} kim-api-v1-collections-management install system ./v1-v2-porting/models/step0/ex_model_Ar_P_MLJ_F03

${PRMPT} kim-api-v1-collections-management install system ./v1-v2-porting/models/step0/ex_model_Ar_P_Morse_07C
#//! [Install step0 models]

#//! [Step0 test example]
${PRMPT} cp -r v1-v2-porting/simulators/step0/ex_test_Ar_fcc_cluster ./v1-ex_test_Ar_fcc_cluster
${PRMPT} cd v1-ex_test_Ar_fcc_cluster
${PRMPT} make >&${REDIRECT_DEV_NULL}
${PRMPT} printf ex_model_Ar_P_MLJ_F03 | ./ex_test_Ar_fcc_cluster

${PRMPT} printf ex_model_Ar_P_Morse_07C | ./ex_test_Ar_fcc_cluster

${PRMPT} cd ..
#//! [Step0 test example]



#//! [Install step1 models]
${PRMPT} kim-api-v1-collections-management install system ./v1-v2-porting/models/step1/ex_model_Ar_P_MLJ_F03

${PRMPT} kim-api-v1-collections-management install system ./v1-v2-porting/models/step1/ex_model_Ar_P_Morse_07C
#//! [Install step1 models]

#//! [Collections v1 list]
${PRMPT} kim-api-v1-collections-management list
#//! [Collections v1 list]

#//! [Step1 test example]
${PRMPT} cd v1-ex_test_Ar_fcc_cluster
${PRMPT} printf ex_model_Ar_P_MLJ_F03_step1 | ./ex_test_Ar_fcc_cluster

${PRMPT} cd ..
#//! [Step1 test example]

#//! [Install step3 models]
${PRMPT} kim-api-v2-collections-management install system ./v1-v2-porting/models/step3/ex_model_Ar_P_MLJ_F03

${PRMPT} kim-api-v2-collections-management install system ./v1-v2-porting/models/step3/ex_model_Ar_P_Morse_07C
#//! [Install step3 models]

#//! [Step3 test setup]
${PRMPT} cp -r v1-v2-porting/simulators/step5/ex_test_Ar_fcc_cluster v2-ex_test_Ar_fcc_cluster
${PRMPT} cd v2-ex_test_Ar_fcc_cluster
${PRMPT} make >&${REDIRECT_DEV_NULL}
#//! [Step3 test setup]
#//! [Step3 test example]
${PRMPT} printf ex_model_Ar_P_Morse_07C_step3 | ./ex_test_Ar_fcc_cluster

${PRMPT} cd ..
#//! [Step3 test example]

#//! [Install step4 models]
${PRMPT} kim-api-v2-collections-management install --force system ./v1-v2-porting/models/step4/ex_model_Ar_P_MLJ_F03

${PRMPT} kim-api-v2-collections-management install --force system ./v1-v2-porting/models/step4/ex_model_Ar_P_Morse_07C
#//! [Install step4 models]

#//! [Step4 test example]
${PRMPT} cd v2-ex_test_Ar_fcc_cluster
${PRMPT} printf ex_model_Ar_P_Morse_07C_step4 | ./ex_test_Ar_fcc_cluster

${PRMPT} cd ..
#//! [Step4 test example]

#//! [Step5 install models]
${PRMPT} kim-api-v2-collections-management install --force system ./v1-v2-porting/models/step5/ex_model_Ar_P_MLJ_F03

${PRMPT} kim-api-v2-collections-management install --force system ./v1-v2-porting/models/step5/ex_model_Ar_P_Morse_07C
#//! [Step5 install models]

#//! [Step6 install models]
${PRMPT} kim-api-v2-collections-management install --force system ./v1-v2-porting/models/step6/ex_model_Ar_P_MLJ_F03

${PRMPT} kim-api-v2-collections-management install --force system ./v1-v2-porting/models/step6/ex_model_Ar_P_Morse_07C
#//! [Step6 install models]

#//! [Step7 test example]
${PRMPT} cd v2-ex_test_Ar_fcc_cluster
${PRMPT} printf ex_model_Ar_P_MLJ_F03 | ./ex_test_Ar_fcc_cluster

${PRMPT} printf ex_model_Ar_P_Morse_07C | ./ex_test_Ar_fcc_cluster

${PRMPT} cd ..
#//! [Step7 test example]


#//! [Copy and run v1 simulator]
${PRMPT} cp -r ./v1-v2-porting/simulators/step1/ex_test_Ar_fcc_cluster v1-ex_test_Ar_fcc_cluster-step1
${PRMPT} cd v1-ex_test_Ar_fcc_cluster-step1
${PRMPT} make >&${REDIRECT_DEV_NULL}
${PRMPT} printf ex_model_Ar_P_MLJ_F03 | ./ex_test_Ar_fcc_cluster

${PRMPT} cd ..
#//! [Copy and run v1 simulator]


#//! [Copy v2 simulator]
${PRMPT} cp -r ./v1-v2-porting/simulators/step5/ex_test_Ar_fcc_cluster v2-ex_test_Ar_fcc_cluster
#//! [Copy v2 simulator]

#//! [Run v2 simulator]
${PRMPT} cd v2-ex_test_Ar_fcc_cluster
${PRMPT} make >&${REDIRECT_DEV_NULL}
${PRMPT} printf ex_model_Ar_P_MLJ_F03 | ./ex_test_Ar_fcc_cluster

${PRMPT} cd ..
#//! [Run v2 simulator]

#//! [Copy v2 cpp and fortran simulators]
${PRMPT} cp -r ./v1-v2-porting/simulators/step5/ex_test_Ar_fcc_cluster_cpp v2-ex_test_Ar_fcc_cluster_cpp
${PRMPT} cp -r ./v1-v2-porting/simulators/step5/ex_test_Ar_fcc_cluster_fortran v2-ex_test_Ar_fcc_cluster_fortran
#//! [Copy v2 cpp and fortran simulators]

#//! [Run cpp and fortran simulators]
${PRMPT} cd v2-ex_test_Ar_fcc_cluster_cpp
${PRMPT} make >&${REDIRECT_DEV_NULL}
${PRMPT} printf ex_model_Ar_P_Morse_07C | ./ex_test_Ar_fcc_cluster_cpp

${PRMPT} cd ..
${PRMPT} cd v2-ex_test_Ar_fcc_cluster_fortran
${PRMPT} make >&${REDIRECT_DEV_NULL}
${PRMPT} printf ex_model_Ar_P_Morse_07C | ./ex_test_Ar_fcc_cluster_fortran

${PRMPT} cd ..
#//! [Run cpp and fortran simulators]
