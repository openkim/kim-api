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
# Copyright (c) 2013--2018, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#


cmake_minimum_required(VERSION 3.4)

set(TEST_MODELS
  LennardJones612_UniversalShifted__MO_959249795837_003
  LennardJones_Ar
  ex_model_Ar_P_LJ
  ex_model_Ar_P_MLJ_Fortran
  ex_model_Ar_P_Morse
  ex_model_Ar_P_Morse_07C
  ex_model_Ar_SLJ_MultiCutoff
  )

add_subdirectory(ex_test_Ar_fcc_cluster)
add_subdirectory(ex_test_Ar_fcc_cluster_cpp)
add_subdirectory(ex_test_Ar_fcc_cluster_fortran)
add_subdirectory(utility_forces_numer_deriv)
