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
# Copyright (c) 2013--2021, Regents of the University of Minnesota.
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


cmake_minimum_required(VERSION 3.10)

set(_test_models
  LennardJones612_UniversalShifted__MO_959249795837_003
  LennardJones_Ar
  ex_model_Ar_P_LJ
  ex_model_Ar_P_MLJ_Fortran
  ex_model_Ar_P_Morse
  ex_model_Ar_P_Morse_07C
  ex_model_Ar_P_Morse_07C_w_Extensions
  ex_model_Ar_P_Morse_MultiCutoff
  ex_model_Ar_SLJ_MultiCutoff
  )

set(_testing_dir "${PROJECT_BINARY_DIR}/Testing")

function(_add_simulator_model_tests _simulator_name _test_models _test_models_fail_list)
  foreach(_model IN LISTS _test_models)
    set(_test_name "Run_${_simulator_name}_${_model}")
    set(_test_dir "${_testing_dir}/${_test_name}")
    file(MAKE_DIRECTORY "${_test_dir}")
    add_kim_api_test(NAME "${_test_name}"
      COMMAND sh -c "echo ${_model} | $<TARGET_FILE:${_simulator_name}> > stdout"
      WORKING_DIRECTORY "${_test_dir}"
      )
    set_property(TEST ${_test_name}
      APPEND PROPERTY ENVIRONMENT
        "${KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR}=${PROJECT_BINARY_DIR}/examples/model-drivers"
        "${KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR}=${PROJECT_BINARY_DIR}/examples/portable-models"
        "${KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR}=${PROJECT_BINARY_DIR}/examples/simulator-models"
      )
    if("${_model}" IN_LIST _test_models_fail_list)
      set_tests_properties("${_test_name}" PROPERTIES WILL_FAIL TRUE)
    endif()
  endforeach()
endfunction(_add_simulator_model_tests)

function(_add_simulator_tests _simulator_dir _simulator_name)
  set(_test_name "Run_${_simulator_name}")
  set(_test_dir "${_testing_dir}/${_test_name}")
  file(MAKE_DIRECTORY "${_test_dir}")
  add_kim_api_test(NAME "${_test_name}"
    COMMAND sh -c "$<TARGET_FILE:${_simulator_name}> > stdout"
    WORKING_DIRECTORY "${_test_dir}"
    )
  set_property(TEST ${_test_name}
    APPEND PROPERTY ENVIRONMENT
    "${KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR}=${PROJECT_BINARY_DIR}/examples/model-drivers"
    "${KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR}=${PROJECT_BINARY_DIR}/examples/portable-models"
    "${KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR}=${PROJECT_BINARY_DIR}/examples/simulator-models"
    )
endfunction(_add_simulator_tests)

add_subdirectory(collections-example)
_add_simulator_tests(collections-example collections-example)
_add_simulator_tests(collections-example collections-example-c)
_add_simulator_tests(collections-example collections-example-fortran)

add_subdirectory(ex_test_Ar_fcc_cluster)
_add_simulator_model_tests(ex_test_Ar_fcc_cluster "${_test_models}" "ex_model_Ar_P_Morse_MultiCutoff;ex_model_Ar_SLJ_MultiCutoff")

add_subdirectory(ex_test_Ar_fcc_cluster_cpp)
_add_simulator_model_tests(ex_test_Ar_fcc_cluster_cpp "${_test_models}" "ex_model_Ar_P_Morse_MultiCutoff;ex_model_Ar_SLJ_MultiCutoff")

add_subdirectory(ex_test_Ar_fcc_cluster_fortran)
_add_simulator_model_tests(ex_test_Ar_fcc_cluster_fortran "${_test_models}" "ex_model_Ar_P_Morse_MultiCutoff;ex_model_Ar_SLJ_MultiCutoff")

add_subdirectory(simulator-model-example simulator-model-example)
_add_simulator_tests(simulator-model-example simulator-model-example)
_add_simulator_tests(simulator-model-example simulator-model-example-c)
_add_simulator_tests(simulator-model-example simulator-model-example-fortran)

add_subdirectory(utility_forces_numer_deriv)
