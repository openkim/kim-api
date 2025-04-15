//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api-2.4.1 package.
//


#ifndef KIM_FUNCTION_TYPES_HPP_
#define KIM_FUNCTION_TYPES_HPP_

#include <string>

namespace KIM
{
// Forward declarations
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
class ModelCreate;
class ModelDriverCreate;
class ModelCompute;
class ModelExtension;
class ModelComputeArgumentsCreate;
class ModelComputeArguments;
class ModelRefresh;
class ModelWriteParameterizedModel;
class ModelComputeArgumentsDestroy;
class ModelDestroy;

/// \brief Generic function type.
///
/// \sa KIM_Function
///
/// \since 2.0
typedef void(Function)(void);  // Generic function pointer

/// \brief Prototype for MODEL_ROUTINE_NAME::Create routine.
///
/// \sa KIM_ModelCreateFunction, kim_model_module::kim_model_create
///
/// \since 2.0
typedef int ModelCreateFunction(ModelCreate * const modelCreate,
                                LengthUnit const requestedLengthUnit,
                                EnergyUnit const requestedEnergyUnit,
                                ChargeUnit const requestedChargeUnit,
                                TemperatureUnit const requestedTemperatureUnit,
                                TimeUnit const requestedTimeUnit);

/// \brief Prototype for MODEL_ROUTINE_NAME::Create routine.
///
/// \sa KIM_ModelDriverCreateFunction, kim_model_module::kim_model_create
///
/// \since 2.0
typedef int
ModelDriverCreateFunction(ModelDriverCreate * const modelDriverCreate,
                          LengthUnit const requestedLengthUnit,
                          EnergyUnit const requestedEnergyUnit,
                          ChargeUnit const requestedChargeUnit,
                          TemperatureUnit const requestedTemperatureUnit,
                          TimeUnit const requestedTimeUnit);

/// \brief Prototype for MODEL_ROUTINE_NAME::ComputeArgumentsCreate
/// routine.
///
/// \sa KIM_ModelComputeArgumentsCreateFunction,
/// kim_model_module::kim_model_compute_arguments_create
///
/// \since 2.0
typedef int ModelComputeArgumentsCreateFunction(
    ModelCompute const * const modelCompute,
    ModelComputeArgumentsCreate * const modelComputeArgumentsCreate);

/// \brief Prototype for MODEL_ROUTINE_NAME::Compute routine.
///
/// \sa KIM_ModelComputeFunction, kim_model_module::kim_model_compute
///
/// \since 2.0
typedef int ModelComputeFunction(
    ModelCompute const * const modelCompute,
    ModelComputeArguments const * const modelComputeArgumentsCreate);

/// \brief Prototype for COMPUTE_CALLBACK_NAME::GetNeighborList routine.
///
/// \sa KIM_GetNeighborListFunction, kim_model_compute_arguments_module::<!--
/// -->kim_model_compute_arguments_get_neighbor_list
///
/// \since 2.0
typedef int GetNeighborListFunction(void * const dataObject,
                                    int const numberOfNeighborLists,
                                    double const * const cutoffs,
                                    int const neighborListIndex,
                                    int const particleNumber,
                                    int * const numberOfNeighbors,
                                    int const ** const neighborsOfParticle);

/// \brief Prototype for COMPUTE_CALLBACK_NAME::ProcessDEDrTerm
/// routine.
///
/// \sa KIM_ProcessDEDrTermFunction, kim_model_compute_arguments_module::<!--
/// -->kim_model_compute_arguments_process_dedr_term
///
/// \since 2.0
typedef int ProcessDEDrTermFunction(void * const dataObject,
                                    double const de,
                                    double const r,
                                    double const * const dx,
                                    int const i,
                                    int const j);

/// \brief Prototype for COMPUTE_CALLBACK_NAME::ProcessD2EDr2Term
/// routine.
///
/// \sa KIM_ProcessD2EDr2TermFunction, kim_model_compute_arguments_module::<!--
/// -->kim_model_compute_arguments_process_d2edr2_term
///
///
/// \since 2.0
typedef int ProcessD2EDr2TermFunction(void * const dataObject,
                                      double const de,
                                      double const * const r,
                                      double const * const dx,
                                      int const * const i,
                                      int const * const j);

/// \brief Prototype for MODEL_ROUTINE_NAME::Extension routine.
///
/// \sa KIM_ModelExtensionFunction, kim_model_module::kim_model_extension
///
/// \since 2.0
typedef int ModelExtensionFunction(ModelExtension * const modelExtension,
                                   void * const extensionStructure);

/// \brief Prototype for MODEL_ROUTINE_NAME::Refresh routine.
///
/// \sa KIM_ModelRefreshFunction,
/// kim_model_module::kim_model_clear_then_refresh
///
/// \since 2.0
typedef int ModelRefreshFunction(ModelRefresh * const modelRefresh);

/// \brief Prototype for MODEL_ROUTINE_NAME::WriteParameterizedModel routine.
///
/// \sa KIM_ModelWriteParameterizedModelFunction,
/// kim_model_module::kim_model_write_parameterized_model
///
/// \since 2.0
typedef int ModelWriteParameterizedModelFunction(
    ModelWriteParameterizedModel const * const modelWriteParameterizedModel);

/// \brief Prototype for MODEL_ROUTINE_NAME::ComputeArgumentsDestroy
/// routine.
///
/// \sa KIM_ModelComputeArgumentsDestroyFunction,
/// kim_model_module::kim_model_compute_arguments_destroy
///
/// \since 2.0
typedef int ModelComputeArgumentsDestroyFunction(
    ModelCompute const * const modelCompute,
    ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy);

/// \brief Prototype for MODEL_ROUTINE_NAME::Destroy routine.
///
/// \sa KIM_ModelDestroyFunction, kim_model_module::kim_model_destroy
///
/// \since 2.0
typedef int ModelDestroyFunction(ModelDestroy * const modelDestroy);

/// \brief Prototype for Log PrintFunction routine.
///
/// \sa KIM_LogPrintFunction,
/// kim_log_module::kim_log_push_default_print_function
///
/// \since 2.2
typedef int LogPrintFunction(std::string const & entryString);
}  // namespace KIM

#endif  // KIM_FUNCTION_TYPES_HPP_
