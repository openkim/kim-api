/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2022, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */
/* SPDX-License-Identifier: LGPL-2.1-or-later                                 */
/*                                                                            */
/* This library is free software; you can redistribute it and/or              */
/* modify it under the terms of the GNU Lesser General Public                 */
/* License as published by the Free Software Foundation; either               */
/* version 2.1 of the License, or (at your option) any later version.         */
/*                                                                            */
/* This library is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          */
/* Lesser General Public License for more details.                            */
/*                                                                            */
/* You should have received a copy of the GNU Lesser General Public License   */
/* along with this library; if not, write to the Free Software Foundation,    */
/* Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api-2.3.0 package.                   */
/*                                                                            */


#ifndef KIM_FUNCTION_TYPES_H_
#define KIM_FUNCTION_TYPES_H_

/* Forward declarations */
#ifndef KIM_LENGTH_UNIT_DEFINED_
#define KIM_LENGTH_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_LengthUnit KIM_LengthUnit;
#endif

#ifndef KIM_ENERGY_UNIT_DEFINED_
#define KIM_ENERGY_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_EnergyUnit KIM_EnergyUnit;
#endif

#ifndef KIM_CHARGE_UNIT_DEFINED_
#define KIM_CHARGE_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ChargeUnit KIM_ChargeUnit;
#endif

#ifndef KIM_TEMPERATURE_UNIT_DEFINED_
#define KIM_TEMPERATURE_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_TemperatureUnit KIM_TemperatureUnit;
#endif

#ifndef KIM_TIME_UNIT_DEFINED_
#define KIM_TIME_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_TimeUnit KIM_TimeUnit;
#endif

#ifndef KIM_MODEL_CREATE_DEFINED_
#define KIM_MODEL_CREATE_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelCreate KIM_ModelCreate;
#endif

#ifndef KIM_MODEL_DRIVER_CREATE_DEFINED_
#define KIM_MODEL_DRIVER_CREATE_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelDriverCreate KIM_ModelDriverCreate;
#endif

#ifndef KIM_MODEL_COMPUTE_DEFINED_
#define KIM_MODEL_COMPUTE_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelCompute KIM_ModelCompute;
#endif

#ifndef KIM_MODEL_EXTENSION_DEFINED_
#define KIM_MODEL_EXTENSION_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelExtension KIM_ModelExtension;
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelComputeArgumentsCreate KIM_ModelComputeArgumentsCreate;
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelComputeArguments KIM_ModelComputeArguments;
#endif

#ifndef KIM_MODEL_REFRESH_DEFINED_
#define KIM_MODEL_REFRESH_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelRefresh KIM_ModelRefresh;
#endif

#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_DEFINED_
#define KIM_MODEL_WRITE_PARAMETERIZED_MODEL_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelWriteParameterizedModel
    KIM_ModelWriteParameterizedModel;
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelComputeArgumentsDestroy
    KIM_ModelComputeArgumentsDestroy;
#endif

#ifndef KIM_MODEL_DESTROY_DEFINED_
#define KIM_MODEL_DESTROY_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelDestroy KIM_ModelDestroy;
#endif


/**
 ** \brief \copybrief KIM::Function
 **
 ** \sa KIM::Function
 **
 ** \since 2.0
 **/
typedef void(KIM_Function)(void); /* Generic function pointer */

/**
 ** \brief \copybrief KIM::ModelCreateFunction
 **
 ** \sa KIM::ModelCreateFunction, kim_model_module::kim_model_create
 **
 ** \since 2.0
 **/
typedef int
KIM_ModelCreateFunction(KIM_ModelCreate * const modelCreate,
                        KIM_LengthUnit const requestedLengthUnit,
                        KIM_EnergyUnit const requestedEnergyUnit,
                        KIM_ChargeUnit const requestedChargeUnit,
                        KIM_TemperatureUnit const requestedTemperatureUnit,
                        KIM_TimeUnit const requestedTimeUnit);

/**
 ** \brief \copybrief KIM::ModelDriverCreateFunction
 **
 ** \sa KIM::ModelDriverCreateFunction, kim_model_module::kim_model_create
 **
 ** \since 2.0
 **/
typedef int KIM_ModelDriverCreateFunction(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LengthUnit const requestedLengthUnit,
    KIM_EnergyUnit const requestedEnergyUnit,
    KIM_ChargeUnit const requestedChargeUnit,
    KIM_TemperatureUnit const requestedTemperatureUnit,
    KIM_TimeUnit const requestedTimeUnit);

/**
 ** \brief \copybrief KIM::ModelComputeArgumentsCreateFunction
 **
 ** \sa KIM::ModelComputeArgumentsCreateFunction,
 ** kim_model_module::kim_model_compute_arguments_create
 **
 ** \since 2.0
 **/
typedef int KIM_ModelComputeArgumentsCreateFunction(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate);

/**
 ** \brief \copybrief KIM::ModelComputeFunction
 **
 ** \sa KIM::ModelComputeFunction, kim_model_module::kim_model_compute
 **
 ** \since 2.0
 **/
typedef int KIM_ModelComputeFunction(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArguments const * const modelComputeArguments);

/**
 ** \brief \copybrief KIM::GetNeighborListFunction
 **
 ** \sa KIM::GetNeighborListFunction, kim_model_compute_arguments_module::<!--
 ** -->kim_model_compute_arguments_get_neighbor_list
 **
 ** \since 2.0
 **/
typedef int KIM_GetNeighborListFunction(void * const dataObject,
                                        int const numberOfNeighborLists,
                                        double const * const cutoffs,
                                        int const neighborListIndex,
                                        int const particleNumber,
                                        int * const numberOfNeighbors,
                                        int const ** const neighborsOfParticle);

/**
 ** \brief \copybrief KIM::ProcessDEDrTermFunction
 **
 ** \sa KIM::ProcessDEDrTermFunction, kim_model_compute_arguments_module::<!--
 ** -->kim_model_compute_arguments_process_dedr_term
 **
 ** \since 2.0
 **/
typedef int KIM_ProcessDEDrTermFunction(void * const dataObject,
                                        double const de,
                                        double const r,
                                        double const * const dx,
                                        int const i,
                                        int const j);

/**
 ** \brief \copybrief KIM::ProcessD2EDr2TermFunction
 **
 ** \sa KIM::ProcessD2EDr2TermFunction,
 ** kim_model_compute_arguments_module::<!--
 ** -->kim_model_compute_arguments_process_d2edr2_term
 **
 ** \since 2.0
 **/
typedef int KIM_ProcessD2EDr2TermFunction(void * const dataObject,
                                          double const de,
                                          double const * const r,
                                          double const * const dx,
                                          int const * const i,
                                          int const * const j);

/**
 ** \brief \copybrief KIM::ModelExtensionFunction
 **
 ** \sa KIM::ModelExtensionFunction, kim_model_module::kim_model_extension
 **
 ** \since 2.0
 **/
typedef int
KIM_ModelExtensionFunction(KIM_ModelExtension * const modelExtension,
                           void * const extensionStructure);

/**
 ** \brief \copybrief KIM::ModelRefreshFunction
 **
 ** \sa KIM::ModelRefreshFunction,
 ** kim_model_module::kim_model_clear_then_refresh
 **
 ** \since 2.0
 **/
typedef int KIM_ModelRefreshFunction(KIM_ModelRefresh * const modelRefresh);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModelFunction
 **
 ** \sa KIM::ModelWriteParameterizedModelFunction, kim_model_module::<!--
 ** kim_model_write_parameterized_model_function
 **
 ** \since 2.0
 **/
typedef int KIM_ModelWriteParameterizedModelFunction(
    KIM_ModelWriteParameterizedModel const * const
        modelWriteParameterizedModel);

/**
 ** \brief \copybrief KIM::ModelComputeArgumentsDestroyFunction
 **
 ** \sa KIM::ModelComputeArgumentsDestroyFunction, kim_model_module::<!--
 ** -->kim_model_compute_arguments_destroy
 **
 ** \since 2.0
 **/
typedef int KIM_ModelComputeArgumentsDestroyFunction(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy);

/**
 ** \brief \copybrief KIM::ModelDestroyFunction
 **
 ** \sa KIM::ModelDestroyFunction, kim_model_module::kim_model_destroy
 **
 ** \since 2.0
 **/
typedef int KIM_ModelDestroyFunction(KIM_ModelDestroy * const modelDestroy);

/**
 ** \brief \copybrief KIM::LogPrintFunction
 **
 ** \sa KIM::LogPrintFunction,
 ** kim_log_module::kim_log_push_default_print_function
 **
 ** \since 2.2
 **/
typedef int KIM_LogPrintFunction(char const * const entryString);

#endif /* KIM_FUNCTION_TYPES_H_ */
