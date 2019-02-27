/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2019, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api-v2-2.0.1 package.                */
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
 ** \sa KIM::ModelCreateFunction
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
 ** \sa KIM::ModelDriverCreateFunction
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
 ** \sa KIM::ModelComputeArgumentsCreateFunction
 **
 ** \since 2.0
 **/
typedef int KIM_ModelComputeArgumentsCreateFunction(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate);

/**
 ** \brief \copybrief KIM::ModelComputeFunction
 **
 ** \sa KIM::ModelComputeFunction
 **
 ** \since 2.0
 **/
typedef int KIM_ModelComputeFunction(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArguments const * const modelComputeArguments);

/**
 ** \brief \copybrief KIM::GetNeighborListFunction
 **
 ** \sa KIM::GetNeighborListFunction
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
 ** \sa KIM::ProcessDEDrTermFunction
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
 ** \sa KIM::ProcessD2EDr2TermFunction
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
 ** \sa KIM::ModelExtensionFunction
 **
 ** \since 2.0
 **/
typedef int
KIM_ModelExtensionFunction(KIM_ModelExtension * const modelExtension,
                           void * const extensionStructure);

/**
 ** \brief \copybrief KIM::ModelRefreshFunction
 **
 ** \sa KIM::ModelRefreshFunction
 **
 ** \since 2.0
 **/
typedef int KIM_ModelRefreshFunction(KIM_ModelRefresh * const modelRefresh);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModelFunction
 **
 ** \sa KIM::ModelWriteParameterizedModelFunction
 **
 ** \since 2.0
 **/
typedef int KIM_ModelWriteParameterizedModelFunction(
    KIM_ModelWriteParameterizedModel const * const
        modelWriteParameterizedModel);

/**
 ** \brief \copybrief KIM::ModelComputeArgumentsDestroyFunction
 **
 ** \sa KIM::ModelComputeArgumentsDestroyFunction
 **
 ** \since 2.0
 **/
typedef int KIM_ModelComputeArgumentsDestroyFunction(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy);

/**
 ** \brief \copybrief KIM::ModelDestroyFunction
 **
 ** \sa KIM::ModelDestroyFunction
 **
 ** \since 2.0
 **/
typedef int KIM_ModelDestroyFunction(KIM_ModelDestroy * const modelDestroy);

#endif /* KIM_FUNCTION_TYPES_H_ */
