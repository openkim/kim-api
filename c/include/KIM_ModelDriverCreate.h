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
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_MODEL_DRIVER_CREATE_H_
#define KIM_MODEL_DRIVER_CREATE_H_

#ifndef KIM_FUNCTION_TYPES_H_
#include "KIM_FunctionTypes.h"
#endif

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

#ifndef KIM_LANGUAGE_NAME_DEFINED_
#define KIM_LANGUAGE_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_LanguageName KIM_LanguageName;
#endif

#ifndef KIM_NUMBERING_DEFINED_
#define KIM_NUMBERING_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_Numbering KIM_Numbering;
#endif

#ifndef KIM_MODEL_ROUTINE_NAME_DEFINED_
#define KIM_MODEL_ROUTINE_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelRoutineName KIM_ModelRoutineName;
#endif

#ifndef KIM_SPECIES_NAME_DEFINED_
#define KIM_SPECIES_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_SpeciesName KIM_SpeciesName;
#endif

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


#ifndef KIM_MODEL_DRIVER_CREATE_DEFINED_
#define KIM_MODEL_DRIVER_CREATE_DEFINED_
/**
 ** \brief \copybrief KIM::ModelDriverCreate
 **
 ** \sa KIM::ModelDriverCreate
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelDriverCreate KIM_ModelDriverCreate;
#endif


/**
 ** \brief \copybrief KIM::ModelDriverCreate::GetNumberOfParameterFiles
 **
 ** \sa KIM::ModelDriverCreate::GetNumberOfParameterFiles
 **
 ** \since 2.0
 **/
void KIM_ModelDriverCreate_GetNumberOfParameterFiles(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    int * const numberOfParameterFiles);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::GetParameterFileName
 **
 ** \sa KIM::ModelDriverCreate::GetParameterFileName
 **
 ** \since 2.0
 **/
int KIM_ModelDriverCreate_GetParameterFileName(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    int const index,
    char const ** const parameterFileName);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetModelNumbering
 **
 ** \sa KIM::ModelDriverCreate::SetModelNumbering
 **
 ** \since 2.0
 **/
int KIM_ModelDriverCreate_SetModelNumbering(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_Numbering const numbering);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetInfluenceDistancePointer
 **
 ** \sa KIM::ModelDriverCreate::SetInfluenceDistancePointer
 **
 ** \since 2.0
 **/
void KIM_ModelDriverCreate_SetInfluenceDistancePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    double const * const influenceDistance);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetNeighborListPointers
 **
 ** \sa KIM::ModelDriverCreate::SetNeighborListPointers
 **
 ** \since 2.0
 **/
void KIM_ModelDriverCreate_SetNeighborListPointers(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const numberOfNeighborLists,
    double const * const cutoffs,
    int const * const modelWillNotRequestNeighborsOfNoncontributingParticles);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetRoutinePointer
 **
 ** \sa KIM::ModelDriverCreate::SetRoutinePointer
 **
 ** \since 2.0
 **/
int KIM_ModelDriverCreate_SetRoutinePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_ModelRoutineName const modelRoutineName,
    KIM_LanguageName const languageName,
    int const required,
    KIM_Function * const fptr);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetSpeciesCode
 **
 ** \sa KIM::ModelDriverCreate::SetSpeciesCode
 **
 ** \since 2.0
 **/
int KIM_ModelDriverCreate_SetSpeciesCode(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_SpeciesName const speciesName,
    int const code);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetParameterPointer
 **
 ** \sa KIM::ModelDriverCreate::SetParameterPointer
 **
 ** \since 2.0
 **/
int KIM_ModelDriverCreate_SetParameterPointerInteger(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const extent,
    int * const ptr,
    char const * const name,
    char const * const description);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetParameterPointer
 **
 ** \sa KIM::ModelDriverCreate::SetParameterPointer
 **
 ** \since 2.0
 **/
int KIM_ModelDriverCreate_SetParameterPointerDouble(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const extent,
    double * const ptr,
    char const * const name,
    char const * const description);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetModelBufferPointer
 **
 ** \sa KIM::ModelDriverCreate::SetModelBufferPointer
 **
 ** \since 2.0
 **/
void KIM_ModelDriverCreate_SetModelBufferPointer(
    KIM_ModelDriverCreate * const modelDriverCreate, void * const ptr);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetUnits
 **
 ** \sa KIM::ModelDriverCreate::SetUnits
 **
 ** \since 2.0
 **/
int KIM_ModelDriverCreate_SetUnits(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LengthUnit const lengthUnit,
    KIM_EnergyUnit const energyUnit,
    KIM_ChargeUnit const chargeUnit,
    KIM_TemperatureUnit const temperatureUnit,
    KIM_TimeUnit const timeUnit);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::ConvertUnit
 **
 ** \sa KIM::ModelDriverCreate::ConvertUnit
 **
 ** \since 2.0
 **/
int KIM_ModelDriverCreate_ConvertUnit(
    KIM_LengthUnit const fromLengthUnit,
    KIM_EnergyUnit const fromEnergyUnit,
    KIM_ChargeUnit const fromChargeUnit,
    KIM_TemperatureUnit const fromTemperatureUnit,
    KIM_TimeUnit const fromTimeUnit,
    KIM_LengthUnit const toLengthUnit,
    KIM_EnergyUnit const toEnergyUnit,
    KIM_ChargeUnit const toChargeUnit,
    KIM_TemperatureUnit const toTemperatureUnit,
    KIM_TimeUnit const toTimeUnit,
    double const lengthExponent,
    double const energyExponent,
    double const chargeExponent,
    double const temperatureExponent,
    double const timeExponent,
    double * const conversionFactor);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::LogEntry
 **
 ** \sa KIM::ModelDriverCreate::LogEntry
 **
 ** \since 2.0
 **/
void KIM_ModelDriverCreate_LogEntry(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::ToString
 **
 ** \sa KIM::ModelDriverCreate::ToString
 **
 ** \since 2.0
 **/
char const * KIM_ModelDriverCreate_ToString(
    KIM_ModelDriverCreate const * const modelDriverCreate);

#endif /* KIM_MODEL_DRIVER_CREATE_H_ */
