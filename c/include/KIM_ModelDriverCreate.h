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
/* Copyright (c) 2016--2018, Regents of the University of Minnesota.          */
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

#ifndef KIM_FUNC_H_
#include "KIM_func.h"
#endif

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

#ifndef KIM_LANGUAGE_NAME_DEFINED_
#define KIM_LANGUAGE_NAME_DEFINED_
typedef struct KIM_LanguageName KIM_LanguageName;
#endif

#ifndef KIM_NUMBERING_DEFINED_
#define KIM_NUMBERING_DEFINED_
typedef struct KIM_Numbering KIM_Numbering;
#endif

#ifndef KIM_SPECIES_NAME_DEFINED_
#define KIM_SPECIES_NAME_DEFINED_
typedef struct KIM_SpeciesName KIM_SpeciesName;
#endif

#ifndef KIM_SUPPORT_STATUS_DEFINED_
#define KIM_SUPPORT_STATUS_DEFINED_
typedef struct KIM_SupportStatus KIM_SupportStatus;
#endif

#ifndef KIM_LENGTH_UNIT_DEFINED_
#define KIM_LENGTH_UNIT_DEFINED_
typedef struct KIM_LengthUnit KIM_LengthUnit;
#endif

#ifndef KIM_ENERGY_UNIT_DEFINED_
#define KIM_ENERGY_UNIT_DEFINED_
typedef struct KIM_EnergyUnit KIM_EnergyUnit;
#endif

#ifndef KIM_CHARGE_UNIT_DEFINED_
#define KIM_CHARGE_UNIT_DEFINED_
typedef struct KIM_ChargeUnit KIM_ChargeUnit;
#endif

#ifndef KIM_TEMPERATURE_UNIT_DEFINED_
#define KIM_TEMPERATURE_UNIT_DEFINED_
typedef struct KIM_TemperatureUnit KIM_TemperatureUnit;
#endif

#ifndef KIM_TIME_UNIT_DEFINED_
#define KIM_TIME_UNIT_DEFINED_
typedef struct KIM_TimeUnit KIM_TimeUnit;
#endif


struct KIM_ModelDriverCreate;

#ifndef KIM_MODEL_DRIVER_CREATE_DEFINED_
#define KIM_MODEL_DRIVER_CREATE_DEFINED_
typedef struct KIM_ModelDriverCreate KIM_ModelDriverCreate;
#endif


void KIM_ModelDriverCreate_GetNumberOfParameterFiles(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int * const numberOfParameterFiles);

int KIM_ModelDriverCreate_GetParameterFileName(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const index, char const ** const parameterFileName);

int KIM_ModelDriverCreate_SetModelNumbering(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_Numbering const numbering);

void KIM_ModelDriverCreate_SetInfluenceDistancePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    double * const influenceDistance);

void KIM_ModelDriverCreate_SetNeighborListPointers(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const numberOfNeighborLists,
    double const * const cutoffs,
    int const * const modelWillNotRequestNeighborsOfNoncontributingParticles);

int KIM_ModelDriverCreate_SetRefreshPointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr);
int KIM_ModelDriverCreate_SetDestroyPointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr);
int KIM_ModelDriverCreate_SetComputeArgumentsCreatePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr);
int KIM_ModelDriverCreate_SetComputeArgumentsDestroyPointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr);
int KIM_ModelDriverCreate_SetComputePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr);

int KIM_ModelDriverCreate_SetSpeciesCode(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_SpeciesName const speciesName, int const code);

int KIM_ModelDriverCreate_SetParameterPointerInteger(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const extent, int * const ptr, char const * const description);
int KIM_ModelDriverCreate_SetParameterPointerDouble(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const extent, double * const ptr, char const * const description);

void KIM_ModelDriverCreate_SetModelBufferPointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    void * const ptr);

int KIM_ModelDriverCreate_SetUnits(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LengthUnit const lengthUnit,
    KIM_EnergyUnit const energyUnit,
    KIM_ChargeUnit const chargeUnit,
    KIM_TemperatureUnit const temperatureUnit,
    KIM_TimeUnit const timeUnit);

int KIM_ModelDriverCreate_ConvertUnit(
    KIM_ModelDriverCreate const * const modelDriverCreate,
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

void KIM_ModelDriverCreate_LogEntry(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName);

char const * KIM_ModelDriverCreate_String(
    KIM_ModelDriverCreate const * const modelDriverCreate);

#endif  /* KIM_MODEL_DRIVE_CREATE_H_ */
