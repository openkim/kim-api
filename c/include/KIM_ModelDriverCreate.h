/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2021, Regents of the University of Minnesota.          */
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
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_MODEL_DRIVER_CREATE_H_
#define KIM_MODEL_DRIVER_CREATE_H_

#ifndef KIM_FUNCTION_TYPES_H_
#include "KIM_FunctionTypes.h" /* IWYU pragma: export */
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
 ** \sa KIM::ModelDriverCreate,
 ** kim_model_driver_create_module::kim_model_driver_create_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelDriverCreate KIM_ModelDriverCreate;
#endif


/**
 ** \brief \copybrief KIM::ModelDriverCreate::GetParameterFileDirectoryName
 **
 ** \sa KIM::ModelDriverCreate::GetParameterFileDirectoryName,
 ** kim_model_driver_create_module::kim_get_parameter_file_directory_name
 **
 ** \since 2.2
 **/
void KIM_ModelDriverCreate_GetParameterFileDirectoryName(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    char const ** const directoryName);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::GetNumberOfParameterFiles
 **
 ** \sa KIM::ModelDriverCreate::GetNumberOfParameterFiles,
 ** kim_model_driver_create_module::kim_get_number_of_parameter_files
 **
 ** \since 2.0
 **/
void KIM_ModelDriverCreate_GetNumberOfParameterFiles(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    int * const numberOfParameterFiles);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::GetParameterFileName
 **
 ** \sa KIM::ModelDriverCreate::GetParameterFileName,
 ** kim_model_driver_create_module::kim_get_parameter_file_name
 **
 ** \since 2.0
 **
 ** \deprecated As of 2.2.  Please use
 ** KIM_ModelDriverCreate_GetParameterFileBasename() instead.
 **/
int KIM_ModelDriverCreate_GetParameterFileName(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    int const index,
    char const ** const parameterFileName);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::GetParameterFileBasename
 **
 ** \sa KIM::ModelDriverCreate::GetParameterFileBasename,
 ** kim_model_driver_create_module::kim_get_parameter_file_basename
 **
 ** \since 2.2
 **/
int KIM_ModelDriverCreate_GetParameterFileBasename(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    int const index,
    char const ** const parameterFileBasename);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetModelNumbering
 **
 ** \sa KIM::ModelDriverCreate::SetModelNumbering,
 ** kim_model_driver_create_module::kim_set_model_numbering
 **
 ** \since 2.0
 **/
int KIM_ModelDriverCreate_SetModelNumbering(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_Numbering const numbering);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetInfluenceDistancePointer
 **
 ** \sa KIM::ModelDriverCreate::SetInfluenceDistancePointer,
 ** kim_model_driver_create_module::kim_set_influence_distance_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelDriverCreate_SetInfluenceDistancePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    double const * const influenceDistance);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetNeighborListPointers
 **
 ** \sa KIM::ModelDriverCreate::SetNeighborListPointers,
 ** kim_model_driver_create_module::kim_set_neighbor_list_pointers
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
 ** \sa KIM::ModelDriverCreate::SetRoutinePointer,
 ** kim_model_driver_create_module::kim_set_routine_pointer
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
 ** \sa KIM::ModelDriverCreate::SetSpeciesCode,
 ** kim_model_driver_create_module::kim_set_species_code
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
 ** \sa KIM::ModelDriverCreate::SetParameterPointer,
 ** kim_model_driver_create_module::kim_set_parameter_pointer
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
 ** \sa KIM::ModelDriverCreate::SetParameterPointer,
 ** kim_model_driver_create_module::kim_set_parameter_pointer
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
 ** \sa KIM::ModelDriverCreate::SetModelBufferPointer,
 ** kim_model_driver_create_module::kim_set_model_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelDriverCreate_SetModelBufferPointer(
    KIM_ModelDriverCreate * const modelDriverCreate, void * const ptr);

/**
 ** \brief \copybrief KIM::ModelDriverCreate::SetUnits
 **
 ** \sa KIM::ModelDriverCreate::SetUnits,
 ** kim_model_driver_create_module::kim_set_units
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
 ** \sa KIM::ModelDriverCreate::ConvertUnit,
 ** kim_model_driver_create_module::kim_convert_unit
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
 ** \sa KIM::ModelDriverCreate::LogEntry,
 ** kim_model_driver_create_module::kim_log_entry
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
 ** \sa KIM::ModelDriverCreate::ToString,
 ** kim_model_driver_create_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_ModelDriverCreate_ToString(
    KIM_ModelDriverCreate const * const modelDriverCreate);

#endif /* KIM_MODEL_DRIVER_CREATE_H_ */
