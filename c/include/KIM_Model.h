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
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_MODEL_H_
#define KIM_MODEL_H_

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

#ifndef KIM_DATA_TYPE_DEFINED_
#define KIM_DATA_TYPE_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_DataType KIM_DataType;
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

#ifndef KIM_NUMBERING_DEFINED_
#define KIM_NUMBERING_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_Numbering KIM_Numbering;
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

#ifndef KIM_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_COMPUTE_ARGUMENTS_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ComputeArguments KIM_ComputeArguments;
#endif


#ifndef KIM_MODEL_DEFINED_
#define KIM_MODEL_DEFINED_
/**
 ** \brief \copybrief KIM::Model
 **
 ** \sa KIM::Model, kim_model_module::kim_model_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_Model KIM_Model;
#endif

/**
 ** \brief \copybrief KIM::Model::Create
 **
 ** \sa KIM::Model::Create, kim_model_module::kim_model_create
 **
 ** \since 2.0
 **/
int KIM_Model_Create(KIM_Numbering const numbering,
                     KIM_LengthUnit const requestedLengthUnit,
                     KIM_EnergyUnit const requestedEnergyUnit,
                     KIM_ChargeUnit const requestedChargeUnit,
                     KIM_TemperatureUnit const requestedTemperatureUnit,
                     KIM_TimeUnit const requestedTimeUnit,
                     char const * const modelName,
                     int * const requestedUnitsAccepted,
                     KIM_Model ** const model);

/**
 ** \brief \copybrief KIM::Model::Destroy
 **
 ** \sa KIM::Model::Destroy, kim_model_module::kim_model_destroy
 **
 ** \since 2.0
 **/
void KIM_Model_Destroy(KIM_Model ** const model);

/**
 ** \brief \copybrief KIM::Model::IsRoutinePresent
 **
 ** \sa KIM::Model::IsRoutinePresent, kim_model_module::kim_is_routine_present
 **
 ** \since 2.0
 **/
int KIM_Model_IsRoutinePresent(KIM_Model const * const model,
                               KIM_ModelRoutineName const modelRoutineName,
                               int * const present,
                               int * const required);

/**
 ** \brief \copybrief KIM::Model::GetInfluenceDistance
 **
 ** \sa KIM::Model::GetInfluenceDistance,
 ** kim_model_module::kim_get_influence_distance
 **
 ** \since 2.0
 **/
void KIM_Model_GetInfluenceDistance(KIM_Model const * const model,
                                    double * const influenceDistance);

/**
 ** \brief \copybrief KIM::Model::GetNeighborListPointers
 **
 ** \sa KIM::Model::GetNeighborListPointers,
 ** kim_model_module::kim_get_number_of_neighbor_lists,
 ** kim_model_module::kim_get_neighbor_list_values
 **
 ** \since 2.0
 **/
void KIM_Model_GetNeighborListPointers(
    KIM_Model const * const model,
    int * const numberOfNeighborLists,
    double const ** const cutoffs,
    int const ** const modelWillNotRequestNeighborsOfNoncontributingParticles);

/**
 ** \brief \copybrief KIM::Model::GetUnits
 **
 ** \sa KIM::Model::GetUnits, kim_model_module::kim_get_units
 **
 ** \since 2.0
 **/
void KIM_Model_GetUnits(KIM_Model const * const model,
                        KIM_LengthUnit * const lengthUnit,
                        KIM_EnergyUnit * const energyUnit,
                        KIM_ChargeUnit * const chargeUnit,
                        KIM_TemperatureUnit * const temperatureUnit,
                        KIM_TimeUnit * const timeUnit);

/**
 ** \brief \copybrief KIM::Model::ComputeArgumentsCreate
 **
 ** \sa KIM::Model::ComputeArgumentsCreate,
 ** kim_model_module::kim_compute_arguments_create
 **
 ** \since 2.0
 **/
int KIM_Model_ComputeArgumentsCreate(
    KIM_Model const * const model,
    KIM_ComputeArguments ** const computeArguments);

/**
 ** \brief \copybrief KIM::Model::ComputeArgumentsDestroy
 **
 ** \sa KIM::Model::ComputeArgumentsDestroy,
 ** kim_model_module::kim_compute_arguments_destroy
 **
 ** \since 2.0
 **/
int KIM_Model_ComputeArgumentsDestroy(
    KIM_Model const * const model,
    KIM_ComputeArguments ** const computeArguments);

/**
 ** \brief \copybrief KIM::Model::Compute
 **
 ** \sa KIM::Model::Compute, kim_model_module::kim_compute
 **
 ** \since 2.0
 **/
int KIM_Model_Compute(KIM_Model const * const model,
                      KIM_ComputeArguments const * const computeArguments);

/**
 ** \brief \copybrief KIM::Model::Extension
 **
 ** \sa KIM::Model::Extension, kim_model_module::kim_extension
 **
 ** \since 2.0
 **/
int KIM_Model_Extension(KIM_Model * const model,
                        char const * const extensionID,
                        void * const extensionStructure);

/**
 ** \brief \copybrief KIM::Model::ClearThenRefresh
 **
 ** \sa KIM::Model::ClearThenRefresh, kim_model_module::kim_clear_then_refresh
 **
 ** \since 2.0
 **/
int KIM_Model_ClearThenRefresh(KIM_Model * const model);

/**
 ** \brief \copybrief KIM::Model::WriteParameterizedModel
 **
 ** \sa KIM::Model::WriteParameterizedModel,
 ** kim_model_module::kim_write_parameterized_model
 **
 ** \since 2.0
 **/
int KIM_Model_WriteParameterizedModel(KIM_Model const * const model,
                                      char const * const path,
                                      char const * const modelName);

/**
 ** \brief \copybrief KIM::Model::GetSpeciesSupportAndCode
 **
 ** \sa KIM::Model::GetSpeciesSupportAndCode,
 ** kim_model_module::kim_get_species_support_and_code
 **
 ** \since 2.0
 **/
int KIM_Model_GetSpeciesSupportAndCode(KIM_Model const * const model,
                                       KIM_SpeciesName const speciesName,
                                       int * const speciesIsSupported,
                                       int * const code);

/**
 ** \brief \copybrief KIM::Model::GetNumberOfParameters
 **
 ** \sa KIM::Model::GetNumberOfParameters,
 ** kim_model_module::kim_get_number_of_parameters
 **
 ** \since 2.0
 **/
void KIM_Model_GetNumberOfParameters(KIM_Model const * const model,
                                     int * const numberOfParameters);

/**
 ** \brief \copybrief KIM::Model::GetParameterMetadata
 **
 ** \sa KIM::Model::GetParameterMetadata,
 ** kim_model_module::kim_get_parameter_metadata
 **
 ** \since 2.0
 **/
int KIM_Model_GetParameterMetadata(KIM_Model const * const model,
                                   int const parameterIndex,
                                   KIM_DataType * const dataType,
                                   int * const extent,
                                   char const ** const name,
                                   char const ** const description);

/**
 ** \brief \copybrief KIM::Model::GetParameter(int const, int const,
 **                                            int * const) const
 **
 ** \sa KIM::Model::GetParameter(int const,int const, int * const) const,
 ** kim_model_module::kim_get_parameter
 **
 ** \since 2.0
 **/
int KIM_Model_GetParameterInteger(KIM_Model const * const model,
                                  int const parameterIndex,
                                  int const arrayIndex,
                                  int * const parameterValue);

/**
 ** \brief \copybrief KIM::Model::GetParameter(int const, int const,
 **                                            int * const) const
 **
 ** \sa KIM::Model::GetParameter(int const,int const, double * const) const,
 ** kim_model_module::kim_get_parameter
 **
 ** \since 2.0
 **/
int KIM_Model_GetParameterDouble(KIM_Model const * const model,
                                 int const parameterIndex,
                                 int const arrayIndex,
                                 double * const parameterValue);

/**
 ** \brief \copybrief KIM::Model::SetParameter(int const, int const, int const)
 **
 ** \sa KIM::Model::SetParameter(int const, int const, int const),
 ** kim_model_module::kim_set_parameter
 **
 ** \since 2.0
 **/
int KIM_Model_SetParameterInteger(KIM_Model * const model,
                                  int const parameterIndex,
                                  int const arrayIndex,
                                  int const parameterValue);

/**
 ** \brief \copybrief KIM::Model::SetParameter(int const, int const, int const)
 **
 ** \sa KIM::Model::SetParameter(int const, int const, double const),
 ** kim_model_module::kim_set_parameter
 **
 ** \since 2.0
 **/
int KIM_Model_SetParameterDouble(KIM_Model * const model,
                                 int const parameterIndex,
                                 int const arrayIndex,
                                 double const parameterValue);

/**
 ** \brief \copybrief KIM::Model::SetSimulatorBufferPointer
 **
 ** \sa KIM::Model::SetSimulatorBufferPointer,
 ** kim_model_module::kim_set_simulator_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_Model_SetSimulatorBufferPointer(KIM_Model * const model,
                                         void * const ptr);

/**
 ** \brief \copybrief KIM::Model::GetSimulatorBufferPointer
 **
 ** \sa KIM::Model::GetSimulatorBufferPointer,
 ** kim_model_module::kim_get_simulator_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_Model_GetSimulatorBufferPointer(KIM_Model const * const model,
                                         void ** const ptr);

/**
 ** \brief \copybrief KIM::Model::ToString
 **
 ** \sa KIM::Model::ToString, kim_model_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_Model_ToString(KIM_Model const * const model);

/**
 ** \brief \copybrief KIM::Model::SetLogID
 **
 ** \sa KIM::Model::SetLogID, kim_model_module::kim_set_log_id
 **
 ** \since 2.0
 **/
void KIM_Model_SetLogID(KIM_Model * const model, char const * const logID);

/**
 ** \brief \copybrief KIM::Model::PushLogVerbosity
 **
 ** \sa KIM::Model::PushLogVerbosity, kim_model_module::kim_push_log_verbosity
 **
 ** \since 2.0
 **/
void KIM_Model_PushLogVerbosity(KIM_Model * const model,
                                KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::Model::PopLogVerbosity
 **
 ** \sa KIM::Model::PopLogVerbosity, kim_model_module::kim_pop_log_verbosity
 **
 ** \since 2.0
 **/
void KIM_Model_PopLogVerbosity(KIM_Model * const model);

#endif /* KIM_MODEL_H_ */
