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


#ifndef KIM_SIMULATOR_MODEL_H_
#define KIM_SIMULATOR_MODEL_H_

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

#ifndef KIM_SIMULATOR_MODEL_DEFINED_
#define KIM_SIMULATOR_MODEL_DEFINED_
/**
 ** \brief \copybrief KIM::SimulatorModel
 **
 ** \copydetails KIM::SimulatorModel
 **
 ** <!-- see also and since not needed here due to use of copydetails -->
 **/
typedef struct KIM_SimulatorModel KIM_SimulatorModel;
#endif

/**
 ** \brief \copybrief KIM::SimulatorModel::Create
 **
 ** \sa KIM::SimulatorModel::Create,
 ** kim_simulator_model_module::kim_simulator_model_create
 **
 ** \since 2.1
 **/
int KIM_SimulatorModel_Create(char const * const simulatorModelName,
                              KIM_SimulatorModel ** const simulatorModel);

/**
 ** \brief \copybrief KIM::SimulatorModel::Destroy
 **
 ** \sa KIM::SimulatorModel::Destroy,
 ** kim_simulator_model_module::kim_simulator_model_destroy
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_Destroy(KIM_SimulatorModel ** const simulatorModel);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetSimulatorNameAndVersion
 **
 ** \sa KIM::SimulatorModel::GetSimulatorNameAndVersion,
 ** kim_simulator_model_module::kim_get_simulator_name_and_version
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_GetSimulatorNameAndVersion(
    KIM_SimulatorModel const * const simulatorModel,
    char const ** const simulatorName,
    char const ** const simulatorVersion);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetNumberOfSupportedSpecies
 **
 ** \sa KIM::SimulatorModel::GetNumberOfSupportedSpecies,
 ** kim_simulator_model_module::kim_get_number_of_supported_species
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_GetNumberOfSupportedSpecies(
    KIM_SimulatorModel const * const simulatorModel,
    int * const numberOfSupportedSpecies);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetSupportedSpecies
 **
 ** \sa KIM::SimulatorModel::GetSupportedSpecies,
 ** kim_simulator_model_module::kim_get_supported_species
 **
 ** \since 2.1
 **/
int KIM_SimulatorModel_GetSupportedSpecies(
    KIM_SimulatorModel const * const simulatorModel,
    int const index,
    char const ** const speciesName);

/**
 ** \brief \copybrief KIM::SimulatorModel::OpenAndInitializeTemplateMap
 **
 ** \sa KIM::SimulatorModel::OpenAndInitializeTemplateMap,
 ** kim_simulator_model_module::kim_open_and_initialize_template_map
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_OpenAndInitializeTemplateMap(
    KIM_SimulatorModel const * const simulatorModel);

/**
 ** \brief \copybrief KIM::SimulatorModel::TemplateMapIsOpen
 **
 ** \sa KIM::SimulatorModel::TemplateMapIsOpen,
 ** kim_simulator_model_module::kim_template_map_is_open
 **
 ** \since 2.1
 **/
int KIM_SimulatorModel_TemplateMapIsOpen(
    KIM_SimulatorModel const * const simulatorModel);

/**
 ** \brief \copybrief KIM::SimulatorModel::AddTemplateMap
 **
 ** \sa KIM::SimulatorModel::AddTemplateMap,
 ** kim_simulator_model_module::kim_add_template_map
 **
 ** \since 2.1
 **/
int KIM_SimulatorModel_AddTemplateMap(
    KIM_SimulatorModel const * const simulatorModel,
    char const * const key,
    char const * const value);

/**
 ** \brief \copybrief KIM::SimulatorModel::CloseTemplateMap
 **
 ** \sa KIM::SimulatorModel::CloseTemplateMap,
 ** kim_simulator_model_module::kim_close_template_map
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_CloseTemplateMap(
    KIM_SimulatorModel const * const simulatorModel);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetNumberOfSimulatorFields
 **
 ** \sa KIM::SimulatorModel::GetNumberOfSimulatorFields,
 ** kim_simulator_model_module::kim_get_number_of_simulator_fields
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_GetNumberOfSimulatorFields(
    KIM_SimulatorModel const * const simulatorModel,
    int * const numberOfSimulatorFields);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetSimulatorFieldMetadata
 **
 ** \sa KIM::SimulatorModel::GetSimulatorFieldMetadata,
 ** kim_simulator_model_module::kim_get_simulator_field_metadata
 **
 ** \since 2.1
 **/
int KIM_SimulatorModel_GetSimulatorFieldMetadata(
    KIM_SimulatorModel const * const simulatorModel,
    int const fieldIndex,
    int * const extent,
    char const ** const fieldName);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetSimulatorFieldLine
 **
 ** \sa KIM::SimulatorModel::GetSimulatorFieldLine,
 ** kim_simulator_model_module::kim_get_simulator_field_line
 **
 ** \since 2.1
 **/
int KIM_SimulatorModel_GetSimulatorFieldLine(
    KIM_SimulatorModel const * const simulatorModel,
    int const fieldIndex,
    int const lineIndex,
    char const ** const lineValue);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetParameterFileDirectoryName
 **
 ** \sa KIM::SimulatorModel::GetParameterFileDirectoryName,
 ** kim_simulator_model_module::kim_get_parameter_file_directory_name
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_GetParameterFileDirectoryName(
    KIM_SimulatorModel const * const simulatorModel,
    char const ** const directoryName);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetSpecificationFileName
 **
 ** \sa KIM::SimulatorModel::GetSpecificationFileName,
 ** kim_simulator_model_module::kim_get_specification_file_name
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_GetSpecificationFileName(
    KIM_SimulatorModel const * const simulatorModel,
    char const ** const specificationFileName);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetNumberOfParameterFiles
 **
 ** \sa KIM::SimulatorModel::GetNumberOfParameterFiles,
 ** kim_simulator_model_module::kim_get_number_of_parameter_files
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_GetNumberOfParameterFiles(
    KIM_SimulatorModel const * const simulatorModel,
    int * const numberOfParameterFiles);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetParameterFileName
 **
 ** \sa KIM::SimulatorModel::GetParameterFileName,
 ** kim_simulator_model_module::kim_get_parameter_file_name
 **
 ** \since 2.1
 **/
int KIM_SimulatorModel_GetParameterFileName(
    KIM_SimulatorModel const * const simulatorModel,
    int const index,
    char const ** const parameterFileName);

/**
 ** \brief \copybrief KIM::SimulatorModel::SetSimulatorBufferPointer
 **
 ** \sa KIM::SimulatorModel::SetSimulatorBufferPointer,
 ** kim_simulator_model_module::kim_set_simulator_buffer_pointer
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_SetSimulatorBufferPointer(
    KIM_SimulatorModel * const simulatorModel, void * const ptr);

/**
 ** \brief \copybrief KIM::SimulatorModel::GetSimulatorBufferPointer
 **
 ** \sa KIM::SimulatorModel::GetSimulatorBufferPointer,
 ** kim_simulator_model_module::kim_get_simulator_buffer_pointer
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_GetSimulatorBufferPointer(
    KIM_SimulatorModel const * const simulatorModel, void ** const ptr);

/**
 ** \brief \copybrief KIM::SimulatorModel::ToString
 **
 ** \sa KIM::SimulatorModel::ToString, kim_simulator_model_module::kim_to_string
 **
 ** \since 2.1
 **/
char const *
KIM_SimulatorModel_ToString(KIM_SimulatorModel const * const simulatorModel);

/**
 ** \brief \copybrief KIM::SimulatorModel::SetLogID
 **
 ** \sa KIM::SimulatorModel::SetLogID,
 ** kim_simulator_model_module::kim_set_log_id
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_SetLogID(KIM_SimulatorModel * const simulatorModel,
                                 char const * const logID);

/**
 ** \brief \copybrief KIM::SimulatorModel::PushLogVerbosity
 **
 ** \sa KIM::SimulatorModel::PushLogVerbosity,
 ** kim_simulator_model_module::kim_push_log_verbosity
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_PushLogVerbosity(
    KIM_SimulatorModel * const simulatorModel,
    KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::SimulatorModel::PopLogVerbosity
 **
 ** \sa KIM::SimulatorModel::PopLogVerbosity,
 ** kim_simulator_model_module::kim_pop_log_verbosity
 **
 ** \since 2.1
 **/
void KIM_SimulatorModel_PopLogVerbosity(
    KIM_SimulatorModel * const simulatorModel);

#endif /* KIM_SIMULATOR_MODEL_H_ */
