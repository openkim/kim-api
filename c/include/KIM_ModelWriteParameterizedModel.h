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


#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_H_
#define KIM_MODEL_WRITE_PARAMETERIZED_MODEL_H_

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


#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_DEFINED_
#define KIM_MODEL_WRITE_PARAMETERIZED_MODEL_DEFINED_
/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel
 **
 ** \sa KIM::ModelWriteParameterizedModel,
 ** kim_model_write_parameterized_model_module::<!--
 ** -->kim_model_write_parameterized_model_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelWriteParameterizedModel
    KIM_ModelWriteParameterizedModel;
#endif

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::GetPath
 **
 ** \sa KIM::ModelWriteParameterizedModel::GetPath,
 ** kim_model_write_parameterized_model_module::kim_get_path
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_GetPath(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    char const ** const path);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::GetModelName
 **
 ** \sa KIM::ModelWriteParameterizedModel::GetModelName,
 ** kim_model_write_parameterized_model_module::kim_get_model_name
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_GetModelName(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    char const ** const modelName);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::SetParameterFileName
 **
 ** \sa KIM::ModelWriteParameterizedModel::SetParameterFileName,
 ** kim_model_write_parameterized_model_module::kim_set_parameter_file_name
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_SetParameterFileName(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::GetModelBufferPointer
 **
 ** \sa KIM::ModelWriteParameterizedModel::GetModelBufferPointer,
 ** kim_model_write_parameterized_model_module::kim_get_model_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_GetModelBufferPointer(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    void ** const ptr);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::LogEntry
 **
 ** \sa KIM::ModelWriteParameterizedModel::LogEntry,
 ** kim_model_write_parameterized_model_module::kim_log_entry
 **
 ** \since 2.0
 **/
void KIM_ModelWriteParameterizedModel_LogEntry(
    KIM_ModelWriteParameterizedModel const * const modelWriteParameterizedModel,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelWriteParameterizedModel::ToString
 **
 ** \sa KIM::ModelWriteParameterizedModel::ToString,
 ** kim_model_write_parameterized_model_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_ModelWriteParameterizedModel_ToString(
    KIM_ModelWriteParameterizedModel const * const
        modelWriteParameterizedModel);

#endif /* KIM_MODEL_WRITE_PARAMETERIZED_MODEL_H_ */
