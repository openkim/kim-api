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
/* Release: This file is part of the kim-api-2.4.1 package.                   */
/*                                                                            */


#ifndef KIM_MODEL_EXTENSION_H_
#define KIM_MODEL_EXTENSION_H_

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

#ifndef KIM_MODEL_DEFINED_
#define KIM_MODEL_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_Model KIM_Model;
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

#ifndef KIM_MODEL_CREATE_DEFINED_
#define KIM_MODEL_CREATE_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelCreate KIM_ModelCreate;
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

#ifndef KIM_MODEL_DRIVER_CREATE_DEFINED_
#define KIM_MODEL_DRIVER_CREATE_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelDriverCreate KIM_ModelDriverCreate;
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

#ifndef KIM_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_COMPUTE_ARGUMENTS_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ComputeArguments KIM_ComputeArguments;
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

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelComputeArgumentsCreate KIM_ModelComputeArgumentsCreate;
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


#ifndef KIM_MODEL_EXTENSION_DEFINED_
#define KIM_MODEL_EXTENSION_DEFINED_
/**
 ** \brief \copybrief KIM::ModelExtension
 **
 ** \sa KIM::ModelExtension,
 ** kim_model_extension_module::kim_model_extension_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelExtension KIM_ModelExtension;
#endif

/**
 ** \brief \copybrief KIM::ModelExtension::GetExtensionID
 **
 ** \sa KIM::ModelExtension::GetExtensionID,
 ** kim_model_extension_module::kim_get_extension_id
 **
 ** \since 2.0
 **/
void KIM_ModelExtension_GetExtensionID(
    KIM_ModelExtension const * const modelExtension,
    char const ** const extensionID);

/**
 ** \brief \copybrief KIM::ModelExtension::Model
 **
 ** \sa KIM::ModelExtension::Model, kim_model_extension_module::kim_to_model
 **
 ** \since 2.0
 **/
KIM_Model *
KIM_ModelExtension_ToModel(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelCompute
 **
 ** \sa KIM::ModelExtension::ModelCompute,
 ** kim_model_extension_module::kim_to_model_compute
 **
 ** \since 2.0
 **/
KIM_ModelCompute *
KIM_ModelExtension_ToModelCompute(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelCreate
 **
 ** \sa KIM::ModelExtension::ModelCreate,
 ** kim_model_extension_module::kim_to_model_create
 **
 ** \since 2.0
 **/
KIM_ModelCreate *
KIM_ModelExtension_ToModelCreate(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelDestroy
 **
 ** \sa KIM::ModelExtension::ModelDestroy,
 ** kim_model_extension_module::kim_to_model_destroy
 **
 ** \since 2.0
 **/
KIM_ModelDestroy *
KIM_ModelExtension_ToModelDestroy(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelDriverCreate
 **
 ** \sa KIM::ModelExtension::ModelDriverCreate,
 ** kim_model_extension_module::kim_to_model_driver_create
 **
 ** \since 2.0
 **/
KIM_ModelDriverCreate * KIM_ModelExtension_ToModelDriverCreate(
    KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelRefresh
 **
 ** \sa KIM::ModelExtension::ModelRefresh,
 ** kim_model_extension_module::kim_to_model_refresh
 **
 ** \since 2.0
 **/
KIM_ModelRefresh *
KIM_ModelExtension_ToModelRefresh(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelWriteParameterizedModel
 **
 ** \sa KIM::ModelExtension::ModelWriteParameterizedModel,
 ** kim_model_extension_module::kim_to_model_write_parameterized_model
 **
 ** \since 2.0
 **/
KIM_ModelWriteParameterizedModel *
KIM_ModelExtension_ToModelWriteParameterizedModel(
    KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelComputeArguments
 **
 ** \sa KIM::ModelExtension::ModelComputeArguments,
 ** kim_model_extension_module::kim_to_model_compute_arguments
 **
 ** \since 2.0
 **/
KIM_ModelComputeArguments * KIM_ModelExtension_ToModelComputeArguments(
    KIM_ModelExtension const * const modelExtension,
    KIM_ComputeArguments * const computeArguments);
KIM_ModelComputeArgumentsCreate *

/**
 ** \brief \copybrief KIM::ModelExtension::ModelComputeArgumentsCreate
 **
 ** \sa KIM::ModelExtension::ModelComputeArgumentsCreate,
 ** kim_model_extension_module::kim_to_model_compute_arguments_create
 **
 ** \since 2.0
 **/
KIM_ModelExtension_ToModelComputeArgumentsCreate(
    KIM_ModelExtension const * const modelExtension,
    KIM_ComputeArguments * const computeArguments);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelComputeArgumentsDestroy
 **
 ** \sa KIM::ModelExtension::ModelComputeArgumentsDestroy,
 ** kim_model_extension_module::kim_to_model_compute_arguments_destroy
 **
 ** \since 2.0
 **/
KIM_ModelComputeArgumentsDestroy *
KIM_ModelExtension_ToModelComputeArgumentsDestroy(
    KIM_ModelExtension const * const modelExtension,
    KIM_ComputeArguments * const computeArguments);

/**
 ** \brief \copybrief KIM::ModelExtension::GetModelBufferPointer
 **
 ** \sa KIM::ModelExtension::GetModelBufferPointer,
 ** kim_model_extension_module::kim_get_model_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelExtension_GetModelBufferPointer(
    KIM_ModelExtension const * const modelCompute, void ** const ptr);

/**
 ** \brief \copybrief KIM::ModelExtension::LogEntry
 **
 ** \sa KIM::ModelExtension::LogEntry,
 ** kim_model_extension_module::kim_log_entry
 **
 ** \since 2.0
 **/
void KIM_ModelExtension_LogEntry(KIM_ModelExtension const * const modelCompute,
                                 KIM_LogVerbosity const logVerbosity,
                                 char const * const message,
                                 int const lineNumber,
                                 char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelExtension::ToString
 **
 ** \sa KIM::ModelExtension::ToString,
 ** kim_model_extension_module::kim_to_string
 **
 ** \since 2.0
 **/
char const *
KIM_ModelExtension_ToString(KIM_ModelExtension const * const modelCompute);

#endif /* KIM_MODEL_EXTENSION_H_ */
