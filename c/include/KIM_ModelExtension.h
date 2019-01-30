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


#ifndef KIM_MODEL_EXTENSION_H_
#define KIM_MODEL_EXTENSION_H_

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

#ifndef KIM_MODEL_DEFINED_
#define KIM_MODEL_DEFINED_
typedef struct KIM_Model KIM_Model;
#endif

#ifndef KIM_MODEL_COMPUTE_DEFINED_
#define KIM_MODEL_COMPUTE_DEFINED_
typedef struct KIM_ModelCompute KIM_ModelCompute;
#endif

#ifndef KIM_MODEL_CREATE_DEFINED_
#define KIM_MODEL_CREATE_DEFINED_
typedef struct KIM_ModelCreate KIM_ModelCreate;
#endif

#ifndef KIM_MODEL_DESTROY_DEFINED_
#define KIM_MODEL_DESTROY_DEFINED_
typedef struct KIM_ModelDestroy KIM_ModelDestroy;
#endif

#ifndef KIM_MODEL_DRIVER_CREATE_DEFINED_
#define KIM_MODEL_DRIVER_CREATE_DEFINED_
typedef struct KIM_ModelDriverCreate KIM_ModelDriverCreate;
#endif

#ifndef KIM_MODEL_REFRESH_DEFINED_
#define KIM_MODEL_REFRESH_DEFINED_
typedef struct KIM_ModelRefresh KIM_ModelRefresh;
#endif

#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_DEFINED_
#define KIM_MODEL_WRITE_PARAMETERIZED_MODEL_DEFINED_
typedef struct KIM_ModelWriteParameterizedModel
    KIM_ModelWriteParameterizedModel;
#endif

#ifndef KIM_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_COMPUTE_ARGUMENTS_DEFINED_
typedef struct KIM_ComputeArguments KIM_ComputeArguments;
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_DEFINED_
typedef struct KIM_ModelComputeArguments KIM_ModelComputeArguments;
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_DEFINED_
typedef struct KIM_ModelComputeArgumentsCreate KIM_ModelComputeArgumentsCreate;
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_DEFINED_
typedef struct KIM_ModelComputeArgumentsDestroy
    KIM_ModelComputeArgumentsDestroy;
#endif


struct KIM_ModelExtension;

#ifndef KIM_MODEL_EXTENSION_DEFINED_
#define KIM_MODEL_EXTENSION_DEFINED_
/**
 ** \brief \copybrief KIM::ModelExtension
 **
 ** \sa KIM::ModelExtension
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelExtension KIM_ModelExtension;
#endif

/**
 ** \brief \copybrief KIM::ModelExtension::GetExtensionID
 **
 ** \sa KIM::ModelExtension::GetExtensionID
 **
 ** \since 2.0
 **/
void KIM_ModelExtension_GetExtensionID(
    KIM_ModelExtension const * const modelExtension,
    char const ** const extensionID);

/**
 ** \brief \copybrief KIM::ModelExtension::Model
 **
 ** \sa KIM::ModelExtension::Model
 **
 ** \since 2.0
 **/
KIM_Model *
KIM_ModelExtension_ToModel(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelCompute
 **
 ** \sa KIM::ModelExtension::ModelCompute
 **
 ** \since 2.0
 **/
KIM_ModelCompute *
KIM_ModelExtension_ToModelCompute(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelCreate
 **
 ** \sa KIM::ModelExtension::ModelCreate
 **
 ** \since 2.0
 **/
KIM_ModelCreate *
KIM_ModelExtension_ToModelCreate(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelDestroy
 **
 ** \sa KIM::ModelExtension::ModelDestroy
 **
 ** \since 2.0
 **/
KIM_ModelDestroy *
KIM_ModelExtension_ToModelDestroy(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelDriverCreate
 **
 ** \sa KIM::ModelExtension::ModelDriverCreate
 **
 ** \since 2.0
 **/
KIM_ModelDriverCreate * KIM_ModelExtension_ToModelDriverCreate(
    KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelRefresh
 **
 ** \sa KIM::ModelExtension::ModelRefresh
 **
 ** \since 2.0
 **/
KIM_ModelRefresh *
KIM_ModelExtension_ToModelRefresh(KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelWriteParameterizedModel
 **
 ** \sa KIM::ModelExtension::ModelWriteParameterizedModel
 **
 ** \since 2.0
 **/
KIM_ModelWriteParameterizedModel *
KIM_ModelExtension_ToModelWriteParameterizedModel(
    KIM_ModelExtension * const modelExtension);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelComputeArguments
 **
 ** \sa KIM::ModelExtension::ModelComputeArguments
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
 ** \sa KIM::ModelExtension::ModelComputeArgumentsCreate
 **
 ** \since 2.0
 **/
KIM_ModelExtension_ToModelComputeArgumentsCreate(
    KIM_ModelExtension const * const modelExtension,
    KIM_ComputeArguments * const computeArguments);

/**
 ** \brief \copybrief KIM::ModelExtension::ModelComputeArgumentsDestroy
 **
 ** \sa KIM::ModelExtension::ModelComputeArgumentsDestroy
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
 ** \sa KIM::ModelExtension::GetModelBufferPointer
 **
 ** \since 2.0
 **/
void KIM_ModelExtension_GetModelBufferPointer(
    KIM_ModelExtension const * const modelCompute, void ** const ptr);

/**
 ** \brief \copybrief KIM::ModelExtension::LogEntry
 **
 ** \sa KIM::ModelExtension::LogEntry
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
 ** \sa KIM::ModelExtension::ToString
 **
 ** \since 2.0
 **/
char const *
KIM_ModelExtension_ToString(KIM_ModelExtension const * const modelCompute);

#endif /* KIM_MODEL_EXTENSION_H_ */
