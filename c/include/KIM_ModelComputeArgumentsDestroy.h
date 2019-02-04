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


#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_H_
#define KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_H_

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


#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_DEFINED_
/**
 ** \brief \copybrief KIM::ModelComputeArgumentsDestroy
 **
 ** \sa KIM::ModelComputeArgumentsDestroy,
 ** kim_model_compute_arguments_destroy_module::<!--
 ** -->kim_model_compute_arguments_destroy_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelComputeArgumentsDestroy
    KIM_ModelComputeArgumentsDestroy;
#endif

/**
 ** \brief \copybrief KIM::ModelComputeArgumentsDestroy::GetModelBufferPointer
 **
 ** \sa KIM::ModelComputeArgumentsDestroy::GetModelBufferPointer,
 ** kim_model_compute_arguments_destroy_module::kim_get_model_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelComputeArgumentsDestroy_GetModelBufferPointer(
    KIM_ModelComputeArgumentsDestroy const * const modelComputeArgumentsDestroy,
    void ** const ptr);

/**
 ** \brief \copybrief KIM::ModelComputeArgumentsDestroy::LogEntry
 **
 ** \sa KIM::ModelComputeArgumentsDestroy::LogEntry,
 ** kim_model_compute_arguments_destroy_module::kim_log_entry
 **
 ** \since 2.0
 **/
void KIM_ModelComputeArgumentsDestroy_LogEntry(
    KIM_ModelComputeArgumentsDestroy const * const modelComputeArgumentsDestroy,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelComputeArgumentsDestroy::ToString
 **
 ** \sa KIM::ModelComputeArgumentsDestroy::ToString,
 ** kim_model_compute_arguments_destroy_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_ModelComputeArgumentsDestroy_ToString(
    KIM_ModelComputeArgumentsDestroy const * const
        modelComputeArgumentsDestroy);

#endif /* KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_H_ */
