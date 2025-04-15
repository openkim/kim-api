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
