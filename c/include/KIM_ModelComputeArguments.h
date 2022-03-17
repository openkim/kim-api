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
/* Release: This file is part of the kim-api-2.3.0 package.                   */
/*                                                                            */


#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_H_
#define KIM_MODEL_COMPUTE_ARGUMENTS_H_

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

#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ComputeArgumentName KIM_ComputeArgumentName;
#endif

#ifndef KIM_COMPUTE_CALLBACK_NAME_DEFINED_
#define KIM_COMPUTE_CALLBACK_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ComputeCallbackName KIM_ComputeCallbackName;
#endif


#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_DEFINED_
/**
 ** \brief \copybrief KIM::ModelComputeArguments
 **
 ** \sa KIM::ModelComputeArguments,
 ** kim_model_compute_arguments_module::kim_model_compute_arguments_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelComputeArguments KIM_ModelComputeArguments;
#endif

/**
 ** \brief \copybrief KIM::ModelComputeArguments::GetNeighborList
 **
 ** \sa KIM::ModelComputeArguments::GetNeighborList,
 ** kim_model_compute_arguments_module::kim_get_neighbor_list
 **
 ** \since 2.0
 **/
int KIM_ModelComputeArguments_GetNeighborList(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    int const neighborListIndex,
    int const particleNumber,
    int * const numberOfNeighbors,
    int const ** const neighborsOfParticle);

/**
 ** \brief \copybrief KIM::ModelComputeArguments::ProcessDEDrTerm
 **
 ** \sa KIM::ModelComputeArguments::ProcessDEDrTerm,
 ** kim_model_compute_arguments_module::kim_process_dedr_term
 **
 ** \since 2.0
 **/
int KIM_ModelComputeArguments_ProcessDEDrTerm(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    double const de,
    double const r,
    double const * const dx,
    int const i,
    int const j);

/**
 ** \brief \copybrief KIM::ModelComputeArguments::ProcessD2EDr2Term
 **
 ** \sa KIM::ModelComputeArguments::ProcessD2EDr2Term,
 ** kim_model_compute_arguments_module::kim_process_d2edr2_term
 **
 ** \since 2.0
 **/
int KIM_ModelComputeArguments_ProcessD2EDr2Term(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    double const de,
    double const * const r,
    double const * const dx,
    int const * const i,
    int const * const j);

/**
 ** \brief \copybrief KIM::ModelComputeArguments::GetArgumentPointer
 **
 ** \sa KIM::ModelComputeArguments::GetArgumentPointer,
 ** kim_model_compute_arguments_module::kim_get_argument_pointer
 **
 ** \since 2.0
 **/
int KIM_ModelComputeArguments_GetArgumentPointerInteger(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    int ** const ptr);

/**
 ** \brief \copybrief KIM::ModelComputeArguments::GetArgumentPointer
 **
 ** \sa KIM::ModelComputeArguments::GetArgumentPointer,
 ** kim_model_compute_arguments_module::kim_get_argument_pointer
 **
 ** \since 2.0
 **/
int KIM_ModelComputeArguments_GetArgumentPointerDouble(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    double ** const ptr);

/**
 ** \brief \copybrief KIM::ModelComputeArguments::IsCallbackPresent
 **
 ** \sa KIM::ModelComputeArguments::IsCallbackPresent,
 ** kim_model_compute_arguments_module::kim_is_callback_present
 **
 ** \since 2.0
 **/
int KIM_ModelComputeArguments_IsCallbackPresent(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_ComputeCallbackName const computeCallbackName,
    int * const present);

/**
 ** \brief \copybrief KIM::ModelComputeArguments::SetModelBufferPointer
 **
 ** \sa KIM::ModelComputeArguments::SetModelBufferPointer,
 ** kim_model_compute_arguments_module::kim_set_model_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelComputeArguments_SetModelBufferPointer(
    KIM_ModelComputeArguments * const modelComputeArguments, void * const ptr);

/**
 ** \brief \copybrief KIM::ModelComputeArguments::GetModelBufferPointer
 **
 ** \sa KIM::ModelComputeArguments::GetModelBufferPointer,
 ** kim_model_compute_arguments_module::kim_get_model_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelComputeArguments_GetModelBufferPointer(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    void ** const ptr);

/**
 ** \brief \copybrief KIM::ModelComputeArguments::LogEntry
 **
 ** \sa KIM::ModelComputeArguments::LogEntry,
 ** kim_model_compute_arguments_module::kim_log_entry
 **
 ** \since 2.0
 **/
void KIM_ModelComputeArguments_LogEntry(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelComputeArguments::ToString
 **
 ** \sa KIM::ModelComputeArguments::ToString,
 ** kim_model_compute_arguments_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_ModelComputeArguments_ToString(
    KIM_ModelComputeArguments const * const modelComputeArguments);

#endif /* KIM_MODEL_COMPUTE_ARGUMENTS_H_ */
