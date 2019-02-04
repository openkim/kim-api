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


#ifndef KIM_MODEL_DESTROY_H_
#define KIM_MODEL_DESTROY_H_

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


#ifndef KIM_MODEL_DESTROY_DEFINED_
#define KIM_MODEL_DESTROY_DEFINED_
/**
 ** \brief \copybrief KIM::ModelDestroy
 **
 ** \sa KIM::ModelDestroy,
 ** kim_model_destroy_module::kim_model_destroy_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelDestroy KIM_ModelDestroy;
#endif

/**
 ** \brief \copybrief KIM::ModelDestroy::GetModelBufferPointer
 **
 ** \sa KIM::ModelDestroy::GetModelBufferPointer,
 ** kim_model_destroy_module::kim_get_model_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelDestroy_GetModelBufferPointer(
    KIM_ModelDestroy const * const modelDestroy, void ** const ptr);

/**
 ** \brief \copybrief KIM::ModelDestroy::LogEntry
 **
 ** \sa KIM::ModelDestroy::LogEntry, kim_model_destroy_module::kim_log_entry
 **
 ** \since 2.0
 **/
void KIM_ModelDestroy_LogEntry(KIM_ModelDestroy const * const modelDestroy,
                               KIM_LogVerbosity const logVerbosity,
                               char const * const message,
                               int const lineNumber,
                               char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelDestroy::ToString
 **
 ** \sa KIM::ModelDestroy::ToString, kim_model_destroy_module::kim_to_string
 **
 ** \since 2.0
 **/
char const *
KIM_ModelDestroy_ToString(KIM_ModelDestroy const * const modelDestroy);

#endif /* KIM_MODEL_DESTROY_H_ */
