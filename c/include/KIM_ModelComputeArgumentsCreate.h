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
/* Copyright (c) 2016--2018, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_H_
#define KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_H_

#ifndef KIM_FUNC_H_
#include "KIM_func.h"
#endif

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

#ifndef KIM_SUPPORT_STATUS_DEFINED_
#define KIM_SUPPORT_STATUS_DEFINED_
typedef struct KIM_SupportStatus KIM_SupportStatus;
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_ComputeArgumentName KIM_ComputeArgumentName;
#endif

#ifndef KIM_COMPUTE_CALLBACK_NAME_DEFINED_
#define KIM_COMPUTE_CALLBACK_NAME_DEFINED_
typedef struct KIM_ComputeCallbackName KIM_ComputeCallbackName;
#endif


struct KIM_ModelComputeArgumentsCreate;

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_DEFINED_
typedef struct KIM_ModelComputeArgumentsCreate KIM_ModelComputeArgumentsCreate;
#endif


int KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus(
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate,
    KIM_ComputeArgumentName const computeArgumentName,
    KIM_SupportStatus const supportStatus);

int KIM_ModelComputeArgumentsCreate_SetCallbackSupportStatus(
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate,
    KIM_ComputeCallbackName const computeCallbackName,
    KIM_SupportStatus const supportStatus);

void KIM_ModelComputeArgumentsCreate_SetModelBufferPointer(
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate,
    void * const ptr);

void KIM_ModelComputeArgumentsCreate_LogEntry(
    KIM_ModelComputeArgumentsCreate const * const modelComputeArgumentsCreate,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName);

char const * KIM_ModelComputeArgumentsCreate_String(
    KIM_ModelComputeArgumentsCreate const * const modelComputeArgumentsCreate);

#endif  /* KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_H_ */
