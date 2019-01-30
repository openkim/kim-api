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


#ifndef KIM_COMPUTE_ARGUMENTS_H_
#define KIM_COMPUTE_ARGUMENTS_H_

#ifndef KIM_FUNCTION_TYPES_H_
#include "KIM_FunctionTypes.h"
#endif

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

#ifndef KIM_LANGUAGE_NAME_DEFINED_
#define KIM_LANGUAGE_NAME_DEFINED_
typedef struct KIM_LanguageName KIM_LanguageName;
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_ComputeArgumentName KIM_ComputeArgumentName;
#endif

#ifndef KIM_COMPUTE_CALLBACK_NAME_DEFINED_
#define KIM_COMPUTE_CALLBACK_NAME_DEFINED_
typedef struct KIM_ComputeCallbackName KIM_ComputeCallbackName;
#endif

#ifndef KIM_SUPPORT_STATUS_DEFINED_
#define KIM_SUPPORT_STATUS_DEFINED_
typedef struct KIM_SupportStatus KIM_SupportStatus;
#endif


/**
 ** \brief \copybrief KIM::ComputeArguments
 **
 ** \sa KIM::ComputeArguments
 **
 ** \since 2.0
 **/
struct KIM_ComputeArguments;

#ifndef KIM_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_COMPUTE_ARGUMENTS_DEFINED_
typedef struct KIM_ComputeArguments KIM_ComputeArguments;
#endif

/**
 ** \brief \copybrief KIM::ComputeArguments::GetArgumentSupportStatus
 **
 ** \sa KIM::ComputeArguments::GetArgumentSupportStatus
 **
 ** \since 2.0
 **/
int KIM_ComputeArguments_GetArgumentSupportStatus(
    KIM_ComputeArguments const * const computeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    KIM_SupportStatus * const supportStatus);

/**
 ** \brief \copybrief KIM::ComputeArguments::GetCallbackSupportStatus
 **
 ** \sa KIM::ComputeArguments::GetCallbackSupportStatus
 **
 ** \since 2.0
 **/
int KIM_ComputeArguments_GetCallbackSupportStatus(
    KIM_ComputeArguments const * const computeArguments,
    KIM_ComputeCallbackName const computeCallbackName,
    KIM_SupportStatus * const supportStatus);

/**
 ** \brief \copybrief KIM::ComputeArguments::SetArgumentPointer
 **
 ** \sa KIM::ComputeArguments::SetArgumentPointer
 **
 ** \since 2.0
 **/
int KIM_ComputeArguments_SetArgumentPointerInteger(
    KIM_ComputeArguments * const computeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    int const * const ptr);

/**
 ** \brief \copybrief KIM::ComputeArguments::SetArgumentPointer
 **
 ** \sa KIM::ComputeArguments::SetArgumentPointer
 **
 ** \since 2.0
 **/
int KIM_ComputeArguments_SetArgumentPointerDouble(
    KIM_ComputeArguments * const computeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    double const * const ptr);

/**
 ** \brief \copybrief KIM::ComputeArguments::SetCallbackPointer
 **
 ** \sa KIM::ComputeArguments::SetCallbackPointer
 **
 ** \since 2.0
 **/
int KIM_ComputeArguments_SetCallbackPointer(
    KIM_ComputeArguments * const computeArguments,
    KIM_ComputeCallbackName const computeCallbackName,
    KIM_LanguageName const languageName,
    KIM_Function * const fptr,
    void * const dataObject);

/**
 ** \brief \copybrief <!--
 ** --> KIM::ComputeArguments::AreAllRequiredArgumentsAndCallbacksPresent
 **
 ** \sa KIM::ComputeArguments::AreAllRequiredArgumentsAndCallbacksPresent
 **
 ** \since 2.0
 **/
void KIM_ComputeArguments_AreAllRequiredArgumentsAndCallbacksPresent(
    KIM_ComputeArguments const * const computeArguments, int * const result);

/**
 ** \brief \copybrief KIM::ComputeArguments::SetSimulatorBufferPointer
 **
 ** \sa KIM::ComputeArguments::SetSimulatorBufferPointer
 **
 ** \since 2.0
 **/
void KIM_ComputeArguments_SetSimulatorBufferPointer(
    KIM_ComputeArguments * const computeArguments, void * const ptr);

/**
 ** \brief \copybrief KIM::ComputeArguments::GetSimulatorBufferPointer
 **
 ** \sa KIM::ComputeArguments::GetSimulatorBufferPointer
 **
 ** \since 2.0
 **/
void KIM_ComputeArguments_GetSimulatorBufferPointer(
    KIM_ComputeArguments const * const computeArguments, void ** const ptr);

/**
 ** \brief \copybrief KIM::ComputeArguments::ToString
 **
 ** \sa KIM::ComputeArguments::ToString
 **
 ** \since 2.0
 **/
char const * KIM_ComputeArguments_ToString(
    KIM_ComputeArguments const * const computeArguments);

/**
 ** \brief \copybrief KIM::ComputeArguments::SetLogID
 **
 ** \sa KIM::ComputeArguments::SetLogID
 **
 ** \since 2.0
 **/
void KIM_ComputeArguments_SetLogID(
    KIM_ComputeArguments * const computeArguments, char const * const logID);

/**
 ** \brief \copybrief KIM::ComputeArguments::PushLogVerbosity
 **
 ** \sa KIM::ComputeArguments::PushLogVerbosity
 **
 ** \since 2.0
 **/
void KIM_ComputeArguments_PushLogVerbosity(
    KIM_ComputeArguments * const computeArguments,
    KIM_LogVerbosity const logVerbosity);

/**
 ** \brief \copybrief KIM::ComputeArguments::PopLogVerbosity
 **
 ** \sa KIM::ComputeArguments::PopLogVerbosity
 **
 ** \since 2.0
 **/
void KIM_ComputeArguments_PopLogVerbosity(
    KIM_ComputeArguments * const computeArguments);

#endif /* KIM_COMPUTE_ARGUMENTS_H_ */
