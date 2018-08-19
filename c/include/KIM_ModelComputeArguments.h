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


#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_H_
#define KIM_MODEL_COMPUTE_ARGUMENTS_H_

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_ComputeArgumentName KIM_ComputeArgumentName;
#endif

#ifndef KIM_COMPUTE_CALLBACK_NAME_DEFINED_
#define KIM_COMPUTE_CALLBACK_NAME_DEFINED_
typedef struct KIM_ComputeCallbackName KIM_ComputeCallbackName;
#endif


struct KIM_ModelComputeArguments;

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_MODEL_COMPUTE_ARGUMENTS_DEFINED_
typedef struct KIM_ModelComputeArguments KIM_ModelComputeArguments;
#endif

int KIM_ModelComputeArguments_GetNeighborList(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    int const neighborListIndex,
    int const particleNumber,
    int * const numberOfNeighbors,
    int const ** const neighborsOfParticle);

int KIM_ModelComputeArguments_ProcessDEDrTerm(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    double const de, double const r,
    double const * const dx, int const i,
    int const j);

int KIM_ModelComputeArguments_ProcessD2EDr2Term(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    double const de, double const * const r,
    double const * const dx,
    int const * const i,
    int const * const j);

int KIM_ModelComputeArguments_GetArgumentPointerInteger(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_ComputeArgumentName const computeArgumentName,
    int ** const ptr);
int KIM_ModelComputeArguments_GetArgumentPointerDouble(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_ComputeArgumentName const computeArgumentName, double ** const ptr);

int KIM_ModelComputeArguments_IsCallbackPresent(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_ComputeCallbackName const computeCallbackName, int * const present);

void KIM_ModelComputeArguments_SetModelBufferPointer(
    KIM_ModelComputeArguments * const modelComputeArguments,
    void * const ptr);
void KIM_ModelComputeArguments_GetModelBufferPointer(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    void ** const ptr);

void KIM_ModelComputeArguments_LogEntry(
    KIM_ModelComputeArguments const * const modelComputeArguments,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName);

char const * KIM_ModelComputeArguments_String(
    KIM_ModelComputeArguments const * const modelComputeArguments);

#endif  /* KIM_MODEL_COMPUTE_ARGUMENTS_H_ */
