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
/* Copyright (c) 2016--2017, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_H_
#define KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_H_

#ifndef KIM_FUNC_H_
#include "KIM_func.h"
#endif

#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_ATTRIBUTE_H_
#include "KIM_COMPUTE_ArgumentAttribute.h"
#endif

/* Forward declarations */
#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_COMPUTE_ArgumentName KIM_COMPUTE_ArgumentName;
#endif


struct KIM_COMPUTE_ModelComputeArguments {
  void * p;
};

#ifndef KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_DEFINED_
typedef struct KIM_COMPUTE_ModelComputeArguments
KIM_COMPUTE_ModelComputeArguments;
#endif

void KIM_COMPUTE_ModelComputeArguments_get_argument_attribute(
    KIM_COMPUTE_ModelComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    KIM_COMPUTE_ArgumentAttribute * const argumentAttribute);

void KIM_COMPUTE_ModelComputeArguments_set_neigh(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_LanguageName const languageName,
    func * const fptr, void const * const dataObject);

void KIM_COMPUTE_ModelComputeArguments_set_process_dEdr(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_LanguageName const languageName,
    func * const fptr);
void KIM_COMPUTE_ModelComputeArguments_set_process_d2Edr2(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_LanguageName const languageName,
    func * const fptr);

/* *data functions */
int KIM_COMPUTE_ModelComputeArguments_set_data_int(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int const extent,
    int const * const ptr);

int KIM_COMPUTE_ModelComputeArguments_set_data_double(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int const extent,
    double const * const ptr);

/* *compute functions */
int KIM_COMPUTE_ModelComputeArguments_set_compute(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int flag);

void KIM_COMPUTE_ModelComputeArguments_set_process_dEdr_compute(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    int flag);
void KIM_COMPUTE_ModelComputeArguments_set_process_d2Edr2_compute(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    int flag);

int KIM_COMPUTE_ModelComputeArguments_get_size(
    KIM_COMPUTE_ModelComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int * const size);

#endif  /* KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_H_ */
