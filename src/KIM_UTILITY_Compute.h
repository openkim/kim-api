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


#ifndef KIM_UTILITY_COMPUTE_H_
#define KIM_UTILITY_COMPUTE_H_

/* Forward declarations */
struct KIM_COMPUTE_ArgumentName;
#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_COMPUTE_ArgumentName KIM_COMPUTE_ArgumentName;
#endif

struct KIM_Model;
#ifndef KIM_MODEL_DEFINED_
#define KIM_MODEL_DEFINED_
typedef struct KIM_Model KIM_Model;
#endif

struct KIM_Simulator;
#ifndef KIM_SIMULATOR_DEFINED_
#define KIM_SIMULATOR_DEFINED_
typedef struct KIM_Simulator KIM_Simulator;
#endif

#include <stdarg.h>

int KIM_UTILITY_COMPUTE_getm_data(
    KIM_Simulator const * const simulator,
    KIM_COMPUTE_ArgumentName const argumentName, ...);
int KIM_UTILITY_COMPUTE_vgetm_data(
    KIM_Simulator const * const simulator,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp);

int KIM_UTILITY_COMPUTE_setm_data(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, ...);
int KIM_UTILITY_COMPUTE_vsetm_data(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp);

/* *method functions */
int KIM_UTILITY_COMPUTE_setm_method(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, ...);
int KIM_UTILITY_COMPUTE_vsetm_method(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp);

/* *compute functions */
int KIM_UTILITY_COMPUTE_getm_compute(
    KIM_Simulator const * const simulator,
    KIM_COMPUTE_ArgumentName const argumentName, ...);
int KIM_UTILITY_COMPUTE_vgetm_compute(
    KIM_Simulator const * const simulator,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp);
int KIM_UTILITY_COMPUTE_setm_compute(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, ...);
int KIM_UTILITY_COMPUTE_vsetm_compute(
    KIM_Model * const model,
    KIM_COMPUTE_ArgumentName const argumentName, va_list argp);

#endif  /* KIM_UTILITY_COMPUTE_H_ */
