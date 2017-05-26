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


#ifndef KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_H_
#define KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_H_

#ifndef KIM_FUNC_H_
#include "KIM_func.h"
#endif

#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
#endif

/* Forward declarations */
struct KIM_COMPUTE_ArgumentName;
#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_COMPUTE_ArgumentName KIM_COMPUTE_ArgumentName;
#endif

struct KIM_Simulator;
#ifndef KIM_SIMULATOR_DEFINED_
#define KIM_SIMULATOR_DEFINED_
typedef struct KIM_Simulator KIM_Simulator;
#endif


struct KIM_COMPUTE_SimulatorComputeArguments {
  void * p;
};

#ifndef KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_DEFINED_
typedef struct KIM_COMPUTE_SimulatorComputeArguments
KIM_COMPUTE_SimulatorComputeArguments;
#endif


void KIM_COMPUTE_SimulatorComputeArguments_get_neighObject(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    void ** const ptr);
int KIM_COMPUTE_SimulatorComputeArguments_get_neigh(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    int const neighborListIndex,
    int const particleNumber,
    int * const numberOfNeighbors,
    int const ** const neighborsOfParticle);

int KIM_COMPUTE_SimulatorComputeArguments_process_dEdr(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_Simulator const * const simulator,
    double const de, double const r,
    double const * const dx, int const i,
    int const j);

int KIM_COMPUTE_SimulatorComputeArguments_process_d2Edr2(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_Simulator const * const simulator,
    double const de, double const * const r,
    double const * const dx, int const * const i,
    int const * const j);

/* *data functions */
int KIM_COMPUTE_SimulatorComputeArguments_get_data(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    void ** const ptr);

/* *compute functions */
int KIM_COMPUTE_SimulatorComputeArguments_get_compute(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int * const flag);

void KIM_COMPUTE_SimulatorComputeArguments_get_process_dEdr_compute(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    int * const flag);
void KIM_COMPUTE_SimulatorComputeArguments_get_process_d2Edr2_compute(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    int * const flag);

int KIM_COMPUTE_SimulatorComputeArguments_get_size(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int * const size);

#endif  /* KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_H_ */
