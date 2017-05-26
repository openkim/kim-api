//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_COMPUTE_ArgumentName.hpp"
#endif

#ifndef KIM_SIMULATOR_HPP_
#include "KIM_Simulator.hpp"
#endif

#ifndef KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_HPP_
#include "KIM_COMPUTE_SimulatorComputeArguments.hpp"
#endif

extern "C"
{
#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#include "KIM_COMPUTE_ArgumentName.h"
#endif

#ifndef KIM_SIMULATOR_H_
#include "KIM_Simulator.h"
#endif

#ifndef KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_H_
#include "KIM_COMPUTE_SimulatorComputeArguments.h"
#endif
}  // extern "C"

namespace
{
KIM::COMPUTE::ArgumentName
makeArgumentNameCpp(KIM_COMPUTE_ArgumentName const argumentName)
{
  return KIM::COMPUTE::ArgumentName(argumentName.argumentNameID);
}
}  // namespace


extern "C"
{
void KIM_COMPUTE_SimulatorComputeArguments_get_neighObject(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    void ** const ptr)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  pArguments->get_neighObject(ptr);
}

int KIM_COMPUTE_SimulatorComputeArguments_get_neigh(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    int const neighborListIndex,
    int const particleNumber,
    int * const numberOfNeighbors,
    int const ** const neighborsOfParticle)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  return pArguments->get_neigh(neighborListIndex, particleNumber,
                               numberOfNeighbors, neighborsOfParticle);
}

int KIM_COMPUTE_SimulatorComputeArguments_process_dEdr(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_Simulator const * const simulator,
    double const de, double const r,
    double const * const dx, int const i,
    int const j)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  KIM::Simulator * pSimulator = (KIM::Simulator *) simulator->p;
  return pArguments->process_dEdr(pSimulator, de, r, dx, i, j);
}

int KIM_COMPUTE_SimulatorComputeArguments_process_d2Edr2(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_Simulator const * const simulator,
    double const de, double const * const r,
    double const * const dx, int const * const i,
    int const * const j)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  KIM::Simulator * pSimulator = (KIM::Simulator *) simulator->p;
  return pArguments->process_d2Edr2(pSimulator, de, r, dx, i, j);
}

// *data functions
int KIM_COMPUTE_SimulatorComputeArguments_get_data_int(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int ** const ptr)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  return pArguments->get_data(makeArgumentNameCpp(argumentName), ptr);
}

int KIM_COMPUTE_SimulatorComputeArguments_get_data_double(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    double ** const ptr)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  return pArguments->get_data(makeArgumentNameCpp(argumentName), ptr);
}

// *compute functions
int KIM_COMPUTE_SimulatorComputeArguments_get_compute(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int * const flag)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pArguments->get_compute(argN, flag);
}

void KIM_COMPUTE_SimulatorComputeArguments_get_process_dEdr_compute(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    int * const flag)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  return pArguments->get_process_dEdr_compute(flag);
}

void KIM_COMPUTE_SimulatorComputeArguments_get_process_d2Edr2_compute(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    int * const flag)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  return pArguments->get_process_d2Edr2_compute(flag);
}

int KIM_COMPUTE_SimulatorComputeArguments_get_size(
    KIM_COMPUTE_SimulatorComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int * const size)
{
  KIM::COMPUTE::SimulatorComputeArguments *
      pArguments = (KIM::COMPUTE::SimulatorComputeArguments *) arguments->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pArguments->get_size(argN, size);
}

}  // extern "C"
