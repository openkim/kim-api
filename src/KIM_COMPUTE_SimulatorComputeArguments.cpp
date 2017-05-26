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

#ifndef KIM_COMPUTE_SIMULATOR_COMPUTE_ARGUMENTS_HPP_
#include "KIM_COMPUTE_SimulatorComputeArguments.hpp"
#endif

#include "old_KIM_API.h"
#include "old_KIM_API_status.h"

namespace KIM
{
namespace COMPUTE
{

namespace
{
char const * const argumentString(ArgumentName const argumentName)
{
  if (argumentName == ARGUMENT_NAME::numberOfParticles)
    return "numberOfParticles";
  else if (argumentName == ARGUMENT_NAME::numberOfSpecies)
    return "numberOfSpecies";
  else if (argumentName == ARGUMENT_NAME::particleSpecies)
    return "particleSpecies";
  else if (argumentName == ARGUMENT_NAME::particleContributing)
    return "particleContributing";
  else if (argumentName == ARGUMENT_NAME::coordinates)
    return "coordinates";
  else if (argumentName == ARGUMENT_NAME::energy)
    return "energy";
  else if (argumentName == ARGUMENT_NAME::forces)
    return "forces";
  else if (argumentName == ARGUMENT_NAME::particleEnergy)
    return "particleEnergy";
  else if (argumentName == ARGUMENT_NAME::virial)
    return "virial";
  else if (argumentName == ARGUMENT_NAME::particleVirial)
    return "particleVirial";
  else if (argumentName == ARGUMENT_NAME::hessian)
    return "hessian";
  else
    return "None";
}
}  // namesapce

void SimulatorComputeArguments::get_neighObject(void ** const ptr) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *ptr = pKIM->get_data("neighObject", &err);
  //return (err < KIM_STATUS_OK);
}

int SimulatorComputeArguments::get_neigh(int const neighborListIndex,
                                         int const particleNumber,
                                         int * const numberOfNeighbors,
                                         int const ** const neighborsOfParticle)
    const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  if (neighborListIndex != 0) return true;  // multiple list handling not ready
  int err = pKIM->get_neigh(this, neighborListIndex, particleNumber,
                            numberOfNeighbors, (int **) neighborsOfParticle);
  return err;  // Simulators should return 2.0 codes
}

int SimulatorComputeArguments::process_dEdr(Simulator const * const simulator,
                                            double const de, double const r,
                                            double const * const dx,
                                            int const i, int const j) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err = OLD_KIM::KIM_API_model::process_dEdr(&pKIM,
                                                 (double *) &de,
                                                 (double *) &r,
                                                 (double **) &dx,
                                                 (int *) &i,
                                                 (int *) &j);
  return (err < KIM_STATUS_OK);
}

int SimulatorComputeArguments::process_d2Edr2(Simulator const * const simulator,
                                              double const de,
                                              double const * const r,
                                              double const * const dx,
                                              int const * const i,
                                              int const * const j) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err = OLD_KIM::KIM_API_model::process_d2Edr2(&pKIM,
                                                   (double *) &de,
                                                   (double **) &r,
                                                   (double **) &dx,
                                                   (int **) &i,
                                                   (int **) &j);
  return (err < KIM_STATUS_OK);
}

// *data functions
int SimulatorComputeArguments::get_data(ArgumentName const argumentName,
                                        int const ** const ptr) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *ptr = (int*) pKIM->get_data(argumentString(argumentName), &err);
  return (err < KIM_STATUS_OK);
}

int SimulatorComputeArguments::get_data(ArgumentName const argumentName,
                                        int ** const ptr) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *ptr = (int*) pKIM->get_data(argumentString(argumentName), &err);
  return (err < KIM_STATUS_OK);
}

int SimulatorComputeArguments::get_data(ArgumentName const argumentName,
                                        double ** const ptr) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *ptr = (double*) pKIM->get_data(argumentString(argumentName), &err);
  return (err < KIM_STATUS_OK);
}

int SimulatorComputeArguments::get_data(ArgumentName const argumentName,
                                        double const ** const ptr) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *ptr = (double*) pKIM->get_data(argumentString(argumentName), &err);
  return (err < KIM_STATUS_OK);
}

// *compute functions
int SimulatorComputeArguments::get_compute(ArgumentName const argumentName,
                                           int * const flag) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *flag = pKIM->get_compute(argumentString(argumentName), &err);
  return (err < KIM_STATUS_OK);
}

void SimulatorComputeArguments::get_process_dEdr_compute(int * const flag)
    const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *flag = pKIM->get_compute("process_dEdr", &err);
}

void SimulatorComputeArguments::get_process_d2Edr2_compute(int * const flag)
    const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *flag = pKIM->get_compute("process_d2Edr2", &err);
}

int SimulatorComputeArguments::get_size(ArgumentName const argumentName,
                                        int * const size) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *size = pKIM->get_size(argumentString(argumentName), &err);
  return (err < KIM_STATUS_OK);
}

SimulatorComputeArguments::SimulatorComputeArguments() : pimpl(0)
{
}

SimulatorComputeArguments::~SimulatorComputeArguments()
{
}

}  // namespace COMPUTE
}  // namespace KIM
