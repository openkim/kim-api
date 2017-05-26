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

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIM_ARGUMENT_NAME_HPP_
#include "KIM_ArgumentName.hpp"
#endif

#ifndef KIM_CALL_BACK_NAME_HPP_
#include "KIM_CallBackName.hpp"
#endif

#ifndef KIM_MODEL_COMPUTE_HPP_
#include "KIM_ModelCompute.hpp"
#endif

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

#define CONVERT_POINTER ModelImplementation *pImpl      \
  = reinterpret_cast<ModelImplementation *>(pimpl)

namespace KIM
{

int ModelCompute::get_neigh(int const neighborListIndex,
                           int const particleNumber,
                           int * const numberOfNeighbors,
                           int const ** const neighborsOfParticle)
    const
{
  CONVERT_POINTER;

  return pImpl->get_neigh(neighborListIndex, particleNumber, numberOfNeighbors,
                          neighborsOfParticle);
}

int ModelCompute::process_dEdr(double const de, double const r,
                              double const * const dx,
                              int const i, int const j) const
{
  CONVERT_POINTER;

  return pImpl->process_dEdr(de, r, dx, i, j);
}

int ModelCompute::process_d2Edr2(double const de, double const * const r,
                                double const * const dx, int const * const i,
                                int const * const j) const
{
  CONVERT_POINTER;

  return pImpl->process_d2Edr2(de, r, dx, i, j);
}

int ModelCompute::get_data(ArgumentName const argumentName,
                          int const ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->get_data(argumentName, ptr);
}

int ModelCompute::get_data(ArgumentName const argumentName,
                          int ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->get_data(argumentName, ptr);
}

int ModelCompute::get_data(ArgumentName const argumentName,
                          double ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->get_data(argumentName, ptr);
}

int ModelCompute::get_data(ArgumentName const argumentName,
                          double const ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->get_data(argumentName, ptr);
}

int ModelCompute::is_call_back_present(CallBackName const callBackName,
                                      int * const present) const
{
  CONVERT_POINTER;

  return pImpl->is_call_back_present(callBackName, present);
}

void ModelCompute::get_model_buffer(void ** const ptr) const
{
  CONVERT_POINTER;

  pImpl->get_model_buffer(ptr);
}

void ModelCompute::Log(LogVerbosity const logVerbosity,
                       std::string const & message,
                       int const lineNumber,
                       std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->Log(logVerbosity, message, lineNumber, fileName);
}

std::string ModelCompute::string() const
{
  CONVERT_POINTER;

  return pImpl->string();
}

ModelCompute::ModelCompute() : pimpl(0)
{
}

ModelCompute::~ModelCompute()
{
}

}  // namespace KIM
