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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.1.2 package.
//

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_HPP_
#include "KIM_ModelComputeArguments.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_HPP_
#include "KIM_ComputeArgumentsImplementation.hpp"
#endif

#define CONVERT_POINTER                  \
  ComputeArgumentsImplementation * pImpl \
      = reinterpret_cast<ComputeArgumentsImplementation *>(pimpl)

namespace KIM
{
int ModelComputeArguments::GetNeighborList(
    int const neighborListIndex,
    int const particleNumber,
    int * const numberOfNeighbors,
    int const ** const neighborsOfParticle) const
{
  CONVERT_POINTER;

  return pImpl->GetNeighborList(neighborListIndex,
                                particleNumber,
                                numberOfNeighbors,
                                neighborsOfParticle);
}

int ModelComputeArguments::ProcessDEDrTerm(double const de,
                                           double const r,
                                           double const * const dx,
                                           int const i,
                                           int const j) const
{
  CONVERT_POINTER;

  return pImpl->ProcessDEDrTerm(de, r, dx, i, j);
}

int ModelComputeArguments::ProcessD2EDr2Term(double const de,
                                             double const * const r,
                                             double const * const dx,
                                             int const * const i,
                                             int const * const j) const
{
  CONVERT_POINTER;

  return pImpl->ProcessD2EDr2Term(de, r, dx, i, j);
}

int ModelComputeArguments::GetArgumentPointer(
    ComputeArgumentName const computeArgumentName, int const ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->GetArgumentPointer(computeArgumentName, ptr);
}

int ModelComputeArguments::GetArgumentPointer(
    ComputeArgumentName const computeArgumentName, int ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->GetArgumentPointer(computeArgumentName, ptr);
}

int ModelComputeArguments::GetArgumentPointer(
    ComputeArgumentName const computeArgumentName, double ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->GetArgumentPointer(computeArgumentName, ptr);
}

int ModelComputeArguments::GetArgumentPointer(
    ComputeArgumentName const computeArgumentName,
    double const ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->GetArgumentPointer(computeArgumentName, ptr);
}

int ModelComputeArguments::IsCallbackPresent(
    ComputeCallbackName const computeCallbackName, int * const present) const
{
  CONVERT_POINTER;

  return pImpl->IsCallbackPresent(computeCallbackName, present);
}

void ModelComputeArguments::SetModelBufferPointer(void * const ptr)
{
  CONVERT_POINTER;

  pImpl->SetModelBufferPointer(ptr);
}

void ModelComputeArguments::GetModelBufferPointer(void ** const ptr) const
{
  CONVERT_POINTER;

  pImpl->GetModelBufferPointer(ptr);
}

void ModelComputeArguments::LogEntry(LogVerbosity const logVerbosity,
                                     std::string const & message,
                                     int const lineNumber,
                                     std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void ModelComputeArguments::LogEntry(LogVerbosity const logVerbosity,
                                     std::stringstream const & message,
                                     int const lineNumber,
                                     std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

std::string const & ModelComputeArguments::ToString() const
{
  CONVERT_POINTER;

  return pImpl->ToString();
}

ModelComputeArguments::ModelComputeArguments() : pimpl(NULL) {}

ModelComputeArguments::~ModelComputeArguments() {}

}  // namespace KIM
