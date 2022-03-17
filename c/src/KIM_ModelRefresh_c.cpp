//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_REFRESH_HPP_
#include "KIM_ModelRefresh.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_REFRESH_H_
#include "KIM_ModelRefresh.h"
#endif
}  // extern "C"


struct KIM_ModelRefresh
{
  void * p;
};

#define CONVERT_POINTER             \
  KIM::ModelRefresh * pModelRefresh \
      = reinterpret_cast<KIM::ModelRefresh *>(modelRefresh->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace

extern "C" {
void KIM_ModelRefresh_SetInfluenceDistancePointer(
    KIM_ModelRefresh * const modelRefresh,
    double const * const influenceDistance)
{
  CONVERT_POINTER;

  pModelRefresh->SetInfluenceDistancePointer(influenceDistance);
}

void KIM_ModelRefresh_SetNeighborListPointers(
    KIM_ModelRefresh * const modelRefresh,
    int const numberOfNeighborLists,
    double const * const cutoffs,
    int const * const modelWillNotRequestNeighborsOfNoncontributingParticles)
{
  CONVERT_POINTER;

  pModelRefresh->SetNeighborListPointers(
      numberOfNeighborLists,
      cutoffs,
      modelWillNotRequestNeighborsOfNoncontributingParticles);
}

void KIM_ModelRefresh_GetModelBufferPointer(
    KIM_ModelRefresh const * const modelRefresh, void ** const ptr)
{
  CONVERT_POINTER;

  pModelRefresh->GetModelBufferPointer(ptr);
}

void KIM_ModelRefresh_LogEntry(KIM_ModelRefresh const * const modelRefresh,
                               KIM_LogVerbosity const logVerbosity,
                               char const * const message,
                               int const lineNumber,
                               char const * const fileName)
{
  CONVERT_POINTER;

  pModelRefresh->LogEntry(
      makeLogVerbosityCpp(logVerbosity), message, lineNumber, fileName);
}

char const *
KIM_ModelRefresh_ToString(KIM_ModelRefresh const * const modelRefresh)
{
  CONVERT_POINTER;

  return pModelRefresh->ToString().c_str();
}

}  // extern "C"
