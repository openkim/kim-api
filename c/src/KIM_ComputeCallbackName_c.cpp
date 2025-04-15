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
// Release: This file is part of the kim-api-2.4.1 package.
//


#include <string>

#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#include "KIM_ComputeCallbackName.hpp"
#endif
extern "C" {
#ifndef KIM_COMPUTE_CALLBACK_NAME_H_
#include "KIM_ComputeCallbackName.h"
#endif
}  // extern "C"


namespace
{
KIM::ComputeCallbackName
makeComputeCallbackNameCpp(KIM_ComputeCallbackName const computeCallbackName)
{
  KIM::ComputeCallbackName const * const computeCallbackNameCpp
      = reinterpret_cast<KIM::ComputeCallbackName const *>(
          &computeCallbackName);
  return *computeCallbackNameCpp;
}

KIM_ComputeCallbackName
makeComputeCallbackNameC(KIM::ComputeCallbackName const computeCallbackName)
{
  KIM_ComputeCallbackName const * const computeCallbackNameC
      = reinterpret_cast<KIM_ComputeCallbackName const *>(&computeCallbackName);
  return *computeCallbackNameC;
}
}  // namespace

extern "C" {
KIM_ComputeCallbackName
KIM_ComputeCallbackName_FromString(char const * const str)
{
  return makeComputeCallbackNameC(KIM::ComputeCallbackName(std::string(str)));
}

int KIM_ComputeCallbackName_Known(
    KIM_ComputeCallbackName const computeCallbackName)
{
  return makeComputeCallbackNameCpp(computeCallbackName).Known();
}

int KIM_ComputeCallbackName_Equal(KIM_ComputeCallbackName const lhs,
                                  KIM_ComputeCallbackName const rhs)
{
  return (lhs.computeCallbackNameID == rhs.computeCallbackNameID);
}

int KIM_ComputeCallbackNameNot_Equal(KIM_ComputeCallbackName const lhs,
                                     KIM_ComputeCallbackName const rhs)
{
  return (!KIM_ComputeCallbackName_Equal(lhs, rhs));
}

char const *
KIM_ComputeCallbackName_ToString(KIM_ComputeCallbackName computeCallbackName)
{
  return makeComputeCallbackNameCpp(computeCallbackName).ToString().c_str();
}

// Order doesn't matter as long as all values are unique
#include "KIM_ComputeCallbackName.inc"
KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_GetNeighborList
    = {ID_GetNeighborList};
KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_ProcessDEDrTerm
    = {ID_ProcessDEDrTerm};
KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_ProcessD2EDr2Term
    = {ID_ProcessD2EDr2Term};

void KIM_COMPUTE_CALLBACK_NAME_GetNumberOfComputeCallbackNames(
    int * const numberOfComputeCallbackNames)
{
  KIM::COMPUTE_CALLBACK_NAME::GetNumberOfComputeCallbackNames(
      numberOfComputeCallbackNames);
}

int KIM_COMPUTE_CALLBACK_NAME_GetComputeCallbackName(
    int const index, KIM_ComputeCallbackName * const computeCallbackName)
{
  KIM::ComputeCallbackName computeCallbackNameCpp;
  int error = KIM::COMPUTE_CALLBACK_NAME::GetComputeCallbackName(
      index, &computeCallbackNameCpp);
  if (error) return error;
  *computeCallbackName = makeComputeCallbackNameC(computeCallbackNameCpp);
  return false;
}

}  // extern "C"
