//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common
// Development and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name
// LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner].
// All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#include "KIM_ComputeCallbackName.hpp"
#endif
extern "C"
{
#ifndef KIM_COMPUTE_CALLBACK_NAME_H_
#include "KIM_ComputeCallbackName.h"
#endif
}  // extern "C"


namespace
{
KIM::ComputeCallbackName makeComputeCallbackNameCpp(
    KIM_ComputeCallbackName const computeCallbackName)
{
  KIM::ComputeCallbackName const * const computeCallbackNameCpp
      = reinterpret_cast<KIM::ComputeCallbackName const * const>
      (&computeCallbackName);
  return *computeCallbackNameCpp;
}

KIM_ComputeCallbackName makeComputeCallbackNameC(
    KIM::ComputeCallbackName const computeCallbackName)
{
  KIM_ComputeCallbackName const * const computeCallbackNameC
      = reinterpret_cast<KIM_ComputeCallbackName const * const>
      (&computeCallbackName);
  return *computeCallbackNameC;
}
}  // namespace

extern "C"
{
KIM_ComputeCallbackName KIM_ComputeCallbackNameFromString(
    char const * const str)
{
  return makeComputeCallbackNameC(KIM::ComputeCallbackName(std::string(str)));
}

int KIM_ComputeCallbackNameEqual(KIM_ComputeCallbackName const left,
                                 KIM_ComputeCallbackName const right)
{
  return (left.computeCallbackNameID == right.computeCallbackNameID);
}

int KIM_ComputeCallbackNameNotEqual(KIM_ComputeCallbackName const left,
                                    KIM_ComputeCallbackName const right)
{
  return (!KIM_ComputeCallbackNameEqual(left, right));
}

char const * const KIM_ComputeCallbackNameString(
    KIM_ComputeCallbackName computeCallbackName)
{
  return makeComputeCallbackNameCpp(computeCallbackName).String().c_str();
}

// Order doesn't matter as long as all values are unique
KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_GetNeighborList
= {KIM::COMPUTE_CALLBACK_NAME::GetNeighborList.computeCallbackNameID};
KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_ProcessDEDrTerm
= {KIM::COMPUTE_CALLBACK_NAME::ProcessDEDrTerm.computeCallbackNameID};
KIM_ComputeCallbackName const KIM_COMPUTE_CALLBACK_NAME_ProcessD2EDr2Term
= {KIM::COMPUTE_CALLBACK_NAME::ProcessD2EDr2Term.computeCallbackNameID};

void KIM_COMPUTE_CALLBACK_NAME_GetNumberOfComputeCallbacks(
    int * const numberOfComputeCallbacks)
{
  KIM::COMPUTE_CALLBACK_NAME::GetNumberOfComputeCallbacks(
      numberOfComputeCallbacks);
}

int KIM_COMPUTE_CALLBACK_NAME_GetComputeCallbackName(
    int const index,
    KIM_ComputeCallbackName * const computeCallbackName)
{
  KIM::ComputeCallbackName computeCallbackNameCpp;
  int err = KIM::COMPUTE_CALLBACK_NAME::GetComputeCallbackName(
      index, &computeCallbackNameCpp);
  if (err) return err;
  KIM_ComputeCallbackName * computeCallbackNameC
      = reinterpret_cast<KIM_ComputeCallbackName *>(&computeCallbackNameCpp);
  *computeCallbackName = *computeCallbackNameC;
  return false;
}
}  // extern "C"
