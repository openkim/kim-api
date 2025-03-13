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
// Release: This file is part of the kim-api.git repository.
//


#include <string>

#ifndef KIM_MODEL_ROUTINE_NAME_HPP_
#include "KIM_ModelRoutineName.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_ROUTINE_NAME_H_
#include "KIM_ModelRoutineName.h"
#endif
}  // extern "C"


namespace
{
KIM::ModelRoutineName
makeModelRoutineNameCpp(KIM_ModelRoutineName const modelRoutineName)
{
  KIM::ModelRoutineName const * const modelRoutineNameCpp
      = reinterpret_cast<KIM::ModelRoutineName const *>(&modelRoutineName);
  return *modelRoutineNameCpp;
}

KIM_ModelRoutineName
makeModelRoutineNameC(KIM::ModelRoutineName const modelRoutineName)
{
  KIM_ModelRoutineName const * const modelRoutineNameC
      = reinterpret_cast<KIM_ModelRoutineName const *>(&modelRoutineName);
  return *modelRoutineNameC;
}
}  // namespace

extern "C" {
KIM_ModelRoutineName KIM_ModelRoutineName_FromString(char const * const str)
{
  return makeModelRoutineNameC(KIM::ModelRoutineName(std::string(str)));
}

int KIM_ModelRoutineName_Known(KIM_ModelRoutineName const modelRoutineName)
{
  return makeModelRoutineNameCpp(modelRoutineName).Known();
}

int KIM_ModelRoutineName_Equal(KIM_ModelRoutineName const lhs,
                               KIM_ModelRoutineName const rhs)
{
  return (lhs.modelRoutineNameID == rhs.modelRoutineNameID);
}

int KIM_ModelRoutineName_NotEqual(KIM_ModelRoutineName const lhs,
                                  KIM_ModelRoutineName const rhs)
{
  return (!KIM_ModelRoutineName_Equal(lhs, rhs));
}

char const *
KIM_ModelRoutineName_ToString(KIM_ModelRoutineName modelRoutineName)
{
  return makeModelRoutineNameCpp(modelRoutineName).ToString().c_str();
}

// Order doesn't matter as long as all values are unique
#include "KIM_ModelRoutineName.inc"
KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Create = {ID_Create};
KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_ComputeArgumentsCreate
    = {ID_ComputeArgumentsCreate};
KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Compute = {ID_Compute};
KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Extension = {ID_Extension};
KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Refresh = {ID_Refresh};
KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_WriteParameterizedModel
    = {ID_WriteParameterizedModel};
KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_ComputeArgumentsDestroy
    = {ID_ComputeArgumentsDestroy};
KIM_ModelRoutineName const KIM_MODEL_ROUTINE_NAME_Destroy = {ID_Destroy};

void KIM_MODEL_ROUTINE_NAME_GetNumberOfModelRoutineNames(
    int * const numberOfModelRoutineNames)
{
  KIM::MODEL_ROUTINE_NAME::GetNumberOfModelRoutineNames(
      numberOfModelRoutineNames);
}

int KIM_MODEL_ROUTINE_NAME_GetModelRoutineName(
    int const index, KIM_ModelRoutineName * const modelRoutineName)
{
  KIM::ModelRoutineName modelRoutineNameCpp;
  int error = KIM::MODEL_ROUTINE_NAME::GetModelRoutineName(
      index, &modelRoutineNameCpp);
  if (error) return error;
  *modelRoutineName = makeModelRoutineNameC(modelRoutineNameCpp);
  return false;
}
}  // extern "C"
