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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


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
