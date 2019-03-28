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
// Release: This file is part of the kim-api-2.0.2 package.
//

#include <map>
#include <vector>

#ifndef KIM_MODEL_ROUTINE_NAME_HPP_
#include "KIM_ModelRoutineName.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace MODEL_ROUTINE_NAME
{
#include "KIM_ModelRoutineName.inc"
ModelRoutineName const Create(ID_Create);
ModelRoutineName const ComputeArgumentsCreate(ID_ComputeArgumentsCreate);
ModelRoutineName const Compute(ID_Compute);
ModelRoutineName const Extension(ID_Extension);
ModelRoutineName const Refresh(ID_Refresh);
ModelRoutineName const WriteParameterizedModel(ID_WriteParameterizedModel);
ModelRoutineName const ComputeArgumentsDestroy(ID_ComputeArgumentsDestroy);
ModelRoutineName const Destroy(ID_Destroy);

namespace
{
typedef std::
    map<ModelRoutineName const, std::string, MODEL_ROUTINE_NAME::Comparator>
        StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[Create] = "Create";
  m[ComputeArgumentsCreate] = "ComputeArgumentsCreate";
  m[Compute] = "Compute";
  m[Extension] = "Extension";
  m[Refresh] = "Refresh";
  m[WriteParameterizedModel] = "WriteParameterizedModel";
  m[ComputeArgumentsDestroy] = "ComputeArgumentsDestroy";
  m[Destroy] = "Destroy";
  return m;
}

StringMap const modelRoutineNameToString = GetStringMap();
std::string const modelRoutineNameUnknown("unknown");
}  // namespace

namespace
{
typedef std::vector<ModelRoutineName> ModelRoutineVector;
ModelRoutineVector const GetModelRoutineVector()
{
  ModelRoutineVector v;
  v.push_back(Create);
  v.push_back(ComputeArgumentsCreate);
  v.push_back(Compute);
  // v.push_back(Refresh);  // required only if adjustable prameters exist
  v.push_back(ComputeArgumentsDestroy);
  v.push_back(Destroy);
  return v;
}
}  // namespace
// used by KIM::ModelImplementation
extern ModelRoutineVector const requiredByAPI_ModelRoutines
    = GetModelRoutineVector();

void GetNumberOfModelRoutineNames(int * const numberOfModelRoutineNames)
{
  *numberOfModelRoutineNames = modelRoutineNameToString.size();
}

int GetModelRoutineName(int const index,
                        ModelRoutineName * const modelRoutineName)
{
  int numberOfModelRoutineNames;
  GetNumberOfModelRoutineNames(&numberOfModelRoutineNames);
  if ((index < 0) || (index >= numberOfModelRoutineNames)) return true;

  StringMap::const_iterator iter = modelRoutineNameToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *modelRoutineName = iter->first;
  return false;  // no error
}
}  // namespace MODEL_ROUTINE_NAME

// implementation of ModelRoutineName
ModelRoutineName::ModelRoutineName() {}
ModelRoutineName::ModelRoutineName(int const id) : modelRoutineNameID(id) {}
ModelRoutineName::ModelRoutineName(std::string const & str)
{
  modelRoutineNameID = -1;
  for (MODEL_ROUTINE_NAME::StringMap::const_iterator iter
       = MODEL_ROUTINE_NAME::modelRoutineNameToString.begin();
       iter != MODEL_ROUTINE_NAME::modelRoutineNameToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      modelRoutineNameID = (iter->first).modelRoutineNameID;
      break;
    }
  }
}

bool ModelRoutineName::Known() const
{
  int numberOfModelRoutineNames;
  MODEL_ROUTINE_NAME::GetNumberOfModelRoutineNames(&numberOfModelRoutineNames);

  for (int i = 0; i < numberOfModelRoutineNames; ++i)
  {
    ModelRoutineName routine;
    MODEL_ROUTINE_NAME::GetModelRoutineName(i, &routine);

    if (*this == routine) { return true; }
  }

  return false;
}

bool ModelRoutineName::operator==(ModelRoutineName const & rhs) const
{
  return modelRoutineNameID == rhs.modelRoutineNameID;
}
bool ModelRoutineName::operator!=(ModelRoutineName const & rhs) const
{
  return modelRoutineNameID != rhs.modelRoutineNameID;
}

std::string const & ModelRoutineName::ToString() const
{
  MODEL_ROUTINE_NAME::StringMap::const_iterator iter
      = MODEL_ROUTINE_NAME::modelRoutineNameToString.find(*this);
  if (iter == MODEL_ROUTINE_NAME::modelRoutineNameToString.end())
    return MODEL_ROUTINE_NAME::modelRoutineNameUnknown;
  else
    return iter->second;
}
}  // namespace KIM
