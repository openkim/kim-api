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
