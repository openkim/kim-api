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

#include <map>
#include <vector>

#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#include "KIM_ComputeCallbackName.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace COMPUTE_CALLBACK_NAME
{
#include "KIM_ComputeCallbackName.inc"
ComputeCallbackName const GetNeighborList(ID_GetNeighborList);
ComputeCallbackName const ProcessDEDrTerm(ID_ProcessDEDrTerm);
ComputeCallbackName const ProcessD2EDr2Term(ID_ProcessD2EDr2Term);

namespace
{
typedef std::map<ComputeCallbackName const,
                 std::string,
                 COMPUTE_CALLBACK_NAME::Comparator>
    StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[GetNeighborList] = "GetNeighborList";
  m[ProcessDEDrTerm] = "ProcessDEDrTerm";
  m[ProcessD2EDr2Term] = "ProcessD2EDr2Term";
  return m;
}

StringMap const computeCallbackNameToString = GetStringMap();
std::string const computeCallbackNameUnknown("unknown");
}  // namespace


namespace
{
typedef std::vector<ComputeCallbackName> ComputeCallbackVector;

ComputeCallbackVector const GetComputeCallbackVector()
{
  ComputeCallbackVector v;
  v.push_back(GetNeighborList);
  return v;
}
}  // namespace
// Used by KIM::ModelImplementation
extern ComputeCallbackVector const requiredByAPI_ComputeCallbacks
    = GetComputeCallbackVector();


void GetNumberOfComputeCallbackNames(int * const numberOfComputeCallbackNames)
{
  *numberOfComputeCallbackNames = computeCallbackNameToString.size();
}

int GetComputeCallbackName(int const index,
                           ComputeCallbackName * const computeCallbackName)
{
  int numberOfComputeCallbackNames;
  GetNumberOfComputeCallbackNames(&numberOfComputeCallbackNames);
  if ((index < 0) || (index >= numberOfComputeCallbackNames)) return true;

  StringMap::const_iterator iter = computeCallbackNameToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *computeCallbackName = iter->first;
  return false;  // no error
}
}  // namespace COMPUTE_CALLBACK_NAME

// implementation of ComputeCallbackName
ComputeCallbackName::ComputeCallbackName() {}
ComputeCallbackName::ComputeCallbackName(int const id) :
    computeCallbackNameID(id)
{
}
ComputeCallbackName::ComputeCallbackName(std::string const & str)
{
  computeCallbackNameID = -1;
  for (COMPUTE_CALLBACK_NAME::StringMap::const_iterator iter
       = COMPUTE_CALLBACK_NAME::computeCallbackNameToString.begin();
       iter != COMPUTE_CALLBACK_NAME::computeCallbackNameToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      computeCallbackNameID = (iter->first).computeCallbackNameID;
      break;
    }
  }
}

bool ComputeCallbackName::Known() const
{
  int numberOfComputeCallbackNames;
  COMPUTE_CALLBACK_NAME::GetNumberOfComputeCallbackNames(
      &numberOfComputeCallbackNames);

  for (int i = 0; i < numberOfComputeCallbackNames; ++i)
  {
    ComputeCallbackName ccn;
    COMPUTE_CALLBACK_NAME::GetComputeCallbackName(i, &ccn);

    if (*this == ccn) { return true; }
  }

  return false;
}

bool ComputeCallbackName::operator==(ComputeCallbackName const & rhs) const
{
  return computeCallbackNameID == rhs.computeCallbackNameID;
}
bool ComputeCallbackName::operator!=(ComputeCallbackName const & rhs) const
{
  return computeCallbackNameID != rhs.computeCallbackNameID;
}

std::string const & ComputeCallbackName::ToString() const
{
  COMPUTE_CALLBACK_NAME::StringMap::const_iterator iter
      = COMPUTE_CALLBACK_NAME::computeCallbackNameToString.find(*this);
  if (iter == COMPUTE_CALLBACK_NAME::computeCallbackNameToString.end())
    return COMPUTE_CALLBACK_NAME::computeCallbackNameUnknown;
  else
    return iter->second;
}
}  // namespace KIM
