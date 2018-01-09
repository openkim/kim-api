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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <vector>
#include <map>

#ifndef KIM_CALLBACK_NAME_HPP_
#include "KIM_CallbackName.hpp"
#endif

namespace KIM
{

// Order doesn't matter as long as all values are unique
namespace CALLBACK_NAME
{
CallbackName const GetNeighborList(0);
CallbackName const ProcessDEDrTerm(1);
CallbackName const ProcessD2EDr2Term(2);

namespace
{
typedef std::map<CallbackName const, std::string, CALLBACK_NAME::Comparator>
StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[GetNeighborList] = "GetNeighborList";
  m[ProcessDEDrTerm] = "ProcessDEDrTerm";
  m[ProcessD2EDr2Term] = "ProcessD2EDr2Term";
  return m;
}
}  // namespace
extern StringMap const callbackNameToString = GetStringMap();

namespace
{
typedef std::vector<CallbackName> CallbackVector;

CallbackVector const GetCallbackVector()
{
  CallbackVector v;
  v.push_back(GetNeighborList);
  return v;
}
}  // namespace
extern CallbackVector const requiredByAPI_Callbacks = GetCallbackVector();

void GetNumberOfCallbacks(int * const numberOfCallbacks)
{
  *numberOfCallbacks = callbackNameToString.size();
}

int GetCallbackName(int const index, CallbackName * const callbackName)
{
  int numberOfCallbacks;
  GetNumberOfCallbacks(&numberOfCallbacks);
  if ((index < 0) || (index >= numberOfCallbacks)) return true;

  StringMap::const_iterator iter = callbackNameToString.begin();
  for (int i=0; i<index; ++i) ++iter;
  *callbackName = iter->first;
  return false;  // no error
}
}  // namespace CALLBACK_NAME

// implementation of CallbackName
CallbackName::CallbackName() : callbackNameID(0){}
CallbackName::CallbackName(int const id) : callbackNameID(id){}
CallbackName::CallbackName(std::string const & str)
{
  callbackNameID = -1;
  for (CALLBACK_NAME::StringMap::const_iterator iter
           = CALLBACK_NAME::callbackNameToString.begin();
       iter != CALLBACK_NAME::callbackNameToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      callbackNameID = (iter->first).callbackNameID;
      break;
    }
  }
}

bool CallbackName::operator==(CallbackName const & rhs) const
{return callbackNameID == rhs.callbackNameID;}
bool CallbackName::operator!=(CallbackName const & rhs) const
{return callbackNameID != rhs.callbackNameID;}

std::string CallbackName::String() const
{
  std::string result;
  CALLBACK_NAME::StringMap::const_iterator iter
      = CALLBACK_NAME::callbackNameToString.find(*this);
  if (iter == CALLBACK_NAME::callbackNameToString.end())
    result = "unknown";
  else
    result = iter->second;

  return result;
}
}  // namespace KIM
