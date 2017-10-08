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
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <vector>

#ifndef KIM_CALLBACK_NAME_HPP_
#include "KIM_CallbackName.hpp"
#endif

namespace KIM
{

CallbackName::CallbackName() : callbackNameID(0){}
CallbackName::CallbackName(int const id) : callbackNameID(id){}

bool CallbackName::operator==(CallbackName const & rhs) const
{return callbackNameID == rhs.callbackNameID;}
bool CallbackName::operator!=(CallbackName const & rhs) const
{return callbackNameID != rhs.callbackNameID;}

std::string CallbackName::String() const
{
  if (*this == CALLBACK_NAME::GetNeighborList) return "GetNeighborList";
  else if (*this == CALLBACK_NAME::ProcessDEDrTerm) return "ProcessDEDrTerm";
  else if (*this == CALLBACK_NAME::ProcessD2EDr2Term)
    return "ProcessD2EDr2Term";

  return "unknown";
}

// Order doesn't matter as long as all values are unique
namespace CALLBACK_NAME
{
CallbackName const GetNeighborList(0);
CallbackName const ProcessDEDrTerm(1);
CallbackName const ProcessD2EDr2Term(2);

extern std::vector<CallbackName> const requiredByAPI_Callbacks = {
  GetNeighborList};

void GetNumberOfCallbacks(int * const numberOfCallbacks)
{
  *numberOfCallbacks = 3;
}

int GetCallbackName(int const index, CallbackName * const callbackName)
{
  switch (index)
  {
    case 0:
      *callbackName = CALLBACK_NAME::GetNeighborList;
      break;
    case 1:
      *callbackName = CALLBACK_NAME::ProcessDEDrTerm;
      break;
    case 2:
      *callbackName = CALLBACK_NAME::ProcessD2EDr2Term;
      break;
    default:
      return true;  // invalid index
      break;
  }

  return false;  // no error
}

}  // namespace CALLBACK_NAME

}  // namespace KIM
