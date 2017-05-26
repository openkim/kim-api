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

#ifndef KIM_CALL_BACK_NAME_HPP_
#include "KIM_CallBackName.hpp"
#endif

namespace KIM
{

CallBackName::CallBackName() : callBackNameID(0){}
CallBackName::CallBackName(int const id) : callBackNameID(id){}

bool CallBackName::operator==(CallBackName const & rhs) const
{return callBackNameID == rhs.callBackNameID;}
bool CallBackName::operator!=(CallBackName const & rhs) const
{return callBackNameID != rhs.callBackNameID;}

std::string CallBackName::string() const
{
  if (*this == CALL_BACK_NAME::get_neigh) return "get_neigh";
  else if (*this == CALL_BACK_NAME::process_dEdr) return "process_dEdr";
  else if (*this == CALL_BACK_NAME::process_d2Edr2) return "process_d2Edr2";

  return "unknown";
}

// Order doesn't matter as long as all values are unique
namespace CALL_BACK_NAME
{
CallBackName const get_neigh(0);
CallBackName const process_dEdr(1);
CallBackName const process_d2Edr2(2);

extern std::vector<CallBackName> const mandatoryCallBacks = {
  get_neigh};

void get_number_of_call_backs(int * const numberOfCallBacks)
{
  *numberOfCallBacks = 3;
}

int get_call_back_name(int const index, CallBackName * const callBackName)
{
  switch (index)
  {
    case 0:
      *callBackName = CALL_BACK_NAME::get_neigh;
      break;
    case 1:
      *callBackName = CALL_BACK_NAME::process_dEdr;
      break;
    case 2:
      *callBackName = CALL_BACK_NAME::process_d2Edr2;
      break;
    default:
      return true;  // invalid index
      break;
  }

  return false;  // no error
}

}  // namespace CALL_BACK_NAME

}  // namespace KIM
