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
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#ifndef KIM_CALL_BACK_NAME_HPP_
#include "KIM_CallBackName.hpp"
#endif
extern "C"
{
#ifndef KIM_CALL_BACK_NAME_H_
#include "KIM_CallBackName.h"
#endif
}  // extern "C"


namespace
{
KIM::CallBackName makeCallBackNameCpp(KIM_CallBackName const callBackName)
{
  KIM::CallBackName const * const callBackNameCpp
      = reinterpret_cast<KIM::CallBackName const * const>(&callBackName);
  return *callBackNameCpp;
}

}  // namespace

extern "C"
{
char const * const KIM_CallBackNameString(KIM_CallBackName callBackName)
{
  return (makeCallBackNameCpp(callBackName)).string().c_str();
}

// Order doesn't matter as long as all values are unique
KIM_CallBackName const KIM_CALL_BACK_NAME_get_neigh = {0};
KIM_CallBackName const KIM_CALL_BACK_NAME_process_dEdr = {1};
KIM_CallBackName const KIM_CALL_BACK_NAME_process_d2Edr2 = {2};

void KIM_CALL_BACK_NAME_get_number_of_call_backs(int * const numberOfCallBacks)
{
  KIM::CALL_BACK_NAME::get_number_of_call_backs(numberOfCallBacks);
}

int KIM_CALL_BACK_NAME_get_call_back_name(int const index,
                                          KIM_CallBackName * const callBackName)
{
  KIM::CallBackName callBackNameCpp;
  int err = KIM::CALL_BACK_NAME::get_call_back_name(index, &callBackNameCpp);
  if (err) return err;
  KIM_CallBackName * callBackNameC
      = reinterpret_cast<KIM_CallBackName *>(&callBackNameCpp);
  *callBackName = *callBackNameC;
  return false;
}
}  // extern "C"
