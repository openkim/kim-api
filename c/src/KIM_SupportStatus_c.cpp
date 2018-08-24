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

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif
extern "C"
{
#ifndef KIM_SUPPORT_STATUS_H_
#include "KIM_SupportStatus.h"
#endif
}  // extern "C"

namespace
{
KIM::SupportStatus makeSupportStatusCpp(
    KIM_SupportStatus const supportStatus)
{
  KIM::SupportStatus const * const supportStatusCpp
      = reinterpret_cast <KIM::SupportStatus const *>(&supportStatus);
  return *supportStatusCpp;
}

KIM_SupportStatus makeSupportStatusC(
    KIM::SupportStatus const supportStatus)
{
  KIM_SupportStatus const * const supportStatusC
      = reinterpret_cast <KIM_SupportStatus const *>(&supportStatus);
  return *supportStatusC;
}
}  // namespace

extern "C"
{
KIM_SupportStatus KIM_SupportStatus_FromString(char const * const str)
{
  return makeSupportStatusC(KIM::SupportStatus(std::string(str)));
}

int KIM_SupportStatus_Equal(KIM_SupportStatus const left,
                            KIM_SupportStatus const right)
{
  return (left.supportStatusID == right.supportStatusID);
}

int KIM_SupportStatus_NotEqual(KIM_SupportStatus const left,
                               KIM_SupportStatus const right)
{
  return (!KIM_SupportStatus_Equal(left, right));
}

char const * KIM_SupportStatus_String(KIM_SupportStatus const supportStatus)
{
  return makeSupportStatusCpp(supportStatus).String().c_str();
}

#include "KIM_SupportStatus.inc"
KIM_SupportStatus const KIM_SUPPORT_STATUS_requiredByAPI = {ID_requiredByAPI};
KIM_SupportStatus const KIM_SUPPORT_STATUS_notSupported = {ID_notSupported};
KIM_SupportStatus const KIM_SUPPORT_STATUS_required = {ID_required};
KIM_SupportStatus const KIM_SUPPORT_STATUS_optional = {ID_optional};

void KIM_SUPPORT_STATUS_GetNumberOfSupportStatuses(
    int * const numberOfSupportStatuses)
{
  KIM::SUPPORT_STATUS::GetNumberOfSupportStatuses(numberOfSupportStatuses);
}

int KIM_SUPPORT_STATUS_GetSupportStatus(
    int const index,
    KIM_SupportStatus * const supportStatus)
{
  KIM::SupportStatus supportStatusCpp;
  int error = KIM::SUPPORT_STATUS::GetSupportStatus(index, &supportStatusCpp);
  if (error) return error;
  *supportStatus = makeSupportStatusC(supportStatusCpp);
  return false;
}

}  // extern "C"
