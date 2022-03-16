//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
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

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif
extern "C" {
#ifndef KIM_SUPPORT_STATUS_H_
#include "KIM_SupportStatus.h"
#endif
}  // extern "C"

namespace
{
KIM::SupportStatus makeSupportStatusCpp(KIM_SupportStatus const supportStatus)
{
  KIM::SupportStatus const * const supportStatusCpp
      = reinterpret_cast<KIM::SupportStatus const *>(&supportStatus);
  return *supportStatusCpp;
}

KIM_SupportStatus makeSupportStatusC(KIM::SupportStatus const supportStatus)
{
  KIM_SupportStatus const * const supportStatusC
      = reinterpret_cast<KIM_SupportStatus const *>(&supportStatus);
  return *supportStatusC;
}
}  // namespace

extern "C" {
KIM_SupportStatus KIM_SupportStatus_FromString(char const * const str)
{
  return makeSupportStatusC(KIM::SupportStatus(std::string(str)));
}

int KIM_SupportStatus_Known(KIM_SupportStatus const supportStatus)
{
  return makeSupportStatusCpp(supportStatus).Known();
}

int KIM_SupportStatus_Equal(KIM_SupportStatus const lhs,
                            KIM_SupportStatus const rhs)
{
  return (lhs.supportStatusID == rhs.supportStatusID);
}

int KIM_SupportStatus_NotEqual(KIM_SupportStatus const lhs,
                               KIM_SupportStatus const rhs)
{
  return (!KIM_SupportStatus_Equal(lhs, rhs));
}

char const * KIM_SupportStatus_ToString(KIM_SupportStatus const supportStatus)
{
  return makeSupportStatusCpp(supportStatus).ToString().c_str();
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

int KIM_SUPPORT_STATUS_GetSupportStatus(int const index,
                                        KIM_SupportStatus * const supportStatus)
{
  KIM::SupportStatus supportStatusCpp;
  int error = KIM::SUPPORT_STATUS::GetSupportStatus(index, &supportStatusCpp);
  if (error) return error;
  *supportStatus = makeSupportStatusC(supportStatusCpp);
  return false;
}

}  // extern "C"
