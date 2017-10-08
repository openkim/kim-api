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

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif

namespace KIM
{

SupportStatus::SupportStatus() : supportStatusID(0){}
SupportStatus::SupportStatus(int const id) : supportStatusID(id){}

bool SupportStatus::operator==(SupportStatus const & rhs) const
{return supportStatusID == rhs.supportStatusID;}
bool SupportStatus::operator!=(SupportStatus const & rhs) const
{return supportStatusID != rhs.supportStatusID;}

std::string SupportStatus::String() const
{
  if (*this == SUPPORT_STATUS::requiredByAPI)
    return "requiredByAPI";
  else if (*this == SUPPORT_STATUS::notSupported)
    return "notSupported";
  else if (*this == SUPPORT_STATUS::required)
    return "required";
  else if (*this == SUPPORT_STATUS::optional)
    return "optional";
  else
    return "unknown";
}

// Order doesn't matter as long as all values are unique
namespace SUPPORT_STATUS
{
SupportStatus const requiredByAPI(0);
SupportStatus const notSupported(1);
SupportStatus const required(2);
SupportStatus const optional(3);
}  // namespace SUPPORT_STATUS

}  // namespace KIM
