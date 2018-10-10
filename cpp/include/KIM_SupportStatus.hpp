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


#ifndef KIM_SUPPORT_STATUS_HPP_
#define KIM_SUPPORT_STATUS_HPP_

#include <string>

namespace KIM
{
class SupportStatus
{
 public:
  int supportStatusID;

  SupportStatus();
  SupportStatus(int const id);
  SupportStatus(std::string const & str);
  bool operator==(SupportStatus const & rhs) const;
  bool operator!=(SupportStatus const & rhs) const;
  std::string const & String() const;
};

namespace SUPPORT_STATUS
{
extern SupportStatus const requiredByAPI;
extern SupportStatus const notSupported;
extern SupportStatus const required;
extern SupportStatus const optional;

void GetNumberOfSupportStatuses(int * const numberOfSupportStatuses);
int GetSupportStatus(int const index, SupportStatus * const supportStatus);

struct Comparator
{
  bool operator()(SupportStatus const & a, SupportStatus const & b) const
  {
    return a.supportStatusID < b.supportStatusID;
  }
};
}  // namespace SUPPORT_STATUS
}  // namespace KIM
#endif  // KIM_SUPPORT_STATUS_HPP_
