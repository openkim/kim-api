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

#ifndef KIM_TIME_UNIT_HPP_
#include "KIM_TimeUnit.hpp"
#endif

namespace KIM
{

TimeUnit::TimeUnit() : timeUnitID(0){}
TimeUnit::TimeUnit(int const id) : timeUnitID(id){}
bool TimeUnit::operator==(TimeUnit const & rhs) const
{return timeUnitID==rhs.timeUnitID;}
bool TimeUnit::operator!=(TimeUnit const & rhs) const
{return timeUnitID!=rhs.timeUnitID;}

std::string TimeUnit::string() const
{
  if (*this == TIME_UNIT::unused) return "unused";
  else if (*this == TIME_UNIT::fs) return "fs";
  else if (*this == TIME_UNIT::ps) return "ps";
  else if (*this == TIME_UNIT::ns) return "ns";
  else if (*this == TIME_UNIT::s) return "s";
  else return "unknown";
}


namespace TIME_UNIT
{
TimeUnit const unused(0);
TimeUnit const fs(1);
TimeUnit const ps(2);
TimeUnit const ns(3);
TimeUnit const s(4);
}  // namespace TIME_UNIT

}  // namespace KIM
