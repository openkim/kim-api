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
// Release: This file is part of the kim-api.git repository.
//


#include <string>

#ifndef KIM_TIME_UNIT_HPP_
#include "KIM_TimeUnit.hpp"
#endif
extern "C" {
#ifndef KIM_TIME_UNIT_H_
#include "KIM_TimeUnit.h"
#endif
}


namespace
{
KIM::TimeUnit makeTimeUnitCpp(KIM_TimeUnit const timeUnit)
{
  KIM::TimeUnit const * const timeUnitCpp
      = reinterpret_cast<KIM::TimeUnit const *>(&timeUnit);
  return *timeUnitCpp;
}

KIM_TimeUnit makeTimeUnitC(KIM::TimeUnit const timeUnit)
{
  KIM_TimeUnit const * const timeUnitC
      = reinterpret_cast<KIM_TimeUnit const *>(&timeUnit);
  return *timeUnitC;
}
}  // namespace

extern "C" {
KIM_TimeUnit KIM_TimeUnit_FromString(char const * const str)
{
  return makeTimeUnitC(KIM::TimeUnit(std::string(str)));
}

int KIM_TimeUnit_Known(KIM_TimeUnit const timeUnit)
{
  return makeTimeUnitCpp(timeUnit).Known();
}

int KIM_TimeUnit_Equal(KIM_TimeUnit const lhs, KIM_TimeUnit const rhs)
{
  return (lhs.timeUnitID == rhs.timeUnitID);
}

int KIM_TimeUnit_NotEqual(KIM_TimeUnit const lhs, KIM_TimeUnit const rhs)
{
  return (!KIM_TimeUnit_Equal(lhs, rhs));
}

char const * KIM_TimeUnit_ToString(KIM_TimeUnit const tiemUnit)
{
  return makeTimeUnitCpp(tiemUnit).ToString().c_str();
}

#include "KIM_TimeUnit.inc"
KIM_TimeUnit const KIM_TIME_UNIT_unused = {ID_unused};
KIM_TimeUnit const KIM_TIME_UNIT_fs = {ID_fs};
KIM_TimeUnit const KIM_TIME_UNIT_ps = {ID_ps};
KIM_TimeUnit const KIM_TIME_UNIT_ns = {ID_ns};
KIM_TimeUnit const KIM_TIME_UNIT_s = {ID_s};

void KIM_TIME_UNIT_GetNumberOfTimeUnits(int * const numberOfTimeUnits)
{
  KIM::TIME_UNIT::GetNumberOfTimeUnits(numberOfTimeUnits);
}

int KIM_TIME_UNIT_GetTimeUnit(int const index, KIM_TimeUnit * const timeUnit)
{
  KIM::TimeUnit timeUnitCpp;
  int error = KIM::TIME_UNIT::GetTimeUnit(index, &timeUnitCpp);
  if (error) return error;
  *timeUnit = makeTimeUnitC(timeUnitCpp);
  return false;
}

}  // extern "C"
