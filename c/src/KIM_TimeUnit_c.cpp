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

#ifndef KIM_TIME_UNIT_HPP_
#include "KIM_TimeUnit.hpp"
#endif
extern "C"
{
#ifndef KIM_TIME_UNIT_H_
#include "KIM_TimeUnit.h"
#endif
}


namespace
{
KIM::TimeUnit makeTimeUnitCpp(KIM_TimeUnit const timeUnit)
{
  KIM::TimeUnit const * const timeUnitCpp
      = reinterpret_cast <KIM::TimeUnit const *>(&timeUnit);
  return *timeUnitCpp;
}

KIM_TimeUnit makeTimeUnitC(KIM::TimeUnit const timeUnit)
{
  KIM_TimeUnit const * const timeUnitC
      = reinterpret_cast <KIM_TimeUnit const *>(&timeUnit);
  return *timeUnitC;
}
}  // namespace

extern "C"
{
KIM_TimeUnit KIM_TimeUnit_FromString(char const * const str)
{
  return makeTimeUnitC(KIM::TimeUnit(std::string(str)));
}

int KIM_TimeUnit_Equal(KIM_TimeUnit const left, KIM_TimeUnit right)
{
  return (left.timeUnitID == right.timeUnitID);
}

int KIM_TimeUnit_NotEqual(KIM_TimeUnit const left, KIM_TimeUnit right)
{
  return (!KIM_TimeUnit_Equal(left, right));
}

char const * KIM_TimeUnit_String(KIM_TimeUnit const tiemUnit)
{
  return makeTimeUnitCpp(tiemUnit).String().c_str();
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
