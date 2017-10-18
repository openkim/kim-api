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
KIM::TimeUnit const makeTimeUnitCpp(KIM_TimeUnit const timeUnit)
{
  KIM::TimeUnit const * const timeUnitCpp
      = reinterpret_cast <KIM::TimeUnit const * const>(&timeUnit);
  return *timeUnitCpp;
}

KIM_TimeUnit const makeTimeUnitC(KIM::TimeUnit const timeUnit)
{
  KIM_TimeUnit const * const timeUnitC
      = reinterpret_cast <KIM_TimeUnit const * const>(&timeUnit);
  return *timeUnitC;
}
}  // namespace

extern "C"
{
KIM_TimeUnit KIM_TimeUnitFromString(char const * const str)
{
  return makeTimeUnitC(KIM::TimeUnit(std::string(str)));
}

int KIM_TimeUnitEqual(KIM_TimeUnit const left, KIM_TimeUnit right)
{
  return (left.timeUnitID == right.timeUnitID);
}

int KIM_TimeUnitNotEqual(KIM_TimeUnit const left, KIM_TimeUnit right)
{
  return (!KIM_TimeUnitEqual(left, right));
}

char const * const KIM_TimeUnitString(KIM_TimeUnit const tiemUnit)
{
  return (makeTimeUnitCpp(tiemUnit)).String().c_str();
}

KIM_TimeUnit const KIM_TIME_UNIT_any = {0};
KIM_TimeUnit const KIM_TIME_UNIT_fs = {1};
KIM_TimeUnit const KIM_TIME_UNIT_ps = {2};
KIM_TimeUnit const KIM_TIME_UNIT_ns = {3};
KIM_TimeUnit const KIM_TIME_UNIT_s = {4};

}  // extern "C"
