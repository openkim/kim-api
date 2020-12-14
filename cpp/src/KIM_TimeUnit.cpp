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
// Copyright (c) 2016--2020, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.2.1 package.
//

#include <map>

#ifndef KIM_TIME_UNIT_HPP_
#include "KIM_TimeUnit.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace TIME_UNIT
{
#include "KIM_TimeUnit.inc"
TimeUnit const unused(ID_unused);
TimeUnit const fs(ID_fs);
TimeUnit const ps(ID_ps);
TimeUnit const ns(ID_ns);
TimeUnit const s(ID_s);

namespace
{
typedef std::map<TimeUnit const, std::string, TIME_UNIT::Comparator> StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[unused] = "unused";
  m[fs] = "fs";
  m[ps] = "ps";
  m[ns] = "ns";
  m[s] = "s";
  return m;
}

StringMap const timeUnitToString = GetStringMap();
std::string const timeUnitUnknown("unknown");
}  // namespace

void GetNumberOfTimeUnits(int * const numberOfTimeUnits)
{
  *numberOfTimeUnits = timeUnitToString.size();
}

int GetTimeUnit(int const index, TimeUnit * const timeUnit)
{
  int numberOfTimeUnits;
  GetNumberOfTimeUnits(&numberOfTimeUnits);
  if ((index < 0) || (index >= numberOfTimeUnits)) return true;

  StringMap::const_iterator iter = timeUnitToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *timeUnit = iter->first;
  return false;  // no error
}
}  // namespace TIME_UNIT

// implementation of TimeUnit
TimeUnit::TimeUnit() {}
TimeUnit::TimeUnit(int const id) : timeUnitID(id) {}
TimeUnit::TimeUnit(std::string const & str)
{
  timeUnitID = -1;
  for (TIME_UNIT::StringMap::const_iterator iter
       = TIME_UNIT::timeUnitToString.begin();
       iter != TIME_UNIT::timeUnitToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      timeUnitID = (iter->first).timeUnitID;
      break;
    }
  }
}

bool TimeUnit::Known() const
{
  int numberOfTimeUnits;
  TIME_UNIT::GetNumberOfTimeUnits(&numberOfTimeUnits);

  for (int i = 0; i < numberOfTimeUnits; ++i)
  {
    TimeUnit tmUnit;
    TIME_UNIT::GetTimeUnit(i, &tmUnit);

    if (*this == tmUnit) { return true; }
  }

  return false;
}

bool TimeUnit::operator==(TimeUnit const & rhs) const
{
  return timeUnitID == rhs.timeUnitID;
}
bool TimeUnit::operator!=(TimeUnit const & rhs) const
{
  return timeUnitID != rhs.timeUnitID;
}

std::string const & TimeUnit::ToString() const
{
  TIME_UNIT::StringMap::const_iterator iter
      = TIME_UNIT::timeUnitToString.find(*this);
  if (iter == TIME_UNIT::timeUnitToString.end())
    return TIME_UNIT::timeUnitUnknown;
  else
    return iter->second;
}
}  // namespace KIM
