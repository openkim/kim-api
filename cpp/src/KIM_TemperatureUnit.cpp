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

#include <map>

#ifndef KIM_TEMPERATURE_UNIT_HPP_
#include "KIM_TemperatureUnit.hpp"
#endif

namespace KIM
{

// Order doesn't matter as long as all values are unique
namespace TEMPERATURE_UNIT
{
TemperatureUnit const unused(0);
TemperatureUnit const K(1);

namespace
{
typedef std::map<TemperatureUnit const, std::string,
                 TEMPERATURE_UNIT::Comparator>
StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[unused] = "unused";
  m[K] = "K";
  return m;
}

StringMap const temperatureUnitToString = GetStringMap();
std::string const temperatureUnitUnknown("unknown");
}  // namespace


void GetNumberOfTemperatureUnits(int * const numberOfTemperatureUnits)
{
  *numberOfTemperatureUnits = temperatureUnitToString.size();
}

int GetTemperatureUnit(int const index, TemperatureUnit * const temperatureUnit)
{
  int numberOfTemperatureUnits;
  GetNumberOfTemperatureUnits(&numberOfTemperatureUnits);
  if ((index < 0) || (index >= numberOfTemperatureUnits)) return true;

  StringMap::const_iterator iter = temperatureUnitToString.begin();
  for (int i=0; i<index; ++i) ++iter;
  *temperatureUnit = iter->first;
  return false;  // no error
}
}  // namespace TEMPERATURE_UNIT

// implementation of TemperatureUnit
TemperatureUnit::TemperatureUnit() : temperatureUnitID(0){}
TemperatureUnit::TemperatureUnit(int const id) : temperatureUnitID(id){}
TemperatureUnit::TemperatureUnit(std::string const & str)
{
  temperatureUnitID = -1;
  for (TEMPERATURE_UNIT::StringMap::const_iterator iter =
           TEMPERATURE_UNIT::temperatureUnitToString.begin();
       iter != TEMPERATURE_UNIT::temperatureUnitToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      temperatureUnitID = (iter->first).temperatureUnitID;
      break;
    }
  }
}

bool TemperatureUnit::operator==(TemperatureUnit const & rhs) const
{return temperatureUnitID==rhs.temperatureUnitID;}
bool TemperatureUnit::operator!=(TemperatureUnit const & rhs) const
{return temperatureUnitID!=rhs.temperatureUnitID;}

std::string const & TemperatureUnit::String() const
{
  TEMPERATURE_UNIT::StringMap::const_iterator iter
      = TEMPERATURE_UNIT::temperatureUnitToString.find(*this);
  if (iter == TEMPERATURE_UNIT::temperatureUnitToString.end())
    return TEMPERATURE_UNIT::temperatureUnitUnknown;
  else
    return iter->second;
}
}  // namespace KIM
