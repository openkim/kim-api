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
// Release: This file is part of the kim-api-2.3.0 package.
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
#include "KIM_TemperatureUnit.inc"
TemperatureUnit const unused(ID_unused);
TemperatureUnit const K(ID_K);

namespace
{
typedef std::
    map<TemperatureUnit const, std::string, TEMPERATURE_UNIT::Comparator>
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
  for (int i = 0; i < index; ++i) ++iter;
  *temperatureUnit = iter->first;
  return false;  // no error
}
}  // namespace TEMPERATURE_UNIT

// implementation of TemperatureUnit
TemperatureUnit::TemperatureUnit() {}
TemperatureUnit::TemperatureUnit(int const id) : temperatureUnitID(id) {}
TemperatureUnit::TemperatureUnit(std::string const & str)
{
  temperatureUnitID = -1;
  for (TEMPERATURE_UNIT::StringMap::const_iterator iter
       = TEMPERATURE_UNIT::temperatureUnitToString.begin();
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

bool TemperatureUnit::Known() const
{
  int numberOfTemperatureUnits;
  TEMPERATURE_UNIT::GetNumberOfTemperatureUnits(&numberOfTemperatureUnits);

  for (int i = 0; i < numberOfTemperatureUnits; ++i)
  {
    TemperatureUnit tempUnit;
    TEMPERATURE_UNIT::GetTemperatureUnit(i, &tempUnit);

    if (*this == tempUnit) { return true; }
  }

  return false;
}

bool TemperatureUnit::operator==(TemperatureUnit const & rhs) const
{
  return temperatureUnitID == rhs.temperatureUnitID;
}
bool TemperatureUnit::operator!=(TemperatureUnit const & rhs) const
{
  return temperatureUnitID != rhs.temperatureUnitID;
}

std::string const & TemperatureUnit::ToString() const
{
  TEMPERATURE_UNIT::StringMap::const_iterator iter
      = TEMPERATURE_UNIT::temperatureUnitToString.find(*this);
  if (iter == TEMPERATURE_UNIT::temperatureUnitToString.end())
    return TEMPERATURE_UNIT::temperatureUnitUnknown;
  else
    return iter->second;
}
}  // namespace KIM
