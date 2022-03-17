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

#include <map>

#ifndef KIM_LENGTH_UNIT_HPP_
#include "KIM_LengthUnit.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace LENGTH_UNIT
{
#include "KIM_LengthUnit.inc"
LengthUnit const unused(ID_unused);
LengthUnit const A(ID_A);
LengthUnit const Bohr(ID_Bohr);
LengthUnit const cm(ID_cm);
LengthUnit const m(ID_m);
LengthUnit const nm(ID_nm);

namespace
{
typedef std::map<LengthUnit const, std::string, LENGTH_UNIT::Comparator>
    StringMap;

StringMap const GetStringMap()
{
  StringMap mm;
  mm[unused] = "unused";
  mm[A] = "A";
  mm[Bohr] = "Bohr";
  mm[cm] = "cm";
  mm[m] = "m";
  mm[nm] = "nm";
  return mm;
}

StringMap const lengthUnitToString = GetStringMap();
std::string const lengthUnitUnknown("unknown");
}  // namespace


void GetNumberOfLengthUnits(int * const numberOfLengthUnits)
{
  *numberOfLengthUnits = lengthUnitToString.size();
}

int GetLengthUnit(int const index, LengthUnit * const lengthUnit)
{
  int numberOfLengthUnits;
  GetNumberOfLengthUnits(&numberOfLengthUnits);
  if ((index < 0) || (index >= numberOfLengthUnits)) return true;

  StringMap::const_iterator iter = lengthUnitToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *lengthUnit = iter->first;
  return false;  // no error
}
}  // namespace LENGTH_UNIT

// implementation of LengthUnit
LengthUnit::LengthUnit() {}
LengthUnit::LengthUnit(int const id) : lengthUnitID(id) {}
LengthUnit::LengthUnit(std::string const & str)
{
  lengthUnitID = -1;
  for (LENGTH_UNIT::StringMap::const_iterator iter
       = LENGTH_UNIT::lengthUnitToString.begin();
       iter != LENGTH_UNIT::lengthUnitToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      lengthUnitID = (iter->first).lengthUnitID;
      break;
    }
  }
}

bool LengthUnit::Known() const
{
  int numberOfLengthUnits;
  LENGTH_UNIT::GetNumberOfLengthUnits(&numberOfLengthUnits);

  for (int i = 0; i < numberOfLengthUnits; ++i)
  {
    LengthUnit lenUnit;
    LENGTH_UNIT::GetLengthUnit(i, &lenUnit);

    if (*this == lenUnit) { return true; }
  }

  return false;
}

bool LengthUnit::operator==(LengthUnit const & rhs) const
{
  return lengthUnitID == rhs.lengthUnitID;
}
bool LengthUnit::operator!=(LengthUnit const & rhs) const
{
  return lengthUnitID != rhs.lengthUnitID;
}

std::string const & LengthUnit::ToString() const
{
  LENGTH_UNIT::StringMap::const_iterator iter
      = LENGTH_UNIT::lengthUnitToString.find(*this);
  if (iter == LENGTH_UNIT::lengthUnitToString.end())
    return LENGTH_UNIT::lengthUnitUnknown;
  else
    return iter->second;
}
}  // namespace KIM
