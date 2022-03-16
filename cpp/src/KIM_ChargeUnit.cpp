//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
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

#ifndef KIM_CHARGE_UNIT_SYSTEM_HPP_
#include "KIM_ChargeUnit.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace CHARGE_UNIT
{
#include "KIM_ChargeUnit.inc"
ChargeUnit const unused(ID_unused);
ChargeUnit const C(ID_C);
ChargeUnit const e(ID_e);
ChargeUnit const statC(ID_statC);

namespace
{
typedef std::map<ChargeUnit const, std::string, CHARGE_UNIT::Comparator>
    StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[unused] = "unused";
  m[C] = "C";
  m[e] = "e";
  m[statC] = "statC";
  return m;
}

StringMap const chargeUnitToString = GetStringMap();
std::string const chargeUnitUnknown("unknown");
}  // namespace


void GetNumberOfChargeUnits(int * const numberOfChargeUnits)
{
  *numberOfChargeUnits = chargeUnitToString.size();
}

int GetChargeUnit(int const index, ChargeUnit * const chargeUnit)
{
  int numberOfChargeUnits;
  GetNumberOfChargeUnits(&numberOfChargeUnits);
  if ((index < 0) || (index >= numberOfChargeUnits)) return true;

  StringMap::const_iterator iter = chargeUnitToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *chargeUnit = iter->first;
  return false;  // no error
}
}  // namespace CHARGE_UNIT

// implementation of ChargeUnit
ChargeUnit::ChargeUnit() {}
ChargeUnit::ChargeUnit(int const id) : chargeUnitID(id) {}
ChargeUnit::ChargeUnit(std::string const & str)
{
  chargeUnitID = -1;
  for (CHARGE_UNIT::StringMap::const_iterator iter
       = CHARGE_UNIT::chargeUnitToString.begin();
       iter != CHARGE_UNIT::chargeUnitToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      chargeUnitID = (iter->first).chargeUnitID;
      break;
    }
  }
}

bool ChargeUnit::Known() const
{
  int numberOfChargeUnits;
  CHARGE_UNIT::GetNumberOfChargeUnits(&numberOfChargeUnits);

  for (int i = 0; i < numberOfChargeUnits; ++i)
  {
    ChargeUnit cgUnit;
    CHARGE_UNIT::GetChargeUnit(i, &cgUnit);

    if (*this == cgUnit) { return true; }
  }

  return false;
}

bool ChargeUnit::operator==(ChargeUnit const & rhs) const
{
  return chargeUnitID == rhs.chargeUnitID;
}
bool ChargeUnit::operator!=(ChargeUnit const & rhs) const
{
  return chargeUnitID != rhs.chargeUnitID;
}

std::string const & ChargeUnit::ToString() const
{
  CHARGE_UNIT::StringMap::const_iterator iter
      = CHARGE_UNIT::chargeUnitToString.find(*this);
  if (iter == CHARGE_UNIT::chargeUnitToString.end())
    return CHARGE_UNIT::chargeUnitUnknown;
  else
    return iter->second;
}
}  // namespace KIM
