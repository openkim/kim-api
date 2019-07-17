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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.1.0 package.
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
