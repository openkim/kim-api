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
// Release: This file is part of the kim-api-2.4.1 package.
//


#include <string>

#ifndef KIM_CHARGE_UNIT_HPP_
#include "KIM_ChargeUnit.hpp"
#endif
extern "C" {
#ifndef KIM_CHARGE_UNIT_H_
#include "KIM_ChargeUnit.h"
#endif
}


namespace
{
KIM::ChargeUnit makeChargeUnitCpp(KIM_ChargeUnit const chargeUnit)
{
  KIM::ChargeUnit const * const chargeUnitCpp
      = reinterpret_cast<KIM::ChargeUnit const *>(&chargeUnit);
  return *chargeUnitCpp;
}

KIM_ChargeUnit makeChargeUnitC(KIM::ChargeUnit const chargeUnit)
{
  KIM_ChargeUnit const * const chargeUnitC
      = reinterpret_cast<KIM_ChargeUnit const *>(&chargeUnit);
  return *chargeUnitC;
}
}  // namespace

extern "C" {
KIM_ChargeUnit KIM_ChargeUnit_FromString(char const * const str)
{
  return makeChargeUnitC(KIM::ChargeUnit(std::string(str)));
}

int KIM_ChargeUnit_Known(KIM_ChargeUnit const chargeUnit)
{
  return makeChargeUnitCpp(chargeUnit).Known();
}

int KIM_ChargeUnit_Equal(KIM_ChargeUnit const lhs, KIM_ChargeUnit const rhs)
{
  return (lhs.chargeUnitID == rhs.chargeUnitID);
}

int KIM_ChargeUnit_NotEqual(KIM_ChargeUnit const lhs, KIM_ChargeUnit const rhs)
{
  return (!KIM_ChargeUnit_Equal(lhs, rhs));
}

char const * KIM_ChargeUnit_ToString(KIM_ChargeUnit const chargeUnit)
{
  return makeChargeUnitCpp(chargeUnit).ToString().c_str();
}

#include "KIM_ChargeUnit.inc"
KIM_ChargeUnit const KIM_CHARGE_UNIT_unused = {ID_unused};
KIM_ChargeUnit const KIM_CHARGE_UNIT_C = {ID_C};
KIM_ChargeUnit const KIM_CHARGE_UNIT_e = {ID_e};
KIM_ChargeUnit const KIM_CHARGE_UNIT_statC = {ID_statC};

void KIM_CHARGE_UNIT_GetNumberOfChargeUnits(int * const numberOfChargeUnits)
{
  KIM::CHARGE_UNIT::GetNumberOfChargeUnits(numberOfChargeUnits);
}

int KIM_CHARGE_UNIT_GetChargeUnit(int const index,
                                  KIM_ChargeUnit * const chargeUnit)
{
  KIM::ChargeUnit chargeUnitCpp;
  int error = KIM::CHARGE_UNIT::GetChargeUnit(index, &chargeUnitCpp);
  if (error) return error;
  *chargeUnit = makeChargeUnitC(chargeUnitCpp);
  return false;
}

}  // extern "C"
