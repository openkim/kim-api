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

#ifndef KIM_CHARGE_UNIT_HPP_
#include "KIM_ChargeUnit.hpp"
#endif
extern "C"
{
#ifndef KIM_CHARGE_UNIT_H_
#include "KIM_ChargeUnit.h"
#endif
}


namespace
{
KIM::ChargeUnit makeChargeUnitCpp(KIM_ChargeUnit const chargeUnit)
{
  KIM::ChargeUnit const * const chargeUnitCpp
      = reinterpret_cast <KIM::ChargeUnit const *>(&chargeUnit);
  return *chargeUnitCpp;
}

KIM_ChargeUnit makeChargeUnitC(KIM::ChargeUnit const chargeUnit)
{
  KIM_ChargeUnit const * const chargeUnitC
      = reinterpret_cast <KIM_ChargeUnit const *>(&chargeUnit);
  return *chargeUnitC;
}
}  // namespace

extern "C"
{
KIM_ChargeUnit KIM_ChargeUnit_FromString(char const * const str)
{
  return makeChargeUnitC(KIM::ChargeUnit(std::string(str)));
}

int KIM_ChargeUnit_Equal(KIM_ChargeUnit const left, KIM_ChargeUnit right)
{
  return (left.chargeUnitID == right.chargeUnitID);
}

int KIM_ChargeUnit_NotEqual(KIM_ChargeUnit const left, KIM_ChargeUnit right)
{
  return (!KIM_ChargeUnit_Equal(left, right));
}

char const * KIM_ChargeUnit_String(KIM_ChargeUnit const chargeUnit)
{
  return makeChargeUnitCpp(chargeUnit).String().c_str();
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
