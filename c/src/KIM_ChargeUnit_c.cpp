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
KIM::ChargeUnit const makeChargeUnitCpp(KIM_ChargeUnit const chargeUnit)
{
  KIM::ChargeUnit const * const chargeUnitCpp
      = reinterpret_cast <KIM::ChargeUnit const * const>(&chargeUnit);
  return *chargeUnitCpp;
}

KIM_ChargeUnit const makeChargeUnitC(KIM::ChargeUnit const chargeUnit)
{
  KIM_ChargeUnit const * const chargeUnitC
      = reinterpret_cast <KIM_ChargeUnit const * const>(&chargeUnit);
  return *chargeUnitC;
}
}  // namespace

extern "C"
{
KIM_ChargeUnit KIM_ChargeUnitFromString(char const * const str)
{
  return makeChargeUnitC(KIM::ChargeUnit(std::string(str)));
}

int KIM_ChargeUnitEqual(KIM_ChargeUnit const left, KIM_ChargeUnit right)
{
  return (left.chargeUnitID == right.chargeUnitID);
}

int KIM_ChargeUnitNotEqual(KIM_ChargeUnit const left, KIM_ChargeUnit right)
{
  return (!KIM_ChargeUnitEqual(left, right));
}

char const * const KIM_ChargeUnitString(KIM_ChargeUnit const chargeUnit)
{
  static std::string result;
  result = makeChargeUnitCpp(chargeUnit).String();
  return result.c_str();
}

KIM_ChargeUnit const KIM_CHARGE_UNIT_unused
= {KIM::CHARGE_UNIT::unused.chargeUnitID};
KIM_ChargeUnit const KIM_CHARGE_UNIT_C
= {KIM::CHARGE_UNIT::C.chargeUnitID};
KIM_ChargeUnit const KIM_CHARGE_UNIT_e
= {KIM::CHARGE_UNIT::e.chargeUnitID};
KIM_ChargeUnit const KIM_CHARGE_UNIT_statC
= {KIM::CHARGE_UNIT::statC.chargeUnitID};

}  // extern "C"
