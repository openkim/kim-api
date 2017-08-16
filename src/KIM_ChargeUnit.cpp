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
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#ifndef KIM_CHARGE_UNIT_SYSTEM_HPP_
#include "KIM_ChargeUnit.hpp"
#endif

namespace KIM
{

ChargeUnit::ChargeUnit() : chargeUnitID(0){}
ChargeUnit::ChargeUnit(int const id) : chargeUnitID(id){}
bool ChargeUnit::operator==(ChargeUnit const & rhs) const
{return chargeUnitID==rhs.chargeUnitID;}
bool ChargeUnit::operator!=(ChargeUnit const & rhs) const
{return chargeUnitID!=rhs.chargeUnitID;}

std::string ChargeUnit::String() const
{
  if (*this == CHARGE_UNIT::unused) return "unused";
  else if (*this == CHARGE_UNIT::C) return "C";
  else if (*this == CHARGE_UNIT::e) return "e";
  else if (*this == CHARGE_UNIT::statC) return "statC";
  else return "unknown";
}


namespace CHARGE_UNIT
{
ChargeUnit const unused(0);
ChargeUnit const C(1);
ChargeUnit const e(2);
ChargeUnit const statC(3);
}  // namespace CHARGE_UNIT

}  // namespace KIM
