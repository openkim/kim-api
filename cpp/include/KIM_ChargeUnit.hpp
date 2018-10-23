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


#ifndef KIM_CHARGE_UNIT_HPP_
#define KIM_CHARGE_UNIT_HPP_

#include <string>

namespace KIM
{
class ChargeUnit
{
 public:
  int chargeUnitID;

  ChargeUnit();
  ChargeUnit(int const id);
  ChargeUnit(std::string const & str);
  bool operator==(ChargeUnit const & rhs) const;
  bool operator!=(ChargeUnit const & rhs) const;
  std::string const & String() const;
};  // class ChargeUnit

namespace CHARGE_UNIT
{
extern ChargeUnit const unused;
extern ChargeUnit const C;
extern ChargeUnit const e;
extern ChargeUnit const statC;

void GetNumberOfChargeUnits(int * const numberOfChargeUnits);
int GetChargeUnit(int const index, ChargeUnit * const chargeUnit);

struct Comparator
{
  bool operator()(ChargeUnit const & a, ChargeUnit const & b) const
  {
    return a.chargeUnitID < b.chargeUnitID;
  }
};  // struct Comparator
}  // namespace CHARGE_UNIT
}  // namespace KIM

#endif  // KIM_CHARGE_UNIT_HPP_
