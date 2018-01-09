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

#ifndef KIM_LENGTH_UNIT_HPP_
#include "KIM_LengthUnit.hpp"
#endif
extern "C"
{
#ifndef KIM_LENGTH_UNIT_H_
#include "KIM_LengthUnit.h"
#endif
}


namespace
{
KIM::LengthUnit const makeLengthUnitCpp(KIM_LengthUnit const lengthUnit)
{
  KIM::LengthUnit const * const lengthUnitCpp
      = reinterpret_cast <KIM::LengthUnit const * const>(&lengthUnit);
  return *lengthUnitCpp;
}

KIM_LengthUnit const makeLengthUnitC(KIM::LengthUnit const lengthUnit)
{
  KIM_LengthUnit const * const lengthUnitC
      = reinterpret_cast <KIM_LengthUnit const * const>(&lengthUnit);
  return *lengthUnitC;
}
}  // namespace

extern "C"
{
KIM_LengthUnit KIM_LengthUnitFromString(char const * const str)
{
  return makeLengthUnitC(KIM::LengthUnit(std::string(str)));
}

int KIM_LengthUnitEqual(KIM_LengthUnit left, KIM_LengthUnit right)
{
  return (left.lengthUnitID == right.lengthUnitID);
}

int KIM_LengthUnitNotEqual(KIM_LengthUnit left, KIM_LengthUnit right)
{
  return (!KIM_LengthUnitEqual(left, right));
}

char const * const KIM_LengthUnitString(KIM_LengthUnit const lengthUnit)
{
  static std::string result;
  result = makeLengthUnitCpp(lengthUnit).String();
  return result.c_str();
}

KIM_LengthUnit const KIM_LENGTH_UNIT_unused
= {KIM::LENGTH_UNIT::unused.lengthUnitID};
KIM_LengthUnit const KIM_LENGTH_UNIT_A
= {KIM::LENGTH_UNIT::A.lengthUnitID};
KIM_LengthUnit const KIM_LENGTH_UNIT_Bohr
= {KIM::LENGTH_UNIT::Bohr.lengthUnitID};
KIM_LengthUnit const KIM_LENGTH_UNIT_cm
= {KIM::LENGTH_UNIT::cm.lengthUnitID};
KIM_LengthUnit const KIM_LENGTH_UNIT_m
= {KIM::LENGTH_UNIT::m.lengthUnitID};
KIM_LengthUnit const KIM_LENGTH_UNIT_nm
= {KIM::LENGTH_UNIT::nm.lengthUnitID};

}  // extern "C"
