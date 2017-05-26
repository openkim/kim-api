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
  return KIM::LengthUnit(lengthUnit.lengthUnitID);
}
}  // namespace

extern "C"
{
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
  return (makeLengthUnitCpp(lengthUnit)).string().c_str();
}

KIM_LengthUnit const KIM_LENGTH_UNIT_any = {0};
KIM_LengthUnit const KIM_LENGTH_UNIT_A = {1};
KIM_LengthUnit const KIM_LENGTH_UNIT_Bohr = {2};
KIM_LengthUnit const KIM_LENGTH_UNIT_cm = {3};
KIM_LengthUnit const KIM_LENGTH_UNIT_m = {4};
KIM_LengthUnit const KIM_LENGTH_UNIT_nm = {5};

}  // extern "C"
