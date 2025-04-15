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

#ifndef KIM_LENGTH_UNIT_HPP_
#include "KIM_LengthUnit.hpp"
#endif
extern "C" {
#ifndef KIM_LENGTH_UNIT_H_
#include "KIM_LengthUnit.h"
#endif
}


namespace
{
KIM::LengthUnit makeLengthUnitCpp(KIM_LengthUnit const lengthUnit)
{
  KIM::LengthUnit const * const lengthUnitCpp
      = reinterpret_cast<KIM::LengthUnit const *>(&lengthUnit);
  return *lengthUnitCpp;
}

KIM_LengthUnit makeLengthUnitC(KIM::LengthUnit const lengthUnit)
{
  KIM_LengthUnit const * const lengthUnitC
      = reinterpret_cast<KIM_LengthUnit const *>(&lengthUnit);
  return *lengthUnitC;
}
}  // namespace

extern "C" {
KIM_LengthUnit KIM_LengthUnit_FromString(char const * const str)
{
  return makeLengthUnitC(KIM::LengthUnit(std::string(str)));
}

int KIM_LengthUnit_Known(KIM_LengthUnit const lengthUnit)
{
  return makeLengthUnitCpp(lengthUnit).Known();
}

int KIM_LengthUnit_Equal(KIM_LengthUnit const lhs, KIM_LengthUnit const rhs)
{
  return (lhs.lengthUnitID == rhs.lengthUnitID);
}

int KIM_LengthUnit_NotEqual(KIM_LengthUnit const lhs, KIM_LengthUnit const rhs)
{
  return (!KIM_LengthUnit_Equal(lhs, rhs));
}

char const * KIM_LengthUnit_ToString(KIM_LengthUnit const lengthUnit)
{
  return makeLengthUnitCpp(lengthUnit).ToString().c_str();
}

#include "KIM_LengthUnit.inc"
KIM_LengthUnit const KIM_LENGTH_UNIT_unused = {ID_unused};
KIM_LengthUnit const KIM_LENGTH_UNIT_A = {ID_A};
KIM_LengthUnit const KIM_LENGTH_UNIT_Bohr = {ID_Bohr};
KIM_LengthUnit const KIM_LENGTH_UNIT_cm = {ID_cm};
KIM_LengthUnit const KIM_LENGTH_UNIT_m = {ID_m};
KIM_LengthUnit const KIM_LENGTH_UNIT_nm = {ID_nm};

void KIM_LENGTH_UNIT_GetNumberOfLengthUnits(int * const numberOfLengthUnits)
{
  KIM::LENGTH_UNIT::GetNumberOfLengthUnits(numberOfLengthUnits);
}

int KIM_LENGTH_UNIT_GetLengthUnit(int const index,
                                  KIM_LengthUnit * const lengthUnit)
{
  KIM::LengthUnit lengthUnitCpp;
  int error = KIM::LENGTH_UNIT::GetLengthUnit(index, &lengthUnitCpp);
  if (error) return error;
  *lengthUnit = makeLengthUnitC(lengthUnitCpp);
  return false;
}

}  // extern "C"
