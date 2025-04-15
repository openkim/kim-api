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

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif
extern "C" {
#ifndef KIM_NUMBERING_H_
#include "KIM_Numbering.h"
#endif
}  // extern "C"

namespace
{
KIM::Numbering makeNumberingCpp(KIM_Numbering const numbering)
{
  KIM::Numbering const * const numberingCpp
      = reinterpret_cast<KIM::Numbering const *>(&numbering);
  return *numberingCpp;
}

KIM_Numbering makeNumberingC(KIM::Numbering const numbering)
{
  KIM_Numbering const * const numberingC
      = reinterpret_cast<KIM_Numbering const *>(&numbering);
  return *numberingC;
}
}  // namespace

extern "C" {
KIM_Numbering KIM_Numbering_FromString(char const * const str)
{
  return makeNumberingC(KIM::Numbering(std::string(str)));
}

int KIM_Numbering_Known(KIM_Numbering const numbering)
{
  return makeNumberingCpp(numbering).Known();
}

int KIM_Numbering_Equal(KIM_Numbering const lhs, KIM_Numbering const rhs)
{
  return (lhs.numberingID == rhs.numberingID);
}

int KIM_Numbering_NotEqual(KIM_Numbering const lhs, KIM_Numbering const rhs)
{
  return (!KIM_Numbering_Equal(lhs, rhs));
}

char const * KIM_Numbering_ToString(KIM_Numbering const numbering)
{
  return makeNumberingCpp(numbering).ToString().c_str();
}

#include "KIM_Numbering.inc"
KIM_Numbering const KIM_NUMBERING_zeroBased = {ID_zeroBased};
KIM_Numbering const KIM_NUMBERING_oneBased = {ID_oneBased};

void KIM_NUMBERING_GetNumberOfNumberings(int * const numberOfNumberings)
{
  KIM::NUMBERING::GetNumberOfNumberings(numberOfNumberings);
}

int KIM_NUMBERING_GetNumbering(int const index, KIM_Numbering * const numbering)
{
  KIM::Numbering numberingCpp;
  int error = KIM::NUMBERING::GetNumbering(index, &numberingCpp);
  if (error) return error;
  *numbering = makeNumberingC(numberingCpp);
  return false;
}

}  // extern "C"
