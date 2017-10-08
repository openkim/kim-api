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

#ifndef KIM_LENGTH_UNIT_HPP_
#include "KIM_LengthUnit.hpp"
#endif

namespace KIM
{

LengthUnit::LengthUnit() : lengthUnitID(0){}
LengthUnit::LengthUnit(int const id) : lengthUnitID(id){}
bool LengthUnit::operator==(LengthUnit const & rhs) const
{return lengthUnitID==rhs.lengthUnitID;}
bool LengthUnit::operator!=(LengthUnit const & rhs) const
{return lengthUnitID!=rhs.lengthUnitID;}

std::string LengthUnit::String() const
{
  if (*this == LENGTH_UNIT::unused) return "unused";
  else if (*this == LENGTH_UNIT::A) return "A";
  else if (*this == LENGTH_UNIT::Bohr) return "Bohr";
  else if (*this == LENGTH_UNIT::cm) return "cm";
  else if (*this == LENGTH_UNIT::m) return "m";
  else if (*this == LENGTH_UNIT::nm) return "nm";
  else return "unknown";
}


namespace LENGTH_UNIT
{
LengthUnit const unused(0);
LengthUnit const A(1);
LengthUnit const Bohr(2);
LengthUnit const cm(3);
LengthUnit const m(4);
LengthUnit const nm(5);
}  // namespace LENGTH_UNIT

}  // namespace KIM
