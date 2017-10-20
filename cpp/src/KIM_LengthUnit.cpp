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

#include <map>

#ifndef KIM_LENGTH_UNIT_HPP_
#include "KIM_LengthUnit.hpp"
#endif

namespace KIM
{

// Order doesn't matter as long as all values are unique
namespace LENGTH_UNIT
{
LengthUnit const unused(0);
LengthUnit const A(1);
LengthUnit const Bohr(2);
LengthUnit const cm(3);
LengthUnit const m(4);
LengthUnit const nm(5);

namespace
{
typedef std::map<LengthUnit const, std::string, LENGTH_UNIT::Comparator>
StringMap;

StringMap const GetStringMap()
{
  StringMap mm;
  mm[unused] = "unused";
  mm[A] = "A";
  mm[Bohr] = "Bohr";
  mm[cm] = "cm";
  mm[m] = "m";
  mm[nm] = "nm";
  return mm;
}
}  // namespace
extern StringMap const lengthUnitToString = GetStringMap();

void GetNumberOfLengthUnits(int * const numberOfLengthUnits)
{
  *numberOfLengthUnits = lengthUnitToString.size();
}

int GetLengthUnit(int const index, LengthUnit * const lengthUnit)
{
  int numberOfLengthUnits;
  GetNumberOfLengthUnits(&numberOfLengthUnits);
  if ((index < 0) || (index >= numberOfLengthUnits)) return true;

  StringMap::const_iterator iter = lengthUnitToString.begin();
  for (int i=0; i<index; ++i) ++iter;
  *lengthUnit = iter->first;
  return false;  // no error
}
}  // namespace LENGTH_UNIT

// implementation of LengthUnit
LengthUnit::LengthUnit() : lengthUnitID(0){}
LengthUnit::LengthUnit(int const id) : lengthUnitID(id){}
LengthUnit::LengthUnit(std::string const & str)
{
  lengthUnitID = -1;
  for (LENGTH_UNIT::StringMap::const_iterator iter
           = LENGTH_UNIT::lengthUnitToString.begin();
       iter != LENGTH_UNIT::lengthUnitToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      lengthUnitID = (iter->first).lengthUnitID;
      break;
    }
  }
}

bool LengthUnit::operator==(LengthUnit const & rhs) const
{return lengthUnitID==rhs.lengthUnitID;}
bool LengthUnit::operator!=(LengthUnit const & rhs) const
{return lengthUnitID!=rhs.lengthUnitID;}

std::string LengthUnit::String() const
{
  std::string result;
  LENGTH_UNIT::StringMap::const_iterator iter
      = LENGTH_UNIT::lengthUnitToString.find(*this);
  if (iter == LENGTH_UNIT::lengthUnitToString.end())
    result = "unknown";
  else
    result = iter->second;

  return result;
}
}  // namespace KIM