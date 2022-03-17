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
// Release: This file is part of the kim-api.git repository.
//

#include <map>

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace NUMBERING
{
#include "KIM_Numbering.inc"
Numbering const zeroBased(ID_zeroBased);
Numbering const oneBased(ID_oneBased);

namespace
{
typedef std::map<Numbering const, std::string, NUMBERING::Comparator> StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[zeroBased] = "zeroBased";
  m[oneBased] = "oneBased";
  return m;
}

StringMap const numberingToString = GetStringMap();
std::string numberingUnknown("unknown");
}  // namespace


void GetNumberOfNumberings(int * const numberOfNumberings)
{
  *numberOfNumberings = numberingToString.size();
}

int GetNumbering(int const index, Numbering * const numbering)
{
  int numberOfNumberings;
  GetNumberOfNumberings(&numberOfNumberings);
  if ((index < 0) || (index >= numberOfNumberings)) return true;

  StringMap::const_iterator iter = numberingToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *numbering = iter->first;
  return false;  // no error
}
}  // namespace NUMBERING

// implementation of Numbering
Numbering::Numbering() {}
Numbering::Numbering(int const id) : numberingID(id) {}
Numbering::Numbering(std::string const & str)
{
  numberingID = -1;
  for (NUMBERING::StringMap::const_iterator iter
       = NUMBERING::numberingToString.begin();
       iter != NUMBERING::numberingToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      numberingID = (iter->first).numberingID;
      break;
    }
  }
}

bool Numbering::Known() const
{
  int numberOfNumberings;
  NUMBERING::GetNumberOfNumberings(&numberOfNumberings);

  for (int i = 0; i < numberOfNumberings; ++i)
  {
    Numbering num;
    NUMBERING::GetNumbering(i, &num);

    if (*this == num) { return true; }
  }

  return false;
}

bool Numbering::operator==(Numbering const & rhs) const
{
  return numberingID == rhs.numberingID;
}
bool Numbering::operator!=(Numbering const & rhs) const
{
  return numberingID != rhs.numberingID;
}

std::string const & Numbering::ToString() const
{
  NUMBERING::StringMap::const_iterator iter
      = NUMBERING::numberingToString.find(*this);
  if (iter == NUMBERING::numberingToString.end())
    return NUMBERING::numberingUnknown;
  else
    return iter->second;
}
}  // namespace KIM
