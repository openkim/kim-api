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

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif

namespace KIM
{

// Order doesn't matter as long as all values are unique
namespace NUMBERING
{
Numbering const zeroBased(0);
Numbering const oneBased(1);

namespace
{
typedef std::map<Numbering const, std::string, NUMBERING::Comparator>
StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[zeroBased] = "zeroBased";
  m[oneBased] = "oneBased";
  return m;
}
}  // namespace
extern StringMap const numberingToString = GetStringMap();

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
  for (int i=0; i<index; ++i) ++iter;
  *numbering = iter->first;
  return false;  // no error
}
}  // namespace NUMBERING

// implementation of Numbering
Numbering::Numbering() : numberingID(0) {}
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

bool Numbering::operator==(Numbering const & rhs) const
{return numberingID==rhs.numberingID;}
bool Numbering::operator!=(Numbering const & rhs) const
{return numberingID!=rhs.numberingID;}

std::string Numbering::String() const
{
  std::string result;
  NUMBERING::StringMap::const_iterator iter
      = NUMBERING::numberingToString.find(*this);
  if (iter == NUMBERING::numberingToString.end())
    result = "unknown";
  else
    result = iter->second;

  return result;
}
}  // namespace KIM
