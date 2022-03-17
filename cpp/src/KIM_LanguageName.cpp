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

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace LANGUAGE_NAME
{
#include "KIM_LanguageName.inc"
LanguageName const cpp(ID_cpp);
LanguageName const c(ID_c);
LanguageName const fortran(ID_fortran);

namespace
{
typedef std::map<LanguageName const, std::string, LANGUAGE_NAME::Comparator>
    StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[cpp] = "cpp";
  m[c] = "c";
  m[fortran] = "fortran";
  return m;
}

StringMap const languageNameToString = GetStringMap();
std::string const languageNameUnknown("unknown");
}  // namespace


void GetNumberOfLanguageNames(int * const numberOfLanguageNames)
{
  *numberOfLanguageNames = languageNameToString.size();
}

int GetLanguageName(int const index, LanguageName * const languageName)
{
  int numberOfLanguageNames;
  GetNumberOfLanguageNames(&numberOfLanguageNames);
  if ((index < 0) || (index >= numberOfLanguageNames)) return true;

  StringMap::const_iterator iter = languageNameToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *languageName = iter->first;
  return false;  // no error
}
}  // namespace LANGUAGE_NAME

// implementation of LanguageName
LanguageName::LanguageName() {}
LanguageName::LanguageName(int const id) : languageNameID(id) {}
LanguageName::LanguageName(std::string const & str)
{
  languageNameID = -1;
  for (LANGUAGE_NAME::StringMap::const_iterator iter
       = LANGUAGE_NAME::languageNameToString.begin();
       iter != LANGUAGE_NAME::languageNameToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      languageNameID = (iter->first).languageNameID;
      break;
    }
  }
}

bool LanguageName::Known() const
{
  int numberOfLanguageNames;
  LANGUAGE_NAME::GetNumberOfLanguageNames(&numberOfLanguageNames);

  for (int i = 0; i < numberOfLanguageNames; ++i)
  {
    LanguageName langName;
    LANGUAGE_NAME::GetLanguageName(i, &langName);

    if (*this == langName) { return true; }
  }

  return false;
}

bool LanguageName::operator==(LanguageName const & rhs) const
{
  return languageNameID == rhs.languageNameID;
}
bool LanguageName::operator!=(LanguageName const & rhs) const
{
  return languageNameID != rhs.languageNameID;
}

std::string const & LanguageName::ToString() const
{
  LANGUAGE_NAME::StringMap::const_iterator iter
      = LANGUAGE_NAME::languageNameToString.find(*this);
  if (iter == LANGUAGE_NAME::languageNameToString.end())
    return LANGUAGE_NAME::languageNameUnknown;
  else
    return iter->second;
}
}  // namespace KIM
