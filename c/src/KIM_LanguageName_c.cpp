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
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <string>

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif
extern "C" {
#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
#endif
}  // extern "C"


namespace
{
KIM::LanguageName makeLanguageNameCpp(KIM_LanguageName languageName)
{
  KIM::LanguageName const * const languageNameCpp
      = reinterpret_cast<KIM::LanguageName const *>(&languageName);
  return *languageNameCpp;
}

KIM_LanguageName makeLanguageNameC(KIM::LanguageName languageName)
{
  KIM_LanguageName const * const languageNameC
      = reinterpret_cast<KIM_LanguageName const *>(&languageName);
  return *languageNameC;
}
}  // namespace

extern "C" {
KIM_LanguageName KIM_LanguageName_FromString(char const * const str)
{
  return makeLanguageNameC(KIM::LanguageName(std::string(str)));
}

int KIM_LanguageName_Known(KIM_LanguageName const languageName)
{
  return makeLanguageNameCpp(languageName).Known();
}

int KIM_LanguageName_Equal(KIM_LanguageName const lhs,
                           KIM_LanguageName const rhs)
{
  return (lhs.languageNameID == rhs.languageNameID);
}

int KIM_LanguageName_NotEqual(KIM_LanguageName const lhs,
                              KIM_LanguageName const rhs)
{
  return (!KIM_LanguageName_Equal(lhs, rhs));
}

char const * KIM_LanguageName_ToString(KIM_LanguageName languageName)
{
  return makeLanguageNameCpp(languageName).ToString().c_str();
}

// Order doesn't matter as long as all values are unique
#include "KIM_LanguageName.inc"
KIM_LanguageName const KIM_LANGUAGE_NAME_cpp = {ID_cpp};
KIM_LanguageName const KIM_LANGUAGE_NAME_c = {ID_c};
KIM_LanguageName const KIM_LANGUAGE_NAME_fortran = {ID_fortran};

void KIM_LANGUAGE_NAME_GetNumberOfLanguageNames(
    int * const numberOfLanguageNames)
{
  KIM::LANGUAGE_NAME::GetNumberOfLanguageNames(numberOfLanguageNames);
}

int KIM_LANGUAGE_NAME_GetLanguageName(int const index,
                                      KIM_LanguageName * const languageName)
{
  KIM::LanguageName languageNameCpp;
  int error = KIM::LANGUAGE_NAME::GetLanguageName(index, &languageNameCpp);
  if (error) return error;
  *languageName = makeLanguageNameC(languageNameCpp);
  return false;
}

}  // extern "C"
