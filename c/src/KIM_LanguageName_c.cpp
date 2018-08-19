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

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif
extern "C"
{
#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
#endif
}  // extern "C"


namespace
{
KIM::LanguageName makeLanguageNameCpp(KIM_LanguageName languageName)
{
  KIM::LanguageName const * const languageNameCpp
      = reinterpret_cast <KIM::LanguageName const *>(&languageName);
  return *languageNameCpp;
}

KIM_LanguageName makeLanguageNameC(KIM::LanguageName languageName)
{
  KIM_LanguageName const * const languageNameC
      = reinterpret_cast <KIM_LanguageName const *>(&languageName);
  return *languageNameC;
}
}  // namespace

extern "C"
{
KIM_LanguageName KIM_LanguageName_FromString(char const * const str)
{
  return makeLanguageNameC(KIM::LanguageName(std::string(str)));
}

int KIM_LanguageName_Equal(KIM_LanguageName const left,
                           KIM_LanguageName const right)
{
  return (left.languageNameID == right.languageNameID);
}

int KIM_LanguageName_NotEqual(KIM_LanguageName const left,
                              KIM_LanguageName const right)
{
  return (!KIM_LanguageName_Equal(left, right));
}

char const * KIM_LanguageName_String(KIM_LanguageName languageName)
{
  return makeLanguageNameCpp(languageName).String().c_str();
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
