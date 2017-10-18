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
KIM::LanguageName const makeLanguageNameCpp(KIM_LanguageName languageName)
{
  KIM::LanguageName const * const languageNameCpp
      = reinterpret_cast <KIM::LanguageName const * const>(&languageName);
  return *languageNameCpp;
}

KIM_LanguageName const makeLanguageNameC(KIM::LanguageName languageName)
{
  KIM_LanguageName const * const languageNameC
      = reinterpret_cast <KIM_LanguageName const * const>(&languageName);
  return *languageNameC;
}
}  // namespace

extern "C"
{
KIM_LanguageName KIM_LanguageNameFromString(char const * const str)
{
  return makeLanguageNameC(KIM::LanguageName(std::string(str)));
}

int KIM_LanguageNameEqual(KIM_LanguageName const left,
                          KIM_LanguageName const right)
{
  return (left.languageNameID == right.languageNameID);
}

int KIM_LanguageNameNotEqual(KIM_LanguageName const left,
                             KIM_LanguageName const right)
{
  return (!KIM_LanguageNameEqual(left, right));
}

char const * const KIM_LanguageNameString(KIM_LanguageName languageName)
{
  return (makeLanguageNameCpp(languageName)).String().c_str();
}

// Order doesn't matter as long as all values are unique
KIM_LanguageName const KIM_LANGUAGE_NAME_cpp = {0};
KIM_LanguageName const KIM_LANGUAGE_NAME_c = {1};
KIM_LanguageName const KIM_LANGUAGE_NAME_fortran = {2};

}  // extern "C"
