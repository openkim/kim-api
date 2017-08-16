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

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

namespace KIM
{
LanguageName::LanguageName(): languageNameID(0){}
LanguageName::LanguageName(int const id): languageNameID(id){}

bool LanguageName::operator==(LanguageName const & rhs) const
{return languageNameID == rhs.languageNameID;}
bool LanguageName::operator!=(LanguageName const & rhs) const
{return languageNameID != rhs.languageNameID;}

std::string LanguageName::String() const
{
  if (*this == LANGUAGE_NAME::cpp) return "cpp";
  else if (*this == LANGUAGE_NAME::c) return "c";
  else if (*this == LANGUAGE_NAME::fortran) return "fortran";
  else return "unknown";
}

namespace LANGUAGE_NAME
{
LanguageName const cpp(0);
LanguageName const c(1);
LanguageName const fortran(2);
}  // namespace LANGUAGE_NAME

}  // namespace KIM
