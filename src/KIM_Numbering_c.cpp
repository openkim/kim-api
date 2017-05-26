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

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif
extern "C"
{
#ifndef KIM_NUMBERING_H_
#include "KIM_Numbering.h"
#endif
}  // extern "C"

namespace
{
KIM::Numbering makeNumberingCpp(
    KIM_Numbering const numbering)
{
  return KIM::Numbering(numbering.numberingID);
}
}  // namespace

extern "C"
{
int KIM_NumberingEqual(KIM_Numbering const left, KIM_Numbering const right)
{
  return (left.numberingID == right.numberingID);
}

int KIM_NumberingNotEqual(KIM_Numbering const left, KIM_Numbering const right)
{
  return (!KIM_NumberingEqual(left, right));
}

char const * const KIM_NumberingString(
    KIM_Numbering const numbering)
{
  return (makeNumberingCpp(numbering)).string().c_str();
}

KIM_Numbering const KIM_NUMBERING_zeroBased = {0};
KIM_Numbering const KIM_NUMBERING_oneBased = {1};

}  // extern "C"
