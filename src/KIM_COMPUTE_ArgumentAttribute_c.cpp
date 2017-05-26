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

#ifndef KIM_COMPUTE_ARGUMENT_ATTRIBUTE_HPP_
#include "KIM_COMPUTE_ArgumentAttribute.hpp"
#endif

extern "C"
{
#ifndef KIM_COMPUTE_ARGUMENT_ATTRIBUTE_H_
#include "KIM_COMPUTE_ArgumentAttribute.h"
#endif
}  // extern "C"

namespace
{
KIM::COMPUTE::ArgumentAttribute const makeArgumentAttributeCpp(
    KIM_COMPUTE_ArgumentAttribute const argumentAttribute)
{
  return KIM::COMPUTE::ArgumentAttribute(argumentAttribute.argumentAttributeID);
}
}  // namespace

extern "C"
{
char const * const KIM_COMPUTE_ArgumentAttributeString(
    KIM_COMPUTE_ArgumentAttribute const argumentAttribute)
{
  return (makeArgumentAttributeCpp(argumentAttribute)).string().c_str();
}

KIM_COMPUTE_ArgumentAttribute const KIM_COMPUTE_ARGUMENT_ATTRIBUTE_notSupported = {0};
KIM_COMPUTE_ArgumentAttribute const KIM_COMPUTE_ARGUMENT_ATTRIBUTE_required = {1};
KIM_COMPUTE_ArgumentAttribute const KIM_COMPUTE_ARGUMENT_ATTRIBUTE_optional = {2};

}  // extern "C"
