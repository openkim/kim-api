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

#ifndef KIM_COMPUTE_ARGUMENT_ATTRIBUTE_HPP_
#include "KIM_COMPUTE_ArgumentAttribute.hpp"
#endif

namespace KIM
{
namespace COMPUTE
{

ArgumentAttribute::ArgumentAttribute() : argumentAttributeID(0){}
ArgumentAttribute::ArgumentAttribute(int const id) : argumentAttributeID(id){}

bool ArgumentAttribute::operator==(ArgumentAttribute const & rhs) const
{return argumentAttributeID == rhs.argumentAttributeID;}
bool ArgumentAttribute::operator!=(ArgumentAttribute const & rhs) const
{return argumentAttributeID != rhs.argumentAttributeID;}

std::string ArgumentAttribute::string() const
{
  if (*this == ARGUMENT_ATTRIBUTE::notSupported)
    return "notSupported";
  else if (*this == ARGUMENT_ATTRIBUTE::required)
    return "required";
  else if (*this == ARGUMENT_ATTRIBUTE::optional)
    return "optional";
  else
    return "unknown";
}

// Order doesn't matter as long as all values are unique
namespace ARGUMENT_ATTRIBUTE
{
ArgumentAttribute const notSupported(0);
ArgumentAttribute const required(1);
ArgumentAttribute const optional(2);
}  // namespace ARGUMENT_ATTRIBUTE

}  // namespace COMPUTE
}  // namespace KIM
