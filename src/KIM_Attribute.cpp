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

#ifndef KIM_ATTRIBUTE_HPP_
#include "KIM_Attribute.hpp"
#endif

namespace KIM
{

Attribute::Attribute() : attributeID(0){}
Attribute::Attribute(int const id) : attributeID(id){}

bool Attribute::operator==(Attribute const & rhs) const
{return attributeID == rhs.attributeID;}
bool Attribute::operator!=(Attribute const & rhs) const
{return attributeID != rhs.attributeID;}

std::string Attribute::string() const
{
  if (*this == ATTRIBUTE::mandatory)
    return "mandatory";
  else if (*this == ATTRIBUTE::notSupported)
    return "notSupported";
  else if (*this == ATTRIBUTE::required)
    return "required";
  else if (*this == ATTRIBUTE::optional)
    return "optional";
  else
    return "unknown";
}

// Order doesn't matter as long as all values are unique
namespace ATTRIBUTE
{
Attribute const mandatory(0);
Attribute const notSupported(1);
Attribute const required(2);
Attribute const optional(3);
}  // namespace ATTRIBUTE

}  // namespace KIM
