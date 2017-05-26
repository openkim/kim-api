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

#ifndef KIM_PARAMETER_HPP_
#include "KIM_Parameter.hpp"
#endif

namespace KIM
{

ParameterDataType::ParameterDataType() : dataTypeID(0) {}
ParameterDataType::ParameterDataType(int const id) : dataTypeID(id) {}
bool ParameterDataType::operator==(ParameterDataType const & rhs) const
{return dataTypeID==rhs.dataTypeID;}
bool ParameterDataType::operator!=(ParameterDataType const & rhs) const
{return dataTypeID!=rhs.dataTypeID;}

namespace PARAMETER_DATA_TYPE
{
ParameterDataType const Integer(1);
ParameterDataType const Real(2);
ParameterDataType const Double(3);
}  // namespace PARAMETER_DATA_TYPE
}  // namespace KIM
