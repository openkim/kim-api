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

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif
extern "C"
{
#ifndef KIM_DATA_TYPE_H_
#include "KIM_DataType.h"
#endif
}  // extern "C"


namespace
{
KIM::DataType const makeDataTypeCpp(KIM_DataType const dataType)
{
  return KIM::DataType(dataType.dataTypeID);
}
}  // namespace

extern "C"
{
int KIM_DataTypeEqual(KIM_DataType const left, KIM_DataType const right)
{
  return (left.dataTypeID == right.dataTypeID);
}

int KIM_DataTypeNotEqual(KIM_DataType const left, KIM_DataType const right)
{
  return (!KIM_DataTypeEqual(left, right));
}

char const * const KIM_DataTypeString(KIM_DataType const dataType)
{
  return (makeDataTypeCpp(dataType)).string().c_str();
}

KIM_DataType const KIM_DATA_TYPE_Integer = {0};
KIM_DataType const KIM_DATA_TYPE_Double = {1};

}  // extern "C"
