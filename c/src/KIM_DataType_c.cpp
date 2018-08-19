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
KIM::DataType makeDataTypeCpp(KIM_DataType const dataType)
{
  KIM::DataType const * const dataTypeCpp
      = reinterpret_cast <KIM::DataType const *>(&dataType);
  return *dataTypeCpp;
}

KIM_DataType makeDataTypeC(KIM::DataType const dataType)
{
  KIM_DataType const * const dataTypeC
      = reinterpret_cast <KIM_DataType const *>(&dataType);
  return *dataTypeC;
}
}  // namespace

extern "C"
{
KIM_DataType KIM_DataType_FromString(char const * const str)
{
  return makeDataTypeC(KIM::DataType(std::string(str)));
}

int KIM_DataType_Equal(KIM_DataType const left, KIM_DataType const right)
{
  return (left.dataTypeID == right.dataTypeID);
}

int KIM_DataType_NotEqual(KIM_DataType const left, KIM_DataType const right)
{
  return (!KIM_DataType_Equal(left, right));
}

char const * KIM_DataType_String(KIM_DataType const dataType)
{
  return makeDataTypeCpp(dataType).String().c_str();
}

#include "KIM_DataType.inc"
KIM_DataType const KIM_DATA_TYPE_Integer = {ID_Integer};
KIM_DataType const KIM_DATA_TYPE_Double = {ID_Double};

void KIM_DATA_TYPE_GetNumberOfDataTypes(int * const numberOfDataTypes)
{
  KIM::DATA_TYPE::GetNumberOfDataTypes(numberOfDataTypes);
}

int KIM_DATA_TYPE_GetDataType(int const index, KIM_DataType * const dataType)
{
  KIM::DataType dataTypeCpp;
  int error = KIM::DATA_TYPE::GetDataType(index, &dataTypeCpp);
  if (error) return error;
  *dataType = makeDataTypeC(dataTypeCpp);
  return false;
}

}  // extern "C"
