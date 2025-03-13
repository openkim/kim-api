//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <string>

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif
extern "C" {
#ifndef KIM_DATA_TYPE_H_
#include "KIM_DataType.h"
#endif
}  // extern "C"


namespace
{
KIM::DataType makeDataTypeCpp(KIM_DataType const dataType)
{
  KIM::DataType const * const dataTypeCpp
      = reinterpret_cast<KIM::DataType const *>(&dataType);
  return *dataTypeCpp;
}

KIM_DataType makeDataTypeC(KIM::DataType const dataType)
{
  KIM_DataType const * const dataTypeC
      = reinterpret_cast<KIM_DataType const *>(&dataType);
  return *dataTypeC;
}
}  // namespace

extern "C" {
KIM_DataType KIM_DataType_FromString(char const * const str)
{
  return makeDataTypeC(KIM::DataType(std::string(str)));
}

int KIM_DataType_Known(KIM_DataType const dataType)
{
  return makeDataTypeCpp(dataType).Known();
}

int KIM_DataType_Equal(KIM_DataType const lhs, KIM_DataType const rhs)
{
  return (lhs.dataTypeID == rhs.dataTypeID);
}

int KIM_DataType_NotEqual(KIM_DataType const lhs, KIM_DataType const rhs)
{
  return (!KIM_DataType_Equal(lhs, rhs));
}

char const * KIM_DataType_ToString(KIM_DataType const dataType)
{
  return makeDataTypeCpp(dataType).ToString().c_str();
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
