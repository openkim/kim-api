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

#include <map>

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace DATA_TYPE
{
#include "KIM_DataType.inc"
DataType const Integer(ID_Integer);
DataType const Double(ID_Double);

namespace
{
typedef std::map<DataType const, std::string, DATA_TYPE::Comparator> StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[Integer] = "Integer";
  m[Double] = "Double";
  return m;
}

StringMap const dataTypeToString = GetStringMap();
std::string const dataTypeUnknown("unknown");
}  // namespace


void GetNumberOfDataTypes(int * const numberOfDataTypes)
{
  *numberOfDataTypes = dataTypeToString.size();
}

int GetDataType(int const index, DataType * const dataType)
{
  int numberOfDataTypes;
  GetNumberOfDataTypes(&numberOfDataTypes);
  if ((index < 0) || (index >= numberOfDataTypes)) return true;

  StringMap::const_iterator iter = dataTypeToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *dataType = iter->first;
  return false;  // no error
}
}  // namespace DATA_TYPE

// implementation of DataType
DataType::DataType() {}
DataType::DataType(int const id) : dataTypeID(id) {}
DataType::DataType(std::string const & str)
{
  dataTypeID = -1;
  for (DATA_TYPE::StringMap::const_iterator iter
       = DATA_TYPE::dataTypeToString.begin();
       iter != DATA_TYPE::dataTypeToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      dataTypeID = (iter->first).dataTypeID;
      break;
    }
  }
}

bool DataType::Known() const
{
  int numberOfDataTypes;
  DATA_TYPE::GetNumberOfDataTypes(&numberOfDataTypes);

  for (int i = 0; i < numberOfDataTypes; ++i)
  {
    DataType dType;
    DATA_TYPE::GetDataType(i, &dType);

    if (*this == dType) { return true; }
  }

  return false;
}

bool DataType::operator==(DataType const & rhs) const
{
  return dataTypeID == rhs.dataTypeID;
}
bool DataType::operator!=(DataType const & rhs) const
{
  return dataTypeID != rhs.dataTypeID;
}

std::string const & DataType::ToString() const
{
  DATA_TYPE::StringMap::const_iterator iter
      = DATA_TYPE::dataTypeToString.find(*this);
  if (iter == DATA_TYPE::dataTypeToString.end())
    return DATA_TYPE::dataTypeUnknown;
  else
    return iter->second;
}
}  // namespace KIM
