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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-v2-2.0.0 package.
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
