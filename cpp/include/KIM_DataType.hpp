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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-v2.0.0-beta.2 package.
//


#ifndef KIM_DATA_TYPE_HPP_
#define KIM_DATA_TYPE_HPP_

#include <string>

namespace KIM
{
class DataType
{
 public:
  int dataTypeID;

  DataType();
  DataType(int const id);
  DataType(std::string const & str);
  bool operator==(DataType const & rhs) const;
  bool operator!=(DataType const & rhs) const;
  std::string const & String() const;
};  // class DataType

namespace DATA_TYPE
{
extern DataType const Integer;
extern DataType const Double;

void GetNumberOfDataTypes(int * const numberOfDataTypes);
int GetDataType(int const index, DataType * const dataType);

struct Comparator
{
  bool operator()(DataType const & a, DataType const & b) const
  {
    return a.dataTypeID < b.dataTypeID;
  }
};  // struct Comparator
}  // namespace DATA_TYPE
}  // namespace KIM

#endif  // KIM_DATA_TYPE_HPP_
