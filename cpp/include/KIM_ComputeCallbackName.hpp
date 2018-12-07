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
// Release: This file is part of the kim-api-v2-2.0.0-beta.3 package.
//


#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#define KIM_COMPUTE_CALLBACK_NAME_HPP_

#include <string>

namespace KIM
{
class ComputeCallbackName
{
 public:
  int computeCallbackNameID;

  ComputeCallbackName();
  ComputeCallbackName(int const id);
  ComputeCallbackName(std::string const & str);
  bool operator==(ComputeCallbackName const & rhs) const;
  bool operator!=(ComputeCallbackName const & rhs) const;
  std::string const & String() const;
};  // class ComputeCallbackName

namespace COMPUTE_CALLBACK_NAME
{
extern ComputeCallbackName const GetNeighborList;
extern ComputeCallbackName const ProcessDEDrTerm;
extern ComputeCallbackName const ProcessD2EDr2Term;

void GetNumberOfComputeCallbackNames(int * const numberOfComputeCallbackNames);
int GetComputeCallbackName(int const index,
                           ComputeCallbackName * const computeCallbackName);

struct Comparator
{
  bool operator()(ComputeCallbackName const & a,
                  ComputeCallbackName const & b) const
  {
    return a.computeCallbackNameID < b.computeCallbackNameID;
  }
};  // struct Comparator
}  // namespace COMPUTE_CALLBACK_NAME
}  // namespace KIM

#endif  // KIM_COMPUTE_CALLBACK_NAME_HPP_
