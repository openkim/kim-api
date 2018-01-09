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
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_CALLBACK_NAME_HPP_
#define KIM_CALLBACK_NAME_HPP_

#include <string>

namespace KIM
{

class CallbackName
{
 public:
  int callbackNameID;

  CallbackName();
  CallbackName(int const id);
  CallbackName(std::string const & str);
  bool operator==(CallbackName const & rhs) const;
  bool operator!=(CallbackName const & rhs) const;
  std::string String() const;
};

namespace CALLBACK_NAME
{
extern CallbackName const GetNeighborList;
extern CallbackName const ProcessDEDrTerm;
extern CallbackName const ProcessD2EDr2Term;

void GetNumberOfCallbacks(int * const numberOfCallbacks);
int GetCallbackName(int const index, CallbackName * const callbackName);

struct Comparator
{
  bool operator()(CallbackName const & a, CallbackName const & b) const
  {
    return a.callbackNameID < b.callbackNameID;
  }
};
}  // namespace CALLBACK_NAME
}  // namespace KIM
#endif  // KIM_CALLBACK_NAME_HPP_
