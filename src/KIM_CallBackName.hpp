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


#ifndef KIM_CALL_BACK_NAME_HPP_
#define KIM_CALL_BACK_NAME_HPP_

#include <string>

namespace KIM
{

class CallBackName
{
 public:
  int callBackNameID;

  CallBackName();
  CallBackName(int const id);
  bool operator==(CallBackName const & rhs) const;
  bool operator!=(CallBackName const & rhs) const;
  std::string string() const;
};

namespace CALL_BACK_NAME
{
extern CallBackName const get_neigh;
extern CallBackName const process_dEdr;
extern CallBackName const process_d2Edr2;

void get_number_of_call_backs(int * const numberOfCallBacks);
int get_call_back_name(int const index, CallBackName * const callBackName);

}  // namespace CALL_BACK_NAME

}  // namespace KIM

namespace std
{
template<>
struct hash<KIM::CallBackName const>
{
  size_t operator()(KIM::CallBackName const & callBackName) const
  {
    return callBackName.callBackNameID;
  }
};
}  // namespace std
#endif  // KIM_CALL_BACK_NAME_HPP_
