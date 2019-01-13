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
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_MODEL_ROUTINE_NAME_HPP_
#define KIM_MODEL_ROUTINE_NAME_HPP_

#include <string>

namespace KIM
{
class ModelRoutineName
{
 public:
  int modelRoutineNameID;

  ModelRoutineName();
  ModelRoutineName(int const id);
  ModelRoutineName(std::string const & str);
  bool operator==(ModelRoutineName const & rhs) const;
  bool operator!=(ModelRoutineName const & rhs) const;
  std::string const & ToString() const;
};  // class ModelRoutineName

namespace MODEL_ROUTINE_NAME
{
extern ModelRoutineName const Create;
extern ModelRoutineName const ComputeArgumentsCreate;
extern ModelRoutineName const Compute;
extern ModelRoutineName const Extension;
extern ModelRoutineName const Refresh;
extern ModelRoutineName const WriteParameterizedModel;
extern ModelRoutineName const ComputeArgumentsDestroy;
extern ModelRoutineName const Destroy;

void GetNumberOfModelRoutineNames(int * const numberOfModelRoutineNames);
int GetModelRoutineName(int const index,
                        ModelRoutineName * const modelRoutineName);

struct Comparator
{
  bool operator()(ModelRoutineName const & a, ModelRoutineName const & b) const
  {
    return a.modelRoutineNameID < b.modelRoutineNameID;
  }
};  // struct Comparator
}  // namespace MODEL_ROUTINE_NAME
}  // namespace KIM

#endif  // KIM_MODEL_ROUTINE_NAME_HPP_
