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


#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#define KIM_COMPUTE_ARGUMENT_NAME_HPP_

#include <string>

namespace KIM
{
// Forward declaration
class DataType;

class ComputeArgumentName
{
 public:
  int computeArgumentNameID;

  ComputeArgumentName();
  ComputeArgumentName(int const id);
  ComputeArgumentName(std::string const & str);
  bool operator==(ComputeArgumentName const & rhs) const;
  bool operator!=(ComputeArgumentName const & rhs) const;
  std::string const & String() const;
};

namespace COMPUTE_ARGUMENT_NAME
{
extern ComputeArgumentName const numberOfParticles;
extern ComputeArgumentName const particleSpeciesCodes;
extern ComputeArgumentName const particleContributing;
extern ComputeArgumentName const coordinates;
extern ComputeArgumentName const partialEnergy;
extern ComputeArgumentName const partialForces;
extern ComputeArgumentName const partialParticleEnergy;
extern ComputeArgumentName const partialVirial;
extern ComputeArgumentName const partialParticleVirial;

void GetNumberOfComputeArgumentNames(int * const numberOfComputeArgumentNames);
int GetComputeArgumentName(int const index,
                           ComputeArgumentName * const computeArgumentName);

int GetComputeArgumentDataType(ComputeArgumentName const computeArgumentName,
                               DataType * const dataType);

struct Comparator
{
  bool operator()(ComputeArgumentName const & a,
                  ComputeArgumentName const & b) const
  {
    return a.computeArgumentNameID < b.computeArgumentNameID;
  }
};
}  // namespace COMPUTE_ARGUMENT_NAME
}  // namespace KIM
#endif  // KIM_COMPUTE_ARGUMENT_NAME_HPP_
