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


#ifndef KIM_ARGUMENT_NAME_HPP_
#define KIM_ARGUMENT_NAME_HPP_

#include <string>

namespace KIM
{
// Forward declaration
class DataType;

class ArgumentName
{
 public:
  int argumentNameID;

  ArgumentName();
  ArgumentName(int const id);
  ArgumentName(std::string const & str);
  bool operator==(ArgumentName const & rhs) const;
  bool operator!=(ArgumentName const & rhs) const;
  std::string const & String() const;
};

namespace ARGUMENT_NAME
{
extern ArgumentName const numberOfParticles;
extern ArgumentName const particleSpeciesCodes;
extern ArgumentName const particleContributing;
extern ArgumentName const coordinates;
extern ArgumentName const partialEnergy;
extern ArgumentName const partialForces;
extern ArgumentName const partialParticleEnergy;
extern ArgumentName const partialVirial;
extern ArgumentName const partialParticleVirial;

void GetNumberOfArguments(int * const numberOfArguments);
int GetArgumentName(int const index, ArgumentName * const argumentName);

int GetArgumentDataType(ArgumentName const argumentName,
                        DataType * const dataType);

struct Comparator
{
  bool operator()(ArgumentName const & a, ArgumentName const & b) const
  {
    return a.argumentNameID < b.argumentNameID;
  }
};
}  // namespace ARGUMENT_NAME
}  // namespace KIM
#endif  // KIM_ARGUMENT_NAME_HPP_
