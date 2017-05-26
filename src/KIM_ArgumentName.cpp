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

#include <vector>
#include <unordered_map>

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif

#ifndef KIM_ARGUMENT_NAME_HPP_
#include "KIM_ArgumentName.hpp"
#endif

namespace KIM
{

// Order doesn't matter as long as all values are unique
namespace ARGUMENT_NAME
{
ArgumentName const numberOfParticles(0);
ArgumentName const particleSpecies(1);
ArgumentName const particleContributing(2);
ArgumentName const coordinates(3);
ArgumentName const energy(4);
ArgumentName const forces(5);
ArgumentName const particleEnergy(6);
ArgumentName const virial(7);
ArgumentName const particleVirial(8);
ArgumentName const hessian(9);

extern std::unordered_map<ArgumentName const, std::string> const
argumentNameToString = {
  std::pair<ArgumentName const, std::string>(numberOfParticles,
                                             "numberOfParticles"),
  std::pair<ArgumentName const, std::string>(particleSpecies,
                                             "particleSpecies"),
  std::pair<ArgumentName const, std::string>(particleContributing,
                                             "particleContributing"),
  std::pair<ArgumentName const, std::string>(coordinates, "coordinates"),
  std::pair<ArgumentName const, std::string>(energy, "energy"),
  std::pair<ArgumentName const, std::string>(forces, "forces"),
  std::pair<ArgumentName const, std::string>(particleEnergy, "particleEnergy"),
  std::pair<ArgumentName const, std::string>(virial, "virial"),
  std::pair<ArgumentName const, std::string>(particleVirial, "particleVirial"),
  std::pair<ArgumentName const, std::string>(hessian, "hessian")
};

extern std::unordered_map<ArgumentName const, DataType> const
argumentNameToDataType = {
  std::pair<ArgumentName const, DataType>(numberOfParticles,
                                          DATA_TYPE::Integer),
  std::pair<ArgumentName const, DataType>(particleSpecies, DATA_TYPE::Integer),
  std::pair<ArgumentName const, DataType>(particleContributing,
                                          DATA_TYPE::Integer),
  std::pair<ArgumentName const, DataType>(coordinates,
                                          DATA_TYPE::Double),
  std::pair<ArgumentName const, DataType>(energy,
                                          DATA_TYPE::Double),
  std::pair<ArgumentName const, DataType>(forces, DATA_TYPE::Double),
  std::pair<ArgumentName const, DataType>(particleEnergy, DATA_TYPE::Double),
  std::pair<ArgumentName const, DataType>(virial, DATA_TYPE::Double),
  std::pair<ArgumentName const, DataType>(particleVirial, DATA_TYPE::Double),
  std::pair<ArgumentName const, DataType>(hessian, DATA_TYPE::Double)};

extern std::vector<ArgumentName> const mandatoryArguments = {
  numberOfParticles,
  particleSpecies,
  particleContributing,
  coordinates};

void get_number_of_arguments(int * const numberOfArguments)
{
  *numberOfArguments = argumentNameToString.size();
}

int get_argument_name(int const index, ArgumentName * const argumentName)
{
  // @@@ is this OK?  Does iterator over a const object always give same order?
  int numberOfArguments;
  get_number_of_arguments(&numberOfArguments);
  if ((index < 0) || (index >= numberOfArguments)) return true;

  auto iter = argumentNameToString.begin();
  int i = 0;
  for (; i<index; ++i) iter++;
  *argumentName = iter->first;
  return false;  // no error
}

int get_argument_data_type(ArgumentName const argumentName,
                           DataType * const dataType)
{
  auto iter = argumentNameToDataType.find(argumentName);

  if (iter == argumentNameToDataType.end())
    return true;
  else
  {
    *dataType = iter->second;
    return false;
  }
}

}  // namespace ARGUMENT_NAME


// implementation of ArgumentName
ArgumentName::ArgumentName() : argumentNameID(0){}
ArgumentName::ArgumentName(int const id) : argumentNameID(id){}
ArgumentName::ArgumentName(std::string const str)
{
  argumentNameID = -1;
  std::unordered_map<std::string, ArgumentName> reverseMap;
  for (auto iter = ARGUMENT_NAME::argumentNameToString.begin();
       iter != ARGUMENT_NAME::argumentNameToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      argumentNameID = (iter->first).argumentNameID;
      break;
    }
  }
}

bool ArgumentName::operator==(ArgumentName const & rhs) const
{return argumentNameID == rhs.argumentNameID;}
bool ArgumentName::operator!=(ArgumentName const & rhs) const
{return argumentNameID != rhs.argumentNameID;}

std::string ArgumentName::string() const
{
  std::string result;
  auto iter = ARGUMENT_NAME::argumentNameToString.find(*this);
  if (iter == ARGUMENT_NAME::argumentNameToString.end())
    result = "unknown";
  else
    result = iter->second;

  return result;
}

}  // namespace KIM
