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

#include <vector>
#include <map>

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
ArgumentName const particleSpeciesCodes(1);
ArgumentName const particleContributing(2);
ArgumentName const coordinates(3);
ArgumentName const partialEnergy(4);
ArgumentName const partialForces(5);
ArgumentName const partialParticleEnergy(6);
ArgumentName const partialVirial(7);
ArgumentName const partialParticleVirial(8);

namespace
{
typedef std::map<ArgumentName const, std::string, ARGUMENT_NAME::Comparator>
StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[numberOfParticles] = "numberOfParticles";
  m[particleSpeciesCodes] = "particleSpeciesCodes";
  m[particleContributing] = "particleContributing";
  m[coordinates] = "coordinates";
  m[partialEnergy] = "partialEnergy";
  m[partialForces] = "partialForces";
  m[partialParticleEnergy] = "partialParticleEnergy";
  m[partialVirial] = "partialVirial";
  m[partialParticleVirial] = "partialParticleVirial";
  return m;
}

StringMap const argumentNameToString = GetStringMap();
std::string const argumentNameUnknown("unknown");
}  // namespace


namespace
{
typedef std::map<ArgumentName const, DataType, ARGUMENT_NAME::Comparator>
DataTypeMap;

DataTypeMap const GetDataTypeMap()
{
  DataTypeMap m;
  m[numberOfParticles] = DATA_TYPE::Integer;
  m[particleSpeciesCodes] = DATA_TYPE::Integer;
  m[particleContributing] = DATA_TYPE::Integer;
  m[coordinates] = DATA_TYPE::Double;
  m[partialEnergy] = DATA_TYPE::Double;
  m[partialForces] = DATA_TYPE::Double;
  m[partialParticleEnergy] = DATA_TYPE::Double;
  m[partialVirial] = DATA_TYPE::Double;
  m[partialParticleVirial] = DATA_TYPE::Double;
  return m;
}

DataTypeMap const argumentNameToDataType = GetDataTypeMap();
}  // namespace


namespace
{
typedef std::vector<ArgumentName> ArgumentVector;
ArgumentVector const GetArgumentVector()
{
  ArgumentVector v;
  v.push_back(numberOfParticles);
  v.push_back(particleSpeciesCodes);
  v.push_back(particleContributing);
  v.push_back(coordinates);
  return v;
}
}  // namespace
// used by KIM::ModelImplementation
extern ArgumentVector const requiredByAPI_Arguments = GetArgumentVector();

void GetNumberOfArguments(int * const numberOfArguments)
{
  *numberOfArguments = argumentNameToString.size();
}

int GetArgumentName(int const index, ArgumentName * const argumentName)
{
  int numberOfArguments;
  GetNumberOfArguments(&numberOfArguments);
  if ((index < 0) || (index >= numberOfArguments)) return true;

  StringMap::const_iterator iter = argumentNameToString.begin();
  for (int i=0; i<index; ++i) ++iter;
  *argumentName = iter->first;
  return false;  // no error
}

int GetArgumentDataType(ArgumentName const argumentName,
                        DataType * const dataType)
{
  DataTypeMap::const_iterator iter = argumentNameToDataType.find(argumentName);

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
ArgumentName::ArgumentName(std::string const & str)
{
  argumentNameID = -1;
  for (ARGUMENT_NAME::StringMap::const_iterator iter
           = ARGUMENT_NAME::argumentNameToString.begin();
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

std::string const & ArgumentName::String() const
{
  ARGUMENT_NAME::StringMap::const_iterator iter
      = ARGUMENT_NAME::argumentNameToString.find(*this);
  if (iter == ARGUMENT_NAME::argumentNameToString.end())
    return ARGUMENT_NAME::argumentNameUnknown;
  else
    return iter->second;
}
}  // namespace KIM
