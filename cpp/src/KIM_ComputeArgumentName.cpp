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

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_ComputeArgumentName.hpp"
#endif

namespace KIM
{

// Order doesn't matter as long as all values are unique
namespace COMPUTE_ARGUMENT_NAME
{
ComputeArgumentName const numberOfParticles(0);
ComputeArgumentName const particleSpeciesCodes(1);
ComputeArgumentName const particleContributing(2);
ComputeArgumentName const coordinates(3);
ComputeArgumentName const partialEnergy(4);
ComputeArgumentName const partialForces(5);
ComputeArgumentName const partialParticleEnergy(6);
ComputeArgumentName const partialVirial(7);
ComputeArgumentName const partialParticleVirial(8);

namespace
{
typedef std::map<ComputeArgumentName const, std::string,
                 COMPUTE_ARGUMENT_NAME::Comparator>
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

StringMap const computeArgumentNameToString = GetStringMap();
std::string const computeArgumentNameUnknown("unknown");
}  // namespace


namespace
{
typedef std::map<ComputeArgumentName const, DataType,
                 COMPUTE_ARGUMENT_NAME::Comparator>
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

DataTypeMap const computeArgumentNameToDataType = GetDataTypeMap();
}  // namespace


namespace
{
typedef std::vector<ComputeArgumentName> ComputeArgumentVector;
ComputeArgumentVector const GetComputeArgumentVector()
{
  ComputeArgumentVector v;
  v.push_back(numberOfParticles);
  v.push_back(particleSpeciesCodes);
  v.push_back(particleContributing);
  v.push_back(coordinates);
  return v;
}
}  // namespace
// used by KIM::ModelImplementation
extern ComputeArgumentVector const requiredByAPI_ComputeArguments
= GetComputeArgumentVector();

void GetNumberOfComputeArguments(int * const numberOfComputeArguments)
{
  *numberOfComputeArguments = computeArgumentNameToString.size();
}

int GetComputeArgumentName(int const index,
                           ComputeArgumentName * const computeArgumentName)
{
  int numberOfComputeArguments;
  GetNumberOfComputeArguments(&numberOfComputeArguments);
  if ((index < 0) || (index >= numberOfComputeArguments)) return true;

  StringMap::const_iterator iter = computeArgumentNameToString.begin();
  for (int i=0; i<index; ++i) ++iter;
  *computeArgumentName = iter->first;
  return false;  // no error
}

int GetComputeArgumentDataType(ComputeArgumentName const computeArgumentName,
                               DataType * const dataType)
{
  DataTypeMap::const_iterator iter
      = computeArgumentNameToDataType.find(computeArgumentName);

  if (iter == computeArgumentNameToDataType.end())
    return true;
  else
  {
    *dataType = iter->second;
    return false;
  }
}
}  // namespace COMPUTE_ARGUMENT_NAME


// implementation of ComputeArgumentName
ComputeArgumentName::ComputeArgumentName() : computeArgumentNameID(0) {}
ComputeArgumentName::ComputeArgumentName(int const id)
    : computeArgumentNameID(id)
{
}

ComputeArgumentName::ComputeArgumentName(std::string const & str)
{
  computeArgumentNameID = -1;
  for (COMPUTE_ARGUMENT_NAME::StringMap::const_iterator iter
           = COMPUTE_ARGUMENT_NAME::computeArgumentNameToString.begin();
       iter != COMPUTE_ARGUMENT_NAME::computeArgumentNameToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      computeArgumentNameID = (iter->first).computeArgumentNameID;
      break;
    }
  }
}

bool ComputeArgumentName::operator==(ComputeArgumentName const & rhs) const
{return computeArgumentNameID == rhs.computeArgumentNameID;}
bool ComputeArgumentName::operator!=(ComputeArgumentName const & rhs) const
{return computeArgumentNameID != rhs.computeArgumentNameID;}

std::string const & ComputeArgumentName::String() const
{
  COMPUTE_ARGUMENT_NAME::StringMap::const_iterator iter
      = COMPUTE_ARGUMENT_NAME::computeArgumentNameToString.find(*this);
  if (iter == COMPUTE_ARGUMENT_NAME::computeArgumentNameToString.end())
    return COMPUTE_ARGUMENT_NAME::computeArgumentNameUnknown;
  else
    return iter->second;
}
}  // namespace KIM
