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
// Release: This file is part of the kim-api-v2-2.0.1 package.
//

#include <map>
#include <vector>

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
#include "KIM_ComputeArgumentName.inc"
ComputeArgumentName const numberOfParticles(ID_numberOfParticles);
ComputeArgumentName const particleSpeciesCodes(ID_particleSpeciesCodes);
ComputeArgumentName const particleContributing(ID_particleContributing);
ComputeArgumentName const coordinates(ID_coordinates);
ComputeArgumentName const partialEnergy(ID_partialEnergy);
ComputeArgumentName const partialForces(ID_partialForces);
ComputeArgumentName const partialParticleEnergy(ID_partialParticleEnergy);
ComputeArgumentName const partialVirial(ID_partialVirial);
ComputeArgumentName const partialParticleVirial(ID_partialParticleVirial);

namespace
{
typedef std::map<ComputeArgumentName const,
                 std::string,
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
typedef std::
    map<ComputeArgumentName const, DataType, COMPUTE_ARGUMENT_NAME::Comparator>
        DataTypeMap;

DataTypeMap const GetDataTypeMap()
{  // Here we must assume that the DATA_TYPE:: constants are not initialized.
  int const Integer = 0;
  int const Double = 1;

  DataTypeMap m;
  m[numberOfParticles] = DataType(Integer);
  m[particleSpeciesCodes] = DataType(Integer);
  m[particleContributing] = DataType(Integer);
  m[coordinates] = DataType(Double);
  m[partialEnergy] = DataType(Double);
  m[partialForces] = DataType(Double);
  m[partialParticleEnergy] = DataType(Double);
  m[partialVirial] = DataType(Double);
  m[partialParticleVirial] = DataType(Double);

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

void GetNumberOfComputeArgumentNames(int * const numberOfComputeArgumentNames)
{
  *numberOfComputeArgumentNames = computeArgumentNameToString.size();
}

int GetComputeArgumentName(int const index,
                           ComputeArgumentName * const computeArgumentName)
{
  int numberOfComputeArgumentNames;
  GetNumberOfComputeArgumentNames(&numberOfComputeArgumentNames);
  if ((index < 0) || (index >= numberOfComputeArgumentNames)) return true;

  StringMap::const_iterator iter = computeArgumentNameToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
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
ComputeArgumentName::ComputeArgumentName() {}
ComputeArgumentName::ComputeArgumentName(int const id) :
    computeArgumentNameID(id)
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

bool ComputeArgumentName::Known() const
{
  int numberOfComputeArgumentNames;
  COMPUTE_ARGUMENT_NAME::GetNumberOfComputeArgumentNames(
      &numberOfComputeArgumentNames);

  for (int i = 0; i < numberOfComputeArgumentNames; ++i)
  {
    ComputeArgumentName cam;
    COMPUTE_ARGUMENT_NAME::GetComputeArgumentName(i, &cam);

    if (*this == cam) { return true; }
  }

  return false;
}

bool ComputeArgumentName::operator==(ComputeArgumentName const & rhs) const
{
  return computeArgumentNameID == rhs.computeArgumentNameID;
}
bool ComputeArgumentName::operator!=(ComputeArgumentName const & rhs) const
{
  return computeArgumentNameID != rhs.computeArgumentNameID;
}

std::string const & ComputeArgumentName::ToString() const
{
  COMPUTE_ARGUMENT_NAME::StringMap::const_iterator iter
      = COMPUTE_ARGUMENT_NAME::computeArgumentNameToString.find(*this);
  if (iter == COMPUTE_ARGUMENT_NAME::computeArgumentNameToString.end())
    return COMPUTE_ARGUMENT_NAME::computeArgumentNameUnknown;
  else
    return iter->second;
}
}  // namespace KIM
