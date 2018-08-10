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


#ifndef KIM_MODEL_CREATE_HPP_
#define KIM_MODEL_CREATE_HPP_

#include <string>
#include <sstream>

#ifndef KIM_FUNC_HPP_
#include "KIM_func.hpp"
#endif


namespace KIM
{
// Forward declarations
class LogVerbosity;
class LanguageName;
class Numbering;
class SpeciesName;
class SupportStatus;
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
class ModelCreateImplementation;


class ModelCreate
{
 public:
  int SetModelNumbering(Numbering const numbering);

  void SetInfluenceDistancePointer(double const * const influenceDistance);

  void SetNeighborListPointers(
      int const numberOfNeighborLists,
      double const * const cutoffs,
      int const * const modelWillNotRequestNeighborsOfNoncontributingParticles);

  int SetRefreshPointer(LanguageName const languageName, func * const fptr);
  int SetDestroyPointer(LanguageName const languageName, func * const fptr);
  int SetComputeArgumentsCreatePointer(LanguageName const languageName,
                                       func * const fptr);
  int SetComputeArgumentsDestroyPointer(LanguageName const languageName,
                                        func * const fptr);
  int SetComputePointer(LanguageName const languageName, func * const fptr);

  int SetSpeciesCode(SpeciesName const speciesName, int const code);

  int SetParameterPointer(int const extent, int * const ptr,
                          std::string const & description);
  int SetParameterPointer(int const extent, double * const ptr,
                          std::string const & description);

  void SetModelBufferPointer(void * const ptr);

  int SetUnits(LengthUnit const lengthUnit,
               EnergyUnit const energyUnit,
               ChargeUnit const chargeUnit,
               TemperatureUnit const temperatureUnit,
               TimeUnit const timeUnit);

  int ConvertUnit(
      LengthUnit const fromLengthUnit,
      EnergyUnit const fromEnergyUnit,
      ChargeUnit const fromChargeUnit,
      TemperatureUnit const fromTemperatureUnit,
      TimeUnit const fromTimeUnit,
      LengthUnit const toLengthUnit,
      EnergyUnit const toEnergyUnit,
      ChargeUnit const toChargeUnit,
      TemperatureUnit const toTemperatureUnit,
      TimeUnit const toTimeUnit,
      double const lengthExponent,
      double const energyExponent,
      double const chargeExponent,
      double const temperatureExponent,
      double const timeExponent,
      double * const conversionFactor) const;

  void LogEntry(LogVerbosity const logVerbosity, std::string const & message,
                int const lineNumber, std::string const & fileName) const;
  void LogEntry(LogVerbosity const logVerbosity,
                std::stringstream const & message,
                int const lineNumber, std::string const & fileName) const;

  std::string const & String() const;

 private:
  // do not allow copy constructor or operator=
  ModelCreate(ModelCreate const &);
  void operator=(ModelCreate const &);

  ModelCreate();
  ~ModelCreate();

  ModelCreateImplementation * pimpl;
};  // class ModelCreate
}  // namespace KIM
#endif  // KIM_MODEL_CREATE_HPP_
