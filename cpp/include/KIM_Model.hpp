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


#ifndef KIM_MODEL_HPP_
#define KIM_MODEL_HPP_

#include <string>

#ifndef KIM_FUNC_HPP_
#include "KIM_func.hpp"
#endif

namespace KIM
{
// Forward declarations
class LogVerbosity;
class DataType;
class LanguageName;
class SpeciesName;
class Numbering;
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
class ComputeArguments;
class ModelImplementation;


class Model
{
 public:
  static int Create(Numbering const numbering,
                    LengthUnit const requestedLengthUnit,
                    EnergyUnit const requestedEnergyUnit,
                    ChargeUnit const requestedChargeUnit,
                    TemperatureUnit const requestedTemperatureUnit,
                    TimeUnit const requestedTimeUnit,
                    std::string const & modelName,
                    int * const requestedUnitsAccepted,
                    Model ** const model);
  static void Destroy(Model ** const model);

  void GetInfluenceDistance(double * const influenceDistance) const;

  void GetNeighborListPointers(
      int * const numberOfNeighborLists,
      double const ** const cutoffs,
      int const ** const modelWillNotRequestNeighborsOfNoncontributingParticles)
      const;

  void GetUnits(LengthUnit * const lengthUnit,
                EnergyUnit * const energyUnit,
                ChargeUnit * const chargeUnit,
                TemperatureUnit * const temperatureUnit,
                TimeUnit * const timeUnit) const;

  int ComputeArgumentsCreate(ComputeArguments ** const computeArguments) const;
  int ComputeArgumentsDestroy(ComputeArguments ** const computeArguments) const;
  int Compute(ComputeArguments const * const computeArguments) const;

  int ClearThenRefresh();

  int GetSpeciesSupportAndCode(SpeciesName const speciesName,
                               int * const speciesIsSupported,
                               int * const code) const;

  void GetNumberOfParameters(int * const numberOfParameters) const;
  int GetParameterDataTypeExtentAndDescription(
      int const index, DataType * const dataType, int * extent,
      std::string const ** const description) const;
  int GetParameter(int const parameterIndex, int const arrayIndex,
                   int * const parameterValue) const;
  int GetParameter(int const parameterIndex, int const arrayIndex,
                   double * const parameterValue) const;
  int SetParameter(int const parameterIndex, int const arrayIndex,
                   int const parameterValue);
  int SetParameter(int const parameterIndex, int const arrayIndex,
                   double const parameterValue);

  void SetSimulatorBufferPointer(void * const ptr);
  void GetSimulatorBufferPointer(void ** const ptr) const;

  std::string const & String() const;

  void SetLogID(std::string const & logID);
  void PushLogVerbosity(LogVerbosity const logVerbosity);
  void PopLogVerbosity();

 private:
  // do not allow copy constructor or operator=
  Model(Model const &);
  void operator=(Model const &);

  Model();
  ~Model();

  ModelImplementation * pimpl;
};  // class Model
}  // namespace KIM
#endif  // KIM_MODEL_HPP_
