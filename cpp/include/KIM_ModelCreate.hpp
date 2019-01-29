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


#ifndef KIM_MODEL_CREATE_HPP_
#define KIM_MODEL_CREATE_HPP_

#include <sstream>
#include <string>

#ifndef KIM_FUNCTION_TYPES_HPP_
#include "KIM_FunctionTypes.hpp"
#endif

namespace KIM
{
// Forward declarations
class LogVerbosity;
class LanguageName;
class Numbering;
class ModelRoutineName;
class SpeciesName;
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
class ModelCreateImplementation;


/// \brief Provides the interface to a %KIM API Model object for use by models
/// within their MODEL_ROUTINE_NAME::Create routine.
///
/// \since 2.0
class ModelCreate
{
 public:
  /// \brief Set the Model's Numbering.
  ///
  /// \param[in] numbering The Model's Numbering.
  ///
  /// \return \c true if \c numbering is unknown.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ModelCreate_SetModelNumbering
  ///
  /// \since 2.0
  int SetModelNumbering(Numbering const numbering);

  /// \brief Set the Model's influence distance data pointer.
  ///
  /// \todo Add more detailed description of \c influenceDistance (or link to
  /// docs elsewhere?)
  ///
  /// \param[in] influenceDistance Pointer to Model's influence distance.
  ///
  /// \note The model is responsible for allocating the memory associated with
  /// the influence distance data.  The model must use the \ref
  /// cache_buffer_pointers "Model's buffer pointer" to retain access to this
  /// memory location and avoid a memory leak.
  ///
  /// \sa KIM_ModelCreate_SetInfluenceDistancePointer
  ///
  /// \since 2.0
  void SetInfluenceDistancePointer(double const * const influenceDistance);

  /// \brief Set the Model's neighbor list data pointers.
  ///
  /// \todo Add more detailed description
  ///
  /// \param[in] numberOfNeighborLists The number of neighbor lists required by
  ///            the Model.
  /// \param[in] cutoffs Array of cutoff values for each of the required
  ///            neighbor lists.
  /// \param[in] modelWillNotRequestNeighborsOfNoncontributingParticles Array
  ///            of integers; \c true or \c false for each neighbor list
  ///            required by the Model.
  ///
  /// \note The model is responsible for allocating the memory associated with
  /// the neighbor list cutoffs and \c
  /// modelWillNotRequestNeighborsOfNoncontributingParticles data.  The model
  /// must use the \ref cache_buffer_pointers "Model's buffer pointer" to
  /// retain access to this memory location and avoid a memory leak.
  ///
  /// \sa KIM_ModelCreate_SetNeighborListPointers
  ///
  /// \since 2.0
  void SetNeighborListPointers(
      int const numberOfNeighborLists,
      double const * const cutoffs,
      int const * const modelWillNotRequestNeighborsOfNoncontributingParticles);

  /// \brief Set the function pointer for the ModelRoutineName of interest.
  ///
  /// \param[in] modelRoutineName The ModelRoutineName of interest.
  /// \param[in] languageName The LanguageName of the ModelRoutineName.

  /// \param[in] required Integer, \c true if the model requires the simulator
  ///            to call the associated ModelRoutineName in order to be used
  ///            correctly, \c false otherwise.
  /// \param[in] fptr Function pointer for the Model's ModelRoutineName
  ///            routine.
  ///
  /// \return \c true if \c modelRoutineName or \c langaugeName are unknown.
  /// \return \c true if `required == false` and \cmodelRoutineName has
  ///         SupportStatus SUPPORT_STATUS::requiredByAPI.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ModelCreate_SetRoutinePointer
  ///
  /// \since 2.0
  int SetRoutinePointer(ModelRoutineName const modelRoutineName,
                        LanguageName const languageName,
                        int const required,
                        Function * const fptr);

  int SetSpeciesCode(SpeciesName const speciesName, int const code);

  int SetParameterPointer(int const extent,
                          int * const ptr,
                          std::string const & name,
                          std::string const & description);
  int SetParameterPointer(int const extent,
                          double * const ptr,
                          std::string const & name,
                          std::string const & description);

  void SetModelBufferPointer(void * const ptr);

  int SetUnits(LengthUnit const lengthUnit,
               EnergyUnit const energyUnit,
               ChargeUnit const chargeUnit,
               TemperatureUnit const temperatureUnit,
               TimeUnit const timeUnit);

  static int ConvertUnit(LengthUnit const fromLengthUnit,
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
                         double * const conversionFactor);

  /// \brief Write a log entry into the log file.
  ///
  /// This results in a no-op if \c logVerbosity is LOG_VERBOSITY::silent or if
  /// \c logVerbosity is greater-than the Log object's top LogVerbosity its
  /// stack.
  ///
  /// \param[in] logVerbosity The LogVerbosity level for the entry.
  /// \param[in] message The body text of the log entry.
  /// \param[in] lineNumber The source code file line number.
  /// \param[in] fileName The source code file name.
  ///
  /// \sa KIM_ModelCreate_LogEntry
  ///
  /// \since 2.0
  void LogEntry(LogVerbosity const logVerbosity,
                std::string const & message,
                int const lineNumber,
                std::string const & fileName) const;

  /// \overload
  void LogEntry(LogVerbosity const logVerbosity,
                std::stringstream const & message,
                int const lineNumber,
                std::string const & fileName) const;

  /// \brief Get a string representing the internal state of the Model object.
  ///
  /// This string is primarily meant for use as a debugging tool.  The string
  /// may be quite long.  It begins and ends with lines consisting only of \c
  /// ='s.
  ///
  /// \sa KIM_ModelCreate_ToString
  ///
  /// \since 2.0
  std::string const & ToString() const;

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
