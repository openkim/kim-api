//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api-2.3.0 package.
//


#ifndef KIM_MODEL_HPP_
#define KIM_MODEL_HPP_

#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class DataType;
class ModelRoutineName;
class SpeciesName;
class Numbering;
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
class ComputeArguments;
class ModelImplementation;

/// \brief Provides the primary interface to a %KIM API Model object and is
/// meant to be used by simulators.
///
/// \sa KIM_Model, kim_model_module::kim_model_handle_type
///
/// \since 2.0
class Model
{
 public:
  /// \brief Create a new %KIM API Model object.
  ///
  /// Allocates a new %KIM API Model object for use by a Simulator and calls
  /// the Model's MODEL_ROUTINE_NAME::Create routine.
  ///
  /// \param[in]  numbering The Numbering value used by the Simulator.
  /// \param[in]  requestedLengthUnit The base LengthUnit requested by the
  ///             Simulator.
  /// \param[in]  requestedEnergyUnit The base EnergyUnit requested by the
  ///             Simulator.
  /// \param[in]  requestedChargeUnit The base ChargeUnit requested by the
  ///             Simulator.
  /// \param[in]  requestedTemperatureUnit The base TemperatureUnit requested
  ///             by the Simulator.
  /// \param[in]  requestedTimeUnit The base TimeUnit requested by the
  ///             Simulator.
  /// \param[in]  modelName The name of the Model to be created.

  /// \param[out] requestedUnitsAccepted An integer that is set to \c true if
  ///             the Model accepts the Simulator's requested base units, \c
  ///             false if the Model will use base units other than those
  ///             requested by the Simulator.
  /// \param[out] model Pointer to the newly created Model object.
  ///
  /// \note The value of `modelName` is required to be a valid C-identifier.
  ///
  /// \note A requested unit of \c unused indicates that the Simulator will not
  /// employ any derived units connected to the associated base unit.  This
  /// avoids the need for the Simulator to make an arbitrary choice that could
  /// cause the Model to not accept its requested base units.
  ///
  /// \return \c true if the %KIM API is unable to allocate a new log object.
  /// \return \c true if \c numbering or any of the base units are unknown.
  /// \return \c true if the requested model's library cannot be found,
  ///         opened, is of the wrong type, or has some other problem.
  /// \return \c true if the Model's MODEL_ROUTINE_NAME::Create routine
  ///         returns \c true.
  /// \return \c true if the Model's MODEL_ROUTINE_NAME::Create routine does
  ///         not set the Model's (1) numbering, (2) base units, (3) influence
  ///         distance, (4) numberOfNeighborLists, (5) cutoff values, (6)
  ///         modelWillNotRequesNeighborsOfNoncontributingParticles, (7)
  ///         required ModelRoutineName pointers, or (8) supported species
  ///         codes.
  /// \return \c true if `max(cutoffs) > influenceDistance`.
  /// \return \c true if parameters are registered but not a
  ///         MODEL_ROUTINE_NAME::Refresh pointer, or vise-versa.
  /// \return \c true if a MODEL_ROUTINE_NAME::WriteParameterizedModel
  ///         pointer is provided but no parameters are registered.
  /// \return \c true if the Model's MODEL_ROUTINE_NAME::Create routine
  ///         does not set the Model's Numbering.
  /// \return \c false otherwise.
  ///
  /// \post \c requestedUnitsAccepted is unchanged and `model == NULL` if an
  ///       error occurs.
  ///
  /// \sa KIM_Model_Create, kim_model_module::kim_model_create
  ///
  /// \since 2.0
  static int Create(Numbering const numbering,
                    LengthUnit const requestedLengthUnit,
                    EnergyUnit const requestedEnergyUnit,
                    ChargeUnit const requestedChargeUnit,
                    TemperatureUnit const requestedTemperatureUnit,
                    TimeUnit const requestedTimeUnit,
                    std::string const & modelName,
                    int * const requestedUnitsAccepted,
                    Model ** const model);

  /// \brief Destroy a previously Model::Create'd object.
  ///
  /// Call the Model's MODEL_ROUTINE_NAME::Destroy routine and deallocate
  /// the Model object.
  ///
  /// \param[inout] model Pointer to the Model object.
  ///
  /// \pre \c *model points to a previously created %KIM API Model object.
  ///
  /// \post `*model == NULL`.
  ///
  /// \sa KIM_Model_Destroy, kim_model_module::kim_model_destroy
  ///
  // \since 2.0
  static void Destroy(Model ** const model);

  /// \brief Determine presence and required status of the given
  /// ModelRoutineName.
  ///
  /// \param[in]  modelRoutineName The ModelRoutineName of interest.
  /// \param[out] present \c true if the Model provides the routine, \c false
  ///             otherwise.
  /// \param[out] required \c true if the Model requires the use of the routine,
  ///             \c false otherwise.
  ///
  /// \return \c true if \c modelRoutineName is unknown.
  /// \return \c false otherwise.
  ///
  /// \pre \c present or \c required may be \c NULL if the corresponding value
  ///      is not needed.
  ///
  /// \post \c present and \c required are unchanged if an error occurs.
  ///
  /// \sa KIM_Model_IsRoutinePresent, kim_model_module::kim_is_routine_present
  ///
  /// \since 2.0
  int IsRoutinePresent(ModelRoutineName const modelRoutineName,
                       int * const present,
                       int * const required) const;

  /// \brief Get the Model's influence distance.
  ///
  /// \param[out] influenceDistance
  ///
  /// \todo Add more detailed description of \c influenceDistance. (or link to
  /// docs elsewhere?)
  ///
  /// \sa KIM_Model_GetInfluenceDistance,
  /// kim_model_module::kim_get_influence_distance
  ///
  /// \since 2.0
  void GetInfluenceDistance(double * const influenceDistance) const;

  /// \brief Get the Model's neighbor list information.
  ///
  /// Each neighbor list has a cutoff value and a flag indicating if the Model
  /// will request the neighbors of non-contributing particles.
  ///
  /// \note Output pointers obtained from this routine are valid until the next
  /// call to Model::ClearThenRefresh of the KIM::Model object is
  /// Model::Destroy'd.
  ///
  /// \param[out] numberOfNeighborLists The number of neighbor lists required
  ///             by the Model.
  /// \param[out] cutoffs The cutoff distance for each neighbor list.
  /// \param[out] modelWillNotRequestNeighborsOfNoncontributingParticles
  ///             \c true if such neighbor lists will not be requested,
  ///             \c false otherwise.
  ///
  /// \pre \c numberOfNeighborLists, \c cutoffs, or
  ///      \c modelWillNotRequestNeighborsOfNoncontributingParticles may be
  ///      \c NULL if the corresponding value is not needed.
  ///
  /// \sa KIM_Model_GetNeighborListPointers,
  /// kim_model_module::kim_get_number_of_neighbor_lists,
  /// kim_model_module::kim_get_neighbor_list_values
  ///
  /// \since 2.0
  void GetNeighborListPointers(
      int * const numberOfNeighborLists,
      double const ** const cutoffs,
      int const ** const modelWillNotRequestNeighborsOfNoncontributingParticles)
      const;

  /// \brief Get the Model's base unit values.
  ///
  /// \param[out] lengthUnit The Model's base LengthUnit.
  /// \param[out] energyUnit The Model's base EnergyUnit.
  /// \param[out] chargeUnit The Model's base ChargeUnit.
  /// \param[out] temperatureUnit The Model's base TemperatureUnit.
  /// \param[out] timeUnit The Model's base TimeUnit.
  ///
  /// \note A unit of \c unused indicates the the Model does not deal with any
  /// quantities whose derived unit involves the corresponding base unit.  For
  /// example, many models only deal with quantities that are derived from just
  /// the energy and length base units (such as force, virial, etc.), and thus
  /// should set \c chargeUnit, \c temperatureUnit, and \c timeUnit to \c
  /// unused.
  ///
  /// \pre \c lengthUnit, \c energyUnit, \c chargeUnit, \c temperatureUnit, or
  ///      \c timeUnit may be \c NULL if the corresponding value is not needed.
  ///
  /// \sa KIM_Model_GetUnits, kim_model_module::kim_get_units
  ///
  /// \since 2.0
  void GetUnits(LengthUnit * const lengthUnit,
                EnergyUnit * const energyUnit,
                ChargeUnit * const chargeUnit,
                TemperatureUnit * const temperatureUnit,
                TimeUnit * const timeUnit) const;

  /// \brief Create a new ComputeArguments object for the Model object.
  ///
  /// Allocates a new ComputeArguments object for use by a Simulator and
  /// calls the Model's MODEL_ROUTINE_NAME::ComputeArgumentsCreate routine.
  ///
  /// \param[inout] computeArguments Pointer to the newly created
  ///               ComputeArguments object.
  ///
  /// \return \c true if the %KIM API is unable to allocate a new
  ///         ComputeArguments object.
  /// \return \c true if the Model's MODEL_ROUTINE_NAME::ComputeArgumentsCreate
  ///         routine returns \c true.
  /// \return \c false otherwise.
  ///
  /// \post `computeArguments == NULL` if an error occurs.
  ///
  /// \sa KIM_Model_ComputeArgumentsCreate,
  /// kim_model_module::kim_compute_arguments_create
  ///
  /// \since 2.0
  int ComputeArgumentsCreate(ComputeArguments ** const computeArguments) const;

  /// \brief Destroy a previously Model::ComputeArgumentsCreate'd object.
  ///
  /// Call the Model's MODEL_ROUTINE_NAME::ComputeArgumentsDestroy routine
  /// and deallocate the ComputeArguments object.
  ///
  /// \param[inout] computeArguments Pointer to the ComputeArguments object.
  ///
  /// \return \c true if the ComputeArguments object was created by a
  ///         different Model (as identified by its name string).
  /// \return \c true if the Model's
  ///         MODEL_ROUTINE_NAME::ComputeArgumentsDestroy routine returns \c
  ///         true.
  /// \return \c false otherwise.
  ///
  /// \post \c computeArguments is unchanged if an error occurs, otherwise
  ///       `computeArguments == NULL`.
  ///
  /// \sa KIM_Model_ComputeArgumentsDestroy,
  /// kim_model_module::kim_compute_arguments_destroy
  ///
  /// \since 2.0
  int ComputeArgumentsDestroy(ComputeArguments ** const computeArguments) const;

  /// \brief Call the Model's MODEL_ROUTINE_NAME::Compute routine.
  ///
  /// \param[in] computeArguments A ComputeArguments object.
  ///
  /// \return \c true if \c computeArguments was created by a different Model
  ///         (as identified by its name string).
  /// \return \c true if
  ///         ComputeArguments::AreAllRequiredArgumentsAndCallbacksPresent
  ///         returns \c false for \c computeArguments.
  /// \return \c true if the Model's MODEL_ROUTINE_NAME::Compute routine
  ///         returns \c true.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_Model_Compute, kim_model_module::kim_compute
  ///
  /// \since 2.0
  int Compute(ComputeArguments const * const computeArguments) const;

  /// \brief Call the Model's MODEL_ROUTINE_NAME::Extension routine.
  ///
  /// \param[in]    extensionID A string uniquely identifying the extension to
  ///               be executed.
  /// \param[inout] extensionStructure Pointer to a data structure of the type
  ///               defined by the extension to be executed.
  ///
  /// \return \c true if the Model does not provide a
  ///         MODEL_ROUTINE_NAME::Extension routine.
  /// \return \c true if the Model's MODEL_ROUTINE_NAME::Extension routine
  ///         returns \c true.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_Model_Extension, kim_model_module::kim_extension
  ///
  /// \since 2.0
  int Extension(std::string const & extensionID,
                void * const extensionStructure);

  /// \brief Clear influence distance and neighbor list pointers and refresh
  /// Model object after parameter changes.
  ///
  /// Nullify the Model's influence distance, neighbor list cutoff, and \c
  /// modelWillNotRequestNeighborsOfNoncontributingParticles pointers.  Then
  /// call the Model's MODEL_ROUTINE_NAME::Refresh routine.
  ///
  /// \return \c true if the Model does not register any parameters.
  /// \return \c true if the Model's MODEL_ROUTINE_NAME::Refresh routine
  ///         returns \c true.
  /// \return \c true if the Model's MODEL_ROUTINE_NAME::Refresh routine
  ///         does not set the influence distance, the number of neighbor lists,
  ///         the neighbor list cutoffs, or the \c
  ///         modelWillNotRequestNeighborsOfNoncontributingParticles pointer.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_Model_ClearThenRefresh, kim_model_module::kim_clear_then_refresh
  ///
  /// \since 2.0
  int ClearThenRefresh();

  /// \brief Call the Model's MODEL_ROUTINE_NAME::WriteParameterizedModel
  /// routine.
  ///
  /// \param[in] path Path string to directory within which the new
  ///            parameterized model files should be written.
  /// \param[in] modelName Name of the parameterized model to be created.  Must
  ///            be a valid C identifier.
  ///
  /// \return \c true if the Model object is not a parameterized model.
  /// \return \c true if \c modelName is not a valid C identifier.
  /// \return \c true if the Model's
  ///         MODEL_ROUTINE_NAME::WriteParameterizedModel routine returns
  ///         \c true.
  /// \return \c true if the %KIM API is unable to write the \c CMakeLists.txt
  ///         file.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_Model_WriteParameterizedModel,
  /// kim_model_module::kim_write_parameterized_model
  ///
  /// \since 2.0
  int WriteParameterizedModel(std::string const & path,
                              std::string const & modelName) const;

  /// \brief Get the Model's support and code for the requested SpeciesName.
  ///
  /// \param[in]  speciesName The SpeciesName of interest.
  /// \param[out] speciesIsSupported \c true if the Model supports the species
  ///             of interest, \c false otherwise.
  /// \param[out] code Value used by the Model to refer to the species of
  ///             interest.
  ///
  /// \return \c true if \c speciesName is unknown.
  /// \return \c false otherwise.
  ///
  /// \pre \c code may be \c NULL if the value is not needed.
  ///
  /// \post \c speciesIsSupported and \c code are unchanged if an error occurs.
  ///       \c code is unchanged if `speciesIsSupported == false`.
  ///
  /// \sa KIM_Model_GetSpeciesSupportAndCode,
  /// kim_model_module::kim_get_species_support_and_code
  ///
  /// \since 2.0
  int GetSpeciesSupportAndCode(SpeciesName const speciesName,
                               int * const speciesIsSupported,
                               int * const code) const;

  /// \brief Get the number of parameter arrays provided by the Model.
  ///
  /// \param[out] numberOfParameters The number of parameter arrays provided
  ///             by the Model.
  ///
  /// \sa KIM_Model_GetNumberOfParameters,
  /// kim_model_module::kim_get_number_of_parameters
  ///
  /// \since 2.0
  void GetNumberOfParameters(int * const numberOfParameters) const;

  /// \brief Get the metadata associated with one of the Model's parameter
  /// arrays.
  ///
  /// \note String pointers obtained from this routine are valid until the
  /// KIM::Model object is Model::Destroy'd.
  ///
  /// \param[in]  parameterIndex Zero-based index for the parameter array.
  /// \param[out] dataType The DataType value for the parameter array.
  /// \param[out] extent The number of parameters in the array.
  /// \param[out] name A string identifying the parameter array (will be a valid
  ///             C identifier).
  /// \param[out] description A free-form string description of the parameter
  ///             array's content.
  ///
  /// \return \c true if \c parameterIndex is invalid
  /// \return \c false otherwise.
  ///
  /// \pre \c dataType, \c extent, \c name, or \c description may be \c NULL if
  ///      the corresponding value is not needed.
  ///
  /// \sa KIM_Model_GetParameterMetadata,
  /// kim_model_module::kim_get_parameter_metadata
  ///
  /// \since 2.0
  int GetParameterMetadata(int const parameterIndex,
                           DataType * const dataType,
                           int * const extent,
                           std::string const ** const name,
                           std::string const ** const description) const;

  /// \brief Get a parameter value from the Model.
  ///
  /// \param[in]  parameterIndex Zero-based index for the parameter array of
  ///             interest.
  /// \param[in]  arrayIndex Zero-based index within the array for the parameter
  ///             of interest.
  /// \param[out] parameterValue The value of the parameter of interest.
  ///
  /// \return \c true if \c parameterIndex is invalid.
  /// \return \c true if the specified parameter and \c parameterValue are of
  ///         different data types.
  /// \return \c true if \c arrayIndex is invalid.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_Model_GetParameterInteger, KIM_Model_GetParameterDouble,
  /// kim_model_module::kim_get_parameter
  ///
  /// \since 2.0
  int GetParameter(int const parameterIndex,
                   int const arrayIndex,
                   int * const parameterValue) const;

  /// \overload
  int GetParameter(int const parameterIndex,
                   int const arrayIndex,
                   double * const parameterValue) const;

  /// \brief Set a parameter value for the Model.
  ///
  /// \param[in] parameterIndex Zero-based index for the parameter array of
  ///            interest.
  /// \param[in] arrayIndex Zero-based index within the array for the parameter
  ///            of interest.
  /// \param[in] parameterValue The new value for the parameter of interest.
  ///
  /// \return \c true if \c parameterIndex is invalid.
  /// \return \c true if the specified parameter and \c parameterValue are of
  ///         different data types.
  /// \return \c true if \c arrayIndex is invalid.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_Model_SetParameterInteger, KIM_Model_SetParameterDouble,
  /// kim_model_module::kim_set_parameter
  ///
  /// \since 2.0
  int SetParameter(int const parameterIndex,
                   int const arrayIndex,
                   int const parameterValue);

  /// \overload
  int SetParameter(int const parameterIndex,
                   int const arrayIndex,
                   double const parameterValue);

  /// \brief Set the \ref cache_buffer_pointers "Simulator's buffer pointer"
  /// within the Model object.
  ///
  /// The simulator buffer pointer may be used by the simulator to associate
  /// a memory buffer with the Model object.
  ///
  /// \param[in] ptr The simulator buffer data pointer.
  ///
  /// \sa KIM_Model_SetSimulatorBufferPointer,
  /// kim_model_module::kim_set_simulator_buffer_pointer
  ///
  /// \since 2.0
  void SetSimulatorBufferPointer(void * const ptr);

  /// \brief Get the \ref cache_buffer_pointers "Simulator's buffer pointer"
  /// from the Model object.
  ///
  /// \param[out] ptr The simulator buffer data pointer.
  ///
  /// \note `ptr == NULL` if the simulator has not previously called
  ///       Model::SetSimulatorBufferPointer.
  ///
  /// \sa KIM_Model_GetSimulatorBufferPointer,
  /// kim_model_module::kim_get_simulator_buffer_pointer
  ///
  /// \since 2.0
  void GetSimulatorBufferPointer(void ** const ptr) const;

  /// \brief Get a string representing the internal state of the Model object.
  ///
  /// This string is primarily meant for use as a debugging tool.  The string
  /// may be quite long.  It begins and ends with lines consisting only of \c
  /// ='s.
  ///
  /// \sa KIM_Model_ToString, kim_model_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;

  /// \brief Set the identity of the Log object associated with the Model
  /// object.
  ///
  /// \param[in] logID String identifying the Model object's Log object.
  ///
  /// \sa KIM_Model_SetLogID, kim_model_module::kim_set_log_id
  ///
  /// \since 2.0
  void SetLogID(std::string const & logID);

  /// \brief Push a new LogVerbosity onto the Model object's Log object
  /// verbosity stack.
  ///
  /// \param[in] logVerbosity A LogVerbosity value.
  ///
  /// \sa KIM_Model_PushLogVerbosity, kim_model_module::kim_push_log_verbosity
  ///
  /// \since 2.0
  void PushLogVerbosity(LogVerbosity const logVerbosity);

  /// \brief Pop a LogVerbosity from the Model object's Log object verbosity
  /// stack.
  ///
  /// \sa KIM_Model_PopLogVerbosity, kim_model_module::kim_pop_log_verbosity
  ///
  /// \since 2.0
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
