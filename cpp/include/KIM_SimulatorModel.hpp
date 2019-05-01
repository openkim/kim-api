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


#ifndef KIM_SIMULATOR_MODEL_HPP_
#define KIM_SIMULATOR_MODEL_HPP_

#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class SimulatorModelImplementation;

/// \brief Provides the primary interface to a %KIM API SimulatorModel object
/// and is meant to be used by simulators.
///
/// \sa KIM_SimulatorModel,
/// kim_simulator_model_module::kim_simulator_model_handle_type
///
/// \since 2.1
class SimulatorModel
{
 public:
  /// \brief Create a new %KIM API SimulatorModel object.
  ///
  /// Allocates a new %KIM API SimulatorModel object for use by a Simulator.
  ///
  /// \param[in] simulatorModelName The name of the SimulatorModel to be
  ///            created.
  /// \param[out] simulatorModel Pointer to the newly created SimulatorModel
  ///             object.
  ///
  /// \todo Verify this documentaion
  ///
  /// \return \c true if the %KIM API is unable to allocate a new log object.
  /// \return \c true if the requested simulator model's library cannot be
  ///         found, opened, is of the wrong type, or has some other problem.
  /// \return \c false otherwise.
  ///
  /// \post `simulatorModel == NULL` if an error occurs.
  ///
  /// \sa KIM_SimulatorModel_Create,
  /// kim_simulator_model_module::kim_simulator_model_create
  ///
  /// \since 2.1
  static int Create(std::string const & simulatorModelName,
                    SimulatorModel ** const simulatorModel);

  /// \brief Destroy a previously SimulatorModel::Create'd object.
  ///
  /// Deallocate the SimulatorModel object.
  ///
  /// \param[inout] simulatorModel Pointer to the SimulatorModel object.
  ///
  /// \pre \c simulatorModel points to a previously created %KIM API
  ///      SimulatorModel object.
  ///
  /// \post `simulatorModel == NULL`.
  ///
  /// \sa KIM_SimulatorModel_Destroy,
  /// kim_simulator_model_module::kim_simulator_model_destroy
  ///
  // \since 2.1
  static void Destroy(SimulatorModel ** const simulatorModel);


  /// @@@ add docs
  void GetSimulatorName(std::string const ** const simulatorName) const;

  /// @@@ add docs
  void GetSimulatorVersion(std::string const ** const simulatorVersion) const;

  /// @@@ add docs
  void GetNumberOfSupportedSpecies(int * const numberOfSupportedSpecies) const;

  /// @@@ add docs
  int GetSupportedSpecies(int const index,
                          std::string const ** const speciesName) const;

  /// \brief Set template map.
  ///
  /// Provide the simulator specific key-value pairs for the field
  /// translations.  These will be added to the standard translations defined
  /// by the KIM API.
  ///
  /// \param[in] templateMap The std::map with key-value pairs to be used for
  ///            translation of simulator fields.
  ///
  /// \sa KIM_SimulatorModel_SetTemplateMap,
  /// kim_simulator_model_module::kim_set_template_map
  ///
  /// \since 2.1

  /// @@@ fixup docs
  void ClearTemplateMap();
  int AddTemplateMap(std::string const & key, std::string const & value);
  void CloseTemplateMap();

  //@@ add docs
  void GetNumberOfSimulatorFields(int * const numberOfSimulatorFields) const;

  //@@ add docs
  int GetSimulatorFieldMetadata(int const fieldIndex,
                                int * const extent,
                                std::string const ** const fieldName) const;

  //@@ add docs
  int GetSimulatorFieldLine(int const fieldIndex,
                            int const lineIndex,
                            std::string const ** const lineValue) const;

  /// \brief Get metadata file name.
  ///
  /// \param[out] originalMetadataFileName The original name of the metadata
  ///             file.
  /// \param[out] metadataFileName The name of the on-disk metadata file.
  ///
  /// \todo should this routine name be improved?
  ///
  /// \sa KIM_SimulatorModel_GetMetadataFileName,
  /// kim_simulator_model_module::kim_get_metadata_file_name
  ///
  /// \since 2.1
  void GetMetadataFileName(std::string const ** const originalMetadataFileName,
                           std::string const ** const metadataFileName) const;

  /// \brief Get the number of parameter files provided by the simulator model.
  ///
  /// \param[out] numberOfParameterFiles The number of parameter files.
  ///
  /// \sa KIM_SimulatorModel_GetNumberOfParameterFiles,
  /// kim_simulator_model_module::kim_get_number_of_parameter_files
  ///
  /// \since 2.1
  void GetNumberOfParameterFiles(int * const numberOfParameterFiles) const;

  /// \brief Get name of a particular parameter file.
  ///
  /// \param[in] index Zero-based index for the parameter file of interest.
  /// \param[out] originalParameterFileName The original file name of the
  ///             parameter file.
  /// \param[out] parameterFileName The on-disk file name of the parameter file.
  ///
  /// \return \c true if \c index is invalid.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_SimulatorModel_GetParameterFileName,
  /// kim_simulator_model_module::kim_get_parameter_file_name
  ///
  /// \since 2.1
  int GetParameterFileName(int const index,
                           std::string const ** const originalParameterFileName,
                           std::string const ** const parameterFileName) const;

  /// \brief Set the \ref cache_buffer_pointers "Simulator's buffer pointer"
  /// within the SimulatorModel object.
  ///
  /// The simulator buffer pointer may be used by the simulator to associate
  /// a memory buffer with the SimulatorModel object.
  ///
  /// \param[in] ptr The simulator buffer data pointer.
  ///
  /// \sa KIM_SimulatorModel_SetSimulatorBufferPointer,
  /// kim_simulator_model_module::kim_set_simulator_buffer_pointer
  ///
  /// \since 2.1
  void SetSimulatorBufferPointer(void * const ptr);

  /// \brief Get the \ref cache_buffer_pointers "Simulator's buffer pointer"
  /// from the SimulatorModel object.
  ///
  /// \param[out] ptr The simulator buffer data pointer.
  ///
  /// \note `ptr == NULL` if the simulator has not previously called
  ///       SimulatorModel::SetSimulatorBufferPointer.
  ///
  /// \sa KIM_SimulatorModel_GetSimulatorBufferPointer,
  /// kim_simulator_model_module::kim_get_simulator_buffer_pointer
  ///
  /// \since 2.1
  void GetSimulatorBufferPointer(void ** const ptr) const;

  /// \brief Get a string representing the internal state of the SimulatorModel
  /// object.
  ///
  /// This string is primarily meant for use as a debugging tool.  The string
  /// may be quite long.  It begins and ends with lines consisting only of \c
  /// ='s.
  ///
  /// \sa KIM_SimulatorModel_ToString,
  /// kim_simulator_model_module::kim_to_string
  ///
  /// \since 2.1
  std::string const & ToString() const;

  /// \brief Set the identity of the Log object associated with the
  /// SimulatorModel object.
  ///
  /// \param[in] logID String identifying the SimulatorModel object's Log
  ///            object.
  ///
  /// \sa KIM_SimulatorModel_SetLogID,
  /// kim_simulator_model_module::kim_set_log_id
  ///
  /// \since 2.1
  void SetLogID(std::string const & logID);

  /// \brief Push a new LogVerbosity onto the SimulatorModel object's Log
  /// object verbosity stack.
  ///
  /// \param[in] logVerbosity A LogVerbosity value.
  ///
  /// \sa KIM_SimulatorModel_PushLogVerbosity,
  /// kim_simulator_model_module::kim_push_log_verbosity
  ///
  /// \since 2.1
  void PushLogVerbosity(LogVerbosity const logVerbosity);

  /// \brief Pop a LogVerbosity from the SimulatorModel object's Log object
  /// verbosity stack.
  ///
  /// \sa KIM_SimulatorModel_PopLogVerbosity,
  /// kim_simulator_model_module::kim_pop_log_verbosity
  ///
  /// \since 2.1
  void PopLogVerbosity();

 private:
  // do not allow copy constructor or operator=
  SimulatorModel(SimulatorModel const &);
  void operator=(SimulatorModel const &);

  SimulatorModel();
  ~SimulatorModel();

  SimulatorModelImplementation * pimpl;
};  // class SimulatorModel
}  // namespace KIM

#endif  // KIM_SIMULATOR_MODEL_HPP_
