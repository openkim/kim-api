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
// Release: This file is part of the kim-api-2.1.1 package.
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
/// Simulator Models (SMs) are a mechanism by which the %KIM API provides
/// support for models that are implemented natively in simulation codes
/// ("Simulators").  An SM consists of all the information and data required to
/// use the model from within its simulator.  This includes a specification
/// file in EDN format (see https://openkim.org/about-edn/) containing
/// instructions and settings that must be specified for the simulator to
/// define and use the model, and one or more parameter files.  The %KIM API
/// SimulatorModel model object provides a generic interface to access the
/// parameter files and the contents of the specification file.  Each simulator
/// may define its own set of "simulator fields" that contain
/// simulator-specific content for the model's setup.  A simulator field
/// consists of zero or more "lines".  Each line is a string containing
/// information that is meaningful to the simulator.  To allow the simulator to
/// specialize the field lines based on user input, the SimulatorModel
/// interface provides a template substitution mechanism.  Each simulator field
/// line may contain template tags of the form "@<key>@" and will be replaced
/// by an appropriate value provided by the SimulatorModel object or the
/// simulator.
///
/// The %KIM API defines the following set of standard template key-value
/// entries \anchor standard_template_entries:
///
/// * \c parameter-file-dir, with a value equal to the absolute path name of
///   the SimulatorModel's temporary parameter file directory.  This directory
///   is created when the SimulatorModel object is SimulatorModel::Create'd and
///   is removed when the object is SimulatorModel::Destroy'd.
/// * \c parameter-file-basename-# (# ranges from 1 to the SimulatorModel's
///   number of parameter files), with a value equal to the basename (file name
///   without path) of the corresponding parameter file. (Parameter file
///   ordering is defined by the order files are listed in the SMs
///   CMakeLists.txt file.)
/// * \c parameter-file-# (# ranges from 1 to the SimulatorModel's number of
///   parameter files), with a value equal to the full absolute file name (path
///   and base name) of the corresponding parameter file.
///
/// To facilitate backward-compatibility, the schema of the specification file
/// is explicitly versioned.  Each version of the schema is documented here.
///
/// ------
///
/// \anchor kim_api_sm_schema_version_1 <h3>`kim-api-sm-schema-version = 1`
/// (Since 2.1):</h3>
///
/// The specification file consists of a single EDN Map.  Each key-value pair
/// in the map has a key element-type of string.  The following list gives the
/// *required* key values and the element-type of their corresponding value:
///
/// * "kim-api-sm-schema-version", integer
/// * "model-name", string
/// * "simulator-name", string
/// * "simulator-version", string
/// * "supported-species", string
///
/// The "model-name" string value must be identical to the SimulatorModel's
/// name.  The "supported-species" string value is a space separated list of
/// labels that identify the species supported by the model.  The %KIM API does
/// not impose any additional constraints on these species labels.
///
/// All other key-value pairs in the EDN map are "simulator fields" which must
/// have a value with element-type string or vector.  If a vector of length
/// zero or greater is provided, each element of the vector must have
/// element-type string.
///
/// **Example of `kim-api-sm-schema-version = 1` file format**
/// \code{.edn}
/// {
///   "kim-api-sm-schema-version" 1
///   "model-name" "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu"
///   "simulator-name" "LAMMPS"
///   "simulator-version" "12 Dec 2018"
///   "supported-species" "Pb Au"
///   "units" "metal"
///   "model-defn" [ "pair_style lj/cut 10.4057000"
///                  "variable alchemy_mapping index @<atom-type-sym-list>@"
///                  "variable alchemy_curr_type loop 10000"
///                  "include @<parameter-file-1>@"
///                  "pair_modify mix arithmetic"
///                ]
/// }
/// \endcode
///
/// In this example the "units" and "model-defn" key-value pairs are "simulator
/// fields" whose format is defined by the LAMMPS kim_init command.  There are
/// also two examples of template tags: "@<atom-type-sym-list>@", which is
/// defined by the LAMMPS simulator, and "@<parameter-file-1>@", which is
/// defined by the SimulatorModel object.
///
/// ------
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
  /// \note The value of `simulatorModelName` is required to be a valid
  /// C-identifier.
  ///
  /// \return \c true if the %KIM API is unable to allocate a new log object.
  /// \return \c true if the requested simulator model's library cannot be
  ///         found, opened, is of the wrong type, or has some other problem.
  /// \return \c true if the simulator model's parameter files cannot be
  ///         written to scratch space.
  /// \return \c true if the simulator model's specification file is invalid
  ///         EDN, is written in an unsupported schema version, or does not
  ///         provide all required data.
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


  /// \brief Get the SimulatorModel's simulator name and version.
  ///
  /// \param[out] simulatorName Simulator name.
  /// \param[out] simulatorVersion Simulator version.
  ///
  /// \pre \c simulatorName and \c simulatorVersion may be \c NULL if the
  ///      cooresponding value is not needed.
  ///
  /// \sa KIM_SimulatorModel_GetSimulatorNameAndVersion,
  /// kim_simulator_model_module::kim_get_simulator_name_and_version
  ///
  /// \since 2.1
  void
  GetSimulatorNameAndVersion(std::string const ** const simulatorName,
                             std::string const ** const simulatorVersion) const;

  /// \brief Get the number of species supported by the SimulatorModel.
  ///
  /// \param[out] numberOfSupportedSpecies The number of species supported by
  ///             the Simulator Model.
  ///
  /// \sa KIM_SimulatorModel_GetNumberOfSupportedSpecies,
  /// kim_simulator_model_module::kim_get_number_of_supported_species
  ///
  /// \since 2.1
  void GetNumberOfSupportedSpecies(int * const numberOfSupportedSpecies) const;

  /// \brief Get a species name supported by the SimulatorModel.
  ///
  /// \param[in]  index Zero-based index for the species name.
  /// \param[out] speciesName The value of the species name of interest.
  ///
  /// \return \c true if \c index is invalid.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_SimulatorModel_GetSupportedSpecies,
  /// kim_simulator_model_module::kim_get_supported_species
  ///
  /// \since 2.1
  int GetSupportedSpecies(int const index,
                          std::string const ** const speciesName) const;

  /// \brief Open and initialize the template map for simulator field line
  /// substitutions.
  ///
  /// This routine clears the template map of all existing entries, adds the
  /// %KIM API \ref standard_template_entries "standard template entries", and
  /// opens the template map for addition of new entries.  This allows the
  /// simulator significant flexibilty. For instance, when only a partial set
  /// of the simulator's template map key-value entries are known (such as when
  /// the simulator's input file has been only partially processed).  In this
  /// case, the simulator can close the template map to obtain certain field
  /// lines that it knows to be complete.  Then it can open and initialize the
  /// template map and continue processing its input.
  ///
  /// \sa KIM_SimulatorModel_OpenAndInitializeTemplateMap,
  /// kim_simulator_model_module::kim_open_and_initialize_template_map
  ///
  /// \since 2.1
  void OpenAndInitializeTemplateMap();

  /// \brief Determine if the template map is open.
  ///
  /// \return \c true if the template map is open.
  /// \return \c false if the template map is closed.
  ///
  /// \sa KIM_SimulatorModel_TemplateMapIsOpen,
  /// kim_simulator_model_module::kim_template_map_is_open
  ///
  /// \since 2.1
  int TemplateMapIsOpen() const;

  /// \brief Add a new key-value entry to the template map.
  ///
  /// As part of the SimulatorModel::Create'ion of a SimulatorModel object its
  /// template map is opened and initialized by (internally) executing a call
  /// to OpenAndInitializeTemplateMap().  The AddTemplateMap() routine allows
  /// new key-value entries to be added to the open template map.
  ///
  /// Once the CloseTemplateMap() routine is executed, the map entries are used
  /// to perform template substitution on the simulator field line strings.  In
  /// each simulator field line and for each map key-value entry, when \c key,
  /// surrounded by the template tags "@<" and ">@", is found it will be
  /// replaced by \c value.  For example, if \c key is \c my-key, and \c value
  /// is \c the-result, then wherever the string `@<my-key>@` is found within a
  /// simulator field line it will be replaced by the string `the-result`.
  //
  /// \param[in] key The \c key value.  Must consist only of digits (0-9),
  ///            lower case letters (a-z), and dashes (-).
  /// \param[in] value The \c value value.  All valid strings are allowed.
  ///
  /// \return \c true if the template map has been closed by a call to
  ///         CloseTempateMap().
  /// \return \c true if \c key contains invalid characters.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_SimulatorModel_AddTemplateMap,
  /// kim_simulator_model_module::kim_add_template_map
  ///
  /// \since 2.1
  int AddTemplateMap(std::string const & key, std::string const & value);

  /// \brief Close the template map and perform template substitutions.
  ///
  /// Close the template map and use the map entries to perform
  /// search-and-replace substitutions on all simulator field lines.  The
  /// template map must be closed to access the simulator field lines via
  /// GetSimulatorFieldLine().
  ///
  /// \sa KIM_SimulatorModel_CloseTemplateMap,
  /// kim_simulator_model_module::kim_close_template_map
  ///
  /// \since 2.1
  void CloseTemplateMap();

  /// \brief Get the number of simulator fields provided by the SimulatorModel.
  ///
  ///
  /// \param[out] numberOfSimulatorFields The number of simulator fields
  ///             provided by the SimulatorModel.
  ///
  /// \sa KIM_SimulatorModel_GetNumberOfSimulatorFields,
  /// kim_simulator_model_module::kim_get_number_of_simulator_fields
  ///
  /// \since 2.1
  void GetNumberOfSimulatorFields(int * const numberOfSimulatorFields) const;

  /// \brief Get the metadata for the simulator field of interest.
  ///
  /// \param[in]  fieldIndex Zero-based index of the simulator field of
  ///             interest.
  /// \param[out] extent Number of lines in the simulator field of interest.
  /// \param[out] fieldName Name of the simulator field.
  ///
  /// \pre \c extent and \c fieldName may be \c NULL if the corresponding value
  ///      is not needed.
  ///
  /// \post \c extent and \c fieldName are unchanged if an error occurs.
  ///
  /// \return \c true if \c fieldIndex is invalid.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_SimulatorModel_GetSimulatorFieldMetadata,
  /// kim_simulator_model_module::kim_get_simulator_field_metadata
  ///
  /// \since 2.1
  int GetSimulatorFieldMetadata(int const fieldIndex,
                                int * const extent,
                                std::string const ** const fieldName) const;


  /// \brief Get a line for the simulator field of interest with all template
  /// substitutions performed (Requires the template map is closed).
  ///
  /// \param[in]  fieldIndex Zero-based index of the simulator field of
  ///             interest.
  /// \param[in]  lineIndex Zero-based index of the line of interest.
  /// \param[out] lineValue The value of the simulator field line.
  ///
  /// \post \c lineValue is unchanged if an error occurs.
  ///
  /// \return \c true if the template map is open.
  /// \return \c true if \c fieldIndex is invalid.
  /// \return \c true if \c lineIndex is invalid.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_SimulatorModel_GetSimulatorFieldLine,
  /// kim_simulator_model_module::kim_get_simulator_field_line
  ///
  /// \since 2.1
  int GetSimulatorFieldLine(int const fieldIndex,
                            int const lineIndex,
                            std::string const ** const lineValue) const;

  /// \brief Get absolute path name of the temporary directory where parameter
  /// files provided by the simulator model are written.
  ///
  /// \param[out] directoryName The absolute path name of the SimulatorModel's
  ///             temporary parameter file directory.
  ///
  /// \sa KIM_SimulatorModel_GetParameterFileDirectoryName,
  /// kim_simulator_model_module::kim_get_parameter_file_directory_name
  ///
  /// \since 2.1
  void
  GetParameterFileDirectoryName(std::string const ** const directoryName) const;

  /// \brief Get the SimulatorModel's specification file basename (file name
  /// without path).  The file is located in the SimulatorModel's parameter
  /// file directory.
  ///
  /// \param[out] specificationFileName The basename (file name without path)
  ///             of the specification file.
  ///
  /// \sa KIM_SimulatorModel_GetSpecificationFileName,
  /// kim_simulator_model_module::kim_get_specification_file_name
  ///
  /// \since 2.1
  void GetSpecificationFileName(
      std::string const ** const specificationFileName) const;

  /// \brief Get the number of parameter files provided by the SimulatorModel.
  ///
  /// \param[out] numberOfParameterFiles The number of parameter files.
  ///
  /// \sa KIM_SimulatorModel_GetNumberOfParameterFiles,
  /// kim_simulator_model_module::kim_get_number_of_parameter_files
  ///
  /// \since 2.1
  void GetNumberOfParameterFiles(int * const numberOfParameterFiles) const;

  /// \brief Get the basename (file name without path) of a particular
  /// parameter file.  The file is located in the SimulatorModel's parameter
  /// file directory.
  ///
  /// \param[in]  index Zero-based index for the parameter file of interest.
  /// \param[out] parameterFileName Basename (file name without path) of the
  ///             parameter file.
  ///
  /// \post \c parameterFileName is unchanged if an error occurs.
  ///
  /// \return \c true if \c index is invalid.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_SimulatorModel_GetParameterFileName,
  /// kim_simulator_model_module::kim_get_parameter_file_name
  ///
  /// \since 2.1
  int GetParameterFileName(int const index,
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
