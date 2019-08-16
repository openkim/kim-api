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
// Release: This file is part of the kim-api-2.1.3 package.
//


#ifndef KIM_MODEL_EXTENSION_HPP_
#define KIM_MODEL_EXTENSION_HPP_

#include <sstream>
#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class Model;
class ModelCompute;
class ModelCreate;
class ModelDestroy;
class ModelDriverCreate;
class ModelRefresh;
class ModelWriteParameterizedModel;
class ComputeArguments;
class ModelComputeArguments;
class ModelComputeArgumentsCreate;
class ModelComputeArgumentsDestroy;
class ModelExtensionImplementation;


/// \brief Provides the interface to a %KIM API Model object for use by models
/// within their MODEL_ROUTINE_NAME::Extension routine.
///
/// \sa KIM_ModelExtension,
/// kim_model_extension_module::kim_model_extension_handle_type
///
/// \since 2.0
class ModelExtension
{
 public:
  /// \brief Get the extension identification string.
  ///
  /// \param[out] extensionID The extension identification string.
  ///
  /// \sa KIM_ModelExtension_GetExtensionID,
  /// kim_model_extension_module::kim_get_extension_id
  ///
  /// \since 2.0
  void GetExtensionID(std::string const ** const extensionID) const;

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// Model interface.
  ///
  /// \sa KIM_ModelExtension_ToModel, kim_model_extension_module::kim_to_model
  ///
  /// \since 2.0
  KIM::Model * Model();

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// ModelCompute interface.
  ///
  /// \sa KIM_ModelExtension_ToModelCompute,
  /// kim_model_extension_module::kim_to_model_compute
  ///
  /// \since 2.0
  KIM::ModelCompute * ModelCompute();

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// ModelCreate interface.
  ///
  /// \sa KIM_ModelExtension_ToModelCreate,
  /// kim_model_extension_module::kim_to_model_create
  ///
  /// \since 2.0
  KIM::ModelCreate * ModelCreate();

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// ModelDestroy interface.
  ///
  /// \sa KIM_ModelExtension_ToModelDestroy,
  /// kim_model_extension_module::kim_to_model_destroy
  ///
  /// \since 2.0
  KIM::ModelDestroy * ModelDestroy();

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// ModelDriverCreate interface.
  ///
  /// \sa KIM_ModelExtension_ToModelDriverCreate,
  /// kim_model_extension_module::kim_to_model_driver_create
  ///
  /// \since 2.0
  KIM::ModelDriverCreate * ModelDriverCreate();

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// ModelRefresh interface.
  ///
  /// \sa KIM_ModelExtension_ToModelRefresh,
  /// kim_model_extension_module::kim_to_model_refresh
  ///
  /// \since 2.0
  KIM::ModelRefresh * ModelRefresh();

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// ModelWriteParameterizedModel interface.
  ///
  /// \sa KIM_ModelExtension_ToModelWriteParameterizedModel,
  /// kim_model_extension_module::kim_to_model_write_parameterized_model
  ///
  /// \since 2.0
  KIM::ModelWriteParameterizedModel * ModelWriteParameterizedModel();

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// ModelComputeArguments interface.
  ///
  /// \sa KIM_ModelExtension_ToModelComputeArguments,
  /// kim_model_extension_module::kim_to_model_compute_arguments
  ///
  /// \since 2.0
  KIM::ModelComputeArguments *
  ModelComputeArguments(ComputeArguments * const computeArguments) const;

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// ModelComputeArgumentsCreate interface.
  ///
  /// \sa KIM_ModelExtension_ToModelComputeArgumentsCreate,
  /// kim_model_extension_module::kim_to_model_compute_arguments_create
  ///
  /// \since 2.0
  KIM::ModelComputeArgumentsCreate *
  ModelComputeArgumentsCreate(ComputeArguments * const computeArguments) const;

  /// \brief Convert the ModelExtension interface to the Model object to a
  /// ModelComputeArgumentsDestroy interface.
  ///
  /// \sa KIM_ModelExtension_ToModelComputeArgumentsDestroy,
  /// kim_model_extension_module::kim_to_model_compute_arguments_destroy
  ///
  /// \since 2.0
  KIM::ModelComputeArgumentsDestroy *
  ModelComputeArgumentsDestroy(ComputeArguments * const computeArguments) const;

  /// \brief Get the \ref cache_buffer_pointers "Model's buffer pointer"
  /// within the Model object.
  ///
  /// The model buffer pointer may be used by the Model to associate
  /// a memory buffer with the Model object.
  ///
  /// \param[out] ptr The model buffer data pointer.
  ///
  /// \note `ptr == NULL` if the model has not previously called
  ///       ModelCreate::SetModelBufferPointer or
  ///       ModelDriverCreate::SetModelBufferPointer.
  ///
  /// \sa KIM_ModelExtension_GetModelBufferPointer,
  /// kim_model_extension_module::kim_get_model_buffer_pointer
  ///
  /// \since 2.0
  void GetModelBufferPointer(void ** const ptr) const;

  /// \brief Write a log entry into the log file.
  ///
  /// This results in a no-op if \c logVerbosity is LOG_VERBOSITY::silent or if
  /// \c logVerbosity is greater-than the Log object's top LogVerbosity on its
  /// stack.
  ///
  /// \param[in] logVerbosity The LogVerbosity level for the entry.
  /// \param[in] message The body text of the log entry.
  /// \param[in] lineNumber The source code file line number.
  /// \param[in] fileName The source code file name.
  ///
  /// \sa KIM_ModelExtension_LogEntry,
  /// kim_model_extension_module::kim_log_entry
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
  /// \sa KIM_ModelExtension_ToString,
  /// kim_model_extension_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;

 private:
  // do not allow copy constructor or operator=
  ModelExtension(ModelExtension const &);
  void operator=(ModelExtension const &);

  ModelExtension();
  ~ModelExtension();

  ModelExtensionImplementation * pimpl;
};  // class ModelExtension
}  // namespace KIM

#endif  // KIM_MODEL_EXTENSION_HPP_
