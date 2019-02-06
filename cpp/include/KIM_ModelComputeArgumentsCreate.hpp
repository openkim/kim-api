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
// Release: This file is part of the kim-api-v2-2.0.0 package.
//


#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_HPP_
#define KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_HPP_

#include <sstream>
#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class SupportStatus;
class ComputeArgumentName;
class ComputeCallbackName;
class ModelComputeArgumentsCreateImplementation;


/// \brief Provides the interface to a %KIM API ComputeArguments object for use
/// by models within their MODEL_ROUTINE_NAME::ComputeArgumentsCreate routine.
///
/// \sa KIM_ModelComputeArgumentsCreate,
/// kim_model_compute_arguments_create_module::<!--
/// -->kim_model_compute_arguments_create_handle_type
///
/// \since 2.0
class ModelComputeArgumentsCreate
{
 public:
  /// \brief Set the SupportStatus of a ComputeArgumentName.
  ///
  /// \param[in] computeArgumentName The ComputeArgumentName of interest.
  /// \param[in] supportStatus The corresponding SupportStatus.
  ///
  /// \return \c true if \c computeArgumentName is unknown.
  /// \return \c true if \c computeArgumentName is SUPPORT_STATUS::requiredByAPI
  ///         and \c supportStatus is not SUPPORT_STATUS::requiredByAPI.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus,
  /// kim_model_compute_arguments_create_module::kim_set_argument_support_status
  ///
  /// \since 2.0
  int SetArgumentSupportStatus(ComputeArgumentName const computeArgumentName,
                               SupportStatus const supportStatus);

  /// \brief Set the SupportStatus of a ComputeCallbackName.
  ///
  /// \param[in] computeCallbackName The ComputeCallbackName of interest.
  /// \param[in] supportStatus The corresponding SupportStatus.
  ///
  /// \return \c true if \c computeCallbackName is unknown.
  /// \return \c true if \c computeCallbackName is SUPPORT_STATUS::requiredByAPI
  ///         and \c supportStatus is not SUPPORT_STATUS::requiredByAPI.
  /// \return \c false otherwise.
  ///
  /// \sa KIM_ModelComputeArgumentsCreate_SetCallbackSupportStatus,
  /// kim_model_compute_arguments_create_module::kim_set_callback_support_status
  ///
  /// \since 2.0
  int SetCallbackSupportStatus(ComputeCallbackName const computeCallbackName,
                               SupportStatus const supportStatus);

  /// \brief Set the \ref cache_buffer_pointers "Model's buffer pointer"
  /// within the ComputeArguments object.
  ///
  /// The model buffer pointer may be used by the model to associate a memory
  /// buffer with the ComputeArguments object.
  ///
  /// \param[in] ptr The model buffer data pointer.
  ///
  /// \sa KIM_ModelComputeArguments_SetModelBufferPointer,
  /// kim_model_compute_arguments_create_module::kim_set_model_buffer_pointer
  ///
  /// \since 2.0
  void SetModelBufferPointer(void * const ptr);

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
  /// \sa KIM_ModelComputeArgumentsCreate_LogEntry,
  /// kim_model_compute_arguments_create_module::kim_log_entry
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

  /// \brief Get a string representing the internal state of the
  /// ComputeArguments object.
  ///
  /// This string is primarily meant for use as a debugging tool.  The string
  /// may be quite long.  It begins and ends with lines consisting only of \c
  /// ='s.
  ///
  /// \sa KIM_ModelComputeArgumentsCreate_ToString,
  /// kim_model_compute_arguments_create_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;

 private:
  // do not allow copy constructor or operator=
  ModelComputeArgumentsCreate(ModelComputeArgumentsCreate const &);
  void operator=(ModelComputeArgumentsCreate const &);

  ModelComputeArgumentsCreate();
  ~ModelComputeArgumentsCreate();

  ModelComputeArgumentsCreateImplementation * pimpl;
};  // class ModelComputeArgumentsCreate
}  // namespace KIM

#endif  // KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_HPP_
