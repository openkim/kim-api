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
// Release: This file is part of the kim-api-2.4.1 package.
//


#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_HPP_
#define KIM_MODEL_WRITE_PARAMETERIZED_MODEL_HPP_

#include <sstream>
#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
class ModelWriteParameterizedModelImplementation;


/// \brief Provides the interface to a %KIM API Model object for use by models
/// within their MODEL_ROUTINE_NAME::WriteParameterizedModel routine.
///
/// \sa KIM_ModelWriteParameterizedModel,
/// kim_model_write_parameterized_model_module::<!--
/// -->kim_model_write_parameterized_model_handle_type
///
/// \since 2.0
class ModelWriteParameterizedModel
{
 public:
  /// \brief Get the directory path where the parameterized model files should
  /// be written.
  ///
  /// \param[out] path Path string.
  ///
  /// \sa KIM_ModelWriteParameterizedModel_GetPath,
  /// kim_model_write_parameterized_model_module::kim_get_path
  ///
  /// \since 2.0
  void GetPath(std::string const ** const path) const;

  /// \brief Get the name of the new parameterized model.
  ///
  /// The model name is a valid C identifier and is available as a convenience.
  /// It is not required for the model to use this name in any way.
  ///
  /// \param[out] modelName The parameterized model name.
  ///
  /// \sa KIM_ModelWriteParameterizedModel_GetModelName,
  /// kim_model_write_parameterized_model_module::kim_get_model_name
  ///
  /// \since 2.0
  void GetModelName(std::string const ** const modelName) const;

  /// \brief Set the file name for the next parameter file.
  ///
  /// This routine must be called once for each parameter file.  The order of
  /// these calls is important and determines the order in which the parameter
  /// files will be listed in the automatically generated CMakeLists.txt file.
  ///
  /// \param[in] fileName File name (basename and extension, without path).
  ///
  /// \sa KIM_ModelWriteParameterizedModel_SetParameterFileName,
  /// kim_model_write_parameterized_model_module::kim_set_parameter_file_name
  ///
  /// \since 2.0
  void SetParameterFileName(std::string const & fileName) const;

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
  /// \sa KIM_ModelWriteParameterizedModel_GetModelBufferPointer,
  /// kim_model_write_parameterized_model_module::kim_get_model_buffer_pointer
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
  /// \sa KIM_ModelWriteParameterizedModel_LogEntry,
  /// kim_model_write_parameterized_model_module::kim_log_entry
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
  /// \sa KIM_ModelWriteParameterizedModel_ToString,
  /// kim_model_write_parameterized_model_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;

 private:
  // do not allow copy constructor or operator=
  ModelWriteParameterizedModel(ModelWriteParameterizedModel const &);
  void operator=(ModelWriteParameterizedModel const &);

  ModelWriteParameterizedModel();
  ~ModelWriteParameterizedModel();

  ModelWriteParameterizedModelImplementation * pimpl;
};  // class ModelWriteParameterizedModel
}  // namespace KIM

#endif  // KIM_MODEL_WRITE_PARAMETERIZED_MODEL_HPP_
