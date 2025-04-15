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


#ifndef KIM_LOG_HPP_
#define KIM_LOG_HPP_

#include <sstream>
#include <string>

#ifndef KIM_FUNCTION_TYPES_HPP_
#include "KIM_FunctionTypes.hpp"  // IWYU pragma: export
#endif

namespace KIM
{
// Forward declarations
class LogVerbosity;
class LanguageName;
class LogImplementation;

/// \brief Provides the logging interface for the %KIM API.
///
/// \sa KIM_Log, kim_log_module::kim_log_handle_type
///
/// \since 2.0
class Log
{
 public:
  /// \brief Create a new %KIM API Log object.
  ///
  /// \param[out] log Pointer to the newly created Log object.
  ///
  /// \return \c false
  ///
  /// \sa KIM_Log_Create, kim_log_module::kim_log_create
  ///
  /// \since 2.0
  static int Create(Log ** const log);

  /// \brief Destroy a previously Log::Create'd object.
  ///
  /// \param[inout] log Pointer to the Log object.
  ///
  /// \pre \c log points to a previously created %KIM API Log object.
  ///
  /// \post `log == NULL`.
  ///
  /// \sa KIM_Log_Destroy, kim_log_module::kim_log_destroy
  ///
  /// \since 2.0
  static void Destroy(Log ** const log);

  /// \brief Push a new default LogVerbosity onto the %KIM API global default
  /// verbosity stack.
  ///
  /// The default LogVerbosity is used when creating new Log objects.
  ///
  /// \param[in] logVerbosity A LogVerbosity value.
  ///
  /// \sa KIM_Log_PushDefaultVerbosity,
  /// kim_log_module::kim_push_default_verbosity
  ///
  /// \since 2.0
  static void PushDefaultVerbosity(LogVerbosity const logVerbosity);

  /// \brief Pop a LogVerbosity from the %KIM API global default verbosity
  /// stack.
  ///
  /// \sa KIM_Log_PopDefaultVerbosity,
  /// kim_log_module::kim_pop_default_verbosity
  ///
  /// \since 2.0
  static void PopDefaultVerbosity();

  /// \brief Push a new default log PrintFunction onto the %KIM API global
  /// default log PrintFunction stack.
  ///
  /// The default log PrintFunction is used when creating new Log objects.
  ///
  /// \param[in] languageName The LanguageName of the function.
  /// \param[in] fptr The function pointer.
  ///
  /// \sa KIM_Log_PushDefaultPrintFunction,
  /// kim_log_module::kim_push_default_print_function
  ///
  /// \since 2.2
  static void PushDefaultPrintFunction(LanguageName const languageName,
                                       Function * const fptr);

  /// \brief Pop a log PrintFunction from the %KIM API global default log
  /// PrintFunction stack.
  ///
  /// \sa KIM_Log_PopDefaultPrintFunction,
  /// kim_log_module::kim_pop_default_print_function
  ///
  /// \since 2.2
  static void PopDefaultPrintFunction();

  /// \brief Get the identity of the Log object.
  ///
  /// \sa KIM_Log_GetID, kim_log_module::kim_get_id
  ///
  /// \since 2.0
  std::string const & GetID() const;

  /// \brief Set the identity of the Log object.
  ///
  /// \param[in] id String identifying the Log object.
  ///
  /// \sa KIM_Log_SetID, kim_log_module::kim_set_id
  ///
  /// \since 2.0
  void SetID(std::string const & id);

  /// \brief Push a new LogVerbosity onto the Log object's verbosity stack.
  ///
  /// \param[in] logVerbosity A LogVerbosity value.
  ///
  /// \sa KIM_Log_PushVerbosity, kim_log_module::kim_push_verbosity
  ///
  /// \since 2.0
  void PushVerbosity(LogVerbosity const logVerbosity);

  /// \brief Pop a LogVerbosity from the Log object's verbosity stack.
  ///
  /// \sa KIM_Log_PopVerbosity, kim_log_module::kim_pop_verbosity
  ///
  /// \since 2.0
  void PopVerbosity();

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
  /// \sa KIM_Log_LogEntry, kim_log_module::kim_log_entry
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

 private:
  // do not allow copy constructor or operator=
  Log(Log const &);
  void operator=(Log const &);

  Log();
  ~Log();

  LogImplementation * pimpl;
};  // class Log
}  // namespace KIM

#endif  // KIM_LOG_HPP_
