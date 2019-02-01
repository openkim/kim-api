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


#ifndef KIM_LOG_HPP_
#define KIM_LOG_HPP_

#include <sstream>
#include <string>

namespace KIM
{
// Forward declarations
class LogVerbosity;
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
  /// \param[inout] log Pointer to the Model object.
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
