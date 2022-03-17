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


#ifndef KIM_LOG_VERBOSITY_HPP_
#define KIM_LOG_VERBOSITY_HPP_

#include <string>


namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// LogVerbosity's supported by the %KIM API.
///
/// The enumeration constants are contained in the LOG_VERBOSITY namespace.
///
/// \sa KIM_LogVerbosity,
/// kim_log_verbosity_module::kim_log_verbosity_type
///
/// \since 2.0
class LogVerbosity
{
 public:
  /// \brief Integer identifying the specific LogVerbosity represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_LogVerbosity::logVerbosityID,
  /// kim_log_verbosity_module::kim_log_verbosity_type::log_verbosity_id
  ///
  /// \since 2.0
  int logVerbosityID;

  /// \brief Create an uninitialized LogVerbosity object.
  ///
  /// \since 2.0
  LogVerbosity();

  /// \brief Create a LogVerbosity object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  LogVerbosity(int const id);

  /// \brief Create a LogVerbosity object corresponding to the provided string.
  /// If the string does not match one of the values defined by the %KIM API,
  /// then an "unknown" object is generated.
  ///
  /// \sa KIM_LogVerbosity_FromString,
  /// kim_log_verbosity_module::kim_from_string
  ///
  /// \since 2.0
  LogVerbosity(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// LogVerbosity's known to the %KIM API are found in the LOG_VERBOSITY
  /// namespace.
  ///
  /// \sa KIM_LogVerbosity_Known, kim_log_verbosity_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares LogVerbosity objects for less-than.
  ///
  /// \sa KIM_LogVerbosity_LessThan, kim_log_verbosity_module::operator(.lt.)
  ///
  /// \since 2.0
  bool operator<(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for greater-than.
  ///
  /// \sa KIM_LogVerbosity_GreaterThan,
  /// kim_log_verbosity_module::operator(.gt.)
  ///
  /// \since 2.0
  bool operator>(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for less-than-equal.
  ///
  /// \sa KIM_LogVerbosity_LessThanEqual,
  /// kim_log_verbosity_module::operator(.le.)
  ///
  /// \since 2.0
  bool operator<=(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for greater-than-equal.
  ///
  /// \sa KIM_LogVerbosity_GreaterThanEqual,
  /// kim_log_verbosity_module::operator(.ge.)
  ///
  /// \since 2.0
  bool operator>=(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_LogVerbosity_Equal, kim_log_verbosity_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_LogVerbosity_NotEqual, kim_log_verbosity_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(LogVerbosity const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the LogVerbosity object.
  ///
  /// \note If the LogVerbosity object does not correspond to a value defined
  /// by the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_LogVerbosity_ToString, kim_log_verbosity_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class LogVerbosity

/// \brief Contains the enumeration constants and the discovery routines for
/// the LogVerbosity \ref extensible_enumeration "Extensible Enumeration".
namespace LOG_VERBOSITY
{
/// \brief The standard \c silent verbosity.
///
/// The \c silent verbosity level should be used when absolutely no messages
/// should be written into the log file.
///
/// \sa KIM_LOG_VERBOSITY_silent,
/// kim_log_verbosity_module::kim_log_verbosity_silent
///
/// \since 2.0
extern LogVerbosity const silent;

/// \brief The standard \c fatal verbosity.
///
/// The \c fatal verbosity level should be used when the execution of the
/// program cannot continue in any way and \c exit will be called.  Generic
/// examples of a \c fatal condition include: failure to allocate requested
/// memory, etc.
///
/// \sa KIM_LOG_VERBOSITY_fatal,
/// kim_log_verbosity_module::kim_log_verbosity_fatal
///
/// \since 2.0
extern LogVerbosity const fatal;

/// \brief The standard \c error verbosity.
///
/// The \c error verbosity level should be used when the execution of some task
/// could not be completed.  Generic examples of an \c error condition include:
/// an email could not be sent, a page could not be rendered, some data could
/// not be stored to a database, etc.  Something has definitively gone wrong.
///
/// \sa KIM_LOG_VERBOSITY_error,
/// kim_log_verbosity_module::kim_log_verbosity_error
///
/// \since 2.0
extern LogVerbosity const error;

/// \brief The standard \c warning verbosity.
///
/// The \c warning verbosity level should be used when something unexpected
/// happened, but that execution can continue, perhaps in a degraded mode.
/// Generic examples of a \c warning condition include: a configuration file
/// was missing but defaults were used, a price was calculated as negative, so
/// it was clamped to zero, etc.  Something is not right, but it has not gone
/// properly wrong yet - warnings are often a sign that there will be an error
/// very soon.
///
/// \sa KIM_LOG_VERBOSITY_warning,
/// kim_log_verbosity_module::kim_log_verbosity_warning
///
/// \since 2.0
extern LogVerbosity const warning;

/// \brief The standard \c information verbosity.
///
/// The \c information verbosity level should be used when something normal but
/// significant happened.  Generic examples of an \c information condition
/// include: the system started, the system stopped, the daily inventory update
/// job ran, etc.  There should not be a continual torrent of these, otherwise
/// there is just too much to read.
///
/// \sa KIM_LOG_VERBOSITY_information,
/// kim_log_verbosity_module::kim_log_verbosity_information
///
/// \since 2.0
extern LogVerbosity const information;

/// \brief The standard \c debug verbosity.
///
/// The \c debug verbosity level should be used when something normal and
/// insignificant happened.  Generic examples of a \c debug condition include:
/// a new user came to the site, a page was rendered, an order was taken, a
/// price was updated.  This is the stuff excluded from \c info because there
/// would be too much of it.
///
/// \sa KIM_LOG_VERBOSITY_debug,
/// kim_log_verbosity_module::kim_log_verbosity_debug
///
/// \since 2.0
extern LogVerbosity const debug;


/// \brief Get the number of standard LogVerbosity's defined by the %KIM
/// API.
///
/// \param[out] numberOfLogVerbosities The number of standard LogVerbosity's
///             defined by the %KIM API.
///
/// \sa KIM_LOG_VERBOSITY_GetNumberOfLogVerbosities,
/// kim_log_verbosity_module::kim_get_number_of_log_verbosities
///
/// \since 2.0
void GetNumberOfLogVerbosities(int * const numberOfLogVerbosities);

/// \brief Get the identity of each defined standard LogVerbosity.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             LogVerbosity.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] logVerbosity The LogVerbosity object associated with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfLogVerbosities`.
/// \return \c false otherwise.
///
/// \sa KIM_LOG_VERBOSITY_GetLogVerbosity,
/// kim_log_verbosity_module::kim_get_log_verbosity
///
/// \since 2.0
int GetLogVerbosity(int const index, LogVerbosity * const logVerbosity);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for LogVerbosity
  /// objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(LogVerbosity const & a, LogVerbosity const & b) const
  {
    return a.logVerbosityID < b.logVerbosityID;
  }
};  // struct Comparator
}  // namespace LOG_VERBOSITY
}  // namespace KIM

#endif  // KIM_LOG_VERBOSITY_HPP_
