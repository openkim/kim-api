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


#ifndef KIM_LOG_VERBOSITY_HPP_
#define KIM_LOG_VERBOSITY_HPP_

#include <string>

#ifndef KIM_LOG_DEFINES_INC_
#include "KIM_LOG_DEFINES.inc"
#endif

namespace KIM
{
/// \brief This class is an \ref extensible_enumeration
/// "Extensible Enumeration" for the LogVerbosity's supported by the %KIM API.
///
/// The enumeration constants are contained in the LOG_VERBOSITY namespace.
///
/// \sa KIM_LogVerbosity
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
  /// \sa KIM_LogVerbosity::logVerbosityID
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
  /// \sa KIM_LogVerbosity_FromString
  ///
  /// \since 2.0
  LogVerbosity(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// LogVerbosity's known to the %KIM API are found in the LOG_VERBOSITY
  /// namespace.
  ///
  /// \sa KIM_LogVerbosity_Known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares LogVerbosity objects for less-than.
  ///
  /// \sa KIM_LogVerbosity_LessThan
  ///
  /// \since 2.0
  bool operator<(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for greater-than.
  ///
  /// \sa KIM_LogVerbosity_GreaterThan
  ///
  /// \since 2.0
  bool operator>(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for less-than-equal.
  ///
  /// \sa KIM_LogVerbosity_LessThanEqual
  ///
  /// \since 2.0
  bool operator<=(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for greater-than-equal.
  ///
  /// \sa KIM_LogVerbosity_GreaterThanEqual
  ///
  /// \since 2.0
  bool operator>=(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_LogVerbosity_Equal
  ///
  /// \since 2.0
  bool operator==(LogVerbosity const & rhs) const;

  /// \brief Compares LogVerbosity objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_LogVerbosity_NotEqual
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
  /// \sa KIM_LogVerbosity_ToString
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class LogVerbosity

/// Contains the enumeration constants and the discovery routines for the
/// LogVerbosity \ref extensible_enumeration "Extensible Enumeration".
namespace LOG_VERBOSITY
{
/// \brief The standard \c silent verbosity.
///
/// \todo Add more detailed description of verbosity.
///
/// \sa KIM_LOG_VERBOSITY_silent
///
/// \since 2.0
extern LogVerbosity const silent;

/// \brief The standard \c fatal verbosity.
///
/// \todo Add more detailed description of verbosity.
///
/// \sa KIM_LOG_VERBOSITY_fatal
///
/// \since 2.0
extern LogVerbosity const fatal;

/// \brief The standard \c error verbosity.
///
/// \todo Add more detailed description of verbosity.
///
/// \sa KIM_LOG_VERBOSITY_error
///
/// \since 2.0
extern LogVerbosity const error;

/// \brief The standard \c warning verbosity.
///
/// \todo Add more detailed description of verbosity.
///
/// \sa KIM_LOG_VERBOSITY_warning
///
/// \since 2.0
extern LogVerbosity const warning;

/// \brief The standard \c information verbosity.
///
/// \todo Add more detailed description of verbosity.
///
/// \sa KIM_LOG_VERBOSITY_information
///
/// \since 2.0
extern LogVerbosity const information;

/// \brief The standard \c debug verbosity.
///
/// \todo Add more detailed description of verbosity.
///
/// \sa KIM_LOG_VERBOSITY_debug
///
/// \since 2.0
extern LogVerbosity const debug;


/// \brief Get the number of standard LogVerbosity's defined by the %KIM
/// API.
///
/// \param[out] numberOfLogVerbosities The number of standard LogVerbosity's
///             defined by the %KIM API.
///
/// \sa KIM_LOG_VERBOSITY_GetNumberOfLogVerbosities
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
/// \sa KIM_LOG_VERBOSITY_GetLogVerbosity
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
