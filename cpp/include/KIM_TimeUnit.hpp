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
// Release: This file is part of the kim-api-2.0.2 package.
//


#ifndef KIM_TIME_UNIT_HPP_
#define KIM_TIME_UNIT_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// TimeUnit's supported by the %KIM API.
///
/// The enumeration constants are contained in the TIME_UNIT namespace.
///
/// \sa KIM_TimeUnit, kim_time_unit_module::kim_time_unit_type
///
/// \since 2.0
class TimeUnit
{
 public:
  /// \brief Integer identifying the specific TimeUnit represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_TimeUnit::timeUnitID,
  /// kim_time_unit_module::kim_time_unit_type::time_unit_id
  ///
  /// \since 2.0
  int timeUnitID;

  /// \brief Create an uninitialized TimeUnit object.
  ///
  /// \since 2.0
  TimeUnit();

  /// \brief Create a TimeUnit object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  TimeUnit(int const id);

  /// \brief Create a TimeUnit object corresponding to the provided string.  If
  /// the string does not match one of the values defined by the %KIM API, then
  /// an "unknown" object is generated.
  ///
  /// \sa KIM_TimeUnit_FromString, kim_time_unit_module::kim_from_string
  ///
  /// \since 2.0
  TimeUnit(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// TimeUnit's known to the %KIM API are found in the TIME_UNIT namespace.
  ///
  /// \sa KIM_TimeUnit_Known, kim_time_unit_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares TimeUnit objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_TimeUnit_Equal, kim_time_unit_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(TimeUnit const & rhs) const;

  /// \brief Compares TimeUnit objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_TimeUnit_NotEqual, kim_time_unit_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(TimeUnit const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the TimeUnit object.
  ///
  /// \note If the TimeUnit object does not correspond to a value defined by
  /// the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_TimeUnit_ToString, kim_time_unit_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class TimeUnit

/// \brief Contains the enumeration constants and the discovery routines for
/// the TimeUnit \ref extensible_enumeration "Extensible Enumeration".
namespace TIME_UNIT
{
/// \brief Indicates that a TimeUnit is not used.
///
/// \sa KIM_TIME_UNIT_unused, kim_time_unit_module::kim_time_unit_unused
///
/// \since 2.0
extern TimeUnit const unused;

/// \brief The standard femtosecond unit of time.
///
/// \sa KIM_TIME_UNIT_fs, kim_time_unit_module::kim_time_unit_fs
///
/// \since 2.0
extern TimeUnit const fs;

/// \brief The standard picosecond unit of time.
///
/// \sa KIM_TIME_UNIT_ps, kim_time_unit_module::kim_time_unit_ps
///
/// \since 2.0
extern TimeUnit const ps;

/// \brief The standard nanosecond unit of time.
///
/// \sa KIM_TIME_UNIT_ns, kim_time_unit_module::kim_time_unit_ns
///
/// \since 2.0
extern TimeUnit const ns;

/// \brief The standard second unit of time.
///
/// \sa KIM_TIME_UNIT_s, kim_time_unit_module::kim_time_unit_s
///
/// \since 2.0
extern TimeUnit const s;

/// \brief Get the number of standard TimeUnit's defined by the %KIM API.
///
/// \param[out] numberOfTimeUnits The number of standard TimeUnit's defined by
///             the %KIM API.
///
/// \sa KIM_TIME_UNIT_GetNumberOfTimeUnits,
/// kim_time_unit_module::kim_get_number_of_time_units
///
/// \since 2.0
void GetNumberOfTimeUnits(int * const numberOfTimeUnits);

/// \brief Get the identity of each defined standard TimeUnit.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             TimeUnitnit.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] timeUnit The TimeUnit object associated with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfTimeUnits`.
/// \return \c false otherwise.
///
/// \sa KIM_TIME_UNIT_GetTimeUnit, kim_time_unit_module::kim_get_time_unit
///
/// \since 2.0
int GetTimeUnit(int const index, TimeUnit * const timeUnit);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for TimeUnit objects
  /// so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(TimeUnit const & a, TimeUnit const & b) const
  {
    return a.timeUnitID < b.timeUnitID;
  }
};  // struct Comparator
}  // namespace TIME_UNIT
}  // namespace KIM

#endif  // KIM_TIME_UNIT_HPP_
