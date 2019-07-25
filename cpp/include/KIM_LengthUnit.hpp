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


#ifndef KIM_LENGTH_UNIT_HPP_
#define KIM_LENGTH_UNIT_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// LengthUnit's supported by the %KIM API.
///
/// The enumeration constants are contained in the LENGTH_UNIT namespace.
///
/// \sa KIM_LengthUnit, kim_length_unit_module::kim_length_unit_type
///
/// \since 2.0
class LengthUnit
{
 public:
  /// \brief Integer identifying the specific LengthUnit represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_LengthUnit::lengthUnitID,
  /// kim_length_unit_module::kim_length_unit_type::length_unit_id
  ///
  /// \since 2.0
  int lengthUnitID;

  /// \brief Create an uninitialized LengthUnit object.
  ///
  /// \since 2.0
  LengthUnit();

  /// \brief Create a LengthUnit object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  LengthUnit(int const id);

  /// \brief Create a LengthUnit object corresponding to the provided string.
  /// If the string does not match one of the values defined by the %KIM API,
  /// then an "unknown" object is generated.
  ///
  /// \sa KIM_LengthUnit_FromString, kim_length_unit_module::kim_from_string
  ///
  /// \since 2.0
  LengthUnit(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// LengthUnit's known to the %KIM API are found in the LENGTH_UNIT
  /// namespace.
  ///
  /// \sa KIM_LengthUnit_Known, kim_length_unit_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares LengthUnit objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_LengthUnit_Equal, kim_length_unit_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(LengthUnit const & rhs) const;

  /// \brief Compares LengthUnit objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_LengthUnit_NotEqual, kim_length_unit_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(LengthUnit const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the LengthUnit object.
  ///
  /// \note If the LengthUnit object does not correspond to a value defined by
  /// the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_LengthUnit_ToString, kim_length_unit_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class LengthUnit

/// \brief Contains the enumeration constants and the discovery routines for
/// the LengthUnit \ref extensible_enumeration "Extensible Enumeration".
namespace LENGTH_UNIT
{
/// \brief Indicates that a LengthUnit is not used.
///
/// \sa KIM_LENGTH_UNIT_unused, kim_length_unit_module::kim_length_unit_unused
///
/// \since 2.0
extern LengthUnit const unused;

/// \brief The standard angstrom unit of length.
///
/// \sa KIM_LENGTH_UNIT_A, kim_length_unit_module::kim_length_unit_a
///
/// \since 2.0
extern LengthUnit const A;

/// \brief The standard Bohr unit of length.
///
/// \sa KIM_LENGTH_UNIT_Bohr, kim_length_unit_module::kim_length_unit_bohr
///
/// \since 2.0
extern LengthUnit const Bohr;

/// \brief The standard centimeter unit of length.
///
/// \sa KIM_LENGTH_UNIT_cm, kim_length_unit_module::kim_length_unit_cm
///
/// \since 2.0
extern LengthUnit const cm;

/// \brief The standard meter unit of length.
///
/// \sa KIM_LENGTH_UNIT_m, kim_length_unit_module::kim_length_unit_m
///
/// \since 2.0
extern LengthUnit const m;

/// \brief The standard nanometer unit of length.
///
/// \sa KIM_LENGTH_UNIT_nm, kim_length_unit_module::kim_length_unit_nm
///
/// \since 2.0
extern LengthUnit const nm;

/// \brief Get the number of standard LengthUnit's defined by the %KIM API.
///
/// \param[out] numberOfLengthUnits The number of standard LengthUnit's defined
///             by the %KIM API.
///
/// \sa KIM_LENGTH_UNIT_GetNumberOfLengthUnits,
/// kim_length_unit_module::kim_get_number_of_length_units
///
/// \since 2.0
void GetNumberOfLengthUnits(int * const numberOfLengthUnits);

/// \brief Get the identity of each defined standard LengthUnit.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             LengthUnit.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] lengthUnit The LengthUnit object associated with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfLengthUnits`.
/// \return \c false otherwise.
///
/// \sa KIM_LENGTH_UNIT_GetLengthUnit,
/// kim_length_unit_module::kim_get_length_unit
///
/// \since 2.0
int GetLengthUnit(int const index, LengthUnit * const lengthUnit);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for LengthUnit
  /// objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(LengthUnit const & a, LengthUnit const & b) const
  {
    return a.lengthUnitID < b.lengthUnitID;
  }
};  // struct Comparator
}  // namespace LENGTH_UNIT
}  // namespace KIM

#endif  // KIM_LENGTH_UNIT_HPP_
