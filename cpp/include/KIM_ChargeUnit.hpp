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
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_CHARGE_UNIT_HPP_
#define KIM_CHARGE_UNIT_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// ChargeUnit's supported by the %KIM API.
///
/// The enumeration constants are contained in the CHARGE_UNIT namespace.
///
/// \sa KIM_ChargeUnit, kim_charge_unit_module
///
/// \since 2.0
class ChargeUnit
{
 public:
  /// \brief Integer identifying the specific ChargeUnit represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_ChargeUnit::chargeUnitID,
  /// kim_charge_unit_module::kim_charge_unit_type::charge_unit_id
  ///
  /// \since 2.0
  int chargeUnitID;

  /// \brief Create an uninitialized ChargeUnit object.
  ///
  /// \since 2.0
  ChargeUnit();

  /// \brief Create a ChargeUnit object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  ChargeUnit(int const id);

  /// \brief Create a ChargeUnit object corresponding to the provided string.
  /// If the string does not match one of the values defined by the %KIM API,
  /// then an "unknown" object is generated.
  ///
  /// \sa KIM_ChargeUnit_FromString, kim_charge_unit_module::kim_from_string
  ///
  /// \since 2.0
  ChargeUnit(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// ChargeUnit's known to the %KIM API are found in the CHARGE_UNIT
  /// namespace.
  ///
  /// \sa KIM_ChargeUnit_Known, kim_charge_unit_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares ChargeUnit objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_ChargeUnit_Equal, kim_charge_unit_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(ChargeUnit const & rhs) const;

  /// \brief Compares ChargeUnit objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_ChargeUnit_NotEqual, kim_charge_unit_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(ChargeUnit const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the ChargeUnit object.
  ///
  /// \note If the ChargeUnit object does not correspond to a value defined by
  /// the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_ChargeUnit_ToString, kim_charge_unit_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class ChargeUnit

/// \brief Contains the enumeration constants and the discovery routines for
/// the ChargeUnit \ref extensible_enumeration "Extensible Enumeration".
namespace CHARGE_UNIT
{
/// \brief Indicates that a ChargeUnit is not used.
///
/// \sa KIM_CHARGE_UNIT_unused, kim_charge_unit_module::kim_charge_unit_unused
///
/// \since 2.0
extern ChargeUnit const unused;

/// \brief The standard Coulomb unit of charge.
///
/// \sa KIM_CHARGE_UNIT_C, kim_charge_unit_module::kim_charge_unit_c
///
/// \since 2.0
extern ChargeUnit const C;

/// \brief The standard electron unit of charge.
///
/// \sa KIM_CHARGE_UNIT_e, kim_charge_unit_module::kim_charge_unit_e
///
/// \since 2.0
extern ChargeUnit const e;

/// \brief The standard statcoulomb unit of charge.
///
/// \sa KIM_CHARGE_UNIT_statC, kim_charge_unit_module::kim_charge_unit_statc
///
/// \since 2.0
extern ChargeUnit const statC;


/// \brief Get the number of standard ChargeUnit's defined by the %KIM API.
///
/// \param[out] numberOfChargeUnits The number of standard ChargeUnit's defined
///             by the %KIM API.
///
/// \sa KIM_CHARGE_UNIT_GetNumberOfChargeUnits,
/// kim_charge_unit_module::kim_get_number_of_charge_units
///
/// \since 2.0
void GetNumberOfChargeUnits(int * const numberOfChargeUnits);

/// \brief Get the identity of each defined standard ChargeUnit.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             ChargeUnit.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] chargeUnit The ChargeUnit object associated with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfChargeUnits`.
/// \return \c false otherwise.
///
/// \sa KIM_CHARGE_UNIT_GetChargeUnit,
/// kim_charge_unit_module::kim_get_charge_unit
///
/// \since 2.0
int GetChargeUnit(int const index, ChargeUnit * const chargeUnit);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for ChargeUnit
  /// objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(ChargeUnit const & a, ChargeUnit const & b) const
  {
    return a.chargeUnitID < b.chargeUnitID;
  }
};  // struct Comparator
}  // namespace CHARGE_UNIT
}  // namespace KIM

#endif  // KIM_CHARGE_UNIT_HPP_
