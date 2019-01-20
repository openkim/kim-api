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


#ifndef KIM_CHARGE_UNIT_HPP_
#define KIM_CHARGE_UNIT_HPP_

#include <string>

namespace KIM
{
/// \brief This class is an \ref extensible_enumeration
/// "Extensible Enumeration" for the ChargeUnit's supported by the %KIM API.
///
/// The enumeration constants are contained in the CHARGE_UNIT namespace.
///
/// \sa KIM_ChargeUnit
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
  /// \sa KIM_ChargeUnit::chargeUnitID
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
  /// \sa KIM_ChargeUnit_FromString
  ///
  /// \since 2.0
  ChargeUnit(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// ChargeUnit's known to the %KIM API are found in the CHARGE_UNIT
  /// namespace.
  ///
  /// \sa KIM_ChargeUnit_Known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares ChargeUnit objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_ChargeUnit_Equal
  ///
  /// \since 2.0
  bool operator==(ChargeUnit const & rhs) const;

  /// \brief Compares ChargeUnit objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_ChargeUnit_NotEqual
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
  /// \sa KIM_ChargeUnit_ToString
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class ChargeUnit

/// Contains the enumeration constants and the discovery routines for the
/// ChargeUnit \ref extensible_enumeration "Extensible Enumeration".
namespace CHARGE_UNIT
{
/// \brief Indicates that a ChargeUnit is not used.
///
/// \sa KIM_CHARGE_UNIT_unused
///
/// \since 2.0
extern ChargeUnit const unused;

/// \brief The standard Coulomb unit of charge.
///
/// \sa KIM_CHARGE_UNIT_C
///
/// \since 2.0
extern ChargeUnit const C;

/// \brief The standard electron unit of charge.
///
/// \sa KIM_CHARGE_UNIT_e
///
/// \since 2.0
extern ChargeUnit const e;

/// \brief The standard statcoulomb unit of charge.
///
/// \sa KIM_CHARGE_UNIT_statC
///
/// \since 2.0
extern ChargeUnit const statC;


/// \brief Get the number of standard ChargeUnit's defined by the %KIM API.
///
/// \param[out] numberOfChargeUnits The number of standard ChargeUnit's defined
///             by the %KIM API.
///
/// \sa KIM_CHARGE_UNIT_GetNumberOfChargeUnits
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
/// \sa KIM_CHARGE_UNIT_GetChargeUnit
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
