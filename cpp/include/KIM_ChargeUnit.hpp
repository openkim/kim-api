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
/// This class is an \ref extensible_enumeration "Extensible Enumeration" for
/// the charge units supported by the %KIM API.  The enumeration constants are
/// contained in the CHARGE_UNIT namespace.
///
/// \since 2.0
class ChargeUnit
{
 public:
  /// Integer identifying the specific charge unit represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \since 2.0
  int chargeUnitID;

  /// Create the value ChargeUnit::unused.
  ///
  /// \since 2.0
  ChargeUnit();

  /// Create a ChargeUnit object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  ChargeUnit(int const id);

  /// Create a ChargeUnit object corresponding to the provided string.  If the
  /// string does not match one of the values defined by the %KIM API, then an
  /// "unknown" object is generated.
  ///
  /// \since 2.0
  ChargeUnit(std::string const & str);

  /// Compares ChargeUnit objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \since 2.0
  bool operator==(ChargeUnit const & rhs) const;

  /// Compares ChargeUnit objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \since 2.0
  bool operator!=(ChargeUnit const & rhs) const;

  /// Converts the object to a string.
  ///
  /// \return A string object representing the ChargeUnit object.
  ///
  /// \note If the ChargeUnit object does not correspond to a value defined by
  /// the %KIM API, then the string "unknown" is returned.
  ///
  /// \since 2.0
  std::string const & String() const;
};  // class ChargeUnit

/// Contains the enumeration constants and the discovery routines for the
/// ChargeUnit Extensible Enumeration.
namespace CHARGE_UNIT
{
/// Indicates that a charge unit is not used.
///
/// \since 2.0
extern ChargeUnit const unused;

/// The standard Coulomb unit of charge.
///
/// \since 2.0
extern ChargeUnit const C;

/// The standard electron unit of charge.
///
/// \since 2.0
extern ChargeUnit const e;

/// The standard statcoulomb unit of charge.
///
/// \since 2.0
extern ChargeUnit const statC;


/// Get the number of standard charge units defined by the %KIM API.
///
/// \param[out] numberOfChargeUnits The number of standard charge units
///             defined by the %KIM API.
///
/// \since 2.0
void GetNumberOfChargeUnits(int * const numberOfChargeUnits);

/// Get the identity of each defined standard charge unit.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             charge unit.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] chargeUnit The ChargeUnit object associated with \c index.
///
/// \return \c true if \p chargeUnit is "unknown".
/// \return \c false otherwise.
///
/// \since 2.0
int GetChargeUnit(int const index, ChargeUnit * const chargeUnit);

/// Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// Provides an (logically unmeaningful) ordering for ChargeUnit objects so
  /// that they can be stored in a std::map.
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
