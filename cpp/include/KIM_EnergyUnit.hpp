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
// Copyright (c) 2016--2020, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.2.1 package.
//


#ifndef KIM_ENERGY_UNIT_HPP_
#define KIM_ENERGY_UNIT_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// EnergyUnit's supported by the %KIM API.
///
/// The enumeration constants are contained in the ENERGY_UNIT namespace.
///
/// \sa KIM_EnergyUnit, kim_energy_unit_module::kim_energy_unit_type
///
/// \since 2.0
class EnergyUnit
{
 public:
  /// \brief Integer identifying the specific EnergyUnit represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_EnergyUnit::energyUnitID,
  /// kim_energy_unit_module::kim_energy_unit_type::energy_unit_id
  ///
  /// \since 2.0
  int energyUnitID;

  /// \brief Create an uninitialized EnergyUnit object.
  ///
  /// \since 2.0
  EnergyUnit();

  /// \brief Create a EnergyUnit object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  EnergyUnit(int const id);

  /// \brief Create an EnergyUnit object corresponding to the provided string.
  /// If the string does not match one of the values defined by the %KIM API,
  /// then an "unknown" object is generated.
  ///
  /// \sa KIM_EnergyUnit_FromString,
  /// kim_energy_unit_module::kim_from_string
  ///
  /// \since 2.0
  EnergyUnit(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// EnergyUnit's known to the %KIM API are found in the ENERGY_UNIT
  /// namespace.
  ///
  /// \sa KIM_EnergyUnit_Known,
  /// kim_energy_unit_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares EnergyUnit objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_EnergyUnit_Equal,
  /// kim_energy_unit_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(EnergyUnit const & rhs) const;

  /// \brief Compares EnergyUnit objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_EnergyUnit_NotEqual,
  /// kim_energy_unit_module::operator(.ne.)
  ///
  /// \since 2.0
  bool operator!=(EnergyUnit const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the EnergyUnit object.
  ///
  /// \note If the EnergyUnit object does not correspond to a value defined by
  /// the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_EnergyUnit_ToString,
  /// kim_energy_unit_module::kim_to_string
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class EnergyUnit

/// \brief Contains the enumeration constants and the discovery routines for
/// the EnergyUnit \ref extensible_enumeration "Extensible Enumeration".
namespace ENERGY_UNIT
{
/// \brief Indicates that a EnergyUnit is not used.
///
/// \sa KIM_ENERGY_UNIT_unused,
/// kim_energy_unit_module::kim_energy_unit_unused
///
/// \since 2.0
extern EnergyUnit const unused;

/// \brief The standard `amu*A`\f$^2\f$`/ps`\f$^2\f$ unit of energy.
///
/// \sa KIM_ENERGY_UNIT_amu_A2_per_ps2,
/// kim_energy_unit_module::kim_energy_unit_amu_a2_per_ps2
///
/// \since 2.0
extern EnergyUnit const amu_A2_per_ps2;

/// \brief The standard erg unit of energy.
///
/// \sa KIM_ENERGY_UNIT_erg,
/// kim_energy_unit_module::kim_energy_unit_erg
///
/// \since 2.0
extern EnergyUnit const erg;

/// \brief The standard electronvolt unit of energy.
///
/// \sa KIM_ENERGY_UNIT_eV,
/// kim_energy_unit_module::kim_energy_unit_ev
///
/// \since 2.0
extern EnergyUnit const eV;

/// \brief The standard Hartree unit of energy.
///
/// \sa KIM_ENERGY_UNIT_Hartre,
/// kim_energy_unit_module::kim_energy_unit_hartree
///
/// \since 2.0
extern EnergyUnit const Hartree;

/// \brief The standard Joule unit of energy.
///
/// \sa KIM_ENERGY_UNIT_J,
/// kim_energy_unit_module::kim_energy_unit_j
///
/// \since 2.0
extern EnergyUnit const J;

/// \brief The standard kilocalorie per mole unit of energy.
///
/// \sa KIM_ENERGY_UNIT_kcal_mol,
/// kim_energy_unit_module::kim_energy_unit_kcal_mol
///
/// \since 2.0
extern EnergyUnit const kcal_mol;

/// \brief Get the number of standard EnergyUnit's defined by the %KIM API.
///
/// \param[out] numberOfEnergyUnits The number of standard EnergyUnit's defined
///             by the %KIM API.
///
/// \sa KIM_ENERGY_UNIT_GetNumberOfEnergyUnits,
/// kim_energy_unit_module::kim_get_number_of_energy_units
///
/// \since 2.0
void GetNumberOfEnergyUnits(int * const numberOfEnergyUnits);

/// \brief Get the identity of each defined standard EnergyUnit.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             EnergyUnit.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] energyUnit The EnergyUnit object associated with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfEnergyUnits`.
/// \return \c false otherwise.
///
/// \sa KIM_ENERGY_UNIT_GetEnergyUnit,
/// kim_energy_unit_module::kim_get_energy_unit
///
/// \since 2.0
int GetEnergyUnit(int const index, EnergyUnit * const energyUnit);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for EnergyUnit
  /// objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(EnergyUnit const & a, EnergyUnit const & b) const
  {
    return a.energyUnitID < b.energyUnitID;
  }
};  // struct Comparator
}  // namespace ENERGY_UNIT
}  // namespace KIM

#endif  // KIM_ENERGY_UNIT_HPP_
