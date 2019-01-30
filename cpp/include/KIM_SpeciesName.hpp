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


#ifndef KIM_SPECIES_NAME_HPP_
#define KIM_SPECIES_NAME_HPP_

#include <string>

namespace KIM
{
/// \brief This class is an \ref extensible_enumeration
/// "Extensible Enumeration" for the SpeciesName's supported by the %KIM API.
///
/// The enumeration constants are contained in the SPECIES_NAME namespace.
///
/// \sa KIM_SpeciesName
///
/// \since 2.0
class SpeciesName
{
 public:
  /// \brief Integer identifying the specific SpeciesName represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_SpeciesName::speciesNameID
  ///
  /// \since 2.0
  int speciesNameID;

  /// \brief Create an uninitialized SpeciesName object.
  ///
  /// \since 2.0
  SpeciesName();

  /// \brief Create a SpeciesName object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.0
  SpeciesName(int const id);

  /// \brief Create a SpeciesName object corresponding to the provided string.
  /// If the string does not match one of the values defined by the %KIM API,
  /// then an "unknown" object is generated.
  ///
  /// \sa KIM_SpeciesName_FromString
  ///
  /// \since 2.0
  SpeciesName(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// SpeciesName's known to the %KIM API are found in the SPECIES_NAME
  /// namespace.
  ///
  /// \sa KIM_SpeciesName_Known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares SpeciesName objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_SpeciesName_Equal
  ///
  /// \since 2.0
  bool operator==(SpeciesName const & rhs) const;

  /// \brief Compares SpeciesName objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_SpeciesName_NotEqual
  ///
  /// \since 2.0
  bool operator!=(SpeciesName const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the SpeciesName object.
  ///
  /// \note If the SpeciesName object does not correspond to a value defined by
  /// the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_SpeciesName_ToString
  ///
  /// \since 2.0
  std::string const & ToString() const;
};  // class SpeciesName

/// \brief Contains the enumeration constants and the discovery routines for
/// the SpeciesName \ref extensible_enumeration "Extensible Enumeration".
namespace SPECIES_NAME
{
/// \brief The standard \c electron species.
///
/// \sa KIM_SPECIES_NAME_electron
///
/// \since 2.0
extern SpeciesName const electron;

/// \brief The standard \c Hydrogen species.
///
/// \sa KIM_SPECIES_NAME_H
///
/// \since 2.0
extern SpeciesName const H;

/// \brief The standard \c Helium species.
///
/// \sa KIM_SPECIES_NAME_He
///
/// \since 2.0
extern SpeciesName const He;

/// \brief The standard \c Lithium species.
///
/// \sa KIM_SPECIES_NAME_Li
///
/// \since 2.0
extern SpeciesName const Li;

/// \brief The standard \c Beryllium species.
///
/// \sa KIM_SPECIES_NAME_Be
///
/// \since 2.0
extern SpeciesName const Be;

/// \brief The standard \c Boron species.
///
/// \sa KIM_SPECIES_NAME_B
///
/// \since 2.0
extern SpeciesName const B;

/// \brief The standard \c Carbon species.
///
/// \sa KIM_SPECIES_NAME_C
///
/// \since 2.0
extern SpeciesName const C;

/// \brief The standard \c Nitrogen species.
///
/// \sa KIM_SPECIES_NAME_N
///
/// \since 2.0
extern SpeciesName const N;

/// \brief The standard \c Oxygen species.
///
/// \sa KIM_SPECIES_NAME_O
///
/// \since 2.0
extern SpeciesName const O;

/// \brief The standard \c Fluorine species.
///
/// \sa KIM_SPECIES_NAME_F
///
/// \since 2.0
extern SpeciesName const F;

/// \brief The standard \c Neon species.
///
/// \sa KIM_SPECIES_NAME_Ne
///
/// \since 2.0
extern SpeciesName const Ne;

/// \brief The standard \c Sodium species.
///
/// \sa KIM_SPECIES_NAME_Na
///
/// \since 2.0
extern SpeciesName const Na;

/// \brief The standard \c Magnesium species.
///
/// \sa KIM_SPECIES_NAME_Mg
///
/// \since 2.0
extern SpeciesName const Mg;

/// \brief The standard \c Aluminum species.
///
/// \sa KIM_SPECIES_NAME_Al
///
/// \since 2.0
extern SpeciesName const Al;

/// \brief The standard \c Silicon species.
///
/// \sa KIM_SPECIES_NAME_Si
///
/// \since 2.0
extern SpeciesName const Si;

/// \brief The standard \c Phosphorus species.
///
/// \sa KIM_SPECIES_NAME_P
///
/// \since 2.0
extern SpeciesName const P;

/// \brief The standard \c Sulfur species.
///
/// \sa KIM_SPECIES_NAME_S
///
/// \since 2.0
extern SpeciesName const S;

/// \brief The standard \c Chlorine species.
///
/// \sa KIM_SPECIES_NAME_Cl
///
/// \since 2.0
extern SpeciesName const Cl;

/// \brief The standard \c Argon species.
///
/// \sa KIM_SPECIES_NAME_Ar
///
/// \since 2.0
extern SpeciesName const Ar;

/// \brief The standard \c Potassium species.
///
/// \sa KIM_SPECIES_NAME_K
///
/// \since 2.0
extern SpeciesName const K;

/// \brief The standard \c Calcium species.
///
/// \sa KIM_SPECIES_NAME_Ca
///
/// \since 2.0
extern SpeciesName const Ca;

/// \brief The standard \c Scandium species.
///
/// \sa KIM_SPECIES_NAME_Sc
///
/// \since 2.0
extern SpeciesName const Sc;

/// \brief The standard \c Titanium species.
///
/// \sa KIM_SPECIES_NAME_Ti
///
/// \since 2.0
extern SpeciesName const Ti;

/// \brief The standard \c Vanadium species.
///
/// \sa KIM_SPECIES_NAME_V
///
/// \since 2.0
extern SpeciesName const V;

/// \brief The standard \c Chromium species.
///
/// \sa KIM_SPECIES_NAME_Cr
///
/// \since 2.0
extern SpeciesName const Cr;

/// \brief The standard \c Manganese species.
///
/// \sa KIM_SPECIES_NAME_Mn
///
/// \since 2.0
extern SpeciesName const Mn;

/// \brief The standard \c Iron species.
///
/// \sa KIM_SPECIES_NAME_Fe
///
/// \since 2.0
extern SpeciesName const Fe;

/// \brief The standard \c Cobalt species.
///
/// \sa KIM_SPECIES_NAME_Co
///
/// \since 2.0
extern SpeciesName const Co;

/// \brief The standard \c Nickel species.
///
/// \sa KIM_SPECIES_NAME_Ni
///
/// \since 2.0
extern SpeciesName const Ni;

/// \brief The standard \c Copper species.
///
/// \sa KIM_SPECIES_NAME_Cu
///
/// \since 2.0
extern SpeciesName const Cu;

/// \brief The standard \c Zinc species.
///
/// \sa KIM_SPECIES_NAME_Zn
///
/// \since 2.0
extern SpeciesName const Zn;

/// \brief The standard \c Gallium species.
///
/// \sa KIM_SPECIES_NAME_Ga
///
/// \since 2.0
extern SpeciesName const Ga;

/// \brief The standard \c Germanium species.
///
/// \sa KIM_SPECIES_NAME_Ge
///
/// \since 2.0
extern SpeciesName const Ge;

/// \brief The standard \c Arsenic species.
///
/// \sa KIM_SPECIES_NAME_As
///
/// \since 2.0
extern SpeciesName const As;

/// \brief The standard \c Selenium species.
///
/// \sa KIM_SPECIES_NAME_Se
///
/// \since 2.0
extern SpeciesName const Se;

/// \brief The standard \c Bromine species.
///
/// \sa KIM_SPECIES_NAME_Br
///
/// \since 2.0
extern SpeciesName const Br;

/// \brief The standard \c Krypton species.
///
/// \sa KIM_SPECIES_NAME_Kr
///
/// \since 2.0
extern SpeciesName const Kr;

/// \brief The standard \c Rubidium species.
///
/// \sa KIM_SPECIES_NAME_Rb
///
/// \since 2.0
extern SpeciesName const Rb;

/// \brief The standard \c Strontium species.
///
/// \sa KIM_SPECIES_NAME_Sr
///
/// \since 2.0
extern SpeciesName const Sr;

/// \brief The standard \c Yttrium species.
///
/// \sa KIM_SPECIES_NAME_Y
///
/// \since 2.0
extern SpeciesName const Y;

/// \brief The standard \c Zirconium species.
///
/// \sa KIM_SPECIES_NAME_Zr
///
/// \since 2.0
extern SpeciesName const Zr;

/// \brief The standard \c Niobium species.
///
/// \sa KIM_SPECIES_NAME_Nb
///
/// \since 2.0
extern SpeciesName const Nb;

/// \brief The standard \c Molybdenum species.
///
/// \sa KIM_SPECIES_NAME_Mo
///
/// \since 2.0
extern SpeciesName const Mo;

/// \brief The standard \c Technetium species.
///
/// \sa KIM_SPECIES_NAME_Tc
///
/// \since 2.0
extern SpeciesName const Tc;

/// \brief The standard \c Ruthenium species.
///
/// \sa KIM_SPECIES_NAME_Ru
///
/// \since 2.0
extern SpeciesName const Ru;

/// \brief The standard \c Rhodium species.
///
/// \sa KIM_SPECIES_NAME_Rh
///
/// \since 2.0
extern SpeciesName const Rh;

/// \brief The standard \c Palladium species.
///
/// \sa KIM_SPECIES_NAME_Pd
///
/// \since 2.0
extern SpeciesName const Pd;

/// \brief The standard \c Silver species.
///
/// \sa KIM_SPECIES_NAME_Ag
///
/// \since 2.0
extern SpeciesName const Ag;

/// \brief The standard \c Cadmium species.
///
/// \sa KIM_SPECIES_NAME_Cd
///
/// \since 2.0
extern SpeciesName const Cd;

/// \brief The standard \c Indium species.
///
/// \sa KIM_SPECIES_NAME_In
///
/// \since 2.0
extern SpeciesName const In;

/// \brief The standard \c Tin species.
///
/// \sa KIM_SPECIES_NAME_Sn
///
/// \since 2.0
extern SpeciesName const Sn;

/// \brief The standard \c Antimony species.
///
/// \sa KIM_SPECIES_NAME_Sb
///
/// \since 2.0
extern SpeciesName const Sb;

/// \brief The standard \c Tellurium species.
///
/// \sa KIM_SPECIES_NAME_Te
///
/// \since 2.0
extern SpeciesName const Te;

/// \brief The standard \c Iodine species.
///
/// \sa KIM_SPECIES_NAME_I
///
/// \since 2.0
extern SpeciesName const I;

/// \brief The standard \c Xenon species.
///
/// \sa KIM_SPECIES_NAME_Xe
///
/// \since 2.0
extern SpeciesName const Xe;

/// \brief The standard \c Cesium species.
///
/// \sa KIM_SPECIES_NAME_Cs
///
/// \since 2.0
extern SpeciesName const Cs;

/// \brief The standard \c Barium species.
///
/// \sa KIM_SPECIES_NAME_Ba
///
/// \since 2.0
extern SpeciesName const Ba;

/// \brief The standard \c Lanthanum species.
///
/// \sa KIM_SPECIES_NAME_La
///
/// \since 2.0
extern SpeciesName const La;

/// \brief The standard \c Cerium species.
///
/// \sa KIM_SPECIES_NAME_Ce
///
/// \since 2.0
extern SpeciesName const Ce;

/// \brief The standard \c Praseodymium species.
///
/// \sa KIM_SPECIES_NAME_Pr
///
/// \since 2.0
extern SpeciesName const Pr;

/// \brief The standard \c Neodymium species.
///
/// \sa KIM_SPECIES_NAME_Nd
///
/// \since 2.0
extern SpeciesName const Nd;

/// \brief The standard \c Promethium species.
///
/// \sa KIM_SPECIES_NAME_Pm
///
/// \since 2.0
extern SpeciesName const Pm;

/// \brief The standard \c Samarium species.
///
/// \sa KIM_SPECIES_NAME_Sm
///
/// \since 2.0
extern SpeciesName const Sm;

/// \brief The standard \c Europium species.
///
/// \sa KIM_SPECIES_NAME_Eu
///
/// \since 2.0
extern SpeciesName const Eu;

/// \brief The standard \c Gadolinium species.
///
/// \sa KIM_SPECIES_NAME_Gd
///
/// \since 2.0
extern SpeciesName const Gd;

/// \brief The standard \c Terbium species.
///
/// \sa KIM_SPECIES_NAME_Tb
///
/// \since 2.0
extern SpeciesName const Tb;

/// \brief The standard \c Dysprosium species.
///
/// \sa KIM_SPECIES_NAME_Dy
///
/// \since 2.0
extern SpeciesName const Dy;

/// \brief The standard \c Holmium species.
///
/// \sa KIM_SPECIES_NAME_Ho
///
/// \since 2.0
extern SpeciesName const Ho;

/// \brief The standard \c Erbium species.
///
/// \sa KIM_SPECIES_NAME_Er
///
/// \since 2.0
extern SpeciesName const Er;

/// \brief The standard \c Thulium species.
///
/// \sa KIM_SPECIES_NAME_Tm
///
/// \since 2.0
extern SpeciesName const Tm;

/// \brief The standard \c Ytterbium species.
///
/// \sa KIM_SPECIES_NAME_Yb
///
/// \since 2.0
extern SpeciesName const Yb;

/// \brief The standard \c Lutetium species.
///
/// \sa KIM_SPECIES_NAME_Lu
///
/// \since 2.0
extern SpeciesName const Lu;

/// \brief The standard \c Hafnium species.
///
/// \sa KIM_SPECIES_NAME_Hf
///
/// \since 2.0
extern SpeciesName const Hf;

/// \brief The standard \c Tantalum species.
///
/// \sa KIM_SPECIES_NAME_Ta
///
/// \since 2.0
extern SpeciesName const Ta;

/// \brief The standard \c Tungsten species.
///
/// \sa KIM_SPECIES_NAME_W
///
/// \since 2.0
extern SpeciesName const W;

/// \brief The standard \c Rhenium species.
///
/// \sa KIM_SPECIES_NAME_Re
///
/// \since 2.0
extern SpeciesName const Re;

/// \brief The standard \c Osmium species.
///
/// \sa KIM_SPECIES_NAME_Os
///
/// \since 2.0
extern SpeciesName const Os;

/// \brief The standard \c Iridium species.
///
/// \sa KIM_SPECIES_NAME_Ir
///
/// \since 2.0
extern SpeciesName const Ir;

/// \brief The standard \c Platinum species.
///
/// \sa KIM_SPECIES_NAME_Pt
///
/// \since 2.0
extern SpeciesName const Pt;

/// \brief The standard \c Gold species.
///
/// \sa KIM_SPECIES_NAME_Au
///
/// \since 2.0
extern SpeciesName const Au;

/// \brief The standard \c Mercury species.
///
/// \sa KIM_SPECIES_NAME_Hg
///
/// \since 2.0
extern SpeciesName const Hg;

/// \brief The standard \c Thallium species.
///
/// \sa KIM_SPECIES_NAME_Tl
///
/// \since 2.0
extern SpeciesName const Tl;

/// \brief The standard \c Lead species.
///
/// \sa KIM_SPECIES_NAME_Pb
///
/// \since 2.0
extern SpeciesName const Pb;

/// \brief The standard \c Bismuth species.
///
/// \sa KIM_SPECIES_NAME_Bi
///
/// \since 2.0
extern SpeciesName const Bi;

/// \brief The standard \c Polonium species.
///
/// \sa KIM_SPECIES_NAME_Po
///
/// \since 2.0
extern SpeciesName const Po;

/// \brief The standard \c Astatine species.
///
/// \sa KIM_SPECIES_NAME_At
///
/// \since 2.0
extern SpeciesName const At;

/// \brief The standard \c Radon species.
///
/// \sa KIM_SPECIES_NAME_Rn
///
/// \since 2.0
extern SpeciesName const Rn;

/// \brief The standard \c Francium species.
///
/// \sa KIM_SPECIES_NAME_Fr
///
/// \since 2.0
extern SpeciesName const Fr;

/// \brief The standard \c Radium species.
///
/// \sa KIM_SPECIES_NAME_Ra
///
/// \since 2.0
extern SpeciesName const Ra;

/// \brief The standard \c Actinium species.
///
/// \sa KIM_SPECIES_NAME_Ac
///
/// \since 2.0
extern SpeciesName const Ac;

/// \brief The standard \c Thorium species.
///
/// \sa KIM_SPECIES_NAME_Th
///
/// \since 2.0
extern SpeciesName const Th;

/// \brief The standard \c Protactinium species.
///
/// \sa KIM_SPECIES_NAME_Pa
///
/// \since 2.0
extern SpeciesName const Pa;

/// \brief The standard \c Uranium species.
///
/// \sa KIM_SPECIES_NAME_U
///
/// \since 2.0
extern SpeciesName const U;

/// \brief The standard \c Neptunium species.
///
/// \sa KIM_SPECIES_NAME_Np
///
/// \since 2.0
extern SpeciesName const Np;

/// \brief The standard \c Plutonium species.
///
/// \sa KIM_SPECIES_NAME_Pu
///
/// \since 2.0
extern SpeciesName const Pu;

/// \brief The standard \c Americium species.
///
/// \sa KIM_SPECIES_NAME_Am
///
/// \since 2.0
extern SpeciesName const Am;

/// \brief The standard \c Curium species.
///
/// \sa KIM_SPECIES_NAME_Cm
///
/// \since 2.0
extern SpeciesName const Cm;

/// \brief The standard \c Berkelium species.
///
/// \sa KIM_SPECIES_NAME_Bk
///
/// \since 2.0
extern SpeciesName const Bk;

/// \brief The standard \c Californium species.
///
/// \sa KIM_SPECIES_NAME_Cf
///
/// \since 2.0
extern SpeciesName const Cf;

/// \brief The standard \c Einsteinium species.
///
/// \sa KIM_SPECIES_NAME_Es
///
/// \since 2.0
extern SpeciesName const Es;

/// \brief The standard \c Fermium species.
///
/// \sa KIM_SPECIES_NAME_Fm
///
/// \since 2.0
extern SpeciesName const Fm;

/// \brief The standard \c Mendelevium species.
///
/// \sa KIM_SPECIES_NAME_Md
///
/// \since 2.0
extern SpeciesName const Md;

/// \brief The standard \c Nobelium species.
///
/// \sa KIM_SPECIES_NAME_No
///
/// \since 2.0
extern SpeciesName const No;

/// \brief The standard \c Lawrencium species.
///
/// \sa KIM_SPECIES_NAME_Lr
///
/// \since 2.0
extern SpeciesName const Lr;

/// \brief The standard \c Rutherfordium species.
///
/// \sa KIM_SPECIES_NAME_Rf
///
/// \since 2.0
extern SpeciesName const Rf;

/// \brief The standard \c Dubnium species.
///
/// \sa KIM_SPECIES_NAME_Db
///
/// \since 2.0
extern SpeciesName const Db;

/// \brief The standard \c Seaborgium species.
///
/// \sa KIM_SPECIES_NAME_Sg
///
/// \since 2.0
extern SpeciesName const Sg;

/// \brief The standard \c Bohrium species.
///
/// \sa KIM_SPECIES_NAME_Bh
///
/// \since 2.0
extern SpeciesName const Bh;

/// \brief The standard \c Hassium species.
///
/// \sa KIM_SPECIES_NAME_Hs
///
/// \since 2.0
extern SpeciesName const Hs;

/// \brief The standard \c Meitnerium species.
///
/// \sa KIM_SPECIES_NAME_Mt
///
/// \since 2.0
extern SpeciesName const Mt;

/// \brief The standard \c Darmstadtium species.
///
/// \sa KIM_SPECIES_NAME_Ds
///
/// \since 2.0
extern SpeciesName const Ds;

/// \brief The standard \c Roentgenium species.
///
/// \sa KIM_SPECIES_NAME_Rg
///
/// \since 2.0
extern SpeciesName const Rg;

/// \brief The standard \c Copernicium species.
///
/// \sa KIM_SPECIES_NAME_Cn
///
/// \since 2.0
extern SpeciesName const Cn;

/// \brief The standard \c Nihonium species.
///
/// \sa KIM_SPECIES_NAME_Nh
///
/// \since 2.0
extern SpeciesName const Nh;

/// \brief The standard \c Flerovium species.
///
/// \sa KIM_SPECIES_NAME_Fl
///
/// \since 2.0
extern SpeciesName const Fl;

/// \brief The standard \c Moscovium species.
///
/// \sa KIM_SPECIES_NAME_Mc
///
/// \since 2.0
extern SpeciesName const Mc;

/// \brief The standard \c Livermorium species.
///
/// \sa KIM_SPECIES_NAME_Lv
///
/// \since 2.0
extern SpeciesName const Lv;

/// \brief The standard \c Tennessine species.
///
/// \sa KIM_SPECIES_NAME_Ts
///
/// \since 2.0
extern SpeciesName const Ts;

/// \brief The standard \c Oganesson species.
///
/// \sa KIM_SPECIES_NAME_Og
///
/// \since 2.0
extern SpeciesName const Og;

/// \brief The standard \c user01 species.
///
/// \sa KIM_SPECIES_NAME_user01
///
/// \since 2.0
extern SpeciesName const user01;

/// \brief The standard \c user02 species.
///
/// \sa KIM_SPECIES_NAME_user02
///
/// \since 2.0
extern SpeciesName const user02;

/// \brief The standard \c user03 species.
///
/// \sa KIM_SPECIES_NAME_user03
///
/// \since 2.0
extern SpeciesName const user03;

/// \brief The standard \c user04 species.
///
/// \sa KIM_SPECIES_NAME_user04
///
/// \since 2.0
extern SpeciesName const user04;

/// \brief The standard \c user05 species.
///
/// \sa KIM_SPECIES_NAME_user05
///
/// \since 2.0
extern SpeciesName const user05;

/// \brief The standard \c user06 species.
///
/// \sa KIM_SPECIES_NAME_user06
///
/// \since 2.0
extern SpeciesName const user06;

/// \brief The standard \c user07 species.
///
/// \sa KIM_SPECIES_NAME_user07
///
/// \since 2.0
extern SpeciesName const user07;

/// \brief The standard \c user08 species.
///
/// \sa KIM_SPECIES_NAME_user08
///
/// \since 2.0
extern SpeciesName const user08;

/// \brief The standard \c user09 species.
///
/// \sa KIM_SPECIES_NAME_user09
///
/// \since 2.0
extern SpeciesName const user09;

/// \brief The standard \c user10 species.
///
/// \sa KIM_SPECIES_NAME_user10
///
/// \since 2.0
extern SpeciesName const user10;

/// \brief The standard \c user11 species.
///
/// \sa KIM_SPECIES_NAME_user11
///
/// \since 2.0
extern SpeciesName const user11;

/// \brief The standard \c user12 species.
///
/// \sa KIM_SPECIES_NAME_user12
///
/// \since 2.0
extern SpeciesName const user12;

/// \brief The standard \c user13 species.
///
/// \sa KIM_SPECIES_NAME_user13
///
/// \since 2.0
extern SpeciesName const user13;

/// \brief The standard \c user14 species.
///
/// \sa KIM_SPECIES_NAME_user14
///
/// \since 2.0
extern SpeciesName const user14;

/// \brief The standard \c user15 species.
///
/// \sa KIM_SPECIES_NAME_user15
///
/// \since 2.0
extern SpeciesName const user15;

/// \brief The standard \c user16 species.
///
/// \sa KIM_SPECIES_NAME_user16
///
/// \since 2.0
extern SpeciesName const user16;

/// \brief The standard \c user17 species.
///
/// \sa KIM_SPECIES_NAME_user17
///
/// \since 2.0
extern SpeciesName const user17;

/// \brief The standard \c user18 species.
///
/// \sa KIM_SPECIES_NAME_user18
///
/// \since 2.0
extern SpeciesName const user18;

/// \brief The standard \c user19 species.
///
/// \sa KIM_SPECIES_NAME_user19
///
/// \since 2.0
extern SpeciesName const user19;

/// \brief The standard \c user20 species.
///
/// \sa KIM_SPECIES_NAME_user20
///
/// \since 2.0
extern SpeciesName const user20;


/// \brief Get the number of standard SpeciesName's defined by the %KIM API.
///
/// \param[out] numberOfSpeciesNames The number of standard SpeciesName's
///             defined by the %KIM API.
///
/// \sa KIM_SPECIES_NAME_GetNumberOfSpeciesNames
///
/// \since 2.0
void GetNumberOfSpeciesNames(int * const numberOfSpeciesNames);

/// \brief Get the identity of each defined standard SpeciesName.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             SpeciesName.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] speciesName The SpeciesName object associated with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfSpeciesNames`.
/// \return \c false otherwise.
///
/// \sa KIM_SPECIES_NAME_GetSpeciesName
///
/// \since 2.0
int GetSpeciesName(int const index, SpeciesName * const speciesName);

/// \brief Structure provided for use with std::map.
///
/// \since 2.0
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for SpeciesName
  /// objects so that they can be stored in a std::map.
  ///
  /// \since 2.0
  bool operator()(SpeciesName const & a, SpeciesName const & b) const
  {
    return a.speciesNameID < b.speciesNameID;
  }
};  // struct Comparator
}  // namespace SPECIES_NAME
}  // namespace KIM

#endif  // KIM_SPECIES_NAME_HPP_
