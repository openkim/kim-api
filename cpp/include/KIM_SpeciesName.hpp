//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
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


#ifndef KIM_SPECIES_NAME_HPP_
#define KIM_SPECIES_NAME_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// SpeciesName's supported by the %KIM API.
///
/// The enumeration constants are contained in the SPECIES_NAME namespace.
///
/// \sa KIM_SpeciesName, kim_species_name_module::kim_species_name_type
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
  /// \sa KIM_SpeciesName::speciesNameID,
  /// kim_species_name_module::kim_species_name_type::species_name_id
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
  /// \sa KIM_SpeciesName_FromString, kim_species_name_module::kim_from_string
  ///
  /// \since 2.0
  SpeciesName(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// SpeciesName's known to the %KIM API are found in the SPECIES_NAME
  /// namespace.
  ///
  /// \sa KIM_SpeciesName_Known, kim_species_name_module::kim_known
  ///
  /// \since 2.0
  bool Known() const;

  /// \brief Compares SpeciesName objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_SpeciesName_Equal, kim_species_name_module::operator(.eq.)
  ///
  /// \since 2.0
  bool operator==(SpeciesName const & rhs) const;

  /// \brief Compares SpeciesName objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_SpeciesName_NotEqual, kim_species_name_module::operator(.ne.)
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
  /// \sa KIM_SpeciesName_ToString, kim_species_name_module::kim_to_string
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
/// \sa KIM_SPECIES_NAME_electron,
/// kim_species_name_module::kim_species_name_electron
///
/// \since 2.0
extern SpeciesName const electron;

/// \brief The standard \c Hydrogen species.
///
/// \sa KIM_SPECIES_NAME_H, kim_species_name_module::kim_species_name_h
///
/// \since 2.0
extern SpeciesName const H;

/// \brief The standard \c Helium species.
///
/// \sa KIM_SPECIES_NAME_He, kim_species_name_module::kim_species_name_he
///
/// \since 2.0
extern SpeciesName const He;

/// \brief The standard \c Lithium species.
///
/// \sa KIM_SPECIES_NAME_Li, kim_species_name_module::kim_species_name_li
///
/// \since 2.0
extern SpeciesName const Li;

/// \brief The standard \c Beryllium species.
///
/// \sa KIM_SPECIES_NAME_Be, kim_species_name_module::kim_species_name_be
///
/// \since 2.0
extern SpeciesName const Be;

/// \brief The standard \c Boron species.
///
/// \sa KIM_SPECIES_NAME_B, kim_species_name_module::kim_species_name_b
///
/// \since 2.0
extern SpeciesName const B;

/// \brief The standard \c Carbon species.
///
/// \sa KIM_SPECIES_NAME_C, kim_species_name_module::kim_species_name_c
///
/// \since 2.0
extern SpeciesName const C;

/// \brief The standard \c Nitrogen species.
///
/// \sa KIM_SPECIES_NAME_N, kim_species_name_module::kim_species_name_n
///
/// \since 2.0
extern SpeciesName const N;

/// \brief The standard \c Oxygen species.
///
/// \sa KIM_SPECIES_NAME_O, kim_species_name_module::kim_species_name_o
///
/// \since 2.0
extern SpeciesName const O;

/// \brief The standard \c Fluorine species.
///
/// \sa KIM_SPECIES_NAME_F, kim_species_name_module::kim_species_name_f
///
/// \since 2.0
extern SpeciesName const F;

/// \brief The standard \c Neon species.
///
/// \sa KIM_SPECIES_NAME_Ne, kim_species_name_module::kim_species_name_ne
///
/// \since 2.0
extern SpeciesName const Ne;

/// \brief The standard \c Sodium species.
///
/// \sa KIM_SPECIES_NAME_Na, kim_species_name_module::kim_species_name_na
///
/// \since 2.0
extern SpeciesName const Na;

/// \brief The standard \c Magnesium species.
///
/// \sa KIM_SPECIES_NAME_Mg, kim_species_name_module::kim_species_name_mg
///
/// \since 2.0
extern SpeciesName const Mg;

/// \brief The standard \c Aluminum species.
///
/// \sa KIM_SPECIES_NAME_Al, kim_species_name_module::kim_species_name_al
///
/// \since 2.0
extern SpeciesName const Al;

/// \brief The standard \c Silicon species.
///
/// \sa KIM_SPECIES_NAME_Si, kim_species_name_module::kim_species_name_si
///
/// \since 2.0
extern SpeciesName const Si;

/// \brief The standard \c Phosphorus species.
///
/// \sa KIM_SPECIES_NAME_P, kim_species_name_module::kim_species_name_p
///
/// \since 2.0
extern SpeciesName const P;

/// \brief The standard \c Sulfur species.
///
/// \sa KIM_SPECIES_NAME_S, kim_species_name_module::kim_species_name_s
///
/// \since 2.0
extern SpeciesName const S;

/// \brief The standard \c Chlorine species.
///
/// \sa KIM_SPECIES_NAME_Cl, kim_species_name_module::kim_species_name_cl
///
/// \since 2.0
extern SpeciesName const Cl;

/// \brief The standard \c Argon species.
///
/// \sa KIM_SPECIES_NAME_Ar, kim_species_name_module::kim_species_name_ar
///
/// \since 2.0
extern SpeciesName const Ar;

/// \brief The standard \c Potassium species.
///
/// \sa KIM_SPECIES_NAME_K, kim_species_name_module::kim_species_name_k
///
/// \since 2.0
extern SpeciesName const K;

/// \brief The standard \c Calcium species.
///
/// \sa KIM_SPECIES_NAME_Ca, kim_species_name_module::kim_species_name_ca
///
/// \since 2.0
extern SpeciesName const Ca;

/// \brief The standard \c Scandium species.
///
/// \sa KIM_SPECIES_NAME_Sc, kim_species_name_module::kim_species_name_sc
///
/// \since 2.0
extern SpeciesName const Sc;

/// \brief The standard \c Titanium species.
///
/// \sa KIM_SPECIES_NAME_Ti, kim_species_name_module::kim_species_name_ti
///
/// \since 2.0
extern SpeciesName const Ti;

/// \brief The standard \c Vanadium species.
///
/// \sa KIM_SPECIES_NAME_V, kim_species_name_module::kim_species_name_v
///
/// \since 2.0
extern SpeciesName const V;

/// \brief The standard \c Chromium species.
///
/// \sa KIM_SPECIES_NAME_Cr, kim_species_name_module::kim_species_name_cr
///
/// \since 2.0
extern SpeciesName const Cr;

/// \brief The standard \c Manganese species.
///
/// \sa KIM_SPECIES_NAME_Mn, kim_species_name_module::kim_species_name_mn
///
/// \since 2.0
extern SpeciesName const Mn;

/// \brief The standard \c Iron species.
///
/// \sa KIM_SPECIES_NAME_Fe, kim_species_name_module::kim_species_name_fe
///
/// \since 2.0
extern SpeciesName const Fe;

/// \brief The standard \c Cobalt species.
///
/// \sa KIM_SPECIES_NAME_Co, kim_species_name_module::kim_species_name_co
///
/// \since 2.0
extern SpeciesName const Co;

/// \brief The standard \c Nickel species.
///
/// \sa KIM_SPECIES_NAME_Ni, kim_species_name_module::kim_species_name_ni
///
/// \since 2.0
extern SpeciesName const Ni;

/// \brief The standard \c Copper species.
///
/// \sa KIM_SPECIES_NAME_Cu, kim_species_name_module::kim_species_name_cu
///
/// \since 2.0
extern SpeciesName const Cu;

/// \brief The standard \c Zinc species.
///
/// \sa KIM_SPECIES_NAME_Zn, kim_species_name_module::kim_species_name_zn
///
/// \since 2.0
extern SpeciesName const Zn;

/// \brief The standard \c Gallium species.
///
/// \sa KIM_SPECIES_NAME_Ga, kim_species_name_module::kim_species_name_ga
///
/// \since 2.0
extern SpeciesName const Ga;

/// \brief The standard \c Germanium species.
///
/// \sa KIM_SPECIES_NAME_Ge, kim_species_name_module::kim_species_name_ge
///
/// \since 2.0
extern SpeciesName const Ge;

/// \brief The standard \c Arsenic species.
///
/// \sa KIM_SPECIES_NAME_As, kim_species_name_module::kim_species_name_as
///
/// \since 2.0
extern SpeciesName const As;

/// \brief The standard \c Selenium species.
///
/// \sa KIM_SPECIES_NAME_Se, kim_species_name_module::kim_species_name_se
///
/// \since 2.0
extern SpeciesName const Se;

/// \brief The standard \c Bromine species.
///
/// \sa KIM_SPECIES_NAME_Br, kim_species_name_module::kim_species_name_br
///
/// \since 2.0
extern SpeciesName const Br;

/// \brief The standard \c Krypton species.
///
/// \sa KIM_SPECIES_NAME_Kr, kim_species_name_module::kim_species_name_kr
///
/// \since 2.0
extern SpeciesName const Kr;

/// \brief The standard \c Rubidium species.
///
/// \sa KIM_SPECIES_NAME_Rb, kim_species_name_module::kim_species_name_rb
///
/// \since 2.0
extern SpeciesName const Rb;

/// \brief The standard \c Strontium species.
///
/// \sa KIM_SPECIES_NAME_Sr, kim_species_name_module::kim_species_name_sr
///
/// \since 2.0
extern SpeciesName const Sr;

/// \brief The standard \c Yttrium species.
///
/// \sa KIM_SPECIES_NAME_Y, kim_species_name_module::kim_species_name_y
///
/// \since 2.0
extern SpeciesName const Y;

/// \brief The standard \c Zirconium species.
///
/// \sa KIM_SPECIES_NAME_Zr, kim_species_name_module::kim_species_name_zr
///
/// \since 2.0
extern SpeciesName const Zr;

/// \brief The standard \c Niobium species.
///
/// \sa KIM_SPECIES_NAME_Nb, kim_species_name_module::kim_species_name_nb
///
/// \since 2.0
extern SpeciesName const Nb;

/// \brief The standard \c Molybdenum species.
///
/// \sa KIM_SPECIES_NAME_Mo, kim_species_name_module::kim_species_name_mo
///
/// \since 2.0
extern SpeciesName const Mo;

/// \brief The standard \c Technetium species.
///
/// \sa KIM_SPECIES_NAME_Tc, kim_species_name_module::kim_species_name_tc
///
/// \since 2.0
extern SpeciesName const Tc;

/// \brief The standard \c Ruthenium species.
///
/// \sa KIM_SPECIES_NAME_Ru, kim_species_name_module::kim_species_name_ru
///
/// \since 2.0
extern SpeciesName const Ru;

/// \brief The standard \c Rhodium species.
///
/// \sa KIM_SPECIES_NAME_Rh, kim_species_name_module::kim_species_name_rh
///
/// \since 2.0
extern SpeciesName const Rh;

/// \brief The standard \c Palladium species.
///
/// \sa KIM_SPECIES_NAME_Pd, kim_species_name_module::kim_species_name_pd
///
/// \since 2.0
extern SpeciesName const Pd;

/// \brief The standard \c Silver species.
///
/// \sa KIM_SPECIES_NAME_Ag, kim_species_name_module::kim_species_name_ag
///
/// \since 2.0
extern SpeciesName const Ag;

/// \brief The standard \c Cadmium species.
///
/// \sa KIM_SPECIES_NAME_Cd, kim_species_name_module::kim_species_name_cd
///
/// \since 2.0
extern SpeciesName const Cd;

/// \brief The standard \c Indium species.
///
/// \sa KIM_SPECIES_NAME_In, kim_species_name_module::kim_species_name_in
///
/// \since 2.0
extern SpeciesName const In;

/// \brief The standard \c Tin species.
///
/// \sa KIM_SPECIES_NAME_Sn, kim_species_name_module::kim_species_name_sn
///
/// \since 2.0
extern SpeciesName const Sn;

/// \brief The standard \c Antimony species.
///
/// \sa KIM_SPECIES_NAME_Sb, kim_species_name_module::kim_species_name_sb
///
/// \since 2.0
extern SpeciesName const Sb;

/// \brief The standard \c Tellurium species.
///
/// \sa KIM_SPECIES_NAME_Te, kim_species_name_module::kim_species_name_te
///
/// \since 2.0
extern SpeciesName const Te;

/// \brief The standard \c Iodine species.
///
/// \sa KIM_SPECIES_NAME_I, kim_species_name_module::kim_species_name_i
///
/// \since 2.0
extern SpeciesName const I;

/// \brief The standard \c Xenon species.
///
/// \sa KIM_SPECIES_NAME_Xe, kim_species_name_module::kim_species_name_xe
///
/// \since 2.0
extern SpeciesName const Xe;

/// \brief The standard \c Cesium species.
///
/// \sa KIM_SPECIES_NAME_Cs, kim_species_name_module::kim_species_name_cs
///
/// \since 2.0
extern SpeciesName const Cs;

/// \brief The standard \c Barium species.
///
/// \sa KIM_SPECIES_NAME_Ba, kim_species_name_module::kim_species_name_ba
///
/// \since 2.0
extern SpeciesName const Ba;

/// \brief The standard \c Lanthanum species.
///
/// \sa KIM_SPECIES_NAME_La, kim_species_name_module::kim_species_name_la
///
/// \since 2.0
extern SpeciesName const La;

/// \brief The standard \c Cerium species.
///
/// \sa KIM_SPECIES_NAME_Ce, kim_species_name_module::kim_species_name_ce
///
/// \since 2.0
extern SpeciesName const Ce;

/// \brief The standard \c Praseodymium species.
///
/// \sa KIM_SPECIES_NAME_Pr, kim_species_name_module::kim_species_name_pr
///
/// \since 2.0
extern SpeciesName const Pr;

/// \brief The standard \c Neodymium species.
///
/// \sa KIM_SPECIES_NAME_Nd, kim_species_name_module::kim_species_name_nd
///
/// \since 2.0
extern SpeciesName const Nd;

/// \brief The standard \c Promethium species.
///
/// \sa KIM_SPECIES_NAME_Pm, kim_species_name_module::kim_species_name_pm
///
/// \since 2.0
extern SpeciesName const Pm;

/// \brief The standard \c Samarium species.
///
/// \sa KIM_SPECIES_NAME_Sm, kim_species_name_module::kim_species_name_sm
///
/// \since 2.0
extern SpeciesName const Sm;

/// \brief The standard \c Europium species.
///
/// \sa KIM_SPECIES_NAME_Eu, kim_species_name_module::kim_species_name_eu
///
/// \since 2.0
extern SpeciesName const Eu;

/// \brief The standard \c Gadolinium species.
///
/// \sa KIM_SPECIES_NAME_Gd, kim_species_name_module::kim_species_name_gd
///
/// \since 2.0
extern SpeciesName const Gd;

/// \brief The standard \c Terbium species.
///
/// \sa KIM_SPECIES_NAME_Tb, kim_species_name_module::kim_species_name_tb
///
/// \since 2.0
extern SpeciesName const Tb;

/// \brief The standard \c Dysprosium species.
///
/// \sa KIM_SPECIES_NAME_Dy, kim_species_name_module::kim_species_name_dy
///
/// \since 2.0
extern SpeciesName const Dy;

/// \brief The standard \c Holmium species.
///
/// \sa KIM_SPECIES_NAME_Ho, kim_species_name_module::kim_species_name_ho
///
/// \since 2.0
extern SpeciesName const Ho;

/// \brief The standard \c Erbium species.
///
/// \sa KIM_SPECIES_NAME_Er, kim_species_name_module::kim_species_name_er
///
/// \since 2.0
extern SpeciesName const Er;

/// \brief The standard \c Thulium species.
///
/// \sa KIM_SPECIES_NAME_Tm, kim_species_name_module::kim_species_name_tm
///
/// \since 2.0
extern SpeciesName const Tm;

/// \brief The standard \c Ytterbium species.
///
/// \sa KIM_SPECIES_NAME_Yb, kim_species_name_module::kim_species_name_yb
///
/// \since 2.0
extern SpeciesName const Yb;

/// \brief The standard \c Lutetium species.
///
/// \sa KIM_SPECIES_NAME_Lu, kim_species_name_module::kim_species_name_lu
///
/// \since 2.0
extern SpeciesName const Lu;

/// \brief The standard \c Hafnium species.
///
/// \sa KIM_SPECIES_NAME_Hf, kim_species_name_module::kim_species_name_hf
///
/// \since 2.0
extern SpeciesName const Hf;

/// \brief The standard \c Tantalum species.
///
/// \sa KIM_SPECIES_NAME_Ta, kim_species_name_module::kim_species_name_ta
///
/// \since 2.0
extern SpeciesName const Ta;

/// \brief The standard \c Tungsten species.
///
/// \sa KIM_SPECIES_NAME_W, kim_species_name_module::kim_species_name_w
///
/// \since 2.0
extern SpeciesName const W;

/// \brief The standard \c Rhenium species.
///
/// \sa KIM_SPECIES_NAME_Re, kim_species_name_module::kim_species_name_re
///
/// \since 2.0
extern SpeciesName const Re;

/// \brief The standard \c Osmium species.
///
/// \sa KIM_SPECIES_NAME_Os, kim_species_name_module::kim_species_name_os
///
/// \since 2.0
extern SpeciesName const Os;

/// \brief The standard \c Iridium species.
///
/// \sa KIM_SPECIES_NAME_Ir, kim_species_name_module::kim_species_name_ir
///
/// \since 2.0
extern SpeciesName const Ir;

/// \brief The standard \c Platinum species.
///
/// \sa KIM_SPECIES_NAME_Pt, kim_species_name_module::kim_species_name_pt
///
/// \since 2.0
extern SpeciesName const Pt;

/// \brief The standard \c Gold species.
///
/// \sa KIM_SPECIES_NAME_Au, kim_species_name_module::kim_species_name_au
///
/// \since 2.0
extern SpeciesName const Au;

/// \brief The standard \c Mercury species.
///
/// \sa KIM_SPECIES_NAME_Hg, kim_species_name_module::kim_species_name_hg
///
/// \since 2.0
extern SpeciesName const Hg;

/// \brief The standard \c Thallium species.
///
/// \sa KIM_SPECIES_NAME_Tl, kim_species_name_module::kim_species_name_tl
///
/// \since 2.0
extern SpeciesName const Tl;

/// \brief The standard \c Lead species.
///
/// \sa KIM_SPECIES_NAME_Pb, kim_species_name_module::kim_species_name_pb
///
/// \since 2.0
extern SpeciesName const Pb;

/// \brief The standard \c Bismuth species.
///
/// \sa KIM_SPECIES_NAME_Bi, kim_species_name_module::kim_species_name_bi
///
/// \since 2.0
extern SpeciesName const Bi;

/// \brief The standard \c Polonium species.
///
/// \sa KIM_SPECIES_NAME_Po, kim_species_name_module::kim_species_name_po
///
/// \since 2.0
extern SpeciesName const Po;

/// \brief The standard \c Astatine species.
///
/// \sa KIM_SPECIES_NAME_At, kim_species_name_module::kim_species_name_at
///
/// \since 2.0
extern SpeciesName const At;

/// \brief The standard \c Radon species.
///
/// \sa KIM_SPECIES_NAME_Rn, kim_species_name_module::kim_species_name_rn
///
/// \since 2.0
extern SpeciesName const Rn;

/// \brief The standard \c Francium species.
///
/// \sa KIM_SPECIES_NAME_Fr, kim_species_name_module::kim_species_name_fr
///
/// \since 2.0
extern SpeciesName const Fr;

/// \brief The standard \c Radium species.
///
/// \sa KIM_SPECIES_NAME_Ra, kim_species_name_module::kim_species_name_ra
///
/// \since 2.0
extern SpeciesName const Ra;

/// \brief The standard \c Actinium species.
///
/// \sa KIM_SPECIES_NAME_Ac, kim_species_name_module::kim_species_name_ac
///
/// \since 2.0
extern SpeciesName const Ac;

/// \brief The standard \c Thorium species.
///
/// \sa KIM_SPECIES_NAME_Th, kim_species_name_module::kim_species_name_th
///
/// \since 2.0
extern SpeciesName const Th;

/// \brief The standard \c Protactinium species.
///
/// \sa KIM_SPECIES_NAME_Pa, kim_species_name_module::kim_species_name_pa
///
/// \since 2.0
extern SpeciesName const Pa;

/// \brief The standard \c Uranium species.
///
/// \sa KIM_SPECIES_NAME_U, kim_species_name_module::kim_species_name_u
///
/// \since 2.0
extern SpeciesName const U;

/// \brief The standard \c Neptunium species.
///
/// \sa KIM_SPECIES_NAME_Np, kim_species_name_module::kim_species_name_np
///
/// \since 2.0
extern SpeciesName const Np;

/// \brief The standard \c Plutonium species.
///
/// \sa KIM_SPECIES_NAME_Pu, kim_species_name_module::kim_species_name_pu
///
/// \since 2.0
extern SpeciesName const Pu;

/// \brief The standard \c Americium species.
///
/// \sa KIM_SPECIES_NAME_Am, kim_species_name_module::kim_species_name_am
///
/// \since 2.0
extern SpeciesName const Am;

/// \brief The standard \c Curium species.
///
/// \sa KIM_SPECIES_NAME_Cm, kim_species_name_module::kim_species_name_cm
///
/// \since 2.0
extern SpeciesName const Cm;

/// \brief The standard \c Berkelium species.
///
/// \sa KIM_SPECIES_NAME_Bk, kim_species_name_module::kim_species_name_bk
///
/// \since 2.0
extern SpeciesName const Bk;

/// \brief The standard \c Californium species.
///
/// \sa KIM_SPECIES_NAME_Cf, kim_species_name_module::kim_species_name_cf
///
/// \since 2.0
extern SpeciesName const Cf;

/// \brief The standard \c Einsteinium species.
///
/// \sa KIM_SPECIES_NAME_Es, kim_species_name_module::kim_species_name_es
///
/// \since 2.0
extern SpeciesName const Es;

/// \brief The standard \c Fermium species.
///
/// \sa KIM_SPECIES_NAME_Fm, kim_species_name_module::kim_species_name_fm
///
/// \since 2.0
extern SpeciesName const Fm;

/// \brief The standard \c Mendelevium species.
///
/// \sa KIM_SPECIES_NAME_Md, kim_species_name_module::kim_species_name_md
///
/// \since 2.0
extern SpeciesName const Md;

/// \brief The standard \c Nobelium species.
///
/// \sa KIM_SPECIES_NAME_No, kim_species_name_module::kim_species_name_no
///
/// \since 2.0
extern SpeciesName const No;

/// \brief The standard \c Lawrencium species.
///
/// \sa KIM_SPECIES_NAME_Lr, kim_species_name_module::kim_species_name_lr
///
/// \since 2.0
extern SpeciesName const Lr;

/// \brief The standard \c Rutherfordium species.
///
/// \sa KIM_SPECIES_NAME_Rf, kim_species_name_module::kim_species_name_rf
///
/// \since 2.0
extern SpeciesName const Rf;

/// \brief The standard \c Dubnium species.
///
/// \sa KIM_SPECIES_NAME_Db, kim_species_name_module::kim_species_name_db
///
/// \since 2.0
extern SpeciesName const Db;

/// \brief The standard \c Seaborgium species.
///
/// \sa KIM_SPECIES_NAME_Sg, kim_species_name_module::kim_species_name_sg
///
/// \since 2.0
extern SpeciesName const Sg;

/// \brief The standard \c Bohrium species.
///
/// \sa KIM_SPECIES_NAME_Bh, kim_species_name_module::kim_species_name_bh
///
/// \since 2.0
extern SpeciesName const Bh;

/// \brief The standard \c Hassium species.
///
/// \sa KIM_SPECIES_NAME_Hs, kim_species_name_module::kim_species_name_hs
///
/// \since 2.0
extern SpeciesName const Hs;

/// \brief The standard \c Meitnerium species.
///
/// \sa KIM_SPECIES_NAME_Mt, kim_species_name_module::kim_species_name_mt
///
/// \since 2.0
extern SpeciesName const Mt;

/// \brief The standard \c Darmstadtium species.
///
/// \sa KIM_SPECIES_NAME_Ds, kim_species_name_module::kim_species_name_ds
///
/// \since 2.0
extern SpeciesName const Ds;

/// \brief The standard \c Roentgenium species.
///
/// \sa KIM_SPECIES_NAME_Rg, kim_species_name_module::kim_species_name_rg
///
/// \since 2.0
extern SpeciesName const Rg;

/// \brief The standard \c Copernicium species.
///
/// \sa KIM_SPECIES_NAME_Cn, kim_species_name_module::kim_species_name_cn
///
/// \since 2.0
extern SpeciesName const Cn;

/// \brief The standard \c Nihonium species.
///
/// \sa KIM_SPECIES_NAME_Nh, kim_species_name_module::kim_species_name_nh
///
/// \since 2.0
extern SpeciesName const Nh;

/// \brief The standard \c Flerovium species.
///
/// \sa KIM_SPECIES_NAME_Fl, kim_species_name_module::kim_species_name_fl
///
/// \since 2.0
extern SpeciesName const Fl;

/// \brief The standard \c Moscovium species.
///
/// \sa KIM_SPECIES_NAME_Mc, kim_species_name_module::kim_species_name_mc
///
/// \since 2.0
extern SpeciesName const Mc;

/// \brief The standard \c Livermorium species.
///
/// \sa KIM_SPECIES_NAME_Lv, kim_species_name_module::kim_species_name_lv
///
/// \since 2.0
extern SpeciesName const Lv;

/// \brief The standard \c Tennessine species.
///
/// \sa KIM_SPECIES_NAME_Ts, kim_species_name_module::kim_species_name_ts
///
/// \since 2.0
extern SpeciesName const Ts;

/// \brief The standard \c Oganesson species.
///
/// \sa KIM_SPECIES_NAME_Og, kim_species_name_module::kim_species_name_og
///
/// \since 2.0
extern SpeciesName const Og;

/// \brief The standard \c user01 species.
///
/// \sa KIM_SPECIES_NAME_user01,
/// kim_species_name_module::kim_species_name_user01
///
/// \since 2.0
extern SpeciesName const user01;

/// \brief The standard \c user02 species.
///
/// \sa KIM_SPECIES_NAME_user02,
/// kim_species_name_module::kim_species_name_user02
///
/// \since 2.0
extern SpeciesName const user02;

/// \brief The standard \c user03 species.
///
/// \sa KIM_SPECIES_NAME_user03,
/// kim_species_name_module::kim_species_name_user03
///
/// \since 2.0
extern SpeciesName const user03;

/// \brief The standard \c user04 species.
///
/// \sa KIM_SPECIES_NAME_user04,
/// kim_species_name_module::kim_species_name_user04
///
/// \since 2.0
extern SpeciesName const user04;

/// \brief The standard \c user05 species.
///
/// \sa KIM_SPECIES_NAME_user05,
/// kim_species_name_module::kim_species_name_user05
///
/// \since 2.0
extern SpeciesName const user05;

/// \brief The standard \c user06 species.
///
/// \sa KIM_SPECIES_NAME_user06,
/// kim_species_name_module::kim_species_name_user06
///
/// \since 2.0
extern SpeciesName const user06;

/// \brief The standard \c user07 species.
///
/// \sa KIM_SPECIES_NAME_user07,
/// kim_species_name_module::kim_species_name_user07
///
/// \since 2.0
extern SpeciesName const user07;

/// \brief The standard \c user08 species.
///
/// \sa KIM_SPECIES_NAME_user08,
/// kim_species_name_module::kim_species_name_user08
///
/// \since 2.0
extern SpeciesName const user08;

/// \brief The standard \c user09 species.
///
/// \sa KIM_SPECIES_NAME_user09,
/// kim_species_name_module::kim_species_name_user09
///
/// \since 2.0
extern SpeciesName const user09;

/// \brief The standard \c user10 species.
///
/// \sa KIM_SPECIES_NAME_user10,
/// kim_species_name_module::kim_species_name_user10
///
/// \since 2.0
extern SpeciesName const user10;

/// \brief The standard \c user11 species.
///
/// \sa KIM_SPECIES_NAME_user11,
/// kim_species_name_module::kim_species_name_user11
///
/// \since 2.0
extern SpeciesName const user11;

/// \brief The standard \c user12 species.
///
/// \sa KIM_SPECIES_NAME_user12,
/// kim_species_name_module::kim_species_name_user12
///
/// \since 2.0
extern SpeciesName const user12;

/// \brief The standard \c user13 species.
///
/// \sa KIM_SPECIES_NAME_user13,
/// kim_species_name_module::kim_species_name_user13
///
/// \since 2.0
extern SpeciesName const user13;

/// \brief The standard \c user14 species.
///
/// \sa KIM_SPECIES_NAME_user14,
/// kim_species_name_module::kim_species_name_user14
///
/// \since 2.0
extern SpeciesName const user14;

/// \brief The standard \c user15 species.
///
/// \sa KIM_SPECIES_NAME_user15,
/// kim_species_name_module::kim_species_name_user15
///
/// \since 2.0
extern SpeciesName const user15;

/// \brief The standard \c user16 species.
///
/// \sa KIM_SPECIES_NAME_user16,
/// kim_species_name_module::kim_species_name_user16
///
/// \since 2.0
extern SpeciesName const user16;

/// \brief The standard \c user17 species.
///
/// \sa KIM_SPECIES_NAME_user17,
/// kim_species_name_module::kim_species_name_user17
///
/// \since 2.0
extern SpeciesName const user17;

/// \brief The standard \c user18 species.
///
/// \sa KIM_SPECIES_NAME_user18,
/// kim_species_name_module::kim_species_name_user18
///
/// \since 2.0
extern SpeciesName const user18;

/// \brief The standard \c user19 species.
///
/// \sa KIM_SPECIES_NAME_user19,
/// kim_species_name_module::kim_species_name_user19
///
/// \since 2.0
extern SpeciesName const user19;

/// \brief The standard \c user20 species.
///
/// \sa KIM_SPECIES_NAME_user20,
/// kim_species_name_module::kim_species_name_user20
///
/// \since 2.0
extern SpeciesName const user20;


/// \brief Get the number of standard SpeciesName's defined by the %KIM API.
///
/// \param[out] numberOfSpeciesNames The number of standard SpeciesName's
///             defined by the %KIM API.
///
/// \sa KIM_SPECIES_NAME_GetNumberOfSpeciesNames,
/// kim_species_name_module::kim_get_number_of_species_names
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
/// \sa KIM_SPECIES_NAME_GetSpeciesName,
/// kim_species_name_module::kim_get_species_name
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
