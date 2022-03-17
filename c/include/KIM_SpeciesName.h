/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2022, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */
/* SPDX-License-Identifier: LGPL-2.1-or-later                                 */
/*                                                                            */
/* This library is free software; you can redistribute it and/or              */
/* modify it under the terms of the GNU Lesser General Public                 */
/* License as published by the Free Software Foundation; either               */
/* version 2.1 of the License, or (at your option) any later version.         */
/*                                                                            */
/* This library is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          */
/* Lesser General Public License for more details.                            */
/*                                                                            */
/* You should have received a copy of the GNU Lesser General Public License   */
/* along with this library; if not, write to the Free Software Foundation,    */
/* Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_SPECIES_NAME_H_
#define KIM_SPECIES_NAME_H_

/**
 ** \brief \copybrief KIM::SpeciesName
 **
 ** \sa KIM::SpeciesName, kim_species_name_module::kim_species_name_type
 **
 ** \since 2.0
 **/
struct KIM_SpeciesName
{
  /**
   ** \brief \copybrief KIM::SpeciesName::speciesNameID
   **
   ** \sa KIM::SpeciesName::speciesNameID,
   ** kim_species_name_module::kim_species_name_type::species_name_id
   **
   ** \since 2.0
   **/
  int speciesNameID;
};

#ifndef KIM_SPECIES_NAME_DEFINED_
#define KIM_SPECIES_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_SpeciesName KIM_SpeciesName;
#endif

/**
 ** \brief \copybrief KIM::SpeciesName::SpeciesName(std::string const &)
 **
 ** \sa KIM::SpeciesName::SpeciesName(std::string const &),
 ** kim_species_name_module::kim_from_string
 **
 ** \since 2.0
 **/
KIM_SpeciesName KIM_SpeciesName_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::SpeciesName::Known
 **
 ** \sa KIM::SpeciesName::Known, kim_species_name_module::kim_known
 **
 ** \since 2.0
 **/
int KIM_SpeciesName_Known(KIM_SpeciesName const speciesName);

/**
 ** \brief \copybrief KIM::SpeciesName::operator==()
 **
 ** \sa KIM::SpeciesName::operator==(), kim_species_name_module::operator(.eq.)
 **
 ** \since 2.0
 **/
int KIM_SpeciesName_Equal(KIM_SpeciesName const lhs, KIM_SpeciesName const rhs);

/**
 ** \brief \copybrief KIM::SpeciesName::operator!=()
 **
 ** \sa KIM::SpeciesName::operator!=(), kim_species_name_module::operator(.ne.)
 **
 ** \since 2.0
 **/
int KIM_SpeciesName_NotEqual(KIM_SpeciesName const lhs,
                             KIM_SpeciesName const rhs);

/**
 ** \brief \copybrief KIM::SpeciesName::ToString
 **
 ** \sa KIM::SpeciesName::ToString, kim_species_name_module::kim_to_string
 **
 ** \since 2.0
 **/
char const * KIM_SpeciesName_ToString(KIM_SpeciesName const speciesName);

/**
 ** \brief \copybrief KIM::SPECIES_NAME::electron
 **
 ** \sa KIM::SPECIES_NAME::electron,
 ** kim_species_name_module::kim_species_name_electron
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_electron;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::H
 **
 ** \sa KIM::SPECIES_NAME::H, kim_species_name_module::kim_species_name_h
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_H;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::He
 **
 ** \sa KIM::SPECIES_NAME::He, kim_species_name_module::kim_species_name_he
 **
 ** \since 2.0
 **/

extern KIM_SpeciesName const KIM_SPECIES_NAME_He;
/**
 ** \brief \copybrief KIM::SPECIES_NAME::Li
 **
 ** \sa KIM::SPECIES_NAME::Li, kim_species_name_module::kim_species_name_li
 **
 ** \since 2.0
 **/

extern KIM_SpeciesName const KIM_SPECIES_NAME_Li;
/**
 ** \brief \copybrief KIM::SPECIES_NAME::Be
 **
 ** \sa KIM::SPECIES_NAME::Be, kim_species_name_module::kim_species_name_be
 **
 ** \since 2.0
 **/

extern KIM_SpeciesName const KIM_SPECIES_NAME_Be;
/**
 ** \brief \copybrief KIM::SPECIES_NAME::B
 **
 ** \sa KIM::SPECIES_NAME::B, kim_species_name_module::kim_species_name_b
 **
 ** \since 2.0
 **/

extern KIM_SpeciesName const KIM_SPECIES_NAME_B;
/**
 ** \brief \copybrief KIM::SPECIES_NAME::C
 **
 ** \sa KIM::SPECIES_NAME::C, kim_species_name_module::kim_species_name_c
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_C;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::N
 **
 ** \sa KIM::SPECIES_NAME::N, kim_species_name_module::kim_species_name_n
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_N;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::O
 **
 ** \sa KIM::SPECIES_NAME::O, kim_species_name_module::kim_species_name_o
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_O;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::F
 **
 ** \sa KIM::SPECIES_NAME::F, kim_species_name_module::kim_species_name_f
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_F;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ne
 **
 ** \sa KIM::SPECIES_NAME::Ne, kim_species_name_module::kim_species_name_ne
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ne;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Na
 **
 ** \sa KIM::SPECIES_NAME::Na, kim_species_name_module::kim_species_name_na
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Na;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mg
 **
 ** \sa KIM::SPECIES_NAME::Mg, kim_species_name_module::kim_species_name_mg
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mg;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Al
 **
 ** \sa KIM::SPECIES_NAME::Al, kim_species_name_module::kim_species_name_al
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Al;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Si
 **
 ** \sa KIM::SPECIES_NAME::Si, kim_species_name_module::kim_species_name_si
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Si;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::P
 **
 ** \sa KIM::SPECIES_NAME::P, kim_species_name_module::kim_species_name_p
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_P;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::S
 **
 ** \sa KIM::SPECIES_NAME::S, kim_species_name_module::kim_species_name_s
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_S;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cl
 **
 ** \sa KIM::SPECIES_NAME::Cl, kim_species_name_module::kim_species_name_cl
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cl;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ar
 **
 ** \sa KIM::SPECIES_NAME::Ar, kim_species_name_module::kim_species_name_ar
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ar;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::K
 **
 ** \sa KIM::SPECIES_NAME::K, kim_species_name_module::kim_species_name_k
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_K;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ca
 **
 ** \sa KIM::SPECIES_NAME::Ca, kim_species_name_module::kim_species_name_ca
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ca;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sc
 **
 ** \sa KIM::SPECIES_NAME::Sc, kim_species_name_module::kim_species_name_sc
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sc;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ti
 **
 ** \sa KIM::SPECIES_NAME::Ti, kim_species_name_module::kim_species_name_ti
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ti;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::V
 **
 ** \sa KIM::SPECIES_NAME::V, kim_species_name_module::kim_species_name_v
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_V;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cr
 **
 ** \sa KIM::SPECIES_NAME::Cr, kim_species_name_module::kim_species_name_cr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mn
 **
 ** \sa KIM::SPECIES_NAME::Mn, kim_species_name_module::kim_species_name_mn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Fe
 **
 ** \sa KIM::SPECIES_NAME::Fe, kim_species_name_module::kim_species_name_fe
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fe;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Co
 **
 ** \sa KIM::SPECIES_NAME::Co, kim_species_name_module::kim_species_name_co
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Co;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ni
 **
 ** \sa KIM::SPECIES_NAME::Ni, kim_species_name_module::kim_species_name_ni
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ni;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cu
 **
 ** \sa KIM::SPECIES_NAME::Cu, kim_species_name_module::kim_species_name_cu
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cu;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Zn
 **
 ** \sa KIM::SPECIES_NAME::Zn, kim_species_name_module::kim_species_name_zn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Zn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ga
 **
 ** \sa KIM::SPECIES_NAME::Ga, kim_species_name_module::kim_species_name_ga
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ga;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ge
 **
 ** \sa KIM::SPECIES_NAME::Ge, kim_species_name_module::kim_species_name_ge
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ge;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::As
 **
 ** \sa KIM::SPECIES_NAME::As, kim_species_name_module::kim_species_name_as
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_As;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Se
 **
 ** \sa KIM::SPECIES_NAME::Se, kim_species_name_module::kim_species_name_se
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Se;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Br
 **
 ** \sa KIM::SPECIES_NAME::Br, kim_species_name_module::kim_species_name_br
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Br;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Kr
 **
 ** \sa KIM::SPECIES_NAME::Kr, kim_species_name_module::kim_species_name_kr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Kr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rb
 **
 ** \sa KIM::SPECIES_NAME::Rb, kim_species_name_module::kim_species_name_rb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sr
 **
 ** \sa KIM::SPECIES_NAME::Sr, kim_species_name_module::kim_species_name_sr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Y
 **
 ** \sa KIM::SPECIES_NAME::Y, kim_species_name_module::kim_species_name_y
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Y;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Zr
 **
 ** \sa KIM::SPECIES_NAME::Zr, kim_species_name_module::kim_species_name_zr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Zr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Nb
 **
 ** \sa KIM::SPECIES_NAME::Nb, kim_species_name_module::kim_species_name_nb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Nb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mo
 **
 ** \sa KIM::SPECIES_NAME::Mo, kim_species_name_module::kim_species_name_mo
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mo;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Tc
 **
 ** \sa KIM::SPECIES_NAME::Tc, kim_species_name_module::kim_species_name_tc
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tc;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ru
 **
 ** \sa KIM::SPECIES_NAME::Ru, kim_species_name_module::kim_species_name_ru
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ru;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rh
 **
 ** \sa KIM::SPECIES_NAME::Rh, kim_species_name_module::kim_species_name_rh
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rh;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pd
 **
 ** \sa KIM::SPECIES_NAME::Pd, kim_species_name_module::kim_species_name_pd
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pd;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ag
 **
 ** \sa KIM::SPECIES_NAME::Ag, kim_species_name_module::kim_species_name_ag
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ag;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cd
 **
 ** \sa KIM::SPECIES_NAME::Cd, kim_species_name_module::kim_species_name_cd
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cd;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::In
 **
 ** \sa KIM::SPECIES_NAME::In, kim_species_name_module::kim_species_name_in
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_In;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sn
 **
 ** \sa KIM::SPECIES_NAME::Sn, kim_species_name_module::kim_species_name_sn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sb
 **
 ** \sa KIM::SPECIES_NAME::Sb, kim_species_name_module::kim_species_name_sb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Te
 **
 ** \sa KIM::SPECIES_NAME::Te, kim_species_name_module::kim_species_name_te
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Te;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::I
 **
 ** \sa KIM::SPECIES_NAME::I, kim_species_name_module::kim_species_name_i
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_I;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Xe
 **
 ** \sa KIM::SPECIES_NAME::Xe, kim_species_name_module::kim_species_name_xe
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Xe;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cs
 **
 ** \sa KIM::SPECIES_NAME::Cs, kim_species_name_module::kim_species_name_cs
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cs;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ba
 **
 ** \sa KIM::SPECIES_NAME::Ba, kim_species_name_module::kim_species_name_ba
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ba;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::La
 **
 ** \sa KIM::SPECIES_NAME::La, kim_species_name_module::kim_species_name_la
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_La;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ce
 **
 ** \sa KIM::SPECIES_NAME::Ce, kim_species_name_module::kim_species_name_ce
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ce;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pr
 **
 ** \sa KIM::SPECIES_NAME::Pr, kim_species_name_module::kim_species_name_pr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Nd
 **
 ** \sa KIM::SPECIES_NAME::Nd, kim_species_name_module::kim_species_name_nd
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Nd;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pm
 **
 ** \sa KIM::SPECIES_NAME::Pm, kim_species_name_module::kim_species_name_pm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sm
 **
 ** \sa KIM::SPECIES_NAME::Sm, kim_species_name_module::kim_species_name_sm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Eu
 **
 ** \sa KIM::SPECIES_NAME::Eu, kim_species_name_module::kim_species_name_eu
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Eu;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Gd
 **
 ** \sa KIM::SPECIES_NAME::Gd, kim_species_name_module::kim_species_name_gd
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Gd;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Tb
 **
 ** \sa KIM::SPECIES_NAME::Tb, kim_species_name_module::kim_species_name_tb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Dy
 **
 ** \sa KIM::SPECIES_NAME::Dy, kim_species_name_module::kim_species_name_dy
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Dy;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ho
 **
 ** \sa KIM::SPECIES_NAME::Ho, kim_species_name_module::kim_species_name_ho
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ho;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Er
 **
 ** \sa KIM::SPECIES_NAME::Er, kim_species_name_module::kim_species_name_er
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Er;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Tm
 **
 ** \sa KIM::SPECIES_NAME::Tm, kim_species_name_module::kim_species_name_tm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Yb
 **
 ** \sa KIM::SPECIES_NAME::Yb, kim_species_name_module::kim_species_name_yb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Yb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Lu
 **
 ** \sa KIM::SPECIES_NAME::Lu, kim_species_name_module::kim_species_name_lu
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Lu;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Hf
 **
 ** \sa KIM::SPECIES_NAME::Hf, kim_species_name_module::kim_species_name_hf
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Hf;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ta
 **
 ** \sa KIM::SPECIES_NAME::Ta, kim_species_name_module::kim_species_name_ta
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ta;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::W
 **
 ** \sa KIM::SPECIES_NAME::W, kim_species_name_module::kim_species_name_w
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_W;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Re
 **
 ** \sa KIM::SPECIES_NAME::Re, kim_species_name_module::kim_species_name_re
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Re;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Os
 **
 ** \sa KIM::SPECIES_NAME::Os, kim_species_name_module::kim_species_name_os
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Os;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ir
 **
 ** \sa KIM::SPECIES_NAME::Ir, kim_species_name_module::kim_species_name_ir
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ir;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pt
 **
 ** \sa KIM::SPECIES_NAME::Pt, kim_species_name_module::kim_species_name_pt
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pt;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Au
 **
 ** \sa KIM::SPECIES_NAME::Au, kim_species_name_module::kim_species_name_au
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Au;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Hg
 **
 ** \sa KIM::SPECIES_NAME::Hg, kim_species_name_module::kim_species_name_hg
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Hg;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Tl
 **
 ** \sa KIM::SPECIES_NAME::Tl, kim_species_name_module::kim_species_name_tl
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tl;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pb
 **
 ** \sa KIM::SPECIES_NAME::Pb, kim_species_name_module::kim_species_name_pb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Bi
 **
 ** \sa KIM::SPECIES_NAME::Bi, kim_species_name_module::kim_species_name_bi
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Bi;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Po
 **
 ** \sa KIM::SPECIES_NAME::Po, kim_species_name_module::kim_species_name_po
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Po;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::At
 **
 ** \sa KIM::SPECIES_NAME::At, kim_species_name_module::kim_species_name_at
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_At;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rn
 **
 ** \sa KIM::SPECIES_NAME::Rn, kim_species_name_module::kim_species_name_rn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Fr
 **
 ** \sa KIM::SPECIES_NAME::Fr, kim_species_name_module::kim_species_name_fr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ra
 **
 ** \sa KIM::SPECIES_NAME::Ra, kim_species_name_module::kim_species_name_ra
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ra;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ac
 **
 ** \sa KIM::SPECIES_NAME::Ac, kim_species_name_module::kim_species_name_ac
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ac;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Th
 **
 ** \sa KIM::SPECIES_NAME::Th, kim_species_name_module::kim_species_name_th
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Th;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pa
 **
 ** \sa KIM::SPECIES_NAME::Pa, kim_species_name_module::kim_species_name_pa
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pa;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::U
 **
 ** \sa KIM::SPECIES_NAME::U, kim_species_name_module::kim_species_name_u
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_U;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Np
 **
 ** \sa KIM::SPECIES_NAME::Np, kim_species_name_module::kim_species_name_np
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Np;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pu
 **
 ** \sa KIM::SPECIES_NAME::Pu, kim_species_name_module::kim_species_name_pu
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pu;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Am
 **
 ** \sa KIM::SPECIES_NAME::Am, kim_species_name_module::kim_species_name_am
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Am;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cm
 **
 ** \sa KIM::SPECIES_NAME::Cm, kim_species_name_module::kim_species_name_cm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Bk
 **
 ** \sa KIM::SPECIES_NAME::Bk, kim_species_name_module::kim_species_name_bk
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Bk;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cf
 **
 ** \sa KIM::SPECIES_NAME::Cf, kim_species_name_module::kim_species_name_cf
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cf;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Es
 **
 ** \sa KIM::SPECIES_NAME::Es, kim_species_name_module::kim_species_name_es
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Es;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Fm
 **
 ** \sa KIM::SPECIES_NAME::Fm, kim_species_name_module::kim_species_name_fm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Md
 **
 ** \sa KIM::SPECIES_NAME::Md, kim_species_name_module::kim_species_name_md
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Md;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::No
 **
 ** \sa KIM::SPECIES_NAME::No, kim_species_name_module::kim_species_name_no
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_No;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Lr
 **
 ** \sa KIM::SPECIES_NAME::Lr, kim_species_name_module::kim_species_name_lr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Lr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rf
 **
 ** \sa KIM::SPECIES_NAME::Rf, kim_species_name_module::kim_species_name_rf
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rf;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Db
 **
 ** \sa KIM::SPECIES_NAME::Db, kim_species_name_module::kim_species_name_db
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Db;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sg
 **
 ** \sa KIM::SPECIES_NAME::Sg, kim_species_name_module::kim_species_name_sg
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sg;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Bh
 **
 ** \sa KIM::SPECIES_NAME::Bh, kim_species_name_module::kim_species_name_bh
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Bh;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Hs
 **
 ** \sa KIM::SPECIES_NAME::Hs, kim_species_name_module::kim_species_name_hs
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Hs;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mt
 **
 ** \sa KIM::SPECIES_NAME::Mt, kim_species_name_module::kim_species_name_mt
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mt;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ds
 **
 ** \sa KIM::SPECIES_NAME::Ds, kim_species_name_module::kim_species_name_ds
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ds;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rg
 **
 ** \sa KIM::SPECIES_NAME::Rg, kim_species_name_module::kim_species_name_rg
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rg;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cn
 **
 ** \sa KIM::SPECIES_NAME::Cn, kim_species_name_module::kim_species_name_cn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Nh
 **
 ** \sa KIM::SPECIES_NAME::Nh, kim_species_name_module::kim_species_name_nh
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Nh;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Fl
 **
 ** \sa KIM::SPECIES_NAME::Fl, kim_species_name_module::kim_species_name_fl
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fl;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mc
 **
 ** \sa KIM::SPECIES_NAME::Mc, kim_species_name_module::kim_species_name_mc
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mc;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Lv
 **
 ** \sa KIM::SPECIES_NAME::Lv, kim_species_name_module::kim_species_name_lv
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Lv;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ts
 **
 ** \sa KIM::SPECIES_NAME::Ts, kim_species_name_module::kim_species_name_ts
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ts;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Og
 **
 ** \sa KIM::SPECIES_NAME::Og, kim_species_name_module::kim_species_name_og
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Og;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user01
 **
 ** \sa KIM::SPECIES_NAME::user01,
 ** kim_species_name_module::kim_species_name_user01
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user01;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user02
 **
 ** \sa KIM::SPECIES_NAME::user02,
 ** kim_species_name_module::kim_species_name_user02
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user02;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user03
 **
 ** \sa KIM::SPECIES_NAME::user03,
 ** kim_species_name_module::kim_species_name_user03
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user03;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user04
 **
 ** \sa KIM::SPECIES_NAME::user04,
 ** kim_species_name_module::kim_species_name_user04
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user04;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user05
 **
 ** \sa KIM::SPECIES_NAME::user05,
 ** kim_species_name_module::kim_species_name_user05
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user05;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user06
 **
 ** \sa KIM::SPECIES_NAME::user06,
 ** kim_species_name_module::kim_species_name_user06
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user06;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user07
 **
 ** \sa KIM::SPECIES_NAME::user07,
 ** kim_species_name_module::kim_species_name_user07
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user07;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user08
 **
 ** \sa KIM::SPECIES_NAME::user08,
 ** kim_species_name_module::kim_species_name_user08
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user08;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user09
 **
 ** \sa KIM::SPECIES_NAME::user09,
 ** kim_species_name_module::kim_species_name_user09
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user09;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user10
 **
 ** \sa KIM::SPECIES_NAME::user10,
 ** kim_species_name_module::kim_species_name_user10
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user10;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user11
 **
 ** \sa KIM::SPECIES_NAME::user11,
 ** kim_species_name_module::kim_species_name_user11
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user11;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user12
 **
 ** \sa KIM::SPECIES_NAME::user12,
 ** kim_species_name_module::kim_species_name_user12
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user12;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user13
 **
 ** \sa KIM::SPECIES_NAME::user13,
 ** kim_species_name_module::kim_species_name_user13
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user13;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user14
 **
 ** \sa KIM::SPECIES_NAME::user14,
 ** kim_species_name_module::kim_species_name_user14
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user14;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user15
 **
 ** \sa KIM::SPECIES_NAME::user15,
 ** kim_species_name_module::kim_species_name_user15
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user15;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user16
 **
 ** \sa KIM::SPECIES_NAME::user16,
 ** kim_species_name_module::kim_species_name_user16
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user16;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user17
 **
 ** \sa KIM::SPECIES_NAME::user17,
 ** kim_species_name_module::kim_species_name_user17
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user17;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user18
 **
 ** \sa KIM::SPECIES_NAME::user18,
 ** kim_species_name_module::kim_species_name_user18
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user18;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user19
 **
 ** \sa KIM::SPECIES_NAME::user19,
 ** kim_species_name_module::kim_species_name_user19
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user19;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user20
 **
 ** \sa KIM::SPECIES_NAME::user20,
 ** kim_species_name_module::kim_species_name_user20
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user20;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::GetNumberOfSpeciesNames
 **
 ** \sa KIM::SPECIES_NAME::GetNumberOfSpeciesNames,
 ** kim_species_name_module::kim_get_number_of_species_names
 **
 ** \since 2.0
 **/
void KIM_SPECIES_NAME_GetNumberOfSpeciesNames(int * const numberOfSpeciesNames);

/**
 ** \brief \copybrief KIM::SPECIES_NAME::GetSpeciesName
 **
 ** \sa KIM::SPECIES_NAME::GetSpeciesName,
 ** kim_species_name_module::kim_get_species_name
 **
 ** \since 2.0
 **/
int KIM_SPECIES_NAME_GetSpeciesName(int const index,
                                    KIM_SpeciesName * const speciesName);

#endif /* KIM_SPECIES_NAME_H_ */
