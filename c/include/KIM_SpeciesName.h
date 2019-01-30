/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2019, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_SPECIES_NAME_H_
#define KIM_SPECIES_NAME_H_

/**
 ** \brief \copybrief KIM::SpeciesName
 **
 ** \sa KIM::SpeciesName
 **
 ** \since 2.0
 **/
struct KIM_SpeciesName
{
  /**
   ** \brief \copybrief KIM::SpeciesName::speciesNameID
   **
   ** \sa KIM::SpeciesName::speciesNameID
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
 ** \sa KIM::SpeciesName::SpeciesName(std::string const &)
 **
 ** \since 2.0
 **/
KIM_SpeciesName KIM_SpeciesName_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::SpeciesName::Known
 **
 ** \sa KIM::SpeciesName::Known
 **
 ** \since 2.0
 **/
int KIM_SpeciesName_Known(KIM_SpeciesName const speciesName);

/**
 ** \brief \copybrief KIM::SpeciesName::operator==()
 **
 ** \sa KIM::SpeciesName::operator==()
 **
 ** \since 2.0
 **/
int KIM_SpeciesName_Equal(KIM_SpeciesName const lhs, KIM_SpeciesName const rhs);

/**
 ** \brief \copybrief KIM::SpeciesName::operator!=()
 **
 ** \sa KIM::SpeciesName::operator!=()
 **
 ** \since 2.0
 **/
int KIM_SpeciesName_NotEqual(KIM_SpeciesName const lhs,
                             KIM_SpeciesName const rhs);

/**
 ** \brief \copybrief KIM::SpeciesName::ToString
 **
 ** \sa KIM::SpeciesName::ToString
 **
 ** \since 2.0
 **/
char const * KIM_SpeciesName_ToString(KIM_SpeciesName const speciesName);

/**
 ** \brief \copybrief KIM::SPECIES_NAME::electron
 **
 ** \sa KIM::SPECIES_NAME::electron
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_electron;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::H
 **
 ** \sa KIM::SPECIES_NAME::H
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_H;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::He
 **
 ** \sa KIM::SPECIES_NAME::He
 **
 ** \since 2.0
 **/

extern KIM_SpeciesName const KIM_SPECIES_NAME_He;
/**
 ** \brief \copybrief KIM::SPECIES_NAME::Li
 **
 ** \sa KIM::SPECIES_NAME::Li
 **
 ** \since 2.0
 **/

extern KIM_SpeciesName const KIM_SPECIES_NAME_Li;
/**
 ** \brief \copybrief KIM::SPECIES_NAME::Be
 **
 ** \sa KIM::SPECIES_NAME::Be
 **
 ** \since 2.0
 **/

extern KIM_SpeciesName const KIM_SPECIES_NAME_Be;
/**
 ** \brief \copybrief KIM::SPECIES_NAME::B
 **
 ** \sa KIM::SPECIES_NAME::B
 **
 ** \since 2.0
 **/

extern KIM_SpeciesName const KIM_SPECIES_NAME_B;
/**
 ** \brief \copybrief KIM::SPECIES_NAME::C
 **
 ** \sa KIM::SPECIES_NAME::C
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_C;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::N
 **
 ** \sa KIM::SPECIES_NAME::N
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_N;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::O
 **
 ** \sa KIM::SPECIES_NAME::O
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_O;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::F
 **
 ** \sa KIM::SPECIES_NAME::F
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_F;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ne
 **
 ** \sa KIM::SPECIES_NAME::Ne
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ne;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Na
 **
 ** \sa KIM::SPECIES_NAME::Na
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Na;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mg
 **
 ** \sa KIM::SPECIES_NAME::Mg
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mg;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Al
 **
 ** \sa KIM::SPECIES_NAME::Al
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Al;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Si
 **
 ** \sa KIM::SPECIES_NAME::Si
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Si;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::P
 **
 ** \sa KIM::SPECIES_NAME::P
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_P;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::S
 **
 ** \sa KIM::SPECIES_NAME::S
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_S;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cl
 **
 ** \sa KIM::SPECIES_NAME::Cl
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cl;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ar
 **
 ** \sa KIM::SPECIES_NAME::Ar
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ar;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::K
 **
 ** \sa KIM::SPECIES_NAME::K
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_K;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ca
 **
 ** \sa KIM::SPECIES_NAME::Ca
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ca;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sc
 **
 ** \sa KIM::SPECIES_NAME::Sc
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sc;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ti
 **
 ** \sa KIM::SPECIES_NAME::Ti
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ti;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::V
 **
 ** \sa KIM::SPECIES_NAME::V
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_V;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cr
 **
 ** \sa KIM::SPECIES_NAME::Cr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mn
 **
 ** \sa KIM::SPECIES_NAME::Mn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Fe
 **
 ** \sa KIM::SPECIES_NAME::Fe
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fe;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Co
 **
 ** \sa KIM::SPECIES_NAME::Co
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Co;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ni
 **
 ** \sa KIM::SPECIES_NAME::Ni
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ni;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cu
 **
 ** \sa KIM::SPECIES_NAME::Cu
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cu;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Zn
 **
 ** \sa KIM::SPECIES_NAME::Zn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Zn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ga
 **
 ** \sa KIM::SPECIES_NAME::Ga
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ga;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ge
 **
 ** \sa KIM::SPECIES_NAME::Ge
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ge;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::As
 **
 ** \sa KIM::SPECIES_NAME::As
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_As;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Se
 **
 ** \sa KIM::SPECIES_NAME::Se
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Se;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Br
 **
 ** \sa KIM::SPECIES_NAME::Br
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Br;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Kr
 **
 ** \sa KIM::SPECIES_NAME::Kr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Kr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rb
 **
 ** \sa KIM::SPECIES_NAME::Rb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sr
 **
 ** \sa KIM::SPECIES_NAME::Sr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Y
 **
 ** \sa KIM::SPECIES_NAME::Y
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Y;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Zr
 **
 ** \sa KIM::SPECIES_NAME::Zr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Zr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Nb
 **
 ** \sa KIM::SPECIES_NAME::Nb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Nb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mo
 **
 ** \sa KIM::SPECIES_NAME::Mo
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mo;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Tc
 **
 ** \sa KIM::SPECIES_NAME::Tc
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tc;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ru
 **
 ** \sa KIM::SPECIES_NAME::Ru
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ru;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rh
 **
 ** \sa KIM::SPECIES_NAME::Rh
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rh;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pd
 **
 ** \sa KIM::SPECIES_NAME::Pd
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pd;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ag
 **
 ** \sa KIM::SPECIES_NAME::Ag
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ag;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cd
 **
 ** \sa KIM::SPECIES_NAME::Cd
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cd;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::In
 **
 ** \sa KIM::SPECIES_NAME::In
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_In;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sn
 **
 ** \sa KIM::SPECIES_NAME::Sn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sb
 **
 ** \sa KIM::SPECIES_NAME::Sb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Te
 **
 ** \sa KIM::SPECIES_NAME::Te
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Te;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::I
 **
 ** \sa KIM::SPECIES_NAME::I
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_I;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Xe
 **
 ** \sa KIM::SPECIES_NAME::Xe
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Xe;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cs
 **
 ** \sa KIM::SPECIES_NAME::Cs
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cs;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ba
 **
 ** \sa KIM::SPECIES_NAME::Ba
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ba;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::La
 **
 ** \sa KIM::SPECIES_NAME::La
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_La;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ce
 **
 ** \sa KIM::SPECIES_NAME::Ce
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ce;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pr
 **
 ** \sa KIM::SPECIES_NAME::Pr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Nd
 **
 ** \sa KIM::SPECIES_NAME::Nd
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Nd;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pm
 **
 ** \sa KIM::SPECIES_NAME::Pm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sm
 **
 ** \sa KIM::SPECIES_NAME::Sm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Eu
 **
 ** \sa KIM::SPECIES_NAME::Eu
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Eu;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Gd
 **
 ** \sa KIM::SPECIES_NAME::Gd
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Gd;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Tb
 **
 ** \sa KIM::SPECIES_NAME::Tb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Dy
 **
 ** \sa KIM::SPECIES_NAME::Dy
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Dy;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ho
 **
 ** \sa KIM::SPECIES_NAME::Ho
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ho;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Er
 **
 ** \sa KIM::SPECIES_NAME::Er
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Er;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Tm
 **
 ** \sa KIM::SPECIES_NAME::Tm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Yb
 **
 ** \sa KIM::SPECIES_NAME::Yb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Yb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Lu
 **
 ** \sa KIM::SPECIES_NAME::Lu
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Lu;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Hf
 **
 ** \sa KIM::SPECIES_NAME::Hf
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Hf;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ta
 **
 ** \sa KIM::SPECIES_NAME::Ta
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ta;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::W
 **
 ** \sa KIM::SPECIES_NAME::W
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_W;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Re
 **
 ** \sa KIM::SPECIES_NAME::Re
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Re;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Os
 **
 ** \sa KIM::SPECIES_NAME::Os
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Os;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ir
 **
 ** \sa KIM::SPECIES_NAME::Ir
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ir;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pt
 **
 ** \sa KIM::SPECIES_NAME::Pt
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pt;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Au
 **
 ** \sa KIM::SPECIES_NAME::Au
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Au;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Hg
 **
 ** \sa KIM::SPECIES_NAME::Hg
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Hg;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Tl
 **
 ** \sa KIM::SPECIES_NAME::Tl
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tl;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pb
 **
 ** \sa KIM::SPECIES_NAME::Pb
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pb;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Bi
 **
 ** \sa KIM::SPECIES_NAME::Bi
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Bi;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Po
 **
 ** \sa KIM::SPECIES_NAME::Po
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Po;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::At
 **
 ** \sa KIM::SPECIES_NAME::At
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_At;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rn
 **
 ** \sa KIM::SPECIES_NAME::Rn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Fr
 **
 ** \sa KIM::SPECIES_NAME::Fr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ra
 **
 ** \sa KIM::SPECIES_NAME::Ra
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ra;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ac
 **
 ** \sa KIM::SPECIES_NAME::Ac
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ac;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Th
 **
 ** \sa KIM::SPECIES_NAME::Th
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Th;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pa
 **
 ** \sa KIM::SPECIES_NAME::Pa
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pa;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::U
 **
 ** \sa KIM::SPECIES_NAME::U
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_U;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Np
 **
 ** \sa KIM::SPECIES_NAME::Np
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Np;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Pu
 **
 ** \sa KIM::SPECIES_NAME::Pu
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pu;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Am
 **
 ** \sa KIM::SPECIES_NAME::Am
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Am;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cm
 **
 ** \sa KIM::SPECIES_NAME::Cm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Bk
 **
 ** \sa KIM::SPECIES_NAME::Bk
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Bk;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cf
 **
 ** \sa KIM::SPECIES_NAME::Cf
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cf;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Es
 **
 ** \sa KIM::SPECIES_NAME::Es
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Es;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Fm
 **
 ** \sa KIM::SPECIES_NAME::Fm
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fm;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Md
 **
 ** \sa KIM::SPECIES_NAME::Md
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Md;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::No
 **
 ** \sa KIM::SPECIES_NAME::No
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_No;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Lr
 **
 ** \sa KIM::SPECIES_NAME::Lr
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Lr;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rf
 **
 ** \sa KIM::SPECIES_NAME::Rf
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rf;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Db
 **
 ** \sa KIM::SPECIES_NAME::Db
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Db;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Sg
 **
 ** \sa KIM::SPECIES_NAME::Sg
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sg;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Bh
 **
 ** \sa KIM::SPECIES_NAME::Bh
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Bh;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Hs
 **
 ** \sa KIM::SPECIES_NAME::Hs
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Hs;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mt
 **
 ** \sa KIM::SPECIES_NAME::Mt
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mt;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ds
 **
 ** \sa KIM::SPECIES_NAME::Ds
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ds;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Rg
 **
 ** \sa KIM::SPECIES_NAME::Rg
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rg;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Cn
 **
 ** \sa KIM::SPECIES_NAME::Cn
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cn;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Nh
 **
 ** \sa KIM::SPECIES_NAME::Nh
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Nh;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Fl
 **
 ** \sa KIM::SPECIES_NAME::Fl
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fl;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Mc
 **
 ** \sa KIM::SPECIES_NAME::Mc
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mc;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Lv
 **
 ** \sa KIM::SPECIES_NAME::Lv
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Lv;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Ts
 **
 ** \sa KIM::SPECIES_NAME::Ts
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ts;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::Og
 **
 ** \sa KIM::SPECIES_NAME::Og
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_Og;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user01
 **
 ** \sa KIM::SPECIES_NAME::user01
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user01;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user02
 **
 ** \sa KIM::SPECIES_NAME::user02
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user02;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user03
 **
 ** \sa KIM::SPECIES_NAME::user03
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user03;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user04
 **
 ** \sa KIM::SPECIES_NAME::user04
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user04;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user05
 **
 ** \sa KIM::SPECIES_NAME::user05
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user05;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user06
 **
 ** \sa KIM::SPECIES_NAME::user06
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user06;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user07
 **
 ** \sa KIM::SPECIES_NAME::user07
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user07;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user08
 **
 ** \sa KIM::SPECIES_NAME::user08
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user08;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user09
 **
 ** \sa KIM::SPECIES_NAME::user09
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user09;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user10
 **
 ** \sa KIM::SPECIES_NAME::user10
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user10;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user11
 **
 ** \sa KIM::SPECIES_NAME::user11
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user11;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user12
 **
 ** \sa KIM::SPECIES_NAME::user12
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user12;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user13
 **
 ** \sa KIM::SPECIES_NAME::user13
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user13;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user14
 **
 ** \sa KIM::SPECIES_NAME::user14
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user14;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user15
 **
 ** \sa KIM::SPECIES_NAME::user15
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user15;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user16
 **
 ** \sa KIM::SPECIES_NAME::user16
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user16;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user17
 **
 ** \sa KIM::SPECIES_NAME::user17
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user17;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user18
 **
 ** \sa KIM::SPECIES_NAME::user18
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user18;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user19
 **
 ** \sa KIM::SPECIES_NAME::user19
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user19;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::user20
 **
 ** \sa KIM::SPECIES_NAME::user20
 **
 ** \since 2.0
 **/
extern KIM_SpeciesName const KIM_SPECIES_NAME_user20;

/**
 ** \brief \copybrief KIM::SPECIES_NAME::GetNumberOfSpeciesNames
 **
 ** \sa KIM::SPECIES_NAME::GetNumberOfSpeciesNames
 **
 ** \since 2.0
 **/
void KIM_SPECIES_NAME_GetNumberOfSpeciesNames(int * const numberOfSpeciesNames);

/**
 ** \brief \copybrief KIM::SPECIES_NAME::GetSpeciesName
 **
 ** \sa KIM::SPECIES_NAME::GetSpeciesName
 **
 ** \since 2.0
 **/
int KIM_SPECIES_NAME_GetSpeciesName(int const index,
                                    KIM_SpeciesName * const speciesName);

#endif /* KIM_SPECIES_NAME_H_ */
