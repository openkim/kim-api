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
/* Copyright (c) 2016--2018, Regents of the University of Minnesota.          */
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

struct KIM_SpeciesName
{
  int speciesNameID;
};

#ifndef KIM_SPECIES_NAME_DEFINED_
#define KIM_SPECIES_NAME_DEFINED_
typedef struct KIM_SpeciesName KIM_SpeciesName;
#endif

KIM_SpeciesName KIM_SpeciesName_FromString(char const * const str);

int KIM_SpeciesName_Equal(KIM_SpeciesName const left,
                          KIM_SpeciesName const right);
int KIM_SpeciesName_NotEqual(KIM_SpeciesName const left,
                             KIM_SpeciesName const right);
char const * KIM_SpeciesName_String(KIM_SpeciesName const speciesName);

extern KIM_SpeciesName const KIM_SPECIES_NAME_electron;
extern KIM_SpeciesName const KIM_SPECIES_NAME_H;
extern KIM_SpeciesName const KIM_SPECIES_NAME_He;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Li;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Be;
extern KIM_SpeciesName const KIM_SPECIES_NAME_B;
extern KIM_SpeciesName const KIM_SPECIES_NAME_C;
extern KIM_SpeciesName const KIM_SPECIES_NAME_N;
extern KIM_SpeciesName const KIM_SPECIES_NAME_O;
extern KIM_SpeciesName const KIM_SPECIES_NAME_F;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ne;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Na;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mg;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Al;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Si;
extern KIM_SpeciesName const KIM_SPECIES_NAME_P;
extern KIM_SpeciesName const KIM_SPECIES_NAME_S;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cl;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ar;
extern KIM_SpeciesName const KIM_SPECIES_NAME_K;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ca;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sc;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ti;
extern KIM_SpeciesName const KIM_SPECIES_NAME_V;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cr;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mn;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fe;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Co;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ni;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cu;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Zn;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ga;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ge;
extern KIM_SpeciesName const KIM_SPECIES_NAME_As;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Se;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Br;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Kr;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rb;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sr;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Y;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Zr;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Nb;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mo;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tc;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ru;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rh;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pd;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ag;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cd;
extern KIM_SpeciesName const KIM_SPECIES_NAME_In;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sn;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sb;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Te;
extern KIM_SpeciesName const KIM_SPECIES_NAME_I;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Xe;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cs;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ba;
extern KIM_SpeciesName const KIM_SPECIES_NAME_La;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ce;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pr;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Nd;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pm;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sm;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Eu;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Gd;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tb;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Dy;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ho;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Er;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tm;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Yb;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Lu;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Hf;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ta;
extern KIM_SpeciesName const KIM_SPECIES_NAME_W;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Re;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Os;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ir;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pt;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Au;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Hg;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Tl;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pb;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Bi;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Po;
extern KIM_SpeciesName const KIM_SPECIES_NAME_At;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rn;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fr;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ra;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ac;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Th;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pa;
extern KIM_SpeciesName const KIM_SPECIES_NAME_U;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Np;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Pu;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Am;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cm;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Bk;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cf;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Es;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fm;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Md;
extern KIM_SpeciesName const KIM_SPECIES_NAME_No;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Lr;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rf;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Db;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Sg;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Bh;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Hs;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Mt;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Ds;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Rg;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Cn;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Uut;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Fl;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Uup;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Lv;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Uus;
extern KIM_SpeciesName const KIM_SPECIES_NAME_Uuo;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user01;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user02;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user03;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user04;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user05;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user06;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user07;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user08;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user09;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user10;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user11;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user12;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user13;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user14;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user15;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user16;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user17;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user18;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user19;
extern KIM_SpeciesName const KIM_SPECIES_NAME_user20;

void KIM_SPECIES_NAME_GetNumberOfSpeciesNames(int * const numberOfSpeciesNames);
int KIM_SPECIES_NAME_GetSpeciesName(int const index,
                                    KIM_SpeciesName * const speciesName);

#endif  /* KIM_SPECIES_NAME_H_ */
