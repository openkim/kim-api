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
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
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

namespace KIM
{

class SpeciesName
{
  int speciesID;
 public:
  SpeciesName();
  SpeciesName(int const id);
  bool operator==(SpeciesName const & rhs) const;
  bool operator!=(SpeciesName const & rhs) const;
};

namespace SPECIES_NAME
{
extern SpeciesName const electron;
extern SpeciesName const H;
extern SpeciesName const He;
extern SpeciesName const Li;
extern SpeciesName const Be;
extern SpeciesName const B;
extern SpeciesName const C;
extern SpeciesName const N;
extern SpeciesName const O;
extern SpeciesName const F;
extern SpeciesName const Ne;
extern SpeciesName const Na;
extern SpeciesName const Mg;
extern SpeciesName const Al;
extern SpeciesName const Si;
extern SpeciesName const P;
extern SpeciesName const S;
extern SpeciesName const Cl;
extern SpeciesName const Ar;
extern SpeciesName const K;
extern SpeciesName const Ca;
extern SpeciesName const Sc;
extern SpeciesName const Ti;
extern SpeciesName const V;
extern SpeciesName const Cr;
extern SpeciesName const Mn;
extern SpeciesName const Fe;
extern SpeciesName const Co;
extern SpeciesName const Ni;
extern SpeciesName const Cu;
extern SpeciesName const Zn;
extern SpeciesName const Ga;
extern SpeciesName const Ge;
extern SpeciesName const As;
extern SpeciesName const Se;
extern SpeciesName const Br;
extern SpeciesName const Kr;
extern SpeciesName const Rb;
extern SpeciesName const Sr;
extern SpeciesName const Y;
extern SpeciesName const Zr;
extern SpeciesName const Nb;
extern SpeciesName const Mo;
extern SpeciesName const Tc;
extern SpeciesName const Ru;
extern SpeciesName const Rh;
extern SpeciesName const Pd;
extern SpeciesName const Ag;
extern SpeciesName const Cd;
extern SpeciesName const In;
extern SpeciesName const Sn;
extern SpeciesName const Sb;
extern SpeciesName const Te;
extern SpeciesName const I;
extern SpeciesName const Xe;
extern SpeciesName const Cs;
extern SpeciesName const Ba;
extern SpeciesName const La;
extern SpeciesName const Ce;
extern SpeciesName const Pr;
extern SpeciesName const Nd;
extern SpeciesName const Pm;
extern SpeciesName const Sm;
extern SpeciesName const Eu;
extern SpeciesName const Gd;
extern SpeciesName const Tb;
extern SpeciesName const Dy;
extern SpeciesName const Ho;
extern SpeciesName const Er;
extern SpeciesName const Tm;
extern SpeciesName const Yb;
extern SpeciesName const Lu;
extern SpeciesName const Hf;
extern SpeciesName const Ta;
extern SpeciesName const W;
extern SpeciesName const Re;
extern SpeciesName const Os;
extern SpeciesName const Ir;
extern SpeciesName const Pt;
extern SpeciesName const Au;
extern SpeciesName const Hg;
extern SpeciesName const Tl;
extern SpeciesName const Pb;
extern SpeciesName const Bi;
extern SpeciesName const Po;
extern SpeciesName const At;
extern SpeciesName const Rn;
extern SpeciesName const Fr;
extern SpeciesName const Ra;
extern SpeciesName const Ac;
extern SpeciesName const Th;
extern SpeciesName const Pa;
extern SpeciesName const U;
extern SpeciesName const Np;
extern SpeciesName const Pu;
extern SpeciesName const Am;
extern SpeciesName const Cm;
extern SpeciesName const Bk;
extern SpeciesName const Cf;
extern SpeciesName const Es;
extern SpeciesName const Fm;
extern SpeciesName const Md;
extern SpeciesName const No;
extern SpeciesName const Lr;
extern SpeciesName const Rf;
extern SpeciesName const Db;
extern SpeciesName const Sg;
extern SpeciesName const Bh;
extern SpeciesName const Hs;
extern SpeciesName const Mt;
extern SpeciesName const Ds;
extern SpeciesName const Rg;
extern SpeciesName const Cn;
extern SpeciesName const Uut;
extern SpeciesName const Fl;
extern SpeciesName const Uup;
extern SpeciesName const Lv;
extern SpeciesName const Uus;
extern SpeciesName const Uuo;
extern SpeciesName const user01;
extern SpeciesName const user02;
extern SpeciesName const user03;
extern SpeciesName const user04;
extern SpeciesName const user05;
extern SpeciesName const user06;
extern SpeciesName const user07;
extern SpeciesName const user08;
extern SpeciesName const user09;
extern SpeciesName const user10;
extern SpeciesName const user11;
extern SpeciesName const user12;
extern SpeciesName const user13;
extern SpeciesName const user14;
extern SpeciesName const user15;
extern SpeciesName const user16;
extern SpeciesName const user17;
extern SpeciesName const user18;
extern SpeciesName const user19;
extern SpeciesName const user20;
extern SpeciesName const End;
}  // namespace SPECIES_NAME

}  // namespace KIM
#endif  // KIM_SPECIES_NAME_HPP_
