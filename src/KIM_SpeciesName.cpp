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
#include "KIM_SpeciesName.hpp"
#endif

namespace KIM
{

SpeciesName::SpeciesName() : speciesID(0){}
SpeciesName::SpeciesName(int const id) : speciesID(id){}
bool SpeciesName::operator==(SpeciesName const & rhs) const
{return speciesID==rhs.speciesID;}
bool SpeciesName::operator!=(SpeciesName const & rhs) const
{return speciesID!=rhs.speciesID;}

std::string SpeciesName::string() const
{
  if (*this == SPECIES_NAME::electron) return "electron";
  else if (*this == SPECIES_NAME::H) return "H";
  else if (*this == SPECIES_NAME::He) return "He";
  else if (*this == SPECIES_NAME::Li) return "Li";
  else if (*this == SPECIES_NAME::Be) return "Be";
  else if (*this == SPECIES_NAME::B) return "B";
  else if (*this == SPECIES_NAME::C) return "C";
  else if (*this == SPECIES_NAME::N) return "N";
  else if (*this == SPECIES_NAME::O) return "O";
  else if (*this == SPECIES_NAME::F) return "F";
  else if (*this == SPECIES_NAME::Ne) return "Ne";
  else if (*this == SPECIES_NAME::Na) return "Na";
  else if (*this == SPECIES_NAME::Mg) return "Mg";
  else if (*this == SPECIES_NAME::Al) return "Al";
  else if (*this == SPECIES_NAME::Si) return "Si";
  else if (*this == SPECIES_NAME::P) return "P";
  else if (*this == SPECIES_NAME::S) return "S";
  else if (*this == SPECIES_NAME::Cl) return "Cl";
  else if (*this == SPECIES_NAME::Ar) return "Ar";
  else if (*this == SPECIES_NAME::K) return "K";
  else if (*this == SPECIES_NAME::Ca) return "Ca";
  else if (*this == SPECIES_NAME::Sc) return "Sc";
  else if (*this == SPECIES_NAME::Ti) return "Ti";
  else if (*this == SPECIES_NAME::V) return "V";
  else if (*this == SPECIES_NAME::Cr) return "Cr";
  else if (*this == SPECIES_NAME::Mn) return "Mn";
  else if (*this == SPECIES_NAME::Fe) return "Fe";
  else if (*this == SPECIES_NAME::Co) return "Co";
  else if (*this == SPECIES_NAME::Ni) return "Ni";
  else if (*this == SPECIES_NAME::Cu) return "Cu";
  else if (*this == SPECIES_NAME::Zn) return "Zn";
  else if (*this == SPECIES_NAME::Ga) return "Ga";
  else if (*this == SPECIES_NAME::Ge) return "Ge";
  else if (*this == SPECIES_NAME::As) return "As";
  else if (*this == SPECIES_NAME::Se) return "Se";
  else if (*this == SPECIES_NAME::Br) return "Br";
  else if (*this == SPECIES_NAME::Kr) return "Kr";
  else if (*this == SPECIES_NAME::Rb) return "Rb";
  else if (*this == SPECIES_NAME::Sr) return "Sr";
  else if (*this == SPECIES_NAME::Y) return "Y";
  else if (*this == SPECIES_NAME::Zr) return "Zr";
  else if (*this == SPECIES_NAME::Nb) return "Nb";
  else if (*this == SPECIES_NAME::Mo) return "Mo";
  else if (*this == SPECIES_NAME::Tc) return "Tc";
  else if (*this == SPECIES_NAME::Ru) return "Ru";
  else if (*this == SPECIES_NAME::Rh) return "Rh";
  else if (*this == SPECIES_NAME::Pd) return "Pd";
  else if (*this == SPECIES_NAME::Ag) return "Ag";
  else if (*this == SPECIES_NAME::Cd) return "Cd";
  else if (*this == SPECIES_NAME::In) return "In";
  else if (*this == SPECIES_NAME::Sn) return "Sn";
  else if (*this == SPECIES_NAME::Sb) return "Sb";
  else if (*this == SPECIES_NAME::Te) return "Te";
  else if (*this == SPECIES_NAME::I) return "I";
  else if (*this == SPECIES_NAME::Xe) return "Xe";
  else if (*this == SPECIES_NAME::Cs) return "Cs";
  else if (*this == SPECIES_NAME::Ba) return "Ba";
  else if (*this == SPECIES_NAME::La) return "La";
  else if (*this == SPECIES_NAME::Ce) return "Ce";
  else if (*this == SPECIES_NAME::Pr) return "Pr";
  else if (*this == SPECIES_NAME::Nd) return "Nd";
  else if (*this == SPECIES_NAME::Pm) return "Pm";
  else if (*this == SPECIES_NAME::Sm) return "Sm";
  else if (*this == SPECIES_NAME::Eu) return "Eu";
  else if (*this == SPECIES_NAME::Gd) return "Gd";
  else if (*this == SPECIES_NAME::Tb) return "Tb";
  else if (*this == SPECIES_NAME::Dy) return "Dy";
  else if (*this == SPECIES_NAME::Ho) return "Ho";
  else if (*this == SPECIES_NAME::Er) return "Er";
  else if (*this == SPECIES_NAME::Tm) return "Tm";
  else if (*this == SPECIES_NAME::Yb) return "Yb";
  else if (*this == SPECIES_NAME::Lu) return "Lu";
  else if (*this == SPECIES_NAME::Hf) return "Hf";
  else if (*this == SPECIES_NAME::Ta) return "Ta";
  else if (*this == SPECIES_NAME::W) return "W";
  else if (*this == SPECIES_NAME::Re) return "Re";
  else if (*this == SPECIES_NAME::Os) return "Os";
  else if (*this == SPECIES_NAME::Ir) return "Ir";
  else if (*this == SPECIES_NAME::Pt) return "Pt";
  else if (*this == SPECIES_NAME::Au) return "Au";
  else if (*this == SPECIES_NAME::Hg) return "Hg";
  else if (*this == SPECIES_NAME::Tl) return "Tl";
  else if (*this == SPECIES_NAME::Pb) return "Pb";
  else if (*this == SPECIES_NAME::Bi) return "Bi";
  else if (*this == SPECIES_NAME::Po) return "Po";
  else if (*this == SPECIES_NAME::At) return "At";
  else if (*this == SPECIES_NAME::Rn) return "Rn";
  else if (*this == SPECIES_NAME::Fr) return "Fr";
  else if (*this == SPECIES_NAME::Ra) return "Ra";
  else if (*this == SPECIES_NAME::Ac) return "Ac";
  else if (*this == SPECIES_NAME::Th) return "Th";
  else if (*this == SPECIES_NAME::Pa) return "Pa";
  else if (*this == SPECIES_NAME::U) return "U";
  else if (*this == SPECIES_NAME::Np) return "Np";
  else if (*this == SPECIES_NAME::Pu) return "Pu";
  else if (*this == SPECIES_NAME::Am) return "Am";
  else if (*this == SPECIES_NAME::Cm) return "Cm";
  else if (*this == SPECIES_NAME::Bk) return "Bk";
  else if (*this == SPECIES_NAME::Cf) return "Cf";
  else if (*this == SPECIES_NAME::Es) return "Es";
  else if (*this == SPECIES_NAME::Fm) return "Fm";
  else if (*this == SPECIES_NAME::Md) return "Md";
  else if (*this == SPECIES_NAME::No) return "No";
  else if (*this == SPECIES_NAME::Lr) return "Lr";
  else if (*this == SPECIES_NAME::Rf) return "Rf";
  else if (*this == SPECIES_NAME::Db) return "Db";
  else if (*this == SPECIES_NAME::Sg) return "Sg";
  else if (*this == SPECIES_NAME::Bh) return "Bh";
  else if (*this == SPECIES_NAME::Hs) return "Hs";
  else if (*this == SPECIES_NAME::Mt) return "Mt";
  else if (*this == SPECIES_NAME::Ds) return "Ds";
  else if (*this == SPECIES_NAME::Rg) return "Rg";
  else if (*this == SPECIES_NAME::Cn) return "Cn";
  else if (*this == SPECIES_NAME::Uut) return "Uut";
  else if (*this == SPECIES_NAME::Fl) return "Fl";
  else if (*this == SPECIES_NAME::Uup) return "Uup";
  else if (*this == SPECIES_NAME::Lv) return "Lv";
  else if (*this == SPECIES_NAME::Uus) return "Uus";
  else if (*this == SPECIES_NAME::Uuo) return "Uuo";
  else if (*this == SPECIES_NAME::user01) return "user01";
  else if (*this == SPECIES_NAME::user02) return "user02";
  else if (*this == SPECIES_NAME::user03) return "user03";
  else if (*this == SPECIES_NAME::user04) return "user04";
  else if (*this == SPECIES_NAME::user05) return "user05";
  else if (*this == SPECIES_NAME::user06) return "user06";
  else if (*this == SPECIES_NAME::user07) return "user07";
  else if (*this == SPECIES_NAME::user08) return "user08";
  else if (*this == SPECIES_NAME::user09) return "user09";
  else if (*this == SPECIES_NAME::user10) return "user10";
  else if (*this == SPECIES_NAME::user11) return "user11";
  else if (*this == SPECIES_NAME::user12) return "user12";
  else if (*this == SPECIES_NAME::user13) return "user13";
  else if (*this == SPECIES_NAME::user14) return "user14";
  else if (*this == SPECIES_NAME::user15) return "user15";
  else if (*this == SPECIES_NAME::user16) return "user16";
  else if (*this == SPECIES_NAME::user17) return "user17";
  else if (*this == SPECIES_NAME::user18) return "user18";
  else if (*this == SPECIES_NAME::user19) return "user19";
  else if (*this == SPECIES_NAME::user20) return "user20";
  else return "unknown";
}

namespace SPECIES_NAME
{
SpeciesName const electron(0);  // electron
SpeciesName const H(1);         // Hydrogen
SpeciesName const He(2);        // Helium
SpeciesName const Li(3);        // Lithium
SpeciesName const Be(4);        // Beryllium
SpeciesName const B(5);         // Boron
SpeciesName const C(6);         // Carbon
SpeciesName const N(7);         // Nitrogen
SpeciesName const O(8);         // Oxygen
SpeciesName const F(9);         // Fluorine
SpeciesName const Ne(10);       // Neon
SpeciesName const Na(11);       // Sodium
SpeciesName const Mg(12);       // Magnesium
SpeciesName const Al(13);       // Aluminum
SpeciesName const Si(14);       // Silicon
SpeciesName const P(15);        // Phosphorus
SpeciesName const S(16);        // Sulfur
SpeciesName const Cl(17);       // Chlorine
SpeciesName const Ar(18);       // Argon
SpeciesName const K(19);        // Potassium
SpeciesName const Ca(20);       // Calcium
SpeciesName const Sc(21);       // Scandium
SpeciesName const Ti(22);       // Titanium
SpeciesName const V(23);        // Vanadium
SpeciesName const Cr(24);       // Chromium
SpeciesName const Mn(25);       // Manganese
SpeciesName const Fe(26);       // Iron
SpeciesName const Co(27);       // Cobalt
SpeciesName const Ni(28);       // Nickel
SpeciesName const Cu(29);       // Copper
SpeciesName const Zn(30);       // Zinc
SpeciesName const Ga(31);       // Gallium
SpeciesName const Ge(32);       // Germanium
SpeciesName const As(33);       // Arsenic
SpeciesName const Se(34);       // Selenium
SpeciesName const Br(35);       // Bromine
SpeciesName const Kr(36);       // Krypton
SpeciesName const Rb(37);       // Rubidium
SpeciesName const Sr(38);       // Strontium
SpeciesName const Y(39);        // Yttrium
SpeciesName const Zr(40);       // Zirconium
SpeciesName const Nb(41);       // Niobium
SpeciesName const Mo(42);       // Molybdenum
SpeciesName const Tc(43);       // Technetium
SpeciesName const Ru(44);       // Ruthenium
SpeciesName const Rh(45);       // Rhodium
SpeciesName const Pd(46);       // Palladium
SpeciesName const Ag(47);       // Silver
SpeciesName const Cd(48);       // Cadmium
SpeciesName const In(49);       // Indium
SpeciesName const Sn(50);       // Tin
SpeciesName const Sb(51);       // Antimony
SpeciesName const Te(52);       // Tellurium
SpeciesName const I(53);        // Iodine
SpeciesName const Xe(54);       // Xenon
SpeciesName const Cs(55);       // Cesium
SpeciesName const Ba(56);       // Barium
SpeciesName const La(57);       // Lanthanum
SpeciesName const Ce(58);       // Cerium
SpeciesName const Pr(59);       // Praseodymium
SpeciesName const Nd(60);       // Neodymium
SpeciesName const Pm(61);       // Promethium
SpeciesName const Sm(62);       // Samarium
SpeciesName const Eu(63);       // Europium
SpeciesName const Gd(64);       // Gadolinium
SpeciesName const Tb(65);       // Terbium
SpeciesName const Dy(66);       // Dysprosium
SpeciesName const Ho(67);       // Holmium
SpeciesName const Er(68);       // Erbium
SpeciesName const Tm(69);       // Thulium
SpeciesName const Yb(70);       // Ytterbium
SpeciesName const Lu(71);       // Lutetium
SpeciesName const Hf(72);       // Hafnium
SpeciesName const Ta(73);       // Tantalum
SpeciesName const W(74);        // Tungsten
SpeciesName const Re(75);       // Rhenium
SpeciesName const Os(76);       // Osmium
SpeciesName const Ir(77);       // Iridium
SpeciesName const Pt(78);       // Platinum
SpeciesName const Au(79);       // Gold
SpeciesName const Hg(80);       // Mercury
SpeciesName const Tl(81);       // Thallium
SpeciesName const Pb(82);       // Lead
SpeciesName const Bi(83);       // Bismuth
SpeciesName const Po(84);       // Polonium
SpeciesName const At(85);       // Astatine
SpeciesName const Rn(86);       // Radon
SpeciesName const Fr(87);       // Francium
SpeciesName const Ra(88);       // Radium
SpeciesName const Ac(89);       // Actinium
SpeciesName const Th(90);       // Thorium
SpeciesName const Pa(91);       // Protactinium
SpeciesName const U(92);        // Uranium
SpeciesName const Np(93);       // Neptunium
SpeciesName const Pu(94);       // Plutonium
SpeciesName const Am(95);       // Americium
SpeciesName const Cm(96);       // Curium
SpeciesName const Bk(97);       // Berkelium
SpeciesName const Cf(98);       // Californium
SpeciesName const Es(99);       // Einsteinium
SpeciesName const Fm(100);      // Fermium
SpeciesName const Md(101);      // Mendelevium
SpeciesName const No(102);      // Nobelium
SpeciesName const Lr(103);      // Lawrencium
SpeciesName const Rf(104);      // Rutherfordium
SpeciesName const Db(105);      // Dubnium
SpeciesName const Sg(106);      // Seaborgium
SpeciesName const Bh(107);      // Bohrium
SpeciesName const Hs(108);      // Hassium
SpeciesName const Mt(109);      // Meitnerium
SpeciesName const Ds(110);      // Darmstadtium
SpeciesName const Rg(111);      // Roentgenium
SpeciesName const Cn(112);      // Copernicium
SpeciesName const Uut(113);     // Ununtrium
SpeciesName const Fl(114);      // Flerovium
SpeciesName const Uup(115);     // Ununpentium
SpeciesName const Lv(116);      // Livermorium
SpeciesName const Uus(117);     // Ununseptium
SpeciesName const Uuo(118);     // Ununoctium
SpeciesName const user01(201);  // user defined
SpeciesName const user02(202);  // user defined
SpeciesName const user03(203);  // user defined
SpeciesName const user04(204);  // user defined
SpeciesName const user05(205);  // user defined
SpeciesName const user06(206);  // user defined
SpeciesName const user07(207);  // user defined
SpeciesName const user08(208);  // user defined
SpeciesName const user09(209);  // user defined
SpeciesName const user10(210);  // user defined
SpeciesName const user11(211);  // user defined
SpeciesName const user12(212);  // user defined
SpeciesName const user13(213);  // user defined
SpeciesName const user14(214);  // user defined
SpeciesName const user15(215);  // user defined
SpeciesName const user16(216);  // user defined
SpeciesName const user17(217);  // user defined
SpeciesName const user18(218);  // user defined
SpeciesName const user19(219);  // user defined
SpeciesName const user20(220);  // user defined


void get_number_of_species(int * const numberOfSpecies)
{
  *numberOfSpecies = 139;
}

int get_species(int const index, SpeciesName * const speciesName)
{
  switch (index)
  {
    case 0:
      *speciesName = electron; // electron
      break;
    case 1:
      *speciesName = H;        // Hydrogen
      break;
    case 2:
      *speciesName = He;       // Helium
      break;
    case 3:
      *speciesName = Li;       // Lithium
      break;
    case 4:
      *speciesName = Be;       // Beryllium
      break;
    case 5:
      *speciesName = B;        // Boron
      break;
    case 6:
      *speciesName = C;        // Carbon
      break;
    case 7:
      *speciesName = N;        // Nitrogen
      break;
    case 8:
      *speciesName = O;        // Oxygen
      break;
    case 9:
      *speciesName = F;        // Fluorine
      break;
    case 10:
      *speciesName = Ne;       // Neon
      break;
    case 11:
      *speciesName = Na;       // Sodium
      break;
    case 12:
      *speciesName = Mg;       // Magnesium
      break;
    case 13:
      *speciesName = Al;       // Aluminum
      break;
    case 14:
      *speciesName = Si;       // Silicon
      break;
    case 15:
      *speciesName = P;        // Phosphorus
      break;
    case 16:
      *speciesName = S;        // Sulfur
      break;
    case 17:
      *speciesName = Cl;       // Chlorine
      break;
    case 18:
      *speciesName = Ar;       // Argon
      break;
    case 19:
      *speciesName = K;        // Potassium
      break;
    case 20:
      *speciesName = Ca;       // Calcium
      break;
    case 21:
      *speciesName = Sc;       // Scandium
      break;
    case 22:
      *speciesName = Ti;       // Titanium
      break;
    case 23:
      *speciesName = V;        // Vanadium
      break;
    case 24:
      *speciesName = Cr;       // Chromium
      break;
    case 25:
      *speciesName = Mn;       // Manganese
      break;
    case 26:
      *speciesName = Fe;       // Iron
      break;
    case 27:
      *speciesName = Co;       // Cobalt
      break;
    case 28:
      *speciesName = Ni;       // Nickel
      break;
    case 29:
      *speciesName = Cu;       // Copper
      break;
    case 30:
      *speciesName = Zn;       // Zinc
      break;
    case 31:
      *speciesName = Ga;       // Gallium
      break;
    case 32:
      *speciesName = Ge;       // Germanium
      break;
    case 33:
      *speciesName = As;       // Arsenic
      break;
    case 34:
      *speciesName = Se;       // Selenium
      break;
    case 35:
      *speciesName = Br;       // Bromine
      break;
    case 36:
      *speciesName = Kr;       // Krypton
      break;
    case 37:
      *speciesName = Rb;       // Rubidium
      break;
    case 38:
      *speciesName = Sr;       // Strontium
      break;
    case 39:
      *speciesName = Y;        // Yttrium
      break;
    case 40:
      *speciesName = Zr;       // Zirconium
      break;
    case 41:
      *speciesName = Nb;       // Niobium
      break;
    case 42:
      *speciesName = Mo;       // Molybdenum
      break;
    case 43:
      *speciesName = Tc;       // Technetium
      break;
    case 44:
      *speciesName = Ru;       // Ruthenium
      break;
    case 45:
      *speciesName = Rh;       // Rhodium
      break;
    case 46:
      *speciesName = Pd;       // Palladium
      break;
    case 47:
      *speciesName = Ag;       // Silver
      break;
    case 48:
      *speciesName = Cd;       // Cadmium
      break;
    case 49:
      *speciesName = In;       // Indium
      break;
    case 50:
      *speciesName = Sn;       // Tin
      break;
    case 51:
      *speciesName = Sb;       // Antimony
      break;
    case 52:
      *speciesName = Te;       // Tellurium
      break;
    case 53:
      *speciesName = I;        // Iodine
      break;
    case 54:
      *speciesName = Xe;       // Xenon
      break;
    case 55:
      *speciesName = Cs;       // Cesium
      break;
    case 56:
      *speciesName = Ba;       // Barium
      break;
    case 57:
      *speciesName = La;       // Lanthanum
      break;
    case 58:
      *speciesName = Ce;       // Cerium
      break;
    case 59:
      *speciesName = Pr;       // Praseodymium
      break;
    case 60:
      *speciesName = Nd;       // Neodymium
      break;
    case 61:
      *speciesName = Pm;       // Promethium
      break;
    case 62:
      *speciesName = Sm;       // Samarium
      break;
    case 63:
      *speciesName = Eu;       // Europium
      break;
    case 64:
      *speciesName = Gd;       // Gadolinium
      break;
    case 65:
      *speciesName = Tb;       // Terbium
      break;
    case 66:
      *speciesName = Dy;       // Dysprosium
      break;
    case 67:
      *speciesName = Ho;       // Holmium
      break;
    case 68:
      *speciesName = Er;       // Erbium
      break;
    case 69:
      *speciesName = Tm;       // Thulium
      break;
    case 70:
      *speciesName = Yb;       // Ytterbium
      break;
    case 71:
      *speciesName = Lu;       // Lutetium
      break;
    case 72:
      *speciesName = Hf;       // Hafnium
      break;
    case 73:
      *speciesName = Ta;       // Tantalum
      break;
    case 74:
      *speciesName = W;        // Tungsten
      break;
    case 75:
      *speciesName = Re;       // Rhenium
      break;
    case 76:
      *speciesName = Os;       // Osmium
      break;
    case 77:
      *speciesName = Ir;       // Iridium
      break;
    case 78:
      *speciesName = Pt;       // Platinum
      break;
    case 79:
      *speciesName = Au;       // Gold
      break;
    case 80:
      *speciesName = Hg;       // Mercury
      break;
    case 81:
      *speciesName = Tl;       // Thallium
      break;
    case 82:
      *speciesName = Pb;       // Lead
      break;
    case 83:
      *speciesName = Bi;       // Bismuth
      break;
    case 84:
      *speciesName = Po;       // Polonium
      break;
    case 85:
      *speciesName = At;       // Astatine
      break;
    case 86:
      *speciesName = Rn;       // Radon
      break;
    case 87:
      *speciesName = Fr;       // Francium
      break;
    case 88:
      *speciesName = Ra;       // Radium
      break;
    case 89:
      *speciesName = Ac;       // Actinium
      break;
    case 90:
      *speciesName = Th;       // Thorium
      break;
    case 91:
      *speciesName = Pa;       // Protactinium
      break;
    case 92:
      *speciesName = U;        // Uranium
      break;
    case 93:
      *speciesName = Np;       // Neptunium
      break;
    case 94:
      *speciesName = Pu;       // Plutonium
      break;
    case 95:
      *speciesName = Am;       // Americium
      break;
    case 96:
      *speciesName = Cm;       // Curium
      break;
    case 97:
      *speciesName = Bk;       // Berkelium
      break;
    case 98:
      *speciesName = Cf;       // Californium
      break;
    case 99:
      *speciesName = Es;       // Einsteinium
      break;
    case 100:
      *speciesName = Fm;       // Fermium
      break;
    case 101:
      *speciesName = Md;       // Mendelevium
      break;
    case 102:
      *speciesName = No;       // Nobelium
      break;
    case 103:
      *speciesName = Lr;       // Lawrencium
      break;
    case 104:
      *speciesName = Rf;       // Rutherfordium
      break;
    case 105:
      *speciesName = Db;       // Dubnium
      break;
    case 106:
      *speciesName = Sg;       // Seaborgium
      break;
    case 107:
      *speciesName = Bh;       // Bohrium
      break;
    case 108:
      *speciesName = Hs;       // Hassium
      break;
    case 109:
      *speciesName = Mt;       // Meitnerium
      break;
    case 110:
      *speciesName = Ds;       // Darmstadtium
      break;
    case 111:
      *speciesName = Rg;       // Roentgenium
      break;
    case 112:
      *speciesName = Cn;       // Copernicium
      break;
    case 113:
      *speciesName = Uut;      // Ununtrium
      break;
    case 114:
      *speciesName = Fl;       // Flerovium
      break;
    case 115:
      *speciesName = Uup;      // Ununpentium
      break;
    case 116:
      *speciesName = Lv;       // Livermorium
      break;
    case 117:
      *speciesName = Uus;      // Ununseptium
      break;
    case 118:
      *speciesName = Uuo;      // Ununoctium
      break;
    case 119:
      *speciesName = user01;   // user defined
      break;
    case 120:
      *speciesName = user02;   // user defined
      break;
    case 121:
      *speciesName = user03;   // user defined
      break;
    case 122:
      *speciesName = user04;   // user defined
      break;
    case 123:
      *speciesName = user05;   // user defined
      break;
    case 124:
      *speciesName = user06;   // user defined
      break;
    case 125:
      *speciesName = user07;   // user defined
      break;
    case 126:
      *speciesName = user08;   // user defined
      break;
    case 127:
      *speciesName = user09;   // user defined
      break;
    case 128:
      *speciesName = user10;   // user defined
      break;
    case 129:
      *speciesName = user11;   // user defined
      break;
    case 130:
      *speciesName = user12;   // user defined
      break;
    case 131:
      *speciesName = user13;   // user defined
      break;
    case 132:
      *speciesName = user14;   // user defined
      break;
    case 133:
      *speciesName = user15;   // user defined
      break;
    case 134:
      *speciesName = user16;   // user defined
      break;
    case 135:
      *speciesName = user17;   // user defined
      break;
    case 136:
      *speciesName = user18;   // user defined
      break;
    case 137:
      *speciesName = user19;   // user defined
      break;
    case 138:
      *speciesName = user20;   // user defined
      break;
    default:
      return true;  // invalid index
      break;
  }

  return false;  // no error
}

}  // namespace SPECIES_NAME
}  // namespace KIM
