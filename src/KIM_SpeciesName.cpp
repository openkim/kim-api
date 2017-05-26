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
SpeciesName const End(32000);
}  // namespace SPECIES_NAME
}  // namespace KIM
