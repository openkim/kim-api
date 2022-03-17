//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
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
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <map>

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif

namespace KIM
{
// Order doesn't matter as long as all values are unique
namespace SPECIES_NAME
{
#include "KIM_SpeciesName.inc"
SpeciesName const electron(ID_electron);  // electron
SpeciesName const H(ID_H);  // Hydrogen
SpeciesName const He(ID_He);  // Helium
SpeciesName const Li(ID_Li);  // Lithium
SpeciesName const Be(ID_Be);  // Beryllium
SpeciesName const B(ID_B);  // Boron
SpeciesName const C(ID_C);  // Carbon
SpeciesName const N(ID_N);  // Nitrogen
SpeciesName const O(ID_O);  // Oxygen
SpeciesName const F(ID_F);  // Fluorine
SpeciesName const Ne(ID_Ne);  // Neon
SpeciesName const Na(ID_Na);  // Sodium
SpeciesName const Mg(ID_Mg);  // Magnesium
SpeciesName const Al(ID_Al);  // Aluminum
SpeciesName const Si(ID_Si);  // Silicon
SpeciesName const P(ID_P);  // Phosphorus
SpeciesName const S(ID_S);  // Sulfur
SpeciesName const Cl(ID_Cl);  // Chlorine
SpeciesName const Ar(ID_Ar);  // Argon
SpeciesName const K(ID_K);  // Potassium
SpeciesName const Ca(ID_Ca);  // Calcium
SpeciesName const Sc(ID_Sc);  // Scandium
SpeciesName const Ti(ID_Ti);  // Titanium
SpeciesName const V(ID_V);  // Vanadium
SpeciesName const Cr(ID_Cr);  // Chromium
SpeciesName const Mn(ID_Mn);  // Manganese
SpeciesName const Fe(ID_Fe);  // Iron
SpeciesName const Co(ID_Co);  // Cobalt
SpeciesName const Ni(ID_Ni);  // Nickel
SpeciesName const Cu(ID_Cu);  // Copper
SpeciesName const Zn(ID_Zn);  // Zinc
SpeciesName const Ga(ID_Ga);  // Gallium
SpeciesName const Ge(ID_Ge);  // Germanium
SpeciesName const As(ID_As);  // Arsenic
SpeciesName const Se(ID_Se);  // Selenium
SpeciesName const Br(ID_Br);  // Bromine
SpeciesName const Kr(ID_Kr);  // Krypton
SpeciesName const Rb(ID_Rb);  // Rubidium
SpeciesName const Sr(ID_Sr);  // Strontium
SpeciesName const Y(ID_Y);  // Yttrium
SpeciesName const Zr(ID_Zr);  // Zirconium
SpeciesName const Nb(ID_Nb);  // Niobium
SpeciesName const Mo(ID_Mo);  // Molybdenum
SpeciesName const Tc(ID_Tc);  // Technetium
SpeciesName const Ru(ID_Ru);  // Ruthenium
SpeciesName const Rh(ID_Rh);  // Rhodium
SpeciesName const Pd(ID_Pd);  // Palladium
SpeciesName const Ag(ID_Ag);  // Silver
SpeciesName const Cd(ID_Cd);  // Cadmium
SpeciesName const In(ID_In);  // Indium
SpeciesName const Sn(ID_Sn);  // Tin
SpeciesName const Sb(ID_Sb);  // Antimony
SpeciesName const Te(ID_Te);  // Tellurium
SpeciesName const I(ID_I);  // Iodine
SpeciesName const Xe(ID_Xe);  // Xenon
SpeciesName const Cs(ID_Cs);  // Cesium
SpeciesName const Ba(ID_Ba);  // Barium
SpeciesName const La(ID_La);  // Lanthanum
SpeciesName const Ce(ID_Ce);  // Cerium
SpeciesName const Pr(ID_Pr);  // Praseodymium
SpeciesName const Nd(ID_Nd);  // Neodymium
SpeciesName const Pm(ID_Pm);  // Promethium
SpeciesName const Sm(ID_Sm);  // Samarium
SpeciesName const Eu(ID_Eu);  // Europium
SpeciesName const Gd(ID_Gd);  // Gadolinium
SpeciesName const Tb(ID_Tb);  // Terbium
SpeciesName const Dy(ID_Dy);  // Dysprosium
SpeciesName const Ho(ID_Ho);  // Holmium
SpeciesName const Er(ID_Er);  // Erbium
SpeciesName const Tm(ID_Tm);  // Thulium
SpeciesName const Yb(ID_Yb);  // Ytterbium
SpeciesName const Lu(ID_Lu);  // Lutetium
SpeciesName const Hf(ID_Hf);  // Hafnium
SpeciesName const Ta(ID_Ta);  // Tantalum
SpeciesName const W(ID_W);  // Tungsten
SpeciesName const Re(ID_Re);  // Rhenium
SpeciesName const Os(ID_Os);  // Osmium
SpeciesName const Ir(ID_Ir);  // Iridium
SpeciesName const Pt(ID_Pt);  // Platinum
SpeciesName const Au(ID_Au);  // Gold
SpeciesName const Hg(ID_Hg);  // Mercury
SpeciesName const Tl(ID_Tl);  // Thallium
SpeciesName const Pb(ID_Pb);  // Lead
SpeciesName const Bi(ID_Bi);  // Bismuth
SpeciesName const Po(ID_Po);  // Polonium
SpeciesName const At(ID_At);  // Astatine
SpeciesName const Rn(ID_Rn);  // Radon
SpeciesName const Fr(ID_Fr);  // Francium
SpeciesName const Ra(ID_Ra);  // Radium
SpeciesName const Ac(ID_Ac);  // Actinium
SpeciesName const Th(ID_Th);  // Thorium
SpeciesName const Pa(ID_Pa);  // Protactinium
SpeciesName const U(ID_U);  // Uranium
SpeciesName const Np(ID_Np);  // Neptunium
SpeciesName const Pu(ID_Pu);  // Plutonium
SpeciesName const Am(ID_Am);  // Americium
SpeciesName const Cm(ID_Cm);  // Curium
SpeciesName const Bk(ID_Bk);  // Berkelium
SpeciesName const Cf(ID_Cf);  // Californium
SpeciesName const Es(ID_Es);  // Einsteinium
SpeciesName const Fm(ID_Fm);  // Fermium
SpeciesName const Md(ID_Md);  // Mendelevium
SpeciesName const No(ID_No);  // Nobelium
SpeciesName const Lr(ID_Lr);  // Lawrencium
SpeciesName const Rf(ID_Rf);  // Rutherfordium
SpeciesName const Db(ID_Db);  // Dubnium
SpeciesName const Sg(ID_Sg);  // Seaborgium
SpeciesName const Bh(ID_Bh);  // Bohrium
SpeciesName const Hs(ID_Hs);  // Hassium
SpeciesName const Mt(ID_Mt);  // Meitnerium
SpeciesName const Ds(ID_Ds);  // Darmstadtium
SpeciesName const Rg(ID_Rg);  // Roentgenium
SpeciesName const Cn(ID_Cn);  // Copernicium
SpeciesName const Nh(ID_Nh);  // Nihonium
SpeciesName const Fl(ID_Fl);  // Flerovium
SpeciesName const Mc(ID_Mc);  // Moscovium
SpeciesName const Lv(ID_Lv);  // Livermorium
SpeciesName const Ts(ID_Ts);  // Tennessien
SpeciesName const Og(ID_Og);  // Oganesson
SpeciesName const user01(ID_user01);  // user defined
SpeciesName const user02(ID_user02);  // user defined
SpeciesName const user03(ID_user03);  // user defined
SpeciesName const user04(ID_user04);  // user defined
SpeciesName const user05(ID_user05);  // user defined
SpeciesName const user06(ID_user06);  // user defined
SpeciesName const user07(ID_user07);  // user defined
SpeciesName const user08(ID_user08);  // user defined
SpeciesName const user09(ID_user09);  // user defined
SpeciesName const user10(ID_user10);  // user defined
SpeciesName const user11(ID_user11);  // user defined
SpeciesName const user12(ID_user12);  // user defined
SpeciesName const user13(ID_user13);  // user defined
SpeciesName const user14(ID_user14);  // user defined
SpeciesName const user15(ID_user15);  // user defined
SpeciesName const user16(ID_user16);  // user defined
SpeciesName const user17(ID_user17);  // user defined
SpeciesName const user18(ID_user18);  // user defined
SpeciesName const user19(ID_user19);  // user defined
SpeciesName const user20(ID_user20);  // user defined

namespace
{
typedef std::map<SpeciesName const, std::string, SPECIES_NAME::Comparator>
    StringMap;

StringMap const GetStringMap()
{
  StringMap m;
  m[electron] = "electron";
  m[H] = "H";
  m[He] = "He";
  m[Li] = "Li";
  m[Be] = "Be";
  m[B] = "B";
  m[C] = "C";
  m[N] = "N";
  m[O] = "O";
  m[F] = "F";
  m[Ne] = "Ne";
  m[Na] = "Na";
  m[Mg] = "Mg";
  m[Al] = "Al";
  m[Si] = "Si";
  m[P] = "P";
  m[S] = "S";
  m[Cl] = "Cl";
  m[Ar] = "Ar";
  m[K] = "K";
  m[Ca] = "Ca";
  m[Sc] = "Sc";
  m[Ti] = "Ti";
  m[V] = "V";
  m[Cr] = "Cr";
  m[Mn] = "Mn";
  m[Fe] = "Fe";
  m[Co] = "Co";
  m[Ni] = "Ni";
  m[Cu] = "Cu";
  m[Zn] = "Zn";
  m[Ga] = "Ga";
  m[Ge] = "Ge";
  m[As] = "As";
  m[Se] = "Se";
  m[Br] = "Br";
  m[Kr] = "Kr";
  m[Rb] = "Rb";
  m[Sr] = "Sr";
  m[Y] = "Y";
  m[Zr] = "Zr";
  m[Nb] = "Nb";
  m[Mo] = "Mo";
  m[Tc] = "Tc";
  m[Ru] = "Ru";
  m[Rh] = "Rh";
  m[Pd] = "Pd";
  m[Ag] = "Ag";
  m[Cd] = "Cd";
  m[In] = "In";
  m[Sn] = "Sn";
  m[Sb] = "Sb";
  m[Te] = "Te";
  m[I] = "I";
  m[Xe] = "Xe";
  m[Cs] = "Cs";
  m[Ba] = "Ba";
  m[La] = "La";
  m[Ce] = "Ce";
  m[Pr] = "Pr";
  m[Nd] = "Nd";
  m[Pm] = "Pm";
  m[Sm] = "Sm";
  m[Eu] = "Eu";
  m[Gd] = "Gd";
  m[Tb] = "Tb";
  m[Dy] = "Dy";
  m[Ho] = "Ho";
  m[Er] = "Er";
  m[Tm] = "Tm";
  m[Yb] = "Yb";
  m[Lu] = "Lu";
  m[Hf] = "Hf";
  m[Ta] = "Ta";
  m[W] = "W";
  m[Re] = "Re";
  m[Os] = "Os";
  m[Ir] = "Ir";
  m[Pt] = "Pt";
  m[Au] = "Au";
  m[Hg] = "Hg";
  m[Tl] = "Tl";
  m[Pb] = "Pb";
  m[Bi] = "Bi";
  m[Po] = "Po";
  m[At] = "At";
  m[Rn] = "Rn";
  m[Fr] = "Fr";
  m[Ra] = "Ra";
  m[Ac] = "Ac";
  m[Th] = "Th";
  m[Pa] = "Pa";
  m[U] = "U";
  m[Np] = "Np";
  m[Pu] = "Pu";
  m[Am] = "Am";
  m[Cm] = "Cm";
  m[Bk] = "Bk";
  m[Cf] = "Cf";
  m[Es] = "Es";
  m[Fm] = "Fm";
  m[Md] = "Md";
  m[No] = "No";
  m[Lr] = "Lr";
  m[Rf] = "Rf";
  m[Db] = "Db";
  m[Sg] = "Sg";
  m[Bh] = "Bh";
  m[Hs] = "Hs";
  m[Mt] = "Mt";
  m[Ds] = "Ds";
  m[Rg] = "Rg";
  m[Cn] = "Cn";
  m[Nh] = "Nh";
  m[Fl] = "Fl";
  m[Mc] = "Mc";
  m[Lv] = "Lv";
  m[Ts] = "Ts";
  m[Og] = "Og";
  m[user01] = "user01";
  m[user02] = "user02";
  m[user03] = "user03";
  m[user04] = "user04";
  m[user05] = "user05";
  m[user06] = "user06";
  m[user07] = "user07";
  m[user08] = "user08";
  m[user09] = "user09";
  m[user10] = "user10";
  m[user11] = "user11";
  m[user12] = "user12";
  m[user13] = "user13";
  m[user14] = "user14";
  m[user15] = "user15";
  m[user16] = "user16";
  m[user17] = "user17";
  m[user18] = "user18";
  m[user19] = "user19";
  m[user20] = "user20";
  return m;
}

StringMap const speciesNameToString = GetStringMap();
std::string const speciesNameUnknown("unknown");
}  // namespace


void GetNumberOfSpeciesNames(int * const numberOfSpeciesNames)
{
  *numberOfSpeciesNames = speciesNameToString.size();
}

int GetSpeciesName(int const index, SpeciesName * const speciesName)
{
  int numberOfSpeciesNames;
  GetNumberOfSpeciesNames(&numberOfSpeciesNames);
  if ((index < 0) || (index >= numberOfSpeciesNames)) return true;

  SPECIES_NAME::StringMap::const_iterator iter = speciesNameToString.begin();
  for (int i = 0; i < index; ++i) ++iter;
  *speciesName = iter->first;
  return false;  // no error
}
}  // namespace SPECIES_NAME

// implementation of SpeciesName
SpeciesName::SpeciesName() {}
SpeciesName::SpeciesName(int const id) : speciesNameID(id) {}
SpeciesName::SpeciesName(std::string const & str)
{
  speciesNameID = -1;
  for (SPECIES_NAME::StringMap::const_iterator iter
       = SPECIES_NAME::speciesNameToString.begin();
       iter != SPECIES_NAME::speciesNameToString.end();
       ++iter)
  {
    if (iter->second == str)
    {
      speciesNameID = (iter->first).speciesNameID;
      break;
    }
  }
}

bool SpeciesName::Known() const
{
  int numberOfSpeciesNames;
  SPECIES_NAME::GetNumberOfSpeciesNames(&numberOfSpeciesNames);

  for (int i = 0; i < numberOfSpeciesNames; ++i)
  {
    SpeciesName specName;
    SPECIES_NAME::GetSpeciesName(i, &specName);

    if (*this == specName) { return true; }
  }

  return false;
}

bool SpeciesName::operator==(SpeciesName const & rhs) const
{
  return speciesNameID == rhs.speciesNameID;
}
bool SpeciesName::operator!=(SpeciesName const & rhs) const
{
  return speciesNameID != rhs.speciesNameID;
}

std::string const & SpeciesName::ToString() const
{
  SPECIES_NAME::StringMap::const_iterator iter
      = SPECIES_NAME::speciesNameToString.find(*this);
  if (iter == SPECIES_NAME::speciesNameToString.end())
    return SPECIES_NAME::speciesNameUnknown;
  else
    return iter->second;
}
}  // namespace KIM
