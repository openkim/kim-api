//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common
// Development and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name
// LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner].
// All rights reserved.
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
extern "C"
{
#ifndef KIM_SPECIES_NAME_H_
#include "KIM_SpeciesName.h"
#endif
}  // extern "C"

namespace
{
KIM::SpeciesName const makeSpeciesNameCpp(KIM_SpeciesName const speciesName)
{
  KIM::SpeciesName const * const speciesNameCpp
      = reinterpret_cast<KIM::SpeciesName const * const>(&speciesName);
  return *speciesNameCpp;
}

KIM_SpeciesName const makeSpeciesNameC(KIM::SpeciesName speciesName)
{
  KIM_SpeciesName const * const speciesNameC
      = reinterpret_cast<KIM_SpeciesName const * const>(&speciesName);
  return *speciesNameC;
}
}  // namespace

extern "C"
{
KIM_SpeciesName KIM_SpeciesNameFromString(char const * const str)
{
  return makeSpeciesNameC(KIM::SpeciesName(std::string(str)));
}

int KIM_SpeciesNameEqual(KIM_SpeciesName const left,
                         KIM_SpeciesName const right)
{
  return (left.speciesNameID == right.speciesNameID);
}

int KIM_SpeciesNameNotEqual(KIM_SpeciesName const left,
                            KIM_SpeciesName const right)
{
  return (!KIM_SpeciesNameEqual(left, right));
}

char const * const KIM_SpeciesNameString(KIM_SpeciesName const speciesName)
{
  return (makeSpeciesNameCpp(speciesName)).String().c_str();
}


KIM_SpeciesName const KIM_SPECIES_NAME_electron = {0};  // electron
KIM_SpeciesName const KIM_SPECIES_NAME_H = {1};         // Hydrogen
KIM_SpeciesName const KIM_SPECIES_NAME_He = {2};        // Helium
KIM_SpeciesName const KIM_SPECIES_NAME_Li = {3};        // Lithium
KIM_SpeciesName const KIM_SPECIES_NAME_Be = {4};        // Beryllium
KIM_SpeciesName const KIM_SPECIES_NAME_B = {5};         // Boron
KIM_SpeciesName const KIM_SPECIES_NAME_C = {6};         // Carbon
KIM_SpeciesName const KIM_SPECIES_NAME_N = {7};         // Nitrogen
KIM_SpeciesName const KIM_SPECIES_NAME_O = {8};         // Oxygen
KIM_SpeciesName const KIM_SPECIES_NAME_F = {9};         // Fluorine
KIM_SpeciesName const KIM_SPECIES_NAME_Ne = {10};       // Neon
KIM_SpeciesName const KIM_SPECIES_NAME_Na = {11};       // Sodium
KIM_SpeciesName const KIM_SPECIES_NAME_Mg = {12};       // Magnesium
KIM_SpeciesName const KIM_SPECIES_NAME_Al = {13};       // Aluminum
KIM_SpeciesName const KIM_SPECIES_NAME_Si = {14};       // Silicon
KIM_SpeciesName const KIM_SPECIES_NAME_P = {15};        // Phosphorus
KIM_SpeciesName const KIM_SPECIES_NAME_S = {16};        // Sulfur
KIM_SpeciesName const KIM_SPECIES_NAME_Cl = {17};       // Chlorine
KIM_SpeciesName const KIM_SPECIES_NAME_Ar = {18};       // Argon
KIM_SpeciesName const KIM_SPECIES_NAME_K = {19};        // Potassium
KIM_SpeciesName const KIM_SPECIES_NAME_Ca = {20};       // Calcium
KIM_SpeciesName const KIM_SPECIES_NAME_Sc = {21};       // Scandium
KIM_SpeciesName const KIM_SPECIES_NAME_Ti = {22};       // Titanium
KIM_SpeciesName const KIM_SPECIES_NAME_V = {23};        // Vanadium
KIM_SpeciesName const KIM_SPECIES_NAME_Cr = {24};       // Chromium
KIM_SpeciesName const KIM_SPECIES_NAME_Mn = {25};       // Manganese
KIM_SpeciesName const KIM_SPECIES_NAME_Fe = {26};       // Iron
KIM_SpeciesName const KIM_SPECIES_NAME_Co = {27};       // Cobalt
KIM_SpeciesName const KIM_SPECIES_NAME_Ni = {28};       // Nickel
KIM_SpeciesName const KIM_SPECIES_NAME_Cu = {29};       // Copper
KIM_SpeciesName const KIM_SPECIES_NAME_Zn = {30};       // Zinc
KIM_SpeciesName const KIM_SPECIES_NAME_Ga = {31};       // Gallium
KIM_SpeciesName const KIM_SPECIES_NAME_Ge = {32};       // Germanium
KIM_SpeciesName const KIM_SPECIES_NAME_As = {33};       // Arsenic
KIM_SpeciesName const KIM_SPECIES_NAME_Se = {34};       // Selenium
KIM_SpeciesName const KIM_SPECIES_NAME_Br = {35};       // Bromine
KIM_SpeciesName const KIM_SPECIES_NAME_Kr = {36};       // Krypton
KIM_SpeciesName const KIM_SPECIES_NAME_Rb = {37};       // Rubidium
KIM_SpeciesName const KIM_SPECIES_NAME_Sr = {38};       // Strontium
KIM_SpeciesName const KIM_SPECIES_NAME_Y = {39};        // Yttrium
KIM_SpeciesName const KIM_SPECIES_NAME_Zr = {40};       // Zirconium
KIM_SpeciesName const KIM_SPECIES_NAME_Nb = {41};       // Niobium
KIM_SpeciesName const KIM_SPECIES_NAME_Mo = {42};       // Molybdenum
KIM_SpeciesName const KIM_SPECIES_NAME_Tc = {43};       // Technetium
KIM_SpeciesName const KIM_SPECIES_NAME_Ru = {44};       // Ruthenium
KIM_SpeciesName const KIM_SPECIES_NAME_Rh = {45};       // Rhodium
KIM_SpeciesName const KIM_SPECIES_NAME_Pd = {46};       // Palladium
KIM_SpeciesName const KIM_SPECIES_NAME_Ag = {47};       // Silver
KIM_SpeciesName const KIM_SPECIES_NAME_Cd = {48};       // Cadmium
KIM_SpeciesName const KIM_SPECIES_NAME_In = {49};       // Indium
KIM_SpeciesName const KIM_SPECIES_NAME_Sn = {50};       // Tin
KIM_SpeciesName const KIM_SPECIES_NAME_Sb = {51};       // Antimony
KIM_SpeciesName const KIM_SPECIES_NAME_Te = {52};       // Tellurium
KIM_SpeciesName const KIM_SPECIES_NAME_I = {53};        // Iodine
KIM_SpeciesName const KIM_SPECIES_NAME_Xe = {54};       // Xenon
KIM_SpeciesName const KIM_SPECIES_NAME_Cs = {55};       // Cesium
KIM_SpeciesName const KIM_SPECIES_NAME_Ba = {56};       // Barium
KIM_SpeciesName const KIM_SPECIES_NAME_La = {57};       // Lanthanum
KIM_SpeciesName const KIM_SPECIES_NAME_Ce = {58};       // Cerium
KIM_SpeciesName const KIM_SPECIES_NAME_Pr = {59};       // Praseodymium
KIM_SpeciesName const KIM_SPECIES_NAME_Nd = {60};       // Neodymium
KIM_SpeciesName const KIM_SPECIES_NAME_Pm = {61};       // Promethium
KIM_SpeciesName const KIM_SPECIES_NAME_Sm = {62};       // Samarium
KIM_SpeciesName const KIM_SPECIES_NAME_Eu = {63};       // Europium
KIM_SpeciesName const KIM_SPECIES_NAME_Gd = {64};       // Gadolinium
KIM_SpeciesName const KIM_SPECIES_NAME_Tb = {65};       // Terbium
KIM_SpeciesName const KIM_SPECIES_NAME_Dy = {66};       // Dysprosium
KIM_SpeciesName const KIM_SPECIES_NAME_Ho = {67};       // Holmium
KIM_SpeciesName const KIM_SPECIES_NAME_Er = {68};       // Erbium
KIM_SpeciesName const KIM_SPECIES_NAME_Tm = {69};       // Thulium
KIM_SpeciesName const KIM_SPECIES_NAME_Yb = {70};       // Ytterbium
KIM_SpeciesName const KIM_SPECIES_NAME_Lu = {71};       // Lutetium
KIM_SpeciesName const KIM_SPECIES_NAME_Hf = {72};       // Hafnium
KIM_SpeciesName const KIM_SPECIES_NAME_Ta = {73};       // Tantalum
KIM_SpeciesName const KIM_SPECIES_NAME_W = {74};        // Tungsten
KIM_SpeciesName const KIM_SPECIES_NAME_Re = {75};       // Rhenium
KIM_SpeciesName const KIM_SPECIES_NAME_Os = {76};       // Osmium
KIM_SpeciesName const KIM_SPECIES_NAME_Ir = {77};       // Iridium
KIM_SpeciesName const KIM_SPECIES_NAME_Pt = {78};       // Platinum
KIM_SpeciesName const KIM_SPECIES_NAME_Au = {79};       // Gold
KIM_SpeciesName const KIM_SPECIES_NAME_Hg = {80};       // Mercury
KIM_SpeciesName const KIM_SPECIES_NAME_Tl = {81};       // Thallium
KIM_SpeciesName const KIM_SPECIES_NAME_Pb = {82};       // Lead
KIM_SpeciesName const KIM_SPECIES_NAME_Bi = {83};       // Bismuth
KIM_SpeciesName const KIM_SPECIES_NAME_Po = {84};       // Polonium
KIM_SpeciesName const KIM_SPECIES_NAME_At = {85};       // Astatine
KIM_SpeciesName const KIM_SPECIES_NAME_Rn = {86};       // Radon
KIM_SpeciesName const KIM_SPECIES_NAME_Fr = {87};       // Francium
KIM_SpeciesName const KIM_SPECIES_NAME_Ra = {88};       // Radium
KIM_SpeciesName const KIM_SPECIES_NAME_Ac = {89};       // Actinium
KIM_SpeciesName const KIM_SPECIES_NAME_Th = {90};       // Thorium
KIM_SpeciesName const KIM_SPECIES_NAME_Pa = {91};       // Protactinium
KIM_SpeciesName const KIM_SPECIES_NAME_U = {92};        // Uranium
KIM_SpeciesName const KIM_SPECIES_NAME_Np = {93};       // Neptunium
KIM_SpeciesName const KIM_SPECIES_NAME_Pu = {94};       // Plutonium
KIM_SpeciesName const KIM_SPECIES_NAME_Am = {95};       // Americium
KIM_SpeciesName const KIM_SPECIES_NAME_Cm = {96};       // Curium
KIM_SpeciesName const KIM_SPECIES_NAME_Bk = {97};       // Berkelium
KIM_SpeciesName const KIM_SPECIES_NAME_Cf = {98};       // Californium
KIM_SpeciesName const KIM_SPECIES_NAME_Es = {99};       // Einsteinium
KIM_SpeciesName const KIM_SPECIES_NAME_Fm = {100};      // Fermium
KIM_SpeciesName const KIM_SPECIES_NAME_Md = {101};      // Mendelevium
KIM_SpeciesName const KIM_SPECIES_NAME_No = {102};      // Nobelium
KIM_SpeciesName const KIM_SPECIES_NAME_Lr = {103};      // Lawrencium
KIM_SpeciesName const KIM_SPECIES_NAME_Rf = {104};      // Rutherfordium
KIM_SpeciesName const KIM_SPECIES_NAME_Db = {105};      // Dubnium
KIM_SpeciesName const KIM_SPECIES_NAME_Sg = {106};      // Seaborgium
KIM_SpeciesName const KIM_SPECIES_NAME_Bh = {107};      // Bohrium
KIM_SpeciesName const KIM_SPECIES_NAME_Hs = {108};      // Hassium
KIM_SpeciesName const KIM_SPECIES_NAME_Mt = {109};      // Meitnerium
KIM_SpeciesName const KIM_SPECIES_NAME_Ds = {110};      // Darmstadtium
KIM_SpeciesName const KIM_SPECIES_NAME_Rg = {111};      // Roentgenium
KIM_SpeciesName const KIM_SPECIES_NAME_Cn = {112};      // Copernicium
KIM_SpeciesName const KIM_SPECIES_NAME_Uut = {113};     // Ununtrium
KIM_SpeciesName const KIM_SPECIES_NAME_Fl = {114};      // Flerovium
KIM_SpeciesName const KIM_SPECIES_NAME_Uup = {115};     // Ununpentium
KIM_SpeciesName const KIM_SPECIES_NAME_Lv = {116};      // Livermorium
KIM_SpeciesName const KIM_SPECIES_NAME_Uus = {117};     // Ununseptium
KIM_SpeciesName const KIM_SPECIES_NAME_Uuo = {118};     // Ununoctium
KIM_SpeciesName const KIM_SPECIES_NAME_user01 = {201};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user02 = {202};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user03 = {203};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user04 = {204};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user05 = {205};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user06 = {206};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user07 = {207};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user08 = {208};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user09 = {209};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user10 = {210};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user11 = {211};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user12 = {212};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user13 = {213};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user14 = {214};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user15 = {215};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user16 = {216};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user17 = {217};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user18 = {218};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user19 = {219};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user20 = {220};  // user defined


void KIM_SPECIES_NAME_GetNumberOfSpeciesNames(
    int * const numberOfSpeciesNames)
{
  KIM::SPECIES_NAME::GetNumberOfSpeciesNames(numberOfSpeciesNames);
}

int KIM_SPECIES_NAME_GetSpeciesName(int const index,
                                    KIM_SpeciesName * const speciesName)
{
  KIM::SpeciesName speciesNameCpp;
  int err = KIM::SPECIES_NAME::GetSpeciesName(index, &speciesNameCpp);
  if (err) return err;
  *speciesName = makeSpeciesNameC(speciesNameCpp);
  return false;
}
}  // extern "C"
