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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
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
KIM::SpeciesName makeSpeciesNameCpp(KIM_SpeciesName const speciesName)
{
  KIM::SpeciesName const * const speciesNameCpp
      = reinterpret_cast<KIM::SpeciesName const *>(&speciesName);
  return *speciesNameCpp;
}

KIM_SpeciesName makeSpeciesNameC(KIM::SpeciesName speciesName)
{
  KIM_SpeciesName const * const speciesNameC
      = reinterpret_cast<KIM_SpeciesName const *>(&speciesName);
  return *speciesNameC;
}
}  // namespace

extern "C"
{
KIM_SpeciesName KIM_SpeciesName_FromString(char const * const str)
{
  return makeSpeciesNameC(KIM::SpeciesName(std::string(str)));
}

int KIM_SpeciesName_Equal(KIM_SpeciesName const left,
                          KIM_SpeciesName const right)
{
  return (left.speciesNameID == right.speciesNameID);
}

int KIM_SpeciesName_NotEqual(KIM_SpeciesName const left,
                             KIM_SpeciesName const right)
{
  return (!KIM_SpeciesName_Equal(left, right));
}

char const * KIM_SpeciesName_String(KIM_SpeciesName const speciesName)
{
  return makeSpeciesNameCpp(speciesName).String().c_str();
}


#include "KIM_SpeciesName.inc"
KIM_SpeciesName const KIM_SPECIES_NAME_electron = {ID_electron};  // electron
KIM_SpeciesName const KIM_SPECIES_NAME_H = {ID_H};            // Hydrogen
KIM_SpeciesName const KIM_SPECIES_NAME_He = {ID_He};          // Helium
KIM_SpeciesName const KIM_SPECIES_NAME_Li = {ID_Li};          // Lithium
KIM_SpeciesName const KIM_SPECIES_NAME_Be = {ID_Be};          // Beryllium
KIM_SpeciesName const KIM_SPECIES_NAME_B = {ID_B};            // Boron
KIM_SpeciesName const KIM_SPECIES_NAME_C = {ID_C};            // Carbon
KIM_SpeciesName const KIM_SPECIES_NAME_N = {ID_N};            // Nitrogen
KIM_SpeciesName const KIM_SPECIES_NAME_O = {ID_O};            // Oxygen
KIM_SpeciesName const KIM_SPECIES_NAME_F = {ID_F};            // Fluorine
KIM_SpeciesName const KIM_SPECIES_NAME_Ne = {ID_Ne};          // Neon
KIM_SpeciesName const KIM_SPECIES_NAME_Na = {ID_Na};          // Sodium
KIM_SpeciesName const KIM_SPECIES_NAME_Mg = {ID_Mg};          // Magnesium
KIM_SpeciesName const KIM_SPECIES_NAME_Al = {ID_Al};          // Aluminum
KIM_SpeciesName const KIM_SPECIES_NAME_Si = {ID_Si};          // Silicon
KIM_SpeciesName const KIM_SPECIES_NAME_P = {ID_P};            // Phosphorus
KIM_SpeciesName const KIM_SPECIES_NAME_S = {ID_S};            // Sulfur
KIM_SpeciesName const KIM_SPECIES_NAME_Cl = {ID_Cl};          // Chlorine
KIM_SpeciesName const KIM_SPECIES_NAME_Ar = {ID_Ar};          // Argon
KIM_SpeciesName const KIM_SPECIES_NAME_K = {ID_K};            // Potassium
KIM_SpeciesName const KIM_SPECIES_NAME_Ca = {ID_Ca};          // Calcium
KIM_SpeciesName const KIM_SPECIES_NAME_Sc = {ID_Sc};          // Scandium
KIM_SpeciesName const KIM_SPECIES_NAME_Ti = {ID_Ti};          // Titanium
KIM_SpeciesName const KIM_SPECIES_NAME_V = {ID_V};            // Vanadium
KIM_SpeciesName const KIM_SPECIES_NAME_Cr = {ID_Cr};          // Chromium
KIM_SpeciesName const KIM_SPECIES_NAME_Mn = {ID_Mn};          // Manganese
KIM_SpeciesName const KIM_SPECIES_NAME_Fe = {ID_Fe};          // Iron
KIM_SpeciesName const KIM_SPECIES_NAME_Co = {ID_Co};          // Cobalt
KIM_SpeciesName const KIM_SPECIES_NAME_Ni = {ID_Ni};          // Nickel
KIM_SpeciesName const KIM_SPECIES_NAME_Cu = {ID_Cu};          // Copper
KIM_SpeciesName const KIM_SPECIES_NAME_Zn = {ID_Zn};          // Zinc
KIM_SpeciesName const KIM_SPECIES_NAME_Ga = {ID_Ga};          // Gallium
KIM_SpeciesName const KIM_SPECIES_NAME_Ge = {ID_Ge};          // Germanium
KIM_SpeciesName const KIM_SPECIES_NAME_As = {ID_As};          // Arsenic
KIM_SpeciesName const KIM_SPECIES_NAME_Se = {ID_Se};          // Selenium
KIM_SpeciesName const KIM_SPECIES_NAME_Br = {ID_Br};          // Bromine
KIM_SpeciesName const KIM_SPECIES_NAME_Kr = {ID_Kr};          // Krypton
KIM_SpeciesName const KIM_SPECIES_NAME_Rb = {ID_Rb};          // Rubidium
KIM_SpeciesName const KIM_SPECIES_NAME_Sr = {ID_Sr};          // Strontium
KIM_SpeciesName const KIM_SPECIES_NAME_Y = {ID_Y};            // Yttrium
KIM_SpeciesName const KIM_SPECIES_NAME_Zr = {ID_Zr};          // Zirconium
KIM_SpeciesName const KIM_SPECIES_NAME_Nb = {ID_Nb};          // Niobium
KIM_SpeciesName const KIM_SPECIES_NAME_Mo = {ID_Mo};          // Molybdenum
KIM_SpeciesName const KIM_SPECIES_NAME_Tc = {ID_Tc};          // Technetium
KIM_SpeciesName const KIM_SPECIES_NAME_Ru = {ID_Ru};          // Ruthenium
KIM_SpeciesName const KIM_SPECIES_NAME_Rh = {ID_Rh};          // Rhodium
KIM_SpeciesName const KIM_SPECIES_NAME_Pd = {ID_Pd};          // Palladium
KIM_SpeciesName const KIM_SPECIES_NAME_Ag = {ID_Ag};          // Silver
KIM_SpeciesName const KIM_SPECIES_NAME_Cd = {ID_Cd};          // Cadmium
KIM_SpeciesName const KIM_SPECIES_NAME_In = {ID_In};          // Indium
KIM_SpeciesName const KIM_SPECIES_NAME_Sn = {ID_Sn};          // Tin
KIM_SpeciesName const KIM_SPECIES_NAME_Sb = {ID_Sb};          // Antimony
KIM_SpeciesName const KIM_SPECIES_NAME_Te = {ID_Te};          // Tellurium
KIM_SpeciesName const KIM_SPECIES_NAME_I = {ID_I};            // Iodine
KIM_SpeciesName const KIM_SPECIES_NAME_Xe = {ID_Xe};          // Xenon
KIM_SpeciesName const KIM_SPECIES_NAME_Cs = {ID_Cs};          // Cesium
KIM_SpeciesName const KIM_SPECIES_NAME_Ba = {ID_Ba};          // Barium
KIM_SpeciesName const KIM_SPECIES_NAME_La = {ID_La};          // Lanthanum
KIM_SpeciesName const KIM_SPECIES_NAME_Ce = {ID_Ce};          // Cerium
KIM_SpeciesName const KIM_SPECIES_NAME_Pr = {ID_Pr};          // Praseodymium
KIM_SpeciesName const KIM_SPECIES_NAME_Nd = {ID_Nd};          // Neodymium
KIM_SpeciesName const KIM_SPECIES_NAME_Pm = {ID_Pm};          // Promethium
KIM_SpeciesName const KIM_SPECIES_NAME_Sm = {ID_Sm};          // Samarium
KIM_SpeciesName const KIM_SPECIES_NAME_Eu = {ID_Eu};          // Europium
KIM_SpeciesName const KIM_SPECIES_NAME_Gd = {ID_Gd};          // Gadolinium
KIM_SpeciesName const KIM_SPECIES_NAME_Tb = {ID_Tb};          // Terbium
KIM_SpeciesName const KIM_SPECIES_NAME_Dy = {ID_Dy};          // Dysprosium
KIM_SpeciesName const KIM_SPECIES_NAME_Ho = {ID_Ho};          // Holmium
KIM_SpeciesName const KIM_SPECIES_NAME_Er = {ID_Er};          // Erbium
KIM_SpeciesName const KIM_SPECIES_NAME_Tm = {ID_Tm};          // Thulium
KIM_SpeciesName const KIM_SPECIES_NAME_Yb = {ID_Yb};          // Ytterbium
KIM_SpeciesName const KIM_SPECIES_NAME_Lu = {ID_Lu};          // Lutetium
KIM_SpeciesName const KIM_SPECIES_NAME_Hf = {ID_Hf};          // Hafnium
KIM_SpeciesName const KIM_SPECIES_NAME_Ta = {ID_Ta};          // Tantalum
KIM_SpeciesName const KIM_SPECIES_NAME_W = {ID_W};            // Tungsten
KIM_SpeciesName const KIM_SPECIES_NAME_Re = {ID_Re};          // Rhenium
KIM_SpeciesName const KIM_SPECIES_NAME_Os = {ID_Os};          // Osmium
KIM_SpeciesName const KIM_SPECIES_NAME_Ir = {ID_Ir};          // Iridium
KIM_SpeciesName const KIM_SPECIES_NAME_Pt = {ID_Pt};          // Platinum
KIM_SpeciesName const KIM_SPECIES_NAME_Au = {ID_Au};          // Gold
KIM_SpeciesName const KIM_SPECIES_NAME_Hg = {ID_Hg};          // Mercury
KIM_SpeciesName const KIM_SPECIES_NAME_Tl = {ID_Tl};          // Thallium
KIM_SpeciesName const KIM_SPECIES_NAME_Pb = {ID_Pb};          // Lead
KIM_SpeciesName const KIM_SPECIES_NAME_Bi = {ID_Bi};          // Bismuth
KIM_SpeciesName const KIM_SPECIES_NAME_Po = {ID_Po};          // Polonium
KIM_SpeciesName const KIM_SPECIES_NAME_At = {ID_At};          // Astatine
KIM_SpeciesName const KIM_SPECIES_NAME_Rn = {ID_Rn};          // Radon
KIM_SpeciesName const KIM_SPECIES_NAME_Fr = {ID_Fr};          // Francium
KIM_SpeciesName const KIM_SPECIES_NAME_Ra = {ID_Ra};          // Radium
KIM_SpeciesName const KIM_SPECIES_NAME_Ac = {ID_Ac};          // Actinium
KIM_SpeciesName const KIM_SPECIES_NAME_Th = {ID_Th};          // Thorium
KIM_SpeciesName const KIM_SPECIES_NAME_Pa = {ID_Pa};          // Protactinium
KIM_SpeciesName const KIM_SPECIES_NAME_U = {ID_U};            // Uranium
KIM_SpeciesName const KIM_SPECIES_NAME_Np = {ID_Np};          // Neptunium
KIM_SpeciesName const KIM_SPECIES_NAME_Pu = {ID_Pu};          // Plutonium
KIM_SpeciesName const KIM_SPECIES_NAME_Am = {ID_Am};          // Americium
KIM_SpeciesName const KIM_SPECIES_NAME_Cm = {ID_Cm};          // Curium
KIM_SpeciesName const KIM_SPECIES_NAME_Bk = {ID_Bk};          // Berkelium
KIM_SpeciesName const KIM_SPECIES_NAME_Cf = {ID_Cf};          // Californium
KIM_SpeciesName const KIM_SPECIES_NAME_Es = {ID_Es};          // Einsteinium
KIM_SpeciesName const KIM_SPECIES_NAME_Fm = {ID_Fm};          // Fermium
KIM_SpeciesName const KIM_SPECIES_NAME_Md = {ID_Md};          // Mendelevium
KIM_SpeciesName const KIM_SPECIES_NAME_No = {ID_No};          // Nobelium
KIM_SpeciesName const KIM_SPECIES_NAME_Lr = {ID_Lr};          // Lawrencium
KIM_SpeciesName const KIM_SPECIES_NAME_Rf = {ID_Rf};          // Rutherfordium
KIM_SpeciesName const KIM_SPECIES_NAME_Db = {ID_Db};          // Dubnium
KIM_SpeciesName const KIM_SPECIES_NAME_Sg = {ID_Sg};          // Seaborgium
KIM_SpeciesName const KIM_SPECIES_NAME_Bh = {ID_Bh};          // Bohrium
KIM_SpeciesName const KIM_SPECIES_NAME_Hs = {ID_Hs};          // Hassium
KIM_SpeciesName const KIM_SPECIES_NAME_Mt = {ID_Mt};          // Meitnerium
KIM_SpeciesName const KIM_SPECIES_NAME_Ds = {ID_Ds};          // Darmstadtium
KIM_SpeciesName const KIM_SPECIES_NAME_Rg = {ID_Rg};          // Roentgenium
KIM_SpeciesName const KIM_SPECIES_NAME_Cn = {ID_Cn};          // Copernicium
KIM_SpeciesName const KIM_SPECIES_NAME_Uut = {ID_Uut};        // Ununtrium
KIM_SpeciesName const KIM_SPECIES_NAME_Fl = {ID_Fl};          // Flerovium
KIM_SpeciesName const KIM_SPECIES_NAME_Uup = {ID_Uup};        // Ununpentium
KIM_SpeciesName const KIM_SPECIES_NAME_Lv = {ID_Lv};          // Livermorium
KIM_SpeciesName const KIM_SPECIES_NAME_Uus = {ID_Uus};        // Ununseptium
KIM_SpeciesName const KIM_SPECIES_NAME_Uuo = {ID_Uuo};        // Ununoctium
KIM_SpeciesName const KIM_SPECIES_NAME_user01 = {ID_user01};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user02 = {ID_user02};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user03 = {ID_user03};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user04 = {ID_user04};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user05 = {ID_user05};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user06 = {ID_user06};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user07 = {ID_user07};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user08 = {ID_user08};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user09 = {ID_user09};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user10 = {ID_user10};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user11 = {ID_user11};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user12 = {ID_user12};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user13 = {ID_user13};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user14 = {ID_user14};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user15 = {ID_user15};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user16 = {ID_user16};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user17 = {ID_user17};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user18 = {ID_user18};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user19 = {ID_user19};  // user defined
KIM_SpeciesName const KIM_SPECIES_NAME_user20 = {ID_user20};  // user defined

void KIM_SPECIES_NAME_GetNumberOfSpeciesNames(int * const numberOfSpeciesNames)
{
  KIM::SPECIES_NAME::GetNumberOfSpeciesNames(numberOfSpeciesNames);
}

int KIM_SPECIES_NAME_GetSpeciesName(int const index,
                                    KIM_SpeciesName * const speciesName)
{
  KIM::SpeciesName speciesNameCpp;
  int error = KIM::SPECIES_NAME::GetSpeciesName(index, &speciesNameCpp);
  if (error) return error;
  *speciesName = makeSpeciesNameC(speciesNameCpp);
  return false;
}

}  // extern "C"
