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


KIM_SpeciesName const KIM_SPECIES_NAME_electron  // electron
= {KIM::SPECIES_NAME::electron.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_H         // Hydrogen
= {KIM::SPECIES_NAME::H.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_He        // Helium
= {KIM::SPECIES_NAME::He.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Li        // Lithium
= {KIM::SPECIES_NAME::Li.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Be        // Beryllium
= {KIM::SPECIES_NAME::Be.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_B         // Boron
= {KIM::SPECIES_NAME::B.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_C         // Carbon
= {KIM::SPECIES_NAME::C.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_N         // Nitrogen
= {KIM::SPECIES_NAME::N.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_O         // Oxygen
= {KIM::SPECIES_NAME::O.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_F         // Fluorine
= {KIM::SPECIES_NAME::F.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ne        // Neon
= {KIM::SPECIES_NAME::Ne.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Na        // Sodium
= {KIM::SPECIES_NAME::Na.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Mg        // Magnesium
= {KIM::SPECIES_NAME::Mg.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Al        // Aluminum
= {KIM::SPECIES_NAME::Al.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Si        // Silicon
= {KIM::SPECIES_NAME::Si.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_P         // Phosphorus
= {KIM::SPECIES_NAME::P.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_S         // Sulfur
= {KIM::SPECIES_NAME::S.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Cl        // Chlorine
= {KIM::SPECIES_NAME::Cl.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ar        // Argon
= {KIM::SPECIES_NAME::Ar.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_K         // Potassium
= {KIM::SPECIES_NAME::K.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ca        // Calcium
= {KIM::SPECIES_NAME::Ca.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Sc        // Scandium
= {KIM::SPECIES_NAME::Sc.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ti        // Titanium
= {KIM::SPECIES_NAME::Ti.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_V         // Vanadium
= {KIM::SPECIES_NAME::V.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Cr        // Chromium
= {KIM::SPECIES_NAME::Cr.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Mn        // Manganese
= {KIM::SPECIES_NAME::Mn.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Fe        // Iron
= {KIM::SPECIES_NAME::Fe.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Co        // Cobalt
= {KIM::SPECIES_NAME::Co.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ni        // Nickel
= {KIM::SPECIES_NAME::Ni.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Cu        // Copper
= {KIM::SPECIES_NAME::Cu.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Zn        // Zinc
= {KIM::SPECIES_NAME::Zn.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ga        // Gallium
= {KIM::SPECIES_NAME::Ga.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ge        // Germanium
= {KIM::SPECIES_NAME::Ge.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_As        // Arsenic
= {KIM::SPECIES_NAME::As.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Se        // Selenium
= {KIM::SPECIES_NAME::Se.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Br        // Bromine
= {KIM::SPECIES_NAME::Br.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Kr        // Krypton
= {KIM::SPECIES_NAME::Kr.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Rb        // Rubidium
= {KIM::SPECIES_NAME::Rb.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Sr        // Strontium
= {KIM::SPECIES_NAME::Sr.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Y         // Yttrium
= {KIM::SPECIES_NAME::Y.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Zr        // Zirconium
= {KIM::SPECIES_NAME::Zr.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Nb        // Niobium
= {KIM::SPECIES_NAME::Nb.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Mo        // Molybdenum
= {KIM::SPECIES_NAME::Mo.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Tc        // Technetium
= {KIM::SPECIES_NAME::Tc.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ru        // Ruthenium
= {KIM::SPECIES_NAME::Ru.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Rh        // Rhodium
= {KIM::SPECIES_NAME::Rh.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Pd        // Palladium
= {KIM::SPECIES_NAME::Pd.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ag        // Silver
= {KIM::SPECIES_NAME::Ag.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Cd        // Cadmium
= {KIM::SPECIES_NAME::Cd.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_In        // Indium
= {KIM::SPECIES_NAME::In.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Sn        // Tin
= {KIM::SPECIES_NAME::Sn.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Sb        // Antimony
= {KIM::SPECIES_NAME::Sb.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Te        // Tellurium
= {KIM::SPECIES_NAME::Te.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_I         // Iodine
= {KIM::SPECIES_NAME::I.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Xe        // Xenon
= {KIM::SPECIES_NAME::Xe.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Cs        // Cesium
= {KIM::SPECIES_NAME::Cs.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ba        // Barium
= {KIM::SPECIES_NAME::Ba.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_La        // Lanthanum
= {KIM::SPECIES_NAME::La.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ce        // Cerium
= {KIM::SPECIES_NAME::Ce.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Pr        // Praseodymium
= {KIM::SPECIES_NAME::Pr.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Nd        // Neodymium
= {KIM::SPECIES_NAME::Nd.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Pm        // Promethium
= {KIM::SPECIES_NAME::Pm.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Sm        // Samarium
= {KIM::SPECIES_NAME::Sm.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Eu        // Europium
= {KIM::SPECIES_NAME::Eu.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Gd        // Gadolinium
= {KIM::SPECIES_NAME::Gd.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Tb        // Terbium
= {KIM::SPECIES_NAME::Tb.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Dy        // Dysprosium
= {KIM::SPECIES_NAME::Dy.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ho        // Holmium
= {KIM::SPECIES_NAME::Ho.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Er        // Erbium
= {KIM::SPECIES_NAME::Er.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Tm        // Thulium
= {KIM::SPECIES_NAME::Tm.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Yb        // Ytterbium
= {KIM::SPECIES_NAME::Yb.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Lu        // Lutetium
= {KIM::SPECIES_NAME::Lu.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Hf        // Hafnium
= {KIM::SPECIES_NAME::Hf.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ta        // Tantalum
= {KIM::SPECIES_NAME::Ta.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_W         // Tungsten
= {KIM::SPECIES_NAME::W.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Re        // Rhenium
= {KIM::SPECIES_NAME::Re.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Os        // Osmium
= {KIM::SPECIES_NAME::Os.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ir        // Iridium
= {KIM::SPECIES_NAME::Ir.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Pt        // Platinum
= {KIM::SPECIES_NAME::Pt.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Au        // Gold
= {KIM::SPECIES_NAME::Au.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Hg        // Mercury
= {KIM::SPECIES_NAME::Hg.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Tl        // Thallium
= {KIM::SPECIES_NAME::Tl.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Pb        // Lead
= {KIM::SPECIES_NAME::Pb.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Bi        // Bismuth
= {KIM::SPECIES_NAME::Bi.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Po        // Polonium
= {KIM::SPECIES_NAME::Po.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_At        // Astatine
= {KIM::SPECIES_NAME::At.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Rn        // Radon
= {KIM::SPECIES_NAME::Rn.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Fr        // Francium
= {KIM::SPECIES_NAME::Fr.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ra        // Radium
= {KIM::SPECIES_NAME::Ra.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ac        // Actinium
= {KIM::SPECIES_NAME::Ac.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Th        // Thorium
= {KIM::SPECIES_NAME::Th.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Pa        // Protactinium
= {KIM::SPECIES_NAME::Pa.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_U         // Uranium
= {KIM::SPECIES_NAME::U.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Np        // Neptunium
= {KIM::SPECIES_NAME::Np.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Pu        // Plutonium
= {KIM::SPECIES_NAME::Pu.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Am        // Americium
= {KIM::SPECIES_NAME::Am.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Cm        // Curium
= {KIM::SPECIES_NAME::Cm.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Bk        // Berkelium
= {KIM::SPECIES_NAME::Bk.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Cf        // Californium
= {KIM::SPECIES_NAME::Cf.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Es        // Einsteinium
= {KIM::SPECIES_NAME::Es.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Fm        // Fermium
= {KIM::SPECIES_NAME::Fm.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Md        // Mendelevium
= {KIM::SPECIES_NAME::Md.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_No        // Nobelium
= {KIM::SPECIES_NAME::No.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Lr        // Lawrencium
= {KIM::SPECIES_NAME::Lr.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Rf        // Rutherfordium
= {KIM::SPECIES_NAME::Rf.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Db        // Dubnium
= {KIM::SPECIES_NAME::Db.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Sg        // Seaborgium
= {KIM::SPECIES_NAME::Sg.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Bh        // Bohrium
= {KIM::SPECIES_NAME::Bh.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Hs        // Hassium
= {KIM::SPECIES_NAME::Hs.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Mt        // Meitnerium
= {KIM::SPECIES_NAME::Mt.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Ds        // Darmstadtium
= {KIM::SPECIES_NAME::Ds.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Rg        // Roentgenium
= {KIM::SPECIES_NAME::Rg.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Cn        // Copernicium
= {KIM::SPECIES_NAME::Cn.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Uut       // Ununtrium
= {KIM::SPECIES_NAME::Uut.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Fl        // Flerovium
= {KIM::SPECIES_NAME::Fl.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Uup       // Ununpentium
= {KIM::SPECIES_NAME::Uup.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Lv        // Livermorium
= {KIM::SPECIES_NAME::Lv.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Uus       // Ununseptium
= {KIM::SPECIES_NAME::Uus.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_Uuo       // Ununoctium
= {KIM::SPECIES_NAME::Uuo.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user01    // user defined
= {KIM::SPECIES_NAME::user01.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user02    // user defined
= {KIM::SPECIES_NAME::user02.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user03    // user defined
= {KIM::SPECIES_NAME::user03.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user04    // user defined
= {KIM::SPECIES_NAME::user04.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user05    // user defined
= {KIM::SPECIES_NAME::user05.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user06    // user defined
= {KIM::SPECIES_NAME::user06.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user07    // user defined
= {KIM::SPECIES_NAME::user07.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user08    // user defined
= {KIM::SPECIES_NAME::user08.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user09    // user defined
= {KIM::SPECIES_NAME::user09.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user10    // user defined
= {KIM::SPECIES_NAME::user10.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user11    // user defined
= {KIM::SPECIES_NAME::user11.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user12    // user defined
= {KIM::SPECIES_NAME::user12.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user13    // user defined
= {KIM::SPECIES_NAME::user13.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user14    // user defined
= {KIM::SPECIES_NAME::user14.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user15    // user defined
= {KIM::SPECIES_NAME::user15.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user16    // user defined
= {KIM::SPECIES_NAME::user16.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user17    // user defined
= {KIM::SPECIES_NAME::user17.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user18    // user defined
= {KIM::SPECIES_NAME::user18.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user19    // user defined
= {KIM::SPECIES_NAME::user19.speciesNameID};
KIM_SpeciesName const KIM_SPECIES_NAME_user20    // user defined
= {KIM::SPECIES_NAME::user20.speciesNameID};


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
