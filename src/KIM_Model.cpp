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

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif

#ifndef KIM_MODEL_HPP_
#include "KIM_Model.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_COMPUTE_ArgumentName.hpp"
#endif

#include "old_KIM_API.h"
#include "old_KIM_API_status.h"

namespace KIM
{

namespace
{
SpeciesName speciesCpp(std::string speciesName)
{
  if (speciesName == "electron") return SPECIES_NAME::electron;
  else if (speciesName == "H") return SPECIES_NAME::H;
  else if (speciesName == "He") return SPECIES_NAME::He;
  else if (speciesName == "Li") return SPECIES_NAME::Li;
  else if (speciesName == "Be") return SPECIES_NAME::Be;
  else if (speciesName == "B") return SPECIES_NAME::B;
  else if (speciesName == "C") return SPECIES_NAME::C;
  else if (speciesName == "N") return SPECIES_NAME::N;
  else if (speciesName == "O") return SPECIES_NAME::O;
  else if (speciesName == "F") return SPECIES_NAME::F;
  else if (speciesName == "Ne") return SPECIES_NAME::Ne;
  else if (speciesName == "Na") return SPECIES_NAME::Na;
  else if (speciesName == "Mg") return SPECIES_NAME::Mg;
  else if (speciesName == "Al") return SPECIES_NAME::Al;
  else if (speciesName == "Si") return SPECIES_NAME::Si;
  else if (speciesName == "P") return SPECIES_NAME::P;
  else if (speciesName == "S") return SPECIES_NAME::S;
  else if (speciesName == "Cl") return SPECIES_NAME::Cl;
  else if (speciesName == "Ar") return SPECIES_NAME::Ar;
  else if (speciesName == "K") return SPECIES_NAME::K;
  else if (speciesName == "Ca") return SPECIES_NAME::Ca;
  else if (speciesName == "Sc") return SPECIES_NAME::Sc;
  else if (speciesName == "Ti") return SPECIES_NAME::Ti;
  else if (speciesName == "V") return SPECIES_NAME::V;
  else if (speciesName == "Cr") return SPECIES_NAME::Cr;
  else if (speciesName == "Mn") return SPECIES_NAME::Mn;
  else if (speciesName == "Fe") return SPECIES_NAME::Fe;
  else if (speciesName == "Co") return SPECIES_NAME::Co;
  else if (speciesName == "Ni") return SPECIES_NAME::Ni;
  else if (speciesName == "Cu") return SPECIES_NAME::Cu;
  else if (speciesName == "Zn") return SPECIES_NAME::Zn;
  else if (speciesName == "Ga") return SPECIES_NAME::Ga;
  else if (speciesName == "Ge") return SPECIES_NAME::Ge;
  else if (speciesName == "As") return SPECIES_NAME::As;
  else if (speciesName == "Se") return SPECIES_NAME::Se;
  else if (speciesName == "Br") return SPECIES_NAME::Br;
  else if (speciesName == "Kr") return SPECIES_NAME::Kr;
  else if (speciesName == "Rb") return SPECIES_NAME::Rb;
  else if (speciesName == "Sr") return SPECIES_NAME::Sr;
  else if (speciesName == "Y") return SPECIES_NAME::Y;
  else if (speciesName == "Zr") return SPECIES_NAME::Zr;
  else if (speciesName == "Nb") return SPECIES_NAME::Nb;
  else if (speciesName == "Mo") return SPECIES_NAME::Mo;
  else if (speciesName == "Tc") return SPECIES_NAME::Tc;
  else if (speciesName == "Ru") return SPECIES_NAME::Ru;
  else if (speciesName == "Rh") return SPECIES_NAME::Rh;
  else if (speciesName == "Pd") return SPECIES_NAME::Pd;
  else if (speciesName == "Ag") return SPECIES_NAME::Ag;
  else if (speciesName == "Cd") return SPECIES_NAME::Cd;
  else if (speciesName == "In") return SPECIES_NAME::In;
  else if (speciesName == "Sn") return SPECIES_NAME::Sn;
  else if (speciesName == "Sb") return SPECIES_NAME::Sb;
  else if (speciesName == "Te") return SPECIES_NAME::Te;
  else if (speciesName == "I") return SPECIES_NAME::I;
  else if (speciesName == "Xe") return SPECIES_NAME::Xe;
  else if (speciesName == "Cs") return SPECIES_NAME::Cs;
  else if (speciesName == "Ba") return SPECIES_NAME::Ba;
  else if (speciesName == "La") return SPECIES_NAME::La;
  else if (speciesName == "Ce") return SPECIES_NAME::Ce;
  else if (speciesName == "Pr") return SPECIES_NAME::Pr;
  else if (speciesName == "Nd") return SPECIES_NAME::Nd;
  else if (speciesName == "Pm") return SPECIES_NAME::Pm;
  else if (speciesName == "Sm") return SPECIES_NAME::Sm;
  else if (speciesName == "Eu") return SPECIES_NAME::Eu;
  else if (speciesName == "Gd") return SPECIES_NAME::Gd;
  else if (speciesName == "Tb") return SPECIES_NAME::Tb;
  else if (speciesName == "Dy") return SPECIES_NAME::Dy;
  else if (speciesName == "Ho") return SPECIES_NAME::Ho;
  else if (speciesName == "Er") return SPECIES_NAME::Er;
  else if (speciesName == "Tm") return SPECIES_NAME::Tm;
  else if (speciesName == "Yb") return SPECIES_NAME::Yb;
  else if (speciesName == "Lu") return SPECIES_NAME::Lu;
  else if (speciesName == "Hf") return SPECIES_NAME::Hf;
  else if (speciesName == "Ta") return SPECIES_NAME::Ta;
  else if (speciesName == "W") return SPECIES_NAME::W;
  else if (speciesName == "Re") return SPECIES_NAME::Re;
  else if (speciesName == "Os") return SPECIES_NAME::Os;
  else if (speciesName == "Ir") return SPECIES_NAME::Ir;
  else if (speciesName == "Pt") return SPECIES_NAME::Pt;
  else if (speciesName == "Au") return SPECIES_NAME::Au;
  else if (speciesName == "Hg") return SPECIES_NAME::Hg;
  else if (speciesName == "Tl") return SPECIES_NAME::Tl;
  else if (speciesName == "Pb") return SPECIES_NAME::Pb;
  else if (speciesName == "Bi") return SPECIES_NAME::Bi;
  else if (speciesName == "Po") return SPECIES_NAME::Po;
  else if (speciesName == "At") return SPECIES_NAME::At;
  else if (speciesName == "Rn") return SPECIES_NAME::Rn;
  else if (speciesName == "Fr") return SPECIES_NAME::Fr;
  else if (speciesName == "Ra") return SPECIES_NAME::Ra;
  else if (speciesName == "Ac") return SPECIES_NAME::Ac;
  else if (speciesName == "Th") return SPECIES_NAME::Th;
  else if (speciesName == "Pa") return SPECIES_NAME::Pa;
  else if (speciesName == "U") return SPECIES_NAME::U;
  else if (speciesName == "Np") return SPECIES_NAME::Np;
  else if (speciesName == "Pu") return SPECIES_NAME::Pu;
  else if (speciesName == "Am") return SPECIES_NAME::Am;
  else if (speciesName == "Cm") return SPECIES_NAME::Cm;
  else if (speciesName == "Bk") return SPECIES_NAME::Bk;
  else if (speciesName == "Cf") return SPECIES_NAME::Cf;
  else if (speciesName == "Es") return SPECIES_NAME::Es;
  else if (speciesName == "Fm") return SPECIES_NAME::Fm;
  else if (speciesName == "Md") return SPECIES_NAME::Md;
  else if (speciesName == "No") return SPECIES_NAME::No;
  else if (speciesName == "Lr") return SPECIES_NAME::Lr;
  else if (speciesName == "Rf") return SPECIES_NAME::Rf;
  else if (speciesName == "Db") return SPECIES_NAME::Db;
  else if (speciesName == "Sg") return SPECIES_NAME::Sg;
  else if (speciesName == "Bh") return SPECIES_NAME::Bh;
  else if (speciesName == "Hs") return SPECIES_NAME::Hs;
  else if (speciesName == "Mt") return SPECIES_NAME::Mt;
  else if (speciesName == "Ds") return SPECIES_NAME::Ds;
  else if (speciesName == "Rg") return SPECIES_NAME::Rg;
  else if (speciesName == "Cn") return SPECIES_NAME::Cn;
  else if (speciesName == "Uut") return SPECIES_NAME::Uut;
  else if (speciesName == "Fl") return SPECIES_NAME::Fl;
  else if (speciesName == "Uup") return SPECIES_NAME::Uup;
  else if (speciesName == "Lv") return SPECIES_NAME::Lv;
  else if (speciesName == "Uus") return SPECIES_NAME::Uus;
  else if (speciesName == "Uuo") return SPECIES_NAME::Uuo;
  else if (speciesName == "user01") return SPECIES_NAME::user01;
  else if (speciesName == "user02") return SPECIES_NAME::user02;
  else if (speciesName == "user03") return SPECIES_NAME::user03;
  else if (speciesName == "user04") return SPECIES_NAME::user04;
  else if (speciesName == "user05") return SPECIES_NAME::user05;
  else if (speciesName == "user06") return SPECIES_NAME::user06;
  else if (speciesName == "user07") return SPECIES_NAME::user07;
  else if (speciesName == "user08") return SPECIES_NAME::user08;
  else if (speciesName == "user09") return SPECIES_NAME::user09;
  else if (speciesName == "user10") return SPECIES_NAME::user10;
  else if (speciesName == "user11") return SPECIES_NAME::user11;
  else if (speciesName == "user12") return SPECIES_NAME::user12;
  else if (speciesName == "user13") return SPECIES_NAME::user13;
  else if (speciesName == "user14") return SPECIES_NAME::user14;
  else if (speciesName == "user15") return SPECIES_NAME::user15;
  else if (speciesName == "user16") return SPECIES_NAME::user16;
  else if (speciesName == "user17") return SPECIES_NAME::user17;
  else if (speciesName == "user18") return SPECIES_NAME::user18;
  else if (speciesName == "user19") return SPECIES_NAME::user19;
  else if (speciesName == "user20") return SPECIES_NAME::user20;
  else return SpeciesName(-1);
}
}  // namesapce


int Model::create(std::string const & simulatorString,
                  std::string const & modelName,
                  Model ** const model)
{
  OLD_KIM::KIM_API_model * mdl;
  mdl = new OLD_KIM::KIM_API_model();

  Model * pModel = new Model();
  pModel->pimpl = (ModelImplementation *) mdl;
  *model = pModel;
  int err = mdl->string_init(simulatorString.c_str(), modelName.c_str());
  if (err >= KIM_STATUS_OK)  // string_init returns 1.0 codes
  {
    err = mdl->model_init();  // Models return 2.0 codes
  }
  else
  {
    err = true;  // set 2.0 code
  }
  return err;  // err is a 2.0 code
}

void Model::destroy(Model ** const model)
{
  OLD_KIM::KIM_API_model * mdl;
  mdl = (OLD_KIM::KIM_API_model *) (*model)->pimpl;
  mdl->model_destroy();
  delete mdl;

  delete (*model);

  *model = 0;
}

int Model::create_compute_arguments(
    COMPUTE::ModelComputeArguments ** const arguments) const
{
  Model* const M = new Model();
  M->pimpl = pimpl;

  *arguments = reinterpret_cast<COMPUTE::ModelComputeArguments *>(M);

  return false;
}

void Model::destroy_compute_arguments(
    COMPUTE::ModelComputeArguments ** const arguments) const
{
  Model* const M = reinterpret_cast<Model *>(*arguments);

  delete M;
  *arguments = 0;
}

void Model::get_influence_distance(double * const influenceDistance) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;
  *influenceDistance = *(pKIM->influenceDistance);
}

void Model::get_cutoffs(int * const numberOfCutoffs, double const ** cutoffs)
    const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  *numberOfCutoffs = pKIM->numberOfCutoffs;
  if (cutoffs != NULL)
    *cutoffs = pKIM->cutoffs;
}

void Model::print() const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  pKIM->print(&err);
}

int Model::compute(COMPUTE::ModelComputeArguments const * const arguments) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;
  int err = pKIM->model_compute(reinterpret_cast<COMPUTE::SimulatorComputeArguments const * const>(arguments));
  return err;  // Models should return 2.0 codes
}

int Model::reinit()
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err = pKIM->model_reinit();
  return err;  // Models should return 2.0 codes
}

void Model::get_num_model_species(int * const numberOfSpecies) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int maxStrLen;
  pKIM->get_num_model_species(numberOfSpecies, &maxStrLen);
}

int Model::get_model_species(int const index,
                             SpeciesName * const speciesName) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  char const * str;
  int err = pKIM->get_model_species(index, &str);
  (*speciesName) = speciesCpp(str);
  return (err < KIM_STATUS_OK);
}


int Model::get_model_kim_string(std::string const & modelName,
                                std::string * const kimString)
{
  char const * kimChar;
  int err = OLD_KIM::KIM_API_model::get_model_kim_str(modelName.c_str(),
                                                      &kimChar);
  *kimString = kimChar;
  return (err < KIM_STATUS_OK);
}

int Model::get_species_code(SpeciesName const speciesName,
                            int * const code) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *code = pKIM->get_species_code(speciesName.string().c_str(), &err);
  return (err < KIM_STATUS_OK);
}


void Model::get_num_params(int * const numberOfParameters) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int maxStrLen;
  pKIM->get_num_params(numberOfParameters, &maxStrLen);
}

int Model::get_parameter(int const index, int * const extent,
                         void ** const ptr)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  char const * str;
  int err;
  err = pKIM->get_parameter((intptr_t) index, &str);
  if (err != KIM_STATUS_OK) return true;

  *ptr = pKIM->get_data(str, &err);
  return (err < KIM_STATUS_OK);
}

int Model::get_parameter_data_type(int const index,
                                   DataType * const dataType) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  char const * str;
  int err;
  err = pKIM->get_parameter(index, &str);
  int old_ind = pKIM->get_index(str, &err);
  OLD_KIM::KIMBaseElement * KBE = (OLD_KIM::KIMBaseElement *) pKIM->model.data.p;
  std::string type = KBE[old_ind].type;
  DataType PDT;
  if (type == "double")
  {
    PDT = DATA_TYPE::Double;
  }
  else if (type == "integer")
  {
    PDT = DATA_TYPE::Integer;
  }
  else
  {
    // Unknown
  }

  *dataType = PDT;
  return (err < KIM_STATUS_OK);
}

int Model::get_parameter_description(int const index,
                                     std::string * const description) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  char const * str;
  err = pKIM->get_parameter(index, &str);
  *description = str;
  return (err < KIM_STATUS_OK);
}


void Model::set_sim_buffer(void const * const ptr)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  pKIM->set_sim_buffer((void *) ptr, &err);
}

void Model::get_sim_buffer(void ** const ptr) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *ptr = pKIM->get_sim_buffer(&err);
}

Model::Model() : pimpl(0)
{
}

Model::~Model()
{
}

}  // namespace KIM
