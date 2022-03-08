//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
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


#include "KIM_SimulatorHeaders.hpp"
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <string>

int main()
{
  KIM::SimulatorModel * SM;
  int error = KIM::SimulatorModel::Create(
      "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu", &SM);

  if (error)
  {
    std::cout << "Can't create SM." << std::endl;
    return 1;
  }

  {
    std::string const * pSimulatorName;
    std::string const * pSimulatorVersion;
    SM->GetSimulatorNameAndVersion(&pSimulatorName, &pSimulatorVersion);
    std::cout << "Simulator name    : " << *pSimulatorName << std::endl
              << "Simulator version : " << *pSimulatorVersion << std::endl
              << std::endl;
  }

  {
    int extent;
    SM->GetNumberOfSupportedSpecies(&extent);
    std::cout << "SM supports " << extent << " species:" << std::endl;
    for (int i = 0; i < extent; ++i)
    {
      std::string const * pSpecies;
      error = SM->GetSupportedSpecies(i, &pSpecies);
      if (error)
      {
        std::cout << "Unable to get species." << std::endl;
        goto fail;
      }
      else
      {
        std::cout << "\t" << std::setw(2) << i << " " << *pSpecies << std::endl;
      }
    }
    std::cout << std::endl;
  }

  {
    error = SM->AddTemplateMap("atom-type-sym-list", "Pb Pb Au Pb");
    if (error)
    {
      std::cout << "Unable to add template map." << std::endl;
      goto fail;
    }
    SM->CloseTemplateMap();
    int numberFields;
    SM->GetNumberOfSimulatorFields(&numberFields);
    std::cout << "SM has " << std::setw(2) << numberFields
              << " fields :" << std::endl;

    for (int i = 0; i < numberFields; ++i)
    {
      int extent;
      std::string const * pFieldName;
      error = SM->GetSimulatorFieldMetadata(i, &extent, &pFieldName);
      std::cout << "  Field " << std::setw(2) << i << " is " << *pFieldName
                << " and has " << std::setw(2) << extent
                << " lines:" << std::endl;
      for (int j = 0; j < extent; ++j)
      {
        std::string const * pFieldLine;
        error = SM->GetSimulatorFieldLine(i, j, &pFieldLine);
        if (error)
        {
          std::cout << "Unable to get field line." << std::endl;
          goto fail;
        }
        else
        {
          std::cout << "\t" << *pFieldLine << std::endl;
        }
      }
    }
    std::cout << std::endl;
  }

  {
    std::string const * pDirName;
    SM->GetParameterFileDirectoryName(&pDirName);
    std::cout << "SM param dir name is " << *pDirName << std::endl;

    std::string const * pSpecName;
    SM->GetSpecificationFileName(&pSpecName);
    std::cout << "SM spec file name is " << *pSpecName << std::endl
              << std::endl;
    error
        = system((std::string("cat ") + *pDirName + "/" + *pSpecName).c_str());
    std::cout << std::endl;

    int numberParamFiles;
    SM->GetNumberOfParameterFiles(&numberParamFiles);
    std::cout << "SM has " << numberParamFiles
              << " parameter files:" << std::endl;
    for (int i = 0; i < numberParamFiles; ++i)
    {
      std::string const * pParamFileBasename;
      error = SM->GetParameterFileBasename(i, &pParamFileBasename);
      if (error)
      {
        std::cout << "Unable to get parameter file basename." << std::endl;
        goto fail;
      }
      else
      {
        std::cout << "Parameter file " << std::setw(2) << i
                  << " has basename : " << *pParamFileBasename << std::endl;
        error = system(
            (std::string("cat ") + *pDirName + "/" + *pParamFileBasename)
                .c_str());
        std::cout << std::endl;
      }
    }
  }

  KIM::SimulatorModel::Destroy(&SM);
  return 0;  // false
fail:
  KIM::SimulatorModel::Destroy(&SM);
  return 1;  // true
}
