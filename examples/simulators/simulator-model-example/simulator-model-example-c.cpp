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


extern "C" {
#include "KIM_SimulatorHeaders.h"
}
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <string>

int main()
{
  KIM_SimulatorModel * SM;
  int error = KIM_SimulatorModel_Create(
      "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu", &SM);

  if (error)
  {
    std::cout << "Can't create SM." << std::endl;
    goto fail;
  }

  {
    char const * pSimulatorName;
    char const * pSimulatorVersion;
    KIM_SimulatorModel_GetSimulatorNameAndVersion(
        SM, &pSimulatorName, &pSimulatorVersion);
    std::cout << "Simulator name    : " << pSimulatorName << std::endl
              << "Simulator version : " << pSimulatorVersion << std::endl
              << std::endl;
  }

  {
    int extent;
    KIM_SimulatorModel_GetNumberOfSupportedSpecies(SM, &extent);
    std::cout << "SM supports " << extent << " species:" << std::endl;
    for (int i = 0; i < extent; ++i)
    {
      char const * pSpecies;
      error = KIM_SimulatorModel_GetSupportedSpecies(SM, i, &pSpecies);
      if (error)
      {
        std::cout << "Unable to get species." << std::endl;
        goto fail;
      }
      else
      {
        std::cout << "\t" << std::setw(2) << i << " " << pSpecies << std::endl;
      }
    }
    std::cout << std::endl;
  }

  {
    error = KIM_SimulatorModel_AddTemplateMap(
        SM, "atom-type-sym-list", "Pb Pb Au Pb");
    if (error)
    {
      std::cout << "Unable to add template map." << std::endl;
      goto fail;
    }
    KIM_SimulatorModel_CloseTemplateMap(SM);
    int numberFields;
    KIM_SimulatorModel_GetNumberOfSimulatorFields(SM, &numberFields);
    std::cout << "SM has " << std::setw(2) << numberFields
              << " fields :" << std::endl;

    for (int i = 0; i < numberFields; ++i)
    {
      int extent;
      char const * pFieldName;
      error = KIM_SimulatorModel_GetSimulatorFieldMetadata(
          SM, i, &extent, &pFieldName);
      std::cout << "  Field " << std::setw(2) << i << " is " << pFieldName
                << " and has " << std::setw(2) << extent
                << " lines:" << std::endl;
      for (int j = 0; j < extent; ++j)
      {
        char const * pFieldLine;
        error = KIM_SimulatorModel_GetSimulatorFieldLine(SM, i, j, &pFieldLine);
        if (error)
        {
          std::cout << "Unable to get field line." << std::endl;
          goto fail;
        }
        else
        {
          std::cout << "\t" << pFieldLine << std::endl;
        }
      }
    }
    std::cout << std::endl;
  }

  {
    char const * pDirName;
    KIM_SimulatorModel_GetParameterFileDirectoryName(SM, &pDirName);
    std::cout << "SM param dir name is " << pDirName << std::endl;

    char const * pSpecName;
    KIM_SimulatorModel_GetSpecificationFileName(SM, &pSpecName);
    std::cout << "SM spec file name is " << pSpecName << std::endl << std::endl;
    error = system((std::string("cat ") + pDirName + "/" + pSpecName).c_str());
    std::cout << std::endl;

    int numberParamFiles;
    KIM_SimulatorModel_GetNumberOfParameterFiles(SM, &numberParamFiles);
    std::cout << "SM has " << numberParamFiles
              << " parameter files:" << std::endl;
    for (int i = 0; i < numberParamFiles; ++i)
    {
      char const * pParamFileBasename;
      error = KIM_SimulatorModel_GetParameterFileBasename(
          SM, i, &pParamFileBasename);
      if (error)
      {
        std::cout << "Unable to get parameter file basename." << std::endl;
        goto fail;
      }
      else
      {
        std::cout << "Parameter file " << std::setw(2) << i
                  << " has basename : " << pParamFileBasename << std::endl;
        error
            = system((std::string("cat ") + pDirName + "/" + pParamFileBasename)
                         .c_str());
        std::cout << std::endl;
      }
    }
  }

  KIM_SimulatorModel_Destroy(&SM);
  return 0;
fail:
  KIM_SimulatorModel_Destroy(&SM);
  return 1;
}
