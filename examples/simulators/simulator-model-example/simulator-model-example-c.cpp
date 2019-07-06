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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
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
    system((std::string("cat ") + pDirName + "/" + pSpecName).c_str());
    std::cout << std::endl;

    int numberParamFiles;
    KIM_SimulatorModel_GetNumberOfParameterFiles(SM, &numberParamFiles);
    std::cout << "SM has " << numberParamFiles
              << " parameter files:" << std::endl;
    for (int i = 0; i < numberParamFiles; ++i)
    {
      char const * pParamFileName;
      error = KIM_SimulatorModel_GetParameterFileName(SM, i, &pParamFileName);
      if (error)
      {
        std::cout << "Unable to get parameter file name." << std::endl;
        goto fail;
      }
      else
      {
        std::cout << "Parameter file " << std::setw(2) << i
                  << " has name : " << pParamFileName << std::endl;
        system((std::string("cat ") + pDirName + "/" + pParamFileName).c_str());
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
