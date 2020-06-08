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
