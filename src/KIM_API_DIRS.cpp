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
// Copyright (c) 2013--2016, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <cstdlib>
#include <cstring>
#include <cctype>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include "KIM_API_DIRS.h"

#define LINELEN 256

void sanitizeString(std::string &str)
{
  std::string::iterator itr;
  for (itr=str.begin(); itr != str.end(); ++itr)
  {
    if (isalnum(*itr))
    {
      *itr = toupper(*itr);
    }
    else
    {
      *itr = '_';
    }
  }
}

std::string getConfigFileName()
{
  std::string configFileName;

  if (INPLACE)
  {
    configFileName = KIMDIR;
  }
  else
  {
    configFileName = getenv("HOME");
  }
  configFileName.append("/.").append(PACKAGENAME);
  configFileName.append("/config-v").append(VERSION_MAJOR);

  std::string varName(PACKAGENAME);
  sanitizeString(varName);
  varName.append("_CONFIG_FILE");
  char const* const varVal = getenv(varName.c_str());
  if (NULL != varVal)
  {
    configFileName = varVal;
  }

  return configFileName;
}

std::vector<std::string> getUserDirs()
{
  std::vector<std::string> userDirs(2);
  std::string configFile(getConfigFileName());
  std::ifstream cfl;
  cfl.open(configFile.c_str(), std::ifstream::in);
  if (!cfl)
  {
    // unable to open file.
    userDirs[0] = "";
    userDirs[1] = "";
  }
  else
  {
    char line[LINELEN];
    if (cfl.getline(line, LINELEN))
    {
      char *word;
      char const* const sep = " \t=";

      word = strtok(line, sep);
      if (strcmp("model_drivers_dir", word))
      {
        // error so exit
        std::cerr << "Unknown line in " << configFile << " file: "
                  << word << std::endl;
        userDirs[0] = "";
      }
      word = strtok(NULL, sep);
      userDirs[0] = word;
      std::size_t found_home = userDirs[0].find("~/");
      std::size_t found_root = userDirs[0].find("/");
      if (found_home == 0)
      {
        userDirs[0].replace(0, 1, getenv("HOME"));
      }
      else if (found_root != 0)
      {
        // error so exit
        std::cerr << "Invalid value in " << configFile << " file: "
                  << word << std::endl;
        userDirs[0] = "";
      }
      else
      {
        // nothing to do
      }
    }

    if (cfl.getline(line, LINELEN))
    {
      char *word;
      char const* const sep = " \t=";

      word = strtok(line, sep);
      if (strcmp("models_dir", word))
      {
        // error so exit
        std::cerr << "Unknown line in " << configFile << " file: "
                  << word << std::endl;
        userDirs[1] = "";
      }
      word = strtok(NULL, sep);
      userDirs[1] = word;
      std::size_t found_home = userDirs[1].find("~/");
      std::size_t found_root = userDirs[1].find("/");
      if (found_home == 0)
      {
        userDirs[1].replace(0, 1, getenv("HOME"));
      }
      else if (found_root != 0)
      {
        // error so exit
        std::cerr << "Invalid value in " << configFile << " file: "
                  << word << std::endl;
        userDirs[1] = "";
      }
      else
      {
        // nothing to do
      }
    }

    cfl.close();
  }

  return userDirs;
}

void pushEnvDirs(DirectoryPathType type,
                 std::list<std::string>* const lst)
{

  std::string varName = PACKAGENAME;
  sanitizeString(varName);
  switch (type)
  {
    case KIM_MODEL_DRIVERS_DIR:
      varName.append("_MODEL_DRIVERS_DIR");
      break;
    case KIM_MODELS_DIR:
      varName.append("_MODELS_DIR");
      break;
    default:
      break;
  }
  char const* const varVal = getenv(varName.c_str());
  if (NULL != varVal)
  {
    std::string varValString(varVal);
    std::istringstream iss(varValString);
    std::string token;
    while (std::getline(iss, token, ':'))
    {
      lst->push_back(token);
    }
  }
}

void searchPaths(DirectoryPathType type, std::list<std::string>* const lst)
{
  std::vector<std::string> userDirs = getUserDirs();

  switch (type)
  {
    case KIM_MODEL_DRIVERS_DIR:
      lst->push_back(std::string("."));
      pushEnvDirs(type,lst);
      if (0 != userDirs[0].compare(""))
      {
        lst->push_back(userDirs[0]);
      }
      lst->push_back(
          std::string(PACKAGEDIR).append("/").append(MODELDRIVERSDIR));
      break;
    case KIM_MODELS_DIR:
      lst->push_back(std::string("."));
      pushEnvDirs(type,lst);
      if (0 != userDirs[1].compare(""))
      {
        lst->push_back(userDirs[1]);
      }
      lst->push_back(
          std::string(PACKAGEDIR).append("/").append(MODELSDIR));
      break;
    default:
      break;
  }
  return;
}
