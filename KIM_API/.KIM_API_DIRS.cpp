#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>
#include "KIM_API_DIRS.h"

#define LINELEN 256
std::string userDirs[2];
void getUserDirs()
{
  std::string configFile(getenv("HOME"));
  configFile.append("/.").append(PACKAGENAME);
  configFile.append("/config-v").append(VERSION_MAJOR);

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

  return;
}

void directoryPath(DirectoryPathType type, std::list<std::string>* const lst)
{
  switch (type)
  {
    case KIM_MODEL_DRIVERS_DIR:
    case KIM_MODELS_DIR:
      getUserDirs();
      break;
    default:
    case KIM_DIR:
      break;

  }

  switch (type)
  {
    case KIM_DIR:
      lst->push_back(std::string(LIBDIR).append(PACKAGENAME));
      break;
    case KIM_MODEL_DRIVERS_DIR:
      lst->push_back(std::string("."));
      if (0 != userDirs[0].compare(""))
      {
        lst->push_back(userDirs[0]);
      }
      lst->push_back(std::string(LIBDIR)
                     .append("/").append(PACKAGENAME).append("/MODEL_DRIVERS"));
      break;
    case KIM_MODELS_DIR:
      lst->push_back(std::string("."));
      if (0 != userDirs[1].compare(""))
      {
        lst->push_back(userDirs[1]);
      }
      lst->push_back(std::string(LIBDIR)
                     .append("/").append(PACKAGENAME).append("/MODELS"));
      break;
    default:
      break;
  }
  return;
}
