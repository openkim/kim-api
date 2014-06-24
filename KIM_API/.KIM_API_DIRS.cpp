#include <cstdlib>
#include "KIM_API_DIRS.h"

void directoryPath(DirectoryPathType type, std::list<std::string>* const lst)
{
  switch (type)
  {
    case KIM_DIR:
      lst->push_back(std::string(LIBDIR)
                     .append(PACKAGENAME));
      break;
    case KIM_MODEL_DRIVERS_DIR:
      lst->push_back(std::string("."));
      lst->push_back(std::string(getenv("HOME"))
                     .append("/.kim/")
                     .append(PACKAGENAME)
                     .append("/MODEL_DRIVERS"));
      lst->push_back(std::string(LIBDIR)
                     .append("/")
                     .append(PACKAGENAME)
                     .append("/MODEL_DRIVERS"));
      break;
    case KIM_MODELS_DIR:
      lst->push_back(std::string("."));
      lst->push_back(std::string(getenv("HOME"))
                     .append("/.kim/")
                     .append(PACKAGENAME)
                     .append("/MODELS"));
      lst->push_back(std::string(LIBDIR)
                     .append("/")
                     .append(PACKAGENAME)
                     .append("/MODELS"));
      break;
    default:
      break;
  }
  return;
}
