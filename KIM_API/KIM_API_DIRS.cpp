#include <cstdlib>
#include "KIM_API_DIRS.h"

void directoryPath(DirectoryPathType type, std::list<std::string>* const lst)
{
  switch (type)
  {
    case KIM_DIR:
      lst->push_back(std::string(KIMDIR));
      break;
    case KIM_MODEL_DRIVERS_DIR:
      lst->push_back(std::string(MODELDRIVERDIR));
      break;
    case KIM_MODELS_DIR:
      lst->push_back(std::string(MODELDIR));
      break;
    default:
      break;
  }
  return;
}
