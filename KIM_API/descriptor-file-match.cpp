#include <fstream>
#include <sstream>
#include "KIM_API.h"
#include "KIM_API_status.h"

int main(int argc, char* argv[])
{
   if (argc != 3)
   {
      std::cerr << "Usage: " << argv[0] << " TestKimFile ModelKimFile"
                << std::endl;
      std::cout << "NOMATCH" << std::endl;
      return 1;
   }

   std::fstream testkimfile;
   std::fstream modelkimfile;
   testkimfile.open(argv[1], std::fstream::in);
   modelkimfile.open(argv[2], std::fstream::in);

   if (testkimfile.fail() || modelkimfile.fail())
   {
      std::cerr << "file open failed." << std::endl;
      std::cout << "NOMATCH" << std::endl;
      return 1;
   }

   std::stringstream testkimfile_str;
   std::stringstream modelkimfile_str;
   testkimfile_str << testkimfile.rdbuf();
   modelkimfile_str << modelkimfile.rdbuf();

   KIM_API_model kim;
   int error;

   error = kim.match((char*) testkimfile_str.str().c_str(),
                     (char*) modelkimfile_str.str().c_str());

   int retval;
   if (error == KIM_STATUS_OK)
   {
      retval = 0;
      std::cout << "MATCH" << std::endl;
   }
   else
   {
      retval = 2;
      std::cout << "NOMATCH" << std::endl;
   }

   return retval;
}
