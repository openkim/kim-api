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


#include <string>
#include <fstream>
#include <sstream>
#include <string>
#include "KIM_API.h"
#include "KIM_API_status.h"

void replaceAll(std::string& str,
                std::string const& from,
                std::string const& to);

int main(int argc, char* argv[])
{
   if (argc != 3)
   {
      std::cerr << "Usage: " << argv[0] << " SimulatorKIM_File ModelKIM_File"
                << std::endl;
      std::cout << "NOMATCH" << std::endl;
      return 1;
   }

   std::fstream simulatorKIM_File;
   std::fstream modelKIM_File;
   simulatorKIM_File.open(argv[1], std::fstream::in);
   modelKIM_File.open(argv[2], std::fstream::in);

   if (simulatorKIM_File.fail() || modelKIM_File.fail())
   {
      std::cerr << "file open failed." << std::endl;
      std::cout << "NOMATCH" << std::endl;
      return 1;
   }

   std::string simulatorKIM;
   {
     std::stringstream simulatorKIM_FileStr;
     simulatorKIM_FileStr << simulatorKIM_File.rdbuf();
     simulatorKIM = simulatorKIM_FileStr.str();
   }
   if (std::string::npos != simulatorKIM.find("\r"))
   {
     std::cerr << "error: simulator file contains carriage-return characters."
               << std::endl;
     replaceAll(simulatorKIM, "\r","");
   }

   std::string modelKIM;
   {
     std::stringstream modelKIM_FileStr;
     modelKIM_FileStr << modelKIM_File.rdbuf();
     modelKIM = modelKIM_FileStr.str();
   }
   if (std::string::npos != modelKIM.find("\r"))
   {
     std::cerr << "error: model file contains carriage-return characters."
               << std::endl;
     replaceAll(modelKIM, "\r","");
   }

   KIM_API_model kim;
   int error;

   error = kim.match((char*) simulatorKIM.c_str(), (char*) modelKIM.c_str());

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


void replaceAll(std::string& str,
                std::string const& from,
                std::string const& to)
{
  if(from.empty())
    return;
  std::string wsRet;
  wsRet.reserve(str.length());
  size_t start_pos = 0, pos;
  while((pos = str.find(from, start_pos)) != std::string::npos)
  {
    wsRet += str.substr(start_pos, pos - start_pos);
    wsRet += to;
    pos += from.length();
    start_pos = pos;
  }
  wsRet += str.substr(start_pos);
  str.swap(wsRet);  // faster than str = wsRet;
}
