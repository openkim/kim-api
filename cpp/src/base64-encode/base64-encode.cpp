//
// Distributed under the Boost Software License 1.0
// This is a simple program that reads a file and encodes it in base64
// and writes it to a new file, like XXD but with base64 encoding
// Currently only option -h, -v, -i are supported, more might be added in future
// For any query please contact authors, Navneeth Mohan (nav-mohan) or Amit
// Gupta (ipcamit)


#include "base64.hpp"
#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#define VERSION "0.1"

// quick replacement of to_string for C++98
inline std::string int_to_string(int value)
{
  std::ostringstream oss;
  oss << value;
  return oss.str();
}


/// \brief Write the Base^4 encode binary file as a C++ source file, emulating
/// XXD
///
/// \param[in] fileName The name of the file to encode
/// \param[in] outputFileName The name of the file to write the encoded file to
/// \sa base64::encode
void WriteEncodedFile(std::string & fileName, std::string & outputFileName)
{
  unsigned int len = 0;  // total written len of all the content
  unsigned int linepos
      = 0;  // current io positions, always < base64::MAX_BASE64_WIDTH
  unsigned int n_base64_char;  // total base64 char obtained
  char rawBuffer[base64::IO_CHUNK];  // buffer to store raw io data
  char encodedBuffer[base64::encoded_size(
      base64::IO_CHUNK)];  // buffer for converted data

  // setup fine names and pointers
  std::string xxdFormatFileName = fileName;
  // Make the file names compatible with xxd variable name format
  std::replace(xxdFormatFileName.begin(), xxdFormatFileName.end(), '.', '_');
  std::replace(xxdFormatFileName.begin(), xxdFormatFileName.end(), '-', '_');

  std::ifstream inputFile(fileName.c_str(), std::ios::binary);
  std::ofstream outputFile(outputFileName.c_str());


  // C++ const is different from C const (equivalent to static const),
  // so we need to use extern
  std::string header
      = "extern unsigned char " + xxdFormatFileName + "[] = \n\"";
  outputFile.write(header.data(), header.length());

  // Read IO_CHUNK, convert to base64 and write to file
  while (inputFile.read(rawBuffer, base64::IO_CHUNK) || inputFile.gcount())
  {
    n_base64_char
        = base64::encode(encodedBuffer, rawBuffer, inputFile.gcount());

    for (unsigned int i = 0; i < n_base64_char; i++)
    {
      outputFile.put(encodedBuffer[i]);
      linepos++;
      len++;
      if (linepos >= base64::MAX_BASE64_WIDTH)
      {
        outputFile.write("\"\n\"", 3);
        linepos = 0;
      }
    }
  }

  std::string footer = "\";\nextern unsigned int " + xxdFormatFileName
                       + "_len = " + int_to_string(len) + ";\n";

  outputFile.write(footer.data(),
                   static_cast<std::streamsize>(footer.length()));
  inputFile.close();
  outputFile.close();
}


void print_help()
{
  std::cout
      << "Usage: \n\n"
      << "base64-encode [OPTIONS] [FILE IN] [FILE OUT]\n\n"
      << "base64-encode is a simple xxd replacement to convert files\n"
      << "into embeddable C++ source code. Instead of using binary array\n"
      << "it used base64 strings, which makes it much more performant \n"
      << "for large files. Currently only -i option is supported for \n"
      << "compatibility. More options to be released in future.\n"
      << "\n"
      << "Options:\n"
      << "  -h        Show this help message\n"
      << "  -v        Show version information\n"
      << "  -i        Convert input file to C++ source\n";
}


int main(int argc, char * argv[])
{
  if ((argc < 2) || (argc > 4))
  {
    std::cerr << "IMPROPER ARGUMENTS, please provide input and output file "
                 "names or options\n";
    print_help();
    return 1;
  }
  std::string option = argv[1];
  if (option == "-h") { print_help(); }
  else if (option == "-v") { std::cout << VERSION << std::endl; }
  else if (option == "-i" && argc == 4)
  {
    std::string fileName = argv[2];
    std::string outputFileName = argv[3];
    WriteEncodedFile(fileName, outputFileName);
  }
  else
  {
    std::cerr << "IMPROPER ARGUMENTS, please provide input and output file "
                 "names or options\n";
    print_help();
    return 1;
  }

  return 0;
}
