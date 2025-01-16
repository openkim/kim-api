//
// Distributed under the existing KIM-API License.
// This is a simple program that reads a file and encodes it in base64
// and writes it to a new file, like XXD but with base64 encoding

#include "KIM_Base64.hpp"
#include "KIM_Version.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <cstdio>
#include <string>

/// \brief Write the Base^4 encode binary file as a C++ source file, emulating XXD
///
/// \param[in] fileName The name of the file to encode
/// \param[in] outputFileName The name of the file to write the encoded file to
/// \sa Base64::encode
void WriteEncodedFile(std::string & fileName, std::string & outputFileName){

    std::string xxdFormatFileName = fileName;
    // Make the file names compatible with xxd variable name format
    std::replace(xxdFormatFileName.begin(), xxdFormatFileName.end(), '.', '_');
    std::replace(xxdFormatFileName.begin(), xxdFormatFileName.end(), '-', '_');

    // std::ifstream inputFile(fileName, std::ios::binary);
    // std::ofstream outputFile(outputFileName);

    FILE *inputFile = fopen(fileName.c_str(), "rb");
    if(!inputFile) { std::cerr << "Error opening input file" << std::endl; return;}

    FILE *outputFile = fopen(outputFileName.c_str(), "w");
    if(!outputFile) {std::cerr << "Error opening output file" << std::endl; return;}

    unsigned int len = 0;

    const size_t chunk = 1024 * 48; // 48 kb buffer, multiple of 3, 
                                    // base64 writes 3 bytes to 4 char
    std::string buffer;             // buffer to store known amount of raw data
    buffer.reserve(chunk);
    size_t linepos =0 ;

    // C++ const is different from C const (equivalent to static const), 
    // so we need to use extern
    // std::string header = "extern unsigned char " + xxdFormatFileName + "[] = R\"(\n";
    std::string header = "extern unsigned char " + xxdFormatFileName + "[] = \"(";

    // outputFile.write(header.data(), header.length());
    fwrite(header.data(), sizeof(char), header.size(), outputFile);

    std::vector<char> rawBuffer(chunk); // buffer to store raw io data
    
    // while (inputFile.read(rawBuffer.data(), chunk) || inputFile.gcount()){
    while( size_t bytesRead = fread(rawBuffer.data(), 1, chunk, inputFile) )
    {
        buffer.assign(rawBuffer.data(), bytesRead); // assign exact string from raw buffer
                                                               // If raw < chunk this will deal with it

        // Possible optimization: stored encoded data in buffer pointer, like the decode function
        std::string encoded = Base64::encode(buffer);
        for(size_t i = 0; i < encoded.size(); i++){
            char & c = encoded[i];
            // outputFile.put(c);
            fputc(c,outputFile);
            linepos++;
            len++;
            if (linepos >= Base64::MAX_BASE64_WIDTH){
                // outputFile.put('\n');
                // fputc('\n',outputFile);
                // fwrite("\\n",3,1,outputFile);
                len++;
                linepos = 0;
            }
        }
    }
    // if (linepos > 0) outputFile.put('\n');
    // if (linepos > 0) fputc('\n',outputFile);
    // if (linepos > 0) fwrite("\\n",3,1,outputFile);


    len += 2; // two \n in beginning and end
    // std::string footer = ")\";\nextern unsigned int " +  xxdFormatFileName + "_len = " +  std::to_string(len) + ";\n";
    std::stringstream footerStream;
    footerStream << ")\";\nextern unsigned int " << xxdFormatFileName << "_len = " << len << ";\n";
    std::string footer = footerStream.str();
    // outputFile.write(footer.data(),  static_cast<std::streamsize>(footer.length()));
    fwrite(footer.data(), sizeof(char), footer.size(), outputFile);
    // inputFile.close();
    fclose(inputFile);
    // outputFile.close();
    fclose(outputFile);
}

void usage(std::string name)
{
    size_t beg = name.find_last_of("/\\");
    if (beg != std::string::npos) name = name.substr(beg + 1, std::string::npos);

    // Follows docopt.org format
    std::cerr << "Usage:\n"
            << "  " << name << " " << "<input-filename> " << "<output-filename>\n"
            << "  " << name << " " << "--version\n";
    // note: this interface is likely to change in future kim-api releases
}

int main(int argc, char * argv[]){

    if ((argc == 2) && (std::string(argv[1]) == "--version"))
    {
        std::cout << KIM_VERSION_STRING << std::endl;
        return 0;
    }
    if ((argc != 3))
    {
        usage(argv[0]);
        return -1;
    }


    std::string fileName = argv[1];
    std::string outputFileName = argv[2];
    WriteEncodedFile(fileName, outputFileName);

    return 0;
}
