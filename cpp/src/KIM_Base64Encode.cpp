//
// Distributed under the existing KIM-API License.
// This is a simple program that reads a file and encodes it in base64
// and writes it to a new file, like XXD but with base64 encoding

#include "KIM_Base64.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>

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

    std::ifstream inputFile(fileName, std::ios::binary);
    std::ofstream outputFile(outputFileName);

    unsigned int len = 0;

    const size_t chunk = 1024 * 48; // 48 kb buffer, multiple of 3, 
                                    // base64 writes 3 bytes to 4 char
    std::string buffer;             // buffer to store known amount of raw data
    buffer.reserve(chunk);
    size_t linepos =0 ;

    // C++ const is different from C const (equivalent to static const), 
    // so we need to use extern
    std::string header = "extern unsigned char " + xxdFormatFileName + "[] = R\"(\n";

    outputFile.write(header.data(), header.length());

    std::vector<char> rawBuffer(chunk); // buffer to store raw io data
    
    while (inputFile.read(rawBuffer.data(), chunk) || inputFile.gcount()){
        buffer.assign(rawBuffer.data(), inputFile.gcount()); // assign exact string from raw buffer
                                                               // If raw < chunk this will deal with it

        // Possible optimization: stored encoded data in buffer pointer, like the decode function
        std::string encoded = Base64::encode(buffer);
        for (char &c : encoded){
            outputFile.put(c);
            linepos++;
            len++;
            if (linepos >= Base64::MAX_BASE64_WIDTH){
                outputFile.put('\n');
                len++;
                linepos = 0;
            }
        }
    }
    if (linepos > 0) outputFile.put('\n');

    len += 2; // two \n in beginning and end
    std::string footer = ")\";\nextern unsigned int " +  xxdFormatFileName + "_len = " +  std::to_string(len) + ";\n";

    outputFile.write(footer.data(),  static_cast<std::streamsize>(footer.length()));
    inputFile.close();
    outputFile.close();
}

int main(int argc, char * argv[]){
    if (argc != 3){
        std::cerr << "Improper arguments, please provide input and output file name\n";
        return 1;
    }

    std::string fileName = argv[1];
    std::string outputFileName = argv[2];
    WriteEncodedFile(fileName, outputFileName);

    return 0;
}
