#include "base64.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>

void writeEncodedFile(std::string & filename, std::string & output_filename){

    std::string parsed_file_string = filename;
    std::replace(parsed_file_string.begin(), parsed_file_string.end(), '.', '_');
    std::replace(parsed_file_string.begin(), parsed_file_string.end(), '-', '_');
    std::string encoded_file_name = output_filename; 

    std::ifstream input_file(filename, std::ios::binary);
    std::ofstream output_file(encoded_file_name);

    unsigned int len = 0;

    const size_t chunk = 1024 * 48; // 48 kb buffer, multiple of 3, base64 3bytes to 4 char
    const size_t linewidth = 76;
    std::string buffer;
    buffer.reserve(chunk);
    size_t linepos =0 ;

    std::string header = "extern unsigned char " + parsed_file_string + "[] = R\"(\n";
    // std::string header = "constexpr unsigned char " + parsed_file_string + "[] = R\"(\n";
    output_file.write(header.data(), header.length());

    std::vector<char> raw_buffer(chunk);
    
    while (input_file.read(raw_buffer.data(), chunk) || input_file.gcount()){
        buffer.assign(raw_buffer.data(), input_file.gcount()); // assign exact string from raw buffer
                                                               // If raw < chunk this will deal with it
        std::string encoded = Base64::encode(buffer);
        for (char &c : encoded){
            output_file.put(c);
            linepos++;
            len++;
            if (linepos >= linewidth){
                output_file.put('\n');
                len++;
                linepos = 0;
            }
        }
    }
    if (linepos > 0) output_file.put('\n');

    len += 2; // two \n in beginning and end
    std::string footer = ")\";\nextern unsigned int " +  parsed_file_string + "_len = " +  std::to_string(len) + ";\n";

    output_file.write(footer.data(), footer.length());
    input_file.close();
    output_file.close();
}

int main(int argc, char * argv[]){
    if (argc != 3){
        for(int i = 0; i < argc; i++){
            std::cout << ">>>>>" << argv[i] << "\n";
        }
        std::cerr << "Improper arguments, please provide just the encoding filename\n";
        return 1;
    }

    std::string filename = argv[1];
    std::string output_filename = argv[2];
    writeEncodedFile(filename, output_filename);

    return 0;
}
