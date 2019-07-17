#define DEBUG (1) 
#include "edn.hpp"
#include <string>
#include <iostream>
#include <sstream>

int main() {
  edn::validSymbolChars += "<>'";
  std::string ednString;
  while (true) { 
    std::cout << "edn> ";
    getline(std::cin, ednString);
    if (ednString.length() == 0) {
      std::cout << std::endl;
    } else { 
      try { 
        edn::EdnNode node = edn::read(ednString);
        std::cout << edn::pprint(node) << std::endl;
      } catch (const char* e) { 
        std::cout << "Error: " << e << std::endl;
      }
    }
  }

  return 0; 
}
