#define DEBUG
#include "edn.hpp"
#include <iostream>

using edn::EdnNode;
using edn::read;
using edn::pprint;

int main() {	
  try {
    EdnNode someMap = read("{:some :map :with [a vector of symbols]}");
    std::cout << pprint(someMap) << std::endl;
  } catch (const char* e) {
    std::cout << "Error parsing: " << e << std::endl;
  }
}
