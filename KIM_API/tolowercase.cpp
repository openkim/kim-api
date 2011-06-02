#include <stdlib.h>
#include <iostream>
#include <ctype.h>
#include <string.h>

#ifndef KIM_DIR
#define KIM_DIR "../../"
#endif

#ifndef KIM_DIR_MODELS
#define KIM_DIR_MODELS "../../MODELs/"
#endif

#ifndef KIM_DIR_TESTS
#define KIM_DIR_TESTS "../../TESTs/"
#endif

using namespace std;
int main(int argc, char** argv) {
	string inputlist="";


	//cin >> inputlist;
	getline(cin, inputlist);

	for(int i=0;i<inputlist.length();i++){
		inputlist[i]=tolower(inputlist[i]);
	}
	cout <<inputlist;
}
