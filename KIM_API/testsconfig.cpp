#include <stdlib.h>
#include <iostream>
#include <fstream>
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
	char  listtestsfile[]="listtestname.txt";
	char  listmodelsfile[]="listmodelname.txt";
	char testsmkfile[]="testsmake.mk";
	
	char list_tests[100][160],list_models[100][160];
	char *tmp;
        ifstream in_testsfile;
        ofstream out_tetsmkfile;
	
	in_testsfile.open(listtestsfile);

	if(!in_testsfile){
             cout<<"Can not open file:"<<listtestsfile<<":"<<endl;
             exit(327);
         }


	//read 1st files
	int counter=0;
	while(!in_testsfile.eof()) {

		in_testsfile.getline(list_tests[counter],160,' ');
		counter++;
	}
        for(int i=0; i<strlen(list_tests[counter-1]);i++){
		if(list_tests[counter-1][i]=='\n') list_tests[counter-1][i]='\0';
	}
        in_testsfile.close();

	//read 2nd files
	in_testsfile.open(listmodelsfile);
	if(!in_testsfile){
             cout<<"Can not open file:"<<listmodelsfile<<":"<<endl;
             exit(327);
         }
	int counter_mdl=0;
	while(!in_testsfile.eof()) {

		in_testsfile.getline(list_models[counter_mdl],160,' ');
		counter_mdl++;
	}
        for(int i=0; i<strlen(list_models[counter_mdl-1]);i++){
		if(list_models[counter_mdl-1][i]=='\n') list_models[counter_mdl-1][i]='\0';
	}
        in_testsfile.close();
        //creating testsmake mk

	out_tetsmkfile.open(testsmkfile);
	if(!out_tetsmkfile){
             cout<<"Can not open file:"<<testsmkfile<<":"<<endl;
             exit(327);
        }
        //create target all;
	out_tetsmkfile << "all:"<<endl;
	out_tetsmkfile << "\t cd $(KIM_API_DIR)/"<<"; $(MAKE) all; cd "<<"$(RETURN_DIR)"<<endl;
	
        for(int i=0; i<counter_mdl;i++){
        	out_tetsmkfile << "\t cd $(KIM_MODELS_DIR)/"<<list_models[i]<<"; $(MAKE) all; cd ";
		out_tetsmkfile <<"$(RETURN_DIR)"<<endl;
        }

        for(int i=0; i<counter;i++){
        	out_tetsmkfile << "\t cd $(KIM_TESTS_DIR)/"<<list_tests[i]<<"; $(MAKE) all; cd ";
		out_tetsmkfile <<"$(RETURN_DIR)"<<endl;
        }
	//create target clean
        out_tetsmkfile << "clean:"<<endl;
	
        for(int i=0; i<counter_mdl;i++){
        	out_tetsmkfile << "\t cd $(KIM_MODELS_DIR)/"<<list_models[i]<<"; $(MAKE) clean; cd ";
		out_tetsmkfile <<"$(RETURN_DIR)"<<endl;
        }

        for(int i=0; i<counter;i++){
        	out_tetsmkfile << "\t cd $(KIM_TESTS_DIR)/"<<list_tests[i]<<"; $(MAKE) clean; cd ";
		out_tetsmkfile <<"$(RETURN_DIR)"<<endl;
        }
	out_tetsmkfile << "\t cd $(KIM_API_DIR)/"<<"; $(MAKE) clean; cd "<<"$(RETURN_DIR)"<<endl;
        out_tetsmkfile.close();   
 
}
