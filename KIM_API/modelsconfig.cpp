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
	char  listmodelsfile[]="listmodelname.txt";
	char listmodelsinitfile[]="listmodelinit.txt";
	char includecpp_inc[]="model_init_include.cpp";
	char includecpp_h[]="model_init_include.h";
	char modelobjlib[]="model_obj_lib.mk";
	
	char listmodels[100][160],listmodelsinit[100][160];
	char *tmp;
        ifstream mdlsfile,mdlsinitfile;
        ofstream model_init_stream;
	
	mdlsfile.open(listmodelsfile);

	if(!mdlsfile){
             cout<<"Can not open file:"<<listmodelsfile<<":"<<endl;
             exit(327);
         }

	mdlsinitfile.open(listmodelsinitfile);

	if(!mdlsinitfile){
             cout<<"Can not open file:"<<listmodelsinitfile<<":"<<endl;
             exit(327);
        }

	//read 1st files
	int counter=0;
	while(!mdlsfile.eof()) {

		mdlsfile.getline(listmodels[counter],160,' ');
		counter++;
	}
        for(int i=0; i<strlen(listmodels[counter-1]);i++){
		if(listmodels[counter-1][i]=='\n')listmodels[counter-1][i]='\0';
	}
	
	//read 2nd files
	counter=0;
	while(!mdlsinitfile.eof()) {

		mdlsinitfile.getline(listmodelsinit[counter],160,' ');
		counter++;
	}
        for(int i=0; i<strlen(listmodelsinit[counter-1]);i++){
		if(listmodelsinit[counter-1][i]=='\n')listmodelsinit[counter-1][i]='\0';
	}
        mdlsfile.close();
	mdlsinitfile.close();

        //creating includecpp_inc
        model_init_stream.open(includecpp_inc);
        for(int i=0; i<counter;i++){
        	model_init_stream << " if(strcmp(modelname,\""<<listmodels[i]<<"\")==0){"<<endl;
		model_init_stream << "        "<< listmodelsinit[i] <<"((void **) pkim);"<<endl;
		model_init_stream << "        return true;"<<endl<<" }"<<endl;  
        }
        model_init_stream.close();

	//creating includecpp_h
	model_init_stream.open(modelobjlib);
        for(int i=0; i<counter;i++){
        	model_init_stream << " MODELOBJ += "<< KIM_DIR_MODELS<<listmodels[i] <<"/"<<listmodels[i];
		model_init_stream << ".a"<<endl;
        }
        model_init_stream.close();

	//creating list of libs
	model_init_stream.open(includecpp_h);
        for(int i=0; i<counter;i++){
        	model_init_stream << " void * "<< listmodelsinit[i] <<"(void **);"<<endl; 
        }
        model_init_stream.close();
        
 
}
