
//                                                                      
// Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
// All rights reserved.                                                 
//                                                                     
// Author: Valeriu Smirichinski                                         
// 


#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string.h>

#ifdef KIM_DYNAMIC
#include <dlfcn.h>
#endif

using namespace std;
#include "KIMservice.h"

#define KEY_CHAR_LENGTH 64


//#define intptr_t int  // for 32 bit machines
KIMBaseElementFlag:: KIMBaseElementFlag(){
    peratom=0;
    freeable=1;
    calculate=0;
 }

 KIMBaseElementUnit:: KIMBaseElementUnit(){
    init();
}
void KIMBaseElementUnit::init(){
    strncpy(units, "standard",KEY_CHAR_LENGTH);
    strncpy(dim,"none",KEY_CHAR_LENGTH);
    scale=1.0;
}
int Atom_Map::comparator(const void* a1, const void* a2){
    Atom_Map *am1 =(Atom_Map *)a1;
    Atom_Map *am2 =(Atom_Map *)a2;
    return strcmp(am1->symbol,am2->symbol);
}
KIM_IOline::KIM_IOline(){
    goodformat=false;init2empty();
}
      
bool KIM_IOline:: getFields(char *inString){
            char *tmp;
   //         static int counter=0;
            init2empty();
            //check for comments part and get it removed
            tmp = strpbrk(inString,"#");
            if(tmp !=NULL) {
                strncpy(comments,tmp,strlen(tmp)+1);
                tmp[0]='\0';
    //           counter++;
            }
             strip(inString);

            if(inString[0] == '\0') return false;
            //check if it is input or output section
            strip(inString);

            if(isitinput(inString)){
                input = true;
                output = false;

                return false;
            }else if(isitoutput(inString)){

                input = false;
                output = true;
                return false;
            }

            //parse field
            tmp = strtok(inString," \t");if(tmp == NULL) return false;
            strncpy(name,tmp,strlen(tmp)+1);


            tmp = strtok(NULL," \t");if(tmp == NULL) return false;
            strncpy(type,tmp,strlen(tmp)+1);

            if(strcmp(type,"dummy")==0) {
                strncpy(dim,"none",5);
                strncpy(units,"none",5);
                strncpy(shape,"[]",3);
                return true;
            }
            
            if(strcmp(type,"spec")==0) {
                strncpy(dim,"none",5);
                strncpy(units,"none",5);
                tmp = strtok(NULL," \t");if(tmp == NULL) return false;
                strcat(shape,"[");
                strcat(shape,tmp);
                strcat(shape,"]");
                return true;
            }else if(strcmp(type,"flex")==0){
                cout<<"KIM_IOline::getFields:flex type for preprocessor only..."<<endl;
                exit (333);
                //return false;
            }
            
            tmp = strtok(NULL," \t");if(tmp == NULL) return false;
            strncpy(dim,tmp,strlen(tmp)+1);

            tmp = strtok(NULL," \t");if(tmp == NULL) return false;
            strncpy(units,tmp,strlen(tmp)+1);

            tmp = strtok(NULL," \t");if(tmp == NULL) return false;
            strncpy(shape,tmp,strlen(tmp)+1);

            tmp = strtok(NULL," \t");if(tmp == NULL) return true;
            strncpy(requirements,tmp,strlen(tmp)+1);

	    return true;
}
double KIM_IOline:: get_unitscale(){

            char unitstmp[strlen(this->units)+1];
            char * tmp;
            tmp = &unitstmp[0];
            strncpy(unitstmp,units,strlen(units)+1);
            double dd=strtod(tmp,&tmp);
            if (dd <= 0.0) dd=1.0; // here should be expanded to handle standard units
            return dd;
 }
int KIM_IOline::get_rank(){
            char *tmp;
            char shapetmp[strlen(shape)+1];
            strncpy(shapetmp,shape,strlen(shape)+1);
            if(shapetmp[0]=='[' && shapetmp[strlen(shape)-1]==']'){
                tmp = strtok(shapetmp,"[,]");
                if (tmp==NULL) return 0;
                int c=0;
                while (tmp!=NULL){
                    tmp = strtok(NULL,"[,]");
                    c++;
                }
                return c;
            }
            cout<<" KIM_IOline:warning: bad shape format"<<endl;
             return 0;
 }
int *  KIM_IOline::get_shape(){

            char shapetmp[strlen(shape)+1];
            strncpy(shapetmp,shape,strlen(shape)+1);
            int rnk = get_rank();
            if (rnk < 1) return NULL;
            int *shp = new int[rnk];
            int i=0;
            char *tmp =strtok(shapetmp,"[,]");
            while(tmp!=NULL){
                double dd = strtod(tmp,&tmp);
                shp[i]=(int)dd;
                tmp = strtok(NULL,"[,]");
                i++;
            }
            return shp;
 }
 bool KIM_IOline::isitpernatomtypes(){
             char shapetmp[strlen(shape)+1];
             strncpy(shapetmp,shape,strlen(shape)+1);
             char *tmp =strtok(shapetmp,"[,]");
             if(tmp==NULL)return false;
             if(strcmp(tmp,"numberAtomTypes")==0) return true;
             return false;
 }
bool KIM_IOline::isitsizedefined(){
             char shapetmp[strlen(shape)+1];
             strncpy(shapetmp,shape,strlen(shape)+1);
             char *tmp =strtok(shapetmp,"[,]");
             if(tmp==NULL)return false;
             double dd = strtod(tmp,&tmp);
             if( (int) dd > 0 ) return true;
             return false;
}
bool KIM_IOline:: isitperatom(){
             char shapetmp[strlen(shape)+1];
             strncpy(shapetmp,shape,strlen(shape)+1);
             char *tmp =strtok(shapetmp,"[,]");
             if(tmp==NULL)return false;
             if(strcmp(tmp,"numberOfAtoms")==0) return true;
             return false;
}
bool KIM_IOline::isitoptional(){
            if(strcmp(this->requirements,"optional")==0) return true;
            return false;
}

void KIM_IOline:: strip(char * strv){
            for(int i=(int)strlen(strv); i>0; i--){
		if(strv[i-1]!=' '){strv[i]='\0'; break;};
            }
            int c=0,key=0;
            for(int i=0;i<=(int)strlen(strv);i++){
            	if(strv[i]!=' '){key=1;};
		if(key==1){strv[c]=strv[i]; c++;};
            }
}
void KIM_IOline::strip(){
            strip(this->name);
            strip(this->type);
            strip(this->dim);
            strip(this->units);
            strip(this->shape);
            strip(this->requirements);
 }
void KIM_IOline:: init2empty(){
            name[0]='\0';
            type[0]='\0';
            dim[0]='\0';
            units[0]='\0';
            shape[0]='\0';
            requirements[0]='\0';
            comments[0]='\0';
}
bool KIM_IOline:: isitinput(char*str){
            char tocmp [] ="MODEL_INPUT:";
            if(strlen(str)<strlen(tocmp)) return false;
            if(strncmp(str,tocmp,strlen(tocmp))==0) return true;
            return false;
}
bool KIM_IOline:: isitoutput(char*str){
            char tocmp [] ="MODEL_OUTPUT:";
            if(strlen(str)<strlen(tocmp)) return false;
            if(strncmp(str,tocmp,strlen(tocmp))==0) return true;
            return false;
}

ostream &operator<<(ostream &stream, KIM_IOline a){
	stream<<a.name<<" "<<a.type<<" "<<a.dim<<" ";
        stream<<a.units<<" "<<a.shape<<" "<<a.requirements;
        stream<<" input: "<<a.input<<" output: "<<a.output<<endl;
	return stream;
};
istream &operator>>(istream &stream, KIM_IOline &a){
	char inputline[161];
	stream.getline(inputline,160);
        if(stream.ios::fail()){
           cerr << "Error: Input line in .kim file longer than 160 characters.\n"
                << "The line starts with:\n\t"
                << inputline << "\nExiting..." << endl;
           exit(-2);
        }
	if(a.getFields(inputline)){
		a.goodformat=true;
	}else{
		a.goodformat=false;
	}
	return stream;
};

void IOline:: strip(){
            strip(&name[0]);
            strip(&value[0]);
            strip(&comment[0]);
}
void IOline::strip(char *nm){
                //strip space
		for(int i=(int)strlen(nm); i>0; i--){
			if(nm[i-1]!=' '){nm[i]='\0'; break;};
		}
		int c=0,key=0;
		for(int i=0;i<=(int)strlen(nm);i++){
			if(nm[i]!=' '){key=1;};
			if(key==1){nm[c]=nm[i]; c++;};
		}
                //strip tub
		for(int i=(int)strlen(nm); i>0; i--){
			if(nm[i-1]!='\t'){nm[i]='\0'; break;};
		}
		c=0;key=0;
		for(int i=0;i<=(int)strlen(nm);i++){
			if(nm[i]!='\t'){key=1;};
			if(key==1){nm[c]=nm[i]; c++;};
		}

}
IOline::IOline(){
    goodformat=false;
}
bool IOline:: getFields(const char *inputString){
		int i;
		for(i=0; i<=(int)strlen(inputString); i++){
                    if(*(inputString+i)=='#'){return false;};
                    if(*(inputString+i)==':' && *(inputString+i+1)=='='){name[i]='\0';i+=2;break;};
			name[i]=*(inputString+i);
		} 
		if(i>=(int)strlen(inputString)){return false;};
		int j=0;
		for(i;i<=(int)strlen(inputString); i++){
			if(*(inputString+i)=='#'){value[j]='\0';i+=2;break;};
			value[j]=*(inputString+i);
			j++;
			value[j]='\0';
		}
		j=0;
		if(i>=(int)strlen(inputString)){comment[0]='\0';strip();return true;};
		for(i;i<=(int)strlen(inputString); i++){
			comment[j]=*(inputString + i);
			comment[j+1]='\0';
			j++;
		}
		strip();return true;
}
  

ostream &operator<<(ostream &stream, IOline a){
	stream<<a.name<<":= "<<a.value<<" #"<<a.comment;
	return stream;
}
istream &operator>>(istream &stream, IOline &a){
	char inputline[161];
	stream.getline(inputline,160);
	if(a.getFields(inputline)){
		a.goodformat=true;
	}else{
		a.goodformat=false;
	}
	return stream;
}

SystemOfUnit::SystemOfUnit(){
        inlines = NULL;
        strncpy(UnitsSystemName,"standard",9);
        mass=      1.0;
        distance=  1.0;
        time=      1.0;
        energy =   1.0;
        velocity = 1.0;
        force =     1.0;
        torque =    1.0;
        temperature=1.0;
        pressure =  1.0;
        dynamic_viscosity =1.0;
        charge =    1.0;
        dipole =    1.0;
        electric_field =1.0;
        density =   1.0;
        garbage =-1.0;
        numlines=0;
 }
 SystemOfUnit:: ~SystemOfUnit(){
        delete [] inlines;
        inlines=NULL;
}
void SystemOfUnit::init(char *infile){
        numlines = readlines(infile,&inlines);
        if (numlines<1) {
            cout<<"SystemOfUnits::init : no inlines found"<<endl;
            return;
        }
        if (numlines<14) {
            cout<<"SystemOfUnits::init : input file is incoplete"<<endl;
            return;
        }
        for (int i=0; i<numlines; i++){
            if (isitunit(& inlines[i].name[0]) ) {
                char  tmp[KEY_CHAR_LENGTH];
                strncpy(tmp,& inlines[i].value[0],KEY_CHAR_LENGTH);
                char *temp = &tmp[0];
                double dd = strtod(temp,&temp);  
                (*this)[& inlines[i].name[0] ] = dd;
            }
 

            if (strcmp(inlines[i].name, "UnitsSystemName")==0) {
                strncpy(UnitsSystemName,inlines[i].value,KEY_CHAR_LENGTH);
            }

        }

}
void SystemOfUnit::free(){
     if (this->inlines !=NULL) delete [] this->inlines;
     this->inlines = NULL;
}
    
float& SystemOfUnit::operator[](char * unit){
        if(strcmp(unit,"mass")==0) return mass;
        if(strcmp(unit,"distance")==0 || strcmp(unit,"length")==0) return distance;
        if(strcmp(unit,"time")==0) return time;
        if(strcmp(unit,"energy")==0) return energy;
        if(strcmp(unit,"velocity")==0) return velocity;
        if(strcmp(unit,"force")==0) return force;
        if(strcmp(unit,"torque")==0) return torque;
        if(strcmp(unit,"temperature")==0) return temperature;
        if(strcmp(unit,"pressure")==0) return pressure;
        if(strcmp(unit,"dynamic_viscosity")==0) return dynamic_viscosity;
        if(strcmp(unit,"charge")==0) return charge;
        if(strcmp(unit,"dipole")==0) return dipole;
        if(strcmp(unit,"electric_field")==0) return electric_field;
        if(strcmp(unit,"density")==0) return density;
        garbage = 0.0;
        return garbage;
}
int SystemOfUnit:: readlines(char * infile, IOline **inlines){
        int counter=0;
        IOline inlne;
        *inlines=NULL;
        ifstream myfile;
         myfile.open(infile);
 //cout<<"SystemOfUnit:  file:"<<infile<<":"<<endl;
         if(!myfile){
             cout<<"SystemOfUnit: can not open file:"<<infile<<":"<<endl;
             exit(327);
         }
         
          while(!myfile.eof()){
                myfile>> inlne;
                if(inlne.goodformat) counter++;
        }
        myfile.close();

        if (counter < 1) return counter;


        (*inlines) =  new IOline [counter] ;

        myfile.open(infile);

        counter=0;
        while(!myfile.eof()){
                myfile >> inlne;

                if(inlne.goodformat) {

                    (*inlines)[counter]=inlne;

                    counter++;
                }
        }
        myfile.close();
        return counter;
}
bool SystemOfUnit::isitunit(char * unit){
        if(strcmp(unit,"mass")==0) return true;
        if(strcmp(unit,"distance")==0 || strcmp(unit,"length")==0) return true;
        if(strcmp(unit,"time")==0) return true;
        if(strcmp(unit,"energy")==0) return true;
        if(strcmp(unit,"velocity")==0) return true;
        if(strcmp(unit,"force")==0) return true;
        if(strcmp(unit,"torque")==0) return true;
        if(strcmp(unit,"temperature")==0) return true;
        if(strcmp(unit,"pressure")==0) return true;
        if(strcmp(unit,"dynamic_viscosity")==0) return true;
        if(strcmp(unit,"charge")==0) return true;
        if(strcmp(unit,"dipole")==0) return true;
        if(strcmp(unit,"electric_field")==0) return true;
        if(strcmp(unit,"density")==0) return true;
        return false;
}


KIMBaseElement:: KIMBaseElement(){
            nullefy();
}
KIMBaseElement::~KIMBaseElement(){
    //free();
}
void KIMBaseElement:: init(char *nm,char * tp,intptr_t sz, intptr_t rnk, int *shp,void * pdata){
            flag = new KIMBaseElementFlag;
   
            unit = new KIMBaseElementUnit;
            name = new char[KEY_CHAR_LENGTH];
   
            type = new char[KEY_CHAR_LENGTH];
            strncpy(name,nm,KEY_CHAR_LENGTH);
            strncpy(type,tp,KEY_CHAR_LENGTH);

    
            if(rnk < 0) {
                cout << "KIMBaseElement_init:rnk < 0"<<endl;
                exit (113);
            }
            size = sz;
            rank = rnk;
            if(rank==1){
                shape = new int[rank];

                if(shp!=NULL){
                   shape[0]=shp[0];
                }else{
                    shape[0]=sz;
                }
            }
            if(rank > 1){
                shape = new int[rank];
                if (shp == NULL) {
                  cout << "KIMBaseElement_init:shp==NULL"<<endl;
                  exit (114);
                }
                for (int i=0;i<rank;i++) shape[i]=shp[i];
            }
            if (rank !=0) ptrptr = pdata;
            data = (void *)  (*(( intptr_t **)pdata));

   
            flag->freeable = 1;
            if(sz==0) flag->freeable = 0;
            flag->calculate = 1;
}
void KIMBaseElement::init(char *nm,char * tp,intptr_t sz, intptr_t rnk, int *shp){
            int szelement=getelemsize(tp);
            char *data = NULL;

            if(sz>0) data=new char[szelement*sz];

            init(nm, tp,sz, rnk, shp,&data);
            
            if (sz>0) flag->freeable=0;

            ptrptr=NULL;
}
void KIMBaseElement::free(){
             if(flag!=NULL) if (flag->freeable == 0){
                delete [] (char *)data;
            }
           if(name!= NULL) delete [] name;
           if(type!=NULL) delete [] type;
           if(shape!=NULL) delete [] shape;
           if(flag!=NULL) delete  flag;
           if(unit!=NULL) delete unit;
           //if(ptrptr!=NULL) delete [] ptrptr;
           //if(reserved!=NULL) delete [] reserved;
           nullefy();

}
void KIMBaseElement::nullefy(){
            data=NULL;
            ptrptr = NULL;
            shape = NULL;
            reserved=NULL;
            size=0;
            rank=0;
            name=NULL;
            type =NULL;
            flag = NULL;
            unit = NULL;

}
bool KIMBaseElement::operator==(KIM_IOline& kimioline){
            if(strcmp(kimioline.name,this->name)==0){
                if(strcmp(kimioline.type,this->type)==0){
                    if(kimioline.get_rank()==(int)(this->rank)) return true;
                }
            }
            return false;
}

int KIMBaseElement::getelemsize(char *tp){
            char realkey[KEY_CHAR_LENGTH]="real";      //key defenitions
            char real8key[KEY_CHAR_LENGTH]="real*8";
            char integerkey[KEY_CHAR_LENGTH]="integer";
            char integer8key[KEY_CHAR_LENGTH]="integer*8";
            char ptrkey[KEY_CHAR_LENGTH]="pointer";
            char methodkey[KEY_CHAR_LENGTH]="method";
            char dummykey[KEY_CHAR_LENGTH]="dummy";// add here to expand...

            if(strcmp(realkey,tp)==0){
                return(sizeof(float));
            }else if (strcmp(integerkey,tp)==0){
                return(sizeof(int));
            } else if (strcmp(integer8key,tp)==0){
                return(sizeof(long long));
            }else if (strcmp(ptrkey,tp)==0){
                return(sizeof(int *));
            }else if (strcmp(real8key,tp)==0){
                return (sizeof(double));
            }else if (strcmp(methodkey,tp)==0){
                return (sizeof(int *));
             }else if (strcmp(dummykey,tp)==0){
                return 0;
            }else{// add here more in else if block...
            printf("KIMBaseElement_getelemsize:key type %s is not:\n %s %s %s, %s, %s, %s \n",tp,realkey,real8key,integerkey,ptrkey,integer8key,dummykey);
                exit(102);
            }
}

ostream &operator<<(ostream &stream, KIMBaseElement a){
    if (a.data==NULL && a.name == NULL && a.type==NULL) {
        stream <<" KIMBaseElement is nullefied "<<endl;
        return stream;
    }
    stream<<endl<<"name: "<<a.name<<" type: "<<a.type<<" rank= "<<a.rank<<endl;
    if (a.rank>0 && a.shape!=NULL){
        stream<<" shape= [ ";
        for(int i=0;i<a.rank;i++) stream<< a.shape[i] <<" ";
        stream << " ]"<<" ";
    }
    stream<<" size= "<<a.size<<endl;
    
    stream<<"flag:calculate "<<a.flag->calculate<<"// 0 -- do not calculate, 1 -- calculate"<<endl;
    stream<<"flag:freeable  "<<a.flag->freeable<<"//0--freeable , 1 is not freeable"<<endl;
    stream<<"flag:peratom  "<<a.flag->peratom<<"//0 -- peratom, 1--per something else"<<endl;
    stream<<" unit system : "<<a.unit->units<<" phys.dimension: "<<a.unit->dim;
    stream<<" scale "<< a.unit->scale<<endl;
    // printin gata itself
    

    stream<<" data: ";


    if(a.data == NULL) {
        stream <<"NULL"<<endl;
        return stream;
    }else if(strcmp(a.type,"real*8")==0){

        for(int i=0;i<a.size;i++) stream<< ((double*)(a.data))[i]<<" ";

    }else if(strcmp(a.type,"real")==0){
        for(int i=0;i<a.size;i++) stream<< ((float*)(a.data))[i]<<" ";
    }else if(strcmp(a.type,"integer")==0){
        for(int i=0;i<a.size;i++) stream<< ((int*)(a.data))[i]<<" ";
    }else if(strcmp(a.type,"integer*8")==0){
        for(int i=0;i<a.size;i++) stream<< ((intptr_t*)(a.data))[i]<<" ";
    }else{
        stream<<"address:"<<(intptr_t)a.data;
    }
    stream<<endl;

    return stream;
}

KIM_API_model:: KIM_API_model(){
       inlines=NULL;
       UnitsSet =NULL;
       unitsFixed=false;
       strncpy(&currentUnits[0],"standard",9);
       //method_A init
       strcpy(NBC_method_A,"CLUSTER");
       strcpy(&arg_NBC_method_A[0][0],"coordinates");
       narg_NBC_method_A=1;

       //method_B init
       strcpy(NBC_method_B,"MI-OPBC-H");
       strcpy(&arg_NBC_method_B[0][0],"coordinates");
       strcpy(&arg_NBC_method_B[1][0],"boxlength");
       strcpy(&arg_NBC_method_B[2][0],"neighObject");
       strcpy(&arg_NBC_method_B[3][0],"get_half_neigh");
       narg_NBC_method_B=4;

       //method_C init
       strcpy(NBC_method_C,"MI-OPBC-F");
       strcpy(&arg_NBC_method_C[0][0],"coordinates");
       strcpy(&arg_NBC_method_C[1][0],"boxlength");
       strcpy(&arg_NBC_method_C[2][0],"neighObject");
       strcpy(&arg_NBC_method_C[3][0],"get_full_neigh");
       narg_NBC_method_C=4;

       //method_D init
       strcpy(NBC_method_D,"NEIGH-RVEC-F");
       strcpy(&arg_NBC_method_D[0][0],"coordinates");
       strcpy(&arg_NBC_method_D[1][0],"neighObject");
       strcpy(&arg_NBC_method_D[2][0],"get_full_neigh");
       narg_NBC_method_D=3;

       //method_E init
       strcpy(NBC_method_E,"NEIGH-PURE-H");
       strcpy(&arg_NBC_method_E[0][0],"coordinates");
       strcpy(&arg_NBC_method_E[1][0],"neighObject");
       strcpy(&arg_NBC_method_E[2][0],"get_half_neigh");
       narg_NBC_method_E=3;

       //method_F init
       strcpy(NBC_method_F,"NEIGH-PURE-F");
       strcpy(&arg_NBC_method_F[0][0],"coordinates");
       strcpy(&arg_NBC_method_F[1][0],"neighObject");
       strcpy(&arg_NBC_method_F[2][0],"get_full_neigh");
       narg_NBC_method_F=3;

       n_NBC_methods = number_NBC_methods;
       nnarg_NBC = new int[n_NBC_methods];
       NBC_methods = new char* [n_NBC_methods];
       arg_NBC_methods = new char**[n_NBC_methods];
       nnarg_NBC[0] =  narg_NBC_method_A;
       nnarg_NBC[1] =  narg_NBC_method_B;
       nnarg_NBC[2] =  narg_NBC_method_C;
       nnarg_NBC[3] =  narg_NBC_method_D;
       nnarg_NBC[4] =  narg_NBC_method_E;
       nnarg_NBC[5] =  narg_NBC_method_F;

       for(int i=0;i<n_NBC_methods;i++){
           arg_NBC_methods[i] = new char *[nnarg_NBC[i]];
       }

       NBC_methods[0] = &NBC_method_A[0];
       NBC_methods[1] = &NBC_method_B[0];
       NBC_methods[2] = &NBC_method_C[0];
       NBC_methods[3] = &NBC_method_D[0];
       NBC_methods[4] = &NBC_method_E[0];
       NBC_methods[5] = &NBC_method_F[0];

       for (int i=0; i<nnarg_NBC[0];i++) arg_NBC_methods[0][i] = & arg_NBC_method_A[i][0];
       for (int i=0; i<nnarg_NBC[1];i++) arg_NBC_methods[1][i] = & arg_NBC_method_B[i][0];
       for (int i=0; i<nnarg_NBC[2];i++) arg_NBC_methods[2][i] = & arg_NBC_method_C[i][0];
       for (int i=0; i<nnarg_NBC[3];i++) arg_NBC_methods[3][i] = & arg_NBC_method_D[i][0];
       for (int i=0; i<nnarg_NBC[4];i++) arg_NBC_methods[4][i] = & arg_NBC_method_E[i][0];
       for (int i=0; i<nnarg_NBC[5];i++) arg_NBC_methods[5][i] = & arg_NBC_method_F[i][0];
       
       strcpy(NBC_method_current,"none");
       

       this->baseConvertKey=0;
       ErrorCode=1;
       AtomsTypes = NULL;
       nAtomsTypes = 0;
}
KIM_API_model:: ~KIM_API_model(){
       //delete [] UnitsSet;
      // free();
      for(int i=0;i<n_NBC_methods;i++){
         delete arg_NBC_methods[i];
      }

      delete [] arg_NBC_methods;
      delete [] NBC_methods;
      delete []  nnarg_NBC;
}
bool KIM_API_model:: preinit(char * initfile,char *modelname){
        read_file(initfile,&inlines,&numlines);
        int *shape=NULL;
        char pointer_str [] = "pointer";
        //get Atoms Types and nAtomsTypes
        if (! init_AtomsTypes()) return false;
 
        model.init(modelname,pointer_str,(intptr_t)(numlines-nAtomsTypes),1,shape);

        int ii=0;
        for (int i=0; i< numlines;i++){

            //check for spec type
            if (!(strcmp(inlines[i].type,"spec")==0)) {
              
                KIMBaseElement *el = new KIMBaseElement ;

                int rank=inlines[i].get_rank();
                shape =inlines[i].get_shape();
                char * name =& (inlines[i].name[0]);
                char * type =& (inlines[i].type[0]);
 
                el->init(name,type,0,rank,shape); //preinit element with zero size
                strncpy(el->unit->dim,inlines[i].dim,strlen(inlines[i].dim)+1);
                double scale=inlines[i].get_unitscale();
 
                strncpy(el->unit->units,inlines[i].units,strlen(inlines[i].units)+1);
                el->unit->scale = (float)scale;
                el->flag->calculate = 1;
                el->flag->peratom = 1;//per something else
                if(inlines[i].isitperatom()) el->flag->peratom = 0; //per atom
                KIMBaseElement **pel =(KIMBaseElement**) model.data;
                pel[ii] =  el;
                ii++;
                delete [] shape;
            }
        }
        //resize inlines (remove spec type variables)
        KIM_IOline * inlinesnew = new KIM_IOline[numlines - nAtomsTypes];
        ii=0;
        for (int i=0; i< numlines;i++){
            //check for spec type
            if (!(strcmp(inlines[i].type,"spec")==0)) {
                inlinesnew[ii]=inlines[i];
                ii++;
            }
        }
        delete [] inlines;
        inlines = inlinesnew;
        inlinesnew = NULL;
        numlines=numlines - nAtomsTypes;
        //end resize inlines
 
        this->supported_units_init();
        
        char coordinates_str [] = "coordinates";
        int ind =this->get_index(coordinates_str);
        char custom_str [] ="custom";

        if(!set_units(inlines[ind].units)) if(!set_units(custom_str)){
             cout<<"KIM_API_model::set_units:  failed"<< initfile<<endl;
            return false;
        }
        if (! are_infileunits_consistent()) {
                cout<<"KIM_API_model::pre_init:  inconsistent units in"<< initfile<<endl;
                return false;
        }
        //extra input like unitFixed flag and,later may be, authors
        IOline *extrainput;
        int nextra = SystemOfUnit::readlines(initfile,&extrainput);
        for (int i=0;i<nextra;i++){
            if(strcmp(extrainput[i].name,"SystemOfUnitsFix")==0){
                if(strcmp(extrainput[i].value,"fixed")==0) this->unitsFixed=true;
            }
        }
        delete [] extrainput;
        return true;

 }
 void KIM_API_model::free(int *error){
        //
        *error = 0;
        if(this==NULL) return;
        this->free();
        *error=1;
 }

 void KIM_API_model::free(){
        //

         KIMBaseElement **pel =  (KIMBaseElement **)  model.data;
        if(model.data != NULL)  for (int i =0;i<model.size;i++) {
            pel[i]->free();
            delete pel[i];
            pel[i]=NULL;
        }
        model.free();

        if(inlines != NULL) {
            delete [] inlines;
            inlines=NULL;
        }

       if(UnitsSet!= NULL) {
            for (int i = 0; i< numUnitsSet;i++){
                UnitsSet[i].free();

            }
            delete [] UnitsSet;
            UnitsSet=NULL;
        }
        numlines=0;
        numUnitsSet=0;
        originalUnits[0]='\0';
        currentUnits[0]='\0';
        unitsFixed=false;

        if(AtomsTypes !=NULL) {
            delete [] AtomsTypes;
            AtomsTypes=NULL;
            nAtomsTypes=0;
        }
 }
int KIM_API_model::pack_pointers(int npointers, ...){
   KIM_API_model * mdl = this;
   #include "pack_pointers.fragment"
}

bool KIM_API_model::set_data(char *nm, intptr_t size, void *dt){
        // set data into preinit element correctly calculates all
        int ind=get_index(nm);
        if (ind<0) {
            return false;
        } //no data in KIM_API_model
        int c=1;
        if((*this)[ind].flag->freeable == 0) {
            if((*this)[ind].data!=NULL) delete [] (char *)((*this)[ind].data);
        }
        
       (*this)[ind].data = dt;
        
        (*this)[ind].size = size;

        if ((*this)[ind].rank > 1) {
            for (int i=1;i<(*this)[ind].rank;i++) {
                c=c * (*this)[ind].shape[i];
            }
            (*this)[ind].shape[0] = size/c;
        }
        (*this)[ind].flag->freeable = 1;
        return true;
}
bool KIM_API_model::set_data_byi(int ind, intptr_t size, void* dt){
    if (ind<0) {
            return false;
        } //no data in KIM_API_model
        int c=1;
       if((*this)[ind].flag->freeable == 0) {
           if((*this)[ind].data!=NULL) delete [] (char *)((*this)[ind].data);
       }
       (*this)[ind].data = dt;

        (*this)[ind].size = size;

        if ((*this)[ind].rank > 1) {
            for (int i=1;i<(*this)[ind].rank;i++) {
                c=c * (*this)[ind].shape[i];
            }
            (*this)[ind].shape[0] = size/c;
        }
        (*this)[ind].flag->freeable = 1;
        return true;
}
int KIM_API_model::unpack_pointers(int npointers, ...){
   KIM_API_model * mdl = this;
   #include "unpack_pointers.fragment"
}
void * KIM_API_model::get_data(char *nm,int *error){
        int i=get_index(nm);
        *error = 0;
        if (i<0) return NULL;
        *error =1;
        return (*this)[i].data;
}

void * KIM_API_model::get_data(char *nm){
        int i=get_index(nm);
        if (i<0) return NULL;
        return (*this)[i].data;
}

int KIM_API_model::get_index(char *nm,int *error){
        for(int i=0; i< model.size;i++){
            if(strcmp((*this)[i].name,nm)==0) {
                *error =1;
                return i;
            }
        }
        *error = 0;
        return -1;
}
int KIM_API_model::get_index(char *nm){
        for(int i=0; i< model.size;i++){
            if(strcmp((*this)[i].name,nm)==0) {
                return i;
            }
        }
        return -1;
}
intptr_t KIM_API_model::get_size(char *nm,int *error){
        int ind=get_index(nm,error);
        *error=0;
        if (ind < 0) return -1;
        *error=1;
        return (*this)[ind].size;
}
intptr_t KIM_API_model::get_rank_shape(char *nm,int * shape, int *error){
        int ind=get_index(nm,error);
        *error =0;
        if (ind < 0) return -1;
        *error =1;
        if((*this)[ind].rank == 0){
            return 0;
        }else if((*this)[ind].rank ==1){
            shape[0] = (int)(*this)[ind].size;
            return 1;
        }else if((*this)[ind].rank>1){
            for (int i=0; i< (*this)[ind].rank; i++) shape[i] =(*this)[ind].shape[i];
            return (*this)[ind].rank;
        }else{
            *error =0;
            return -1;
        }
}
void KIM_API_model::set2_compute(char *nm){
        (*this)[nm].flag->calculate = 1;
}
void KIM_API_model::set2_donotcompute(char *nm){
        (*this)[nm].flag->calculate = 0;
}
bool KIM_API_model::isit_compute(char *nm){
        if ((*this)[nm].flag->calculate == 1) return true;
        return false;
}
KIMBaseElement & KIM_API_model::operator[](int i){
        KIMBaseElement **pel =(KIMBaseElement**) model.data;
        return *pel[i];
}
KIMBaseElement & KIM_API_model::operator[](char *nm){
       
        int ind=get_index(nm);
        KIMBaseElement **pel =(KIMBaseElement**) model.data;
        return *pel[ind];
}

void KIM_API_model::read_file(char * initfile,KIM_IOline ** lns, int * numlns){
        int counter=0;
        KIM_IOline inln;
        ifstream myfile;
        myfile.open(initfile);
        if(!myfile){
            cout<<"KIM_API_model: can not open file:"<<initfile<<":"<<endl;
            exit (327);
        }
        while(!myfile.eof()){
                myfile >> inln;
                if(inln.goodformat) counter++;
        }
        myfile.close();
        *numlns = counter;

        *lns = new KIM_IOline[counter];

        myfile.open(initfile);
        counter=0;
        while(!myfile.eof()){
                myfile >> inln;
                if(inln.goodformat) {
                    (*lns)[counter]=inln;
                    counter++;
                }
        }

        myfile.close();
}
bool KIM_API_model::is_it_match(KIM_API_model & mdtst,char * inputinitfile){
    KIM_IOline * IOlines;
    int  nlns;
    bool match=false;   
    //check if lines are match with Model api variable
    read_file(inputinitfile,&IOlines,&nlns);
    match =is_it_match(mdtst,IOlines,nlns);
    if (IOlines != NULL) delete [] IOlines;
    return match;
    
 }//will be private
bool KIM_API_model::is_it_match(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns){
    bool match;
    //check if lines are match with Model api variable
    match =true;
    for (int i=0; i<nlns || match==false;i++){
        match=false;
 
        if(IOlines[i].isitoptional()){
            match=true;
        }

        if(strcmp(IOlines[i].type,"spec")==0){
            match=true;
        }
        if ( is_it_par(IOlines[i].name) ) match=true;

        for(int j=0;j<mdtst.model.size;j++){
            if(mdtst[j]== IOlines[i]) {
                match = true;
                break;
            }else if(is_it_par(mdtst[j].name)){
                match = true;
                break;
            }
        }
        if(!match) {
            cout << "The following may not match"<<endl;
            cout<<IOlines[i]<<endl;
            return match;
        }
    }
    return match;
}//will be private
bool KIM_API_model::is_it_match_noDummyCount(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns){
    bool match;
    //check if lines are match with Model api variable
    match =true;
    for (int i=0; i<nlns || match==false;i++){
        match=false;

        if(IOlines[i].isitoptional()){
            match=true;
        }

        if(strcmp(IOlines[i].type,"spec")==0){
            match=true;
        }
        
        if ( is_it_par(IOlines[i].name) ) match=true;

        if(strcmp(IOlines[i].type,"dummy")==0){
            match=true;
        }
        for(int j=0;j<mdtst.model.size;j++){
            if(mdtst[j]== IOlines[i]) {
                match = true;
                break;
            }else if(strcmp(mdtst[j].type,"dummy")==0){
                match = true;
                break;
            }else if(is_it_par(mdtst[j].name)){
                match = true;
                break;
            }
        }
        if(!match) {
            cout << "The following does not  match"<<endl;
            cout<<IOlines[i]<<endl;
            return match;
        }
    }
    return match;
}//will be private
bool KIM_API_model::is_it_match(KIM_API_model &test,KIM_API_model & mdl){
    //preinit model from standard template kim file
    KIM_API_model stdmdl;
    char modelfile[160] = KIM_DIR_API;
    char modelstdname[32]="standard";
    strcat(modelfile,"standard.kim");
    if(!stdmdl.preinit(modelfile,modelstdname)){
        cout<<" preinit of :"<<modelfile<<" failed"<<endl;
        stdmdl.free();
        return false;
    }

    // test and mdl must be preinit.
    bool test2modelmatch= is_it_match(test,mdl.inlines,mdl.numlines);
    bool model2testmatch= is_it_match(mdl,test.inlines,test.numlines);

    bool test2modelmatch_noDC= is_it_match_noDummyCount(test,mdl.inlines,mdl.numlines);
    bool model2testmatch_noDC= is_it_match_noDummyCount(mdl,test.inlines,test.numlines);

    bool test2standardmatch = is_it_match(stdmdl,test.inlines,test.numlines);
    bool model2standardmatch = is_it_match(stdmdl,mdl.inlines,mdl.numlines);

    bool test2standardAtomsTypesMatch = do_AtomsTypes_match(test,stdmdl);
    bool model2standardAtomsTypesMatch = do_AtomsTypes_match(mdl,stdmdl);
    bool test2modelAtomsTypesMatch = do_AtomsTypes_match(test,mdl);
    bool AtomsTypesMatch=test2standardAtomsTypesMatch&&model2standardAtomsTypesMatch&&test2modelAtomsTypesMatch;
    stdmdl.free();
    if(test.unitsFixed && mdl.unitsFixed){
        if(strcmp(test.currentUnits,mdl.currentUnits)!=0){
            cout<<"system of units for test and model do not match "<<endl;
            return false;
        }
    }
    bool NBC_methodsmatch = this->NBC_methods_match(test,mdl);
    NBC_methodsmatch=NBC_methodsmatch&&test.check_consistance_NBC_method();
    NBC_methodsmatch=NBC_methodsmatch&&mdl.check_consistance_NBC_method();

    if(!test2standardmatch) cout<<"there are non-standard variables in test:"<<endl;
    if(!model2standardmatch) cout<<"there are non-standard variables in model"<<endl;
    if(!test2standardAtomsTypesMatch) cout<<"there are non-standard AtomsTypes in test"<<endl;
    if(!model2standardAtomsTypesMatch) cout<<"there are non-standard AtomsTypes in model"<<endl;
    if(!test2modelAtomsTypesMatch) cout<<"test-model AtomsTypes do not match"<<endl;
    if(!NBC_methodsmatch) cout<<"NBC methods do not match"<<endl;

    if (test2modelmatch && model2testmatch && test2standardmatch &&
            model2standardmatch && AtomsTypesMatch && NBC_methodsmatch) return true;
    if (test2modelmatch_noDC && model2testmatch_noDC && test2standardmatch 
            && model2standardmatch && AtomsTypesMatch && NBC_methodsmatch){
        return this->do_dummy_match(test,mdl);
    }
    return false;
}

bool KIM_API_model::is_it_in_and_is_it_dummy(KIM_API_model& mdl,char * name){
    int i = mdl.get_index(name);
    if (i<0) return false;
    if (strcmp(mdl[i].type,"dummy")!=0) return false;
    return true;
}

bool KIM_API_model::do_dummy_match(KIM_API_model& tst, KIM_API_model& mdl){
    // here the assumption : besides dummy type , everything is a match
    // the logic is hard wired to the .kim file in violation of descriptor file concept.
    // Proposed  by KIM Technical Commettee.

    // check dummy for tst
    bool ZeroBasedLists_tst =is_it_in_and_is_it_dummy(tst, "ZeroBasedLists");
    bool OneBasedLists_tst =is_it_in_and_is_it_dummy(tst, "OneBasedLists");

    bool Neigh_HalfList_tst =is_it_in_and_is_it_dummy(tst, "Neigh_HalfList");
    bool Neigh_FullList_tst =is_it_in_and_is_it_dummy(tst, "Neigh_FullList");
    bool Neigh_BothList_tst =is_it_in_and_is_it_dummy(tst, "Neigh_BothList");
    
    bool Neigh_IterAccess_tst=is_it_in_and_is_it_dummy(tst, "Neigh_IterAccess");
    bool Neigh_LocaAccess_tst=is_it_in_and_is_it_dummy(tst, "Neigh_LocaAccess");
    bool Neigh_BothAccess_tst=is_it_in_and_is_it_dummy(tst, "Neigh_BothAccess");

    bool Neigh_CalcRij_tst=is_it_in_and_is_it_dummy(tst, "Neigh_CalcRij");

    // check dummy for mdl
    bool ZeroBasedLists_mdl =is_it_in_and_is_it_dummy(mdl, "ZeroBasedLists");
    bool OneBasedLists_mdl =is_it_in_and_is_it_dummy(mdl, "OneBasedLists");

    bool Neigh_HalfList_mdl =is_it_in_and_is_it_dummy(mdl, "Neigh_HalfList");
    bool Neigh_FullList_mdl =is_it_in_and_is_it_dummy(mdl, "Neigh_FullList");
    bool Neigh_BothList_mdl =is_it_in_and_is_it_dummy(mdl, "Neigh_BothList");

    bool Neigh_IterAccess_mdl=is_it_in_and_is_it_dummy(mdl, "Neigh_IterAccess");
    bool Neigh_LocaAccess_mdl=is_it_in_and_is_it_dummy(mdl, "Neigh_LocaAccess");
    bool Neigh_BothAccess_mdl=is_it_in_and_is_it_dummy(mdl, "Neigh_BothAccess");

    bool Neigh_CalcRij_mdl=is_it_in_and_is_it_dummy(mdl, "Neigh_CalcRij");


    //logic for Zero or One base list handling
    if ((!ZeroBasedLists_tst && !OneBasedLists_tst)||(ZeroBasedLists_tst && OneBasedLists_tst) ) {
        cout<< "test .kim has to have ONE ZeroBasedLists or OneBasedLists line"<<endl;
        return false;
    }
     if ((!ZeroBasedLists_mdl && !OneBasedLists_mdl)||(ZeroBasedLists_mdl && OneBasedLists_mdl)) {
        cout<< "model .kim has to have ONE ZeroBasedLists or OneBasedLists line"<<endl;
        return false;
    }
    baseConvertKey = 0;
    if (ZeroBasedLists_tst && OneBasedLists_mdl) baseConvertKey = 1;
    if (OneBasedLists_tst && ZeroBasedLists_mdl) baseConvertKey = -1;

    //logic for checking Both/Full/Half base list
    // checking if test o.k. when model requires both
 /*   if (Neigh_BothList_mdl){
        if(!(Neigh_BothList_tst || Neigh_FullList_tst && Neigh_HalfList_tst)){
            cout<< "model .kim requres Neigh_BothList "<<endl;
            return false;
        }
     //checking if test o.k. when model may work with half or full
     }else if (Neigh_FullList_mdl && Neigh_HalfList_mdl){

        if(!(Neigh_FullList_tst || Neigh_HalfList_tst || Neigh_BothList_tst)){
            cout<< "model .kim requres Neigh_HalfList or Neigh_FullList "<<endl;
            return false;
        }
     //checking if test o.k. with full
     }else if(Neigh_FullList_mdl){
         if(!(Neigh_FullList_tst || Neigh_BothList_tst)) {
             cout<< "model .kim requres  Neigh_FullList"<<endl;
             return false;
         }
     //checking if test o.k. with half
     }else if(Neigh_HalfList_mdl){
         if(!(Neigh_HalfList_tst || Neigh_BothList_tst)) {
             cout<< "model .kim requres  Neigh_HalfList"<<endl;
             return false;
         }
     }
*/
    //
    //logic for checking Both/Loca/Iter
    // checking if test o.k. when model requires both
    if (Neigh_BothAccess_mdl){
        if(!(Neigh_BothAccess_tst || Neigh_LocaAccess_tst && Neigh_IterAccess_tst)){
            cout<< "model .kim requres Neigh_BothAccess "<<endl;
            return false;
        }
     //checking if test o.k. when model may work with loca or iter
     }else if (Neigh_LocaAccess_mdl && Neigh_IterAccess_mdl){

        if(!(Neigh_LocaAccess_tst || Neigh_IterAccess_tst || Neigh_BothAccess_tst)){
            cout<< "model .kim requres IterAccess or LocaAccess  "<<endl;
            return false;
        }
     //checking if test o.k. with loca
     }else if(Neigh_LocaAccess_mdl){
         if(!(Neigh_LocaAccess_tst || Neigh_BothAccess_tst)) {
             cout<< "model .kim requres  Neigh_LocaAccess"<<endl;
             return false;
         }
     //checking if test o.k. with iter
     }else if(Neigh_IterAccess_mdl){
         if(!(Neigh_IterAccess_tst || Neigh_BothAccess_tst)) {
             cout<< "model .kim requres  Neigh_IterAccess"<<endl;
             return false;
         }
     }

    //logic for Neigh_CalcRij
  /*  if (Neigh_CalcRij_mdl && !Neigh_CalcRij_tst){
             cout<< "model .kim requres Neigh_CalcRij"<<endl;
             return false;
    }
  */
    return true;
    
}
bool KIM_API_model::do_AtomsTypes_match(KIM_API_model& test, KIM_API_model& mdl){
    bool match;

    if (test.nAtomsTypes == 0 && strcmp(mdl.model.name,"standard")==0) return true;
    if (test.nAtomsTypes == 0 && mdl.nAtomsTypes == 0) return true;
    if (test.nAtomsTypes == 0 && mdl.nAtomsTypes > 0) return false;
    if (test.nAtomsTypes > 0 && mdl.nAtomsTypes == 0) return false;
    for (int i=0;i < test.nAtomsTypes; i++){
        match = false;
        for (int j=0;j<mdl.nAtomsTypes;j++){
            if(strcmp(test.AtomsTypes[i].symbol, mdl.AtomsTypes[j].symbol)==0){
                match = true;
                break;
            }
        }
        if (!match) {
            cout <<"the following simbol:"<<test.AtomsTypes[i].symbol<<" in ";
            cout<< test.model.name << "  not found in "<<mdl.model.name<<endl;
            return false;
        }
    }
    return true;
}

bool KIM_API_model::is_it_fixed_par(char* name){
     char tmpname[KEY_CHAR_LENGTH]="";
     strncpy(&tmpname[0],name,strlen(name)+1);
     char * tmp = strtok(tmpname,"_");if(tmp == NULL) return false;
     if(strcmp(tmp,"PARAM")==0) {
         tmp = strtok(NULL,"_");if(tmp == NULL) return false;
         if(strcmp(tmp,"FIXED")==0) return true;
     }
     return false;
}
bool KIM_API_model::is_it_free_par(char* name){
     char tmpname[KEY_CHAR_LENGTH]="";
     strncpy(&tmpname[0],name,strlen(name)+1);
     char * tmp = strtok(tmpname,"_");if(tmp == NULL) return false;
     if(strcmp(tmp,"PARAM")==0) {
         tmp = strtok(NULL,"_");if(tmp == NULL) return false;
         if(strcmp(tmp,"FREE")==0) return true;
     }
     return false;
}
bool KIM_API_model::is_it_par(char* name){
    return is_it_free_par(name) || is_it_fixed_par(name);
}
bool KIM_API_model::init(char * testinputfile,char* testname, char * modelinputfile,char *modelname){
    //check test-model match and preinit test-model-API
    KIM_API_model test,mdl;
    //preinit test and model API object

    test.preinit(testinputfile,testname);
    mdl.preinit(modelinputfile,modelname);

    //check if they match
    if (is_it_match(test,mdl)){
        this->preinit(modelinputfile,modelname);
        this->irrelevantVars2donotcompute(test,*this);
        
        strcpy(this->NBC_method_current, mdl.NBC_method_current);

        test.free(); mdl.free();
        char computestr [] = "compute";
        compute_index = get_index(computestr);
        get_full_neigh_index = get_index("get_full_neigh");
        get_half_neigh_index = get_index("get_half_neigh");
        return true;
    }else{
        test.free(); mdl.free();
 cout<<"Do not match  " << modelname << " and "<< testname<<endl;
        return false;
    }

}

bool KIM_API_model::init(char* testname, char* modelname){
    char testfile[160] = KIM_DIR_TESTS;
    char modelfile[160] = KIM_DIR_MODELS;
    strcat(testfile,testname);strcat(testfile,"/");strcat(testfile,testname);
    strcat(testfile,".kim");
    strcat(modelfile,modelname);strcat(modelfile,"/");strcat(modelfile,modelname);
    strcat(modelfile,".kim");

    return this->init(testfile,testname,modelfile,modelname);
}
#ifndef KIM_DYNAMIC
extern "C" {
#include "model_init_include.h"
}
bool KIM_API_model::model_init(){
    char modelname[KEY_CHAR_LENGTH]="";
    KIM_API_model * kim;
    void ** pkim;
    strcpy(modelname,this->model.name);
    kim=this;
    pkim =(void**) &kim;
cout<< "KIM_API_model::model_init: call statically linked initialize routine for::"<<modelname<<endl;
#include "model_init_include.cpp"

    cout<< "KIM_API_model::model_init: model initiliser failed for ";
    cout<<modelname<<endl;
    return false;
}
#else
bool KIM_API_model::model_init(){
    char modelname[KEY_CHAR_LENGTH]="";
    KIM_API_model * kim;
    void ** pkim;
    char model_slib_file[160];
    char model_init_routine_name[160];
    strcpy(modelname,this->model.name); 
    kim=this;
    pkim =(void**) &kim;
    sprintf(model_slib_file,"%s%s/%s.so",KIM_DIR_MODELS,modelname,modelname);

cout<<"KIM_API_model::model_init: call dynamically linked initialize routine for:"<<modelname<<endl;
cout<<"               from the shared library:"<<model_slib_file<<endl;
    sprintf(model_init_routine_name,"%s_init_",modelname);
    for(int i=0;i<strlen(model_init_routine_name);i++){
         model_init_routine_name[i]=tolower(model_init_routine_name[i]);
    }
    model_lib_handle = dlopen(model_slib_file,RTLD_NOW);
    if(!model_lib_handle) {
         cout<< "KIM_API_model::model_init: model initiliser failed for ";
         cout<<modelname<<endl<<dlerror()<<endl;
         fprintf(stderr,"%s not found...\n",model_slib_file);
	 return false;
    }
    typedef void (*Model_Init)(void **);//prototype for model_init
    Model_Init mdl_init = (Model_Init)dlsym(model_lib_handle,model_init_routine_name);
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        cerr << "Cannot load symbol: " << dlsym_error <<endl;
        dlclose(model_lib_handle);
        return false;
    }
    (*mdl_init)(pkim);
    return true;
}
#endif

void KIM_API_model::model_destroy(int *error){
  typedef void (*Model_Destroy)(void *,int *);//prototype for model_destroy
  *error =0;
  Model_Destroy mdl_destroy = (Model_Destroy) (*this)["destroy"].data;
  //call model_destroy
  KIM_API_model *pkim = this;

  if (mdl_destroy != NULL) {  
      (*mdl_destroy)((void *)&pkim, error);
  }

#ifdef KIM_DYNAMIC
  dlclose(model_lib_handle);
#endif
 *error =1;
}
void KIM_API_model::model_compute(int *error){
  // set model_compute pointer
  typedef void (*Model_Compute)(void *,int *);//prototype for model_compute
  *error = 0;
  Model_Compute mdl_compute = (Model_Compute) (*this)[compute_index].data;
  if (mdl_compute == NULL) return;
  *error = 1;
  //call model_compute
  KIM_API_model *pkim = this;
  (*mdl_compute)((void *)&pkim, error);
}

int KIM_API_model::get_full_neigh(int mode, int request, int *atom,
        int *numnei, int** nei1atom, double** Rij){
    int locrequest=request;
    int locmode = mode;
    if(mode!=0 && mode!=1) return -2;
    if(this == NULL) return -3;
    typedef int(*Get_Neigh)(void **, int *, int *, int *, int *, int **,double **);
    if (get_full_neigh_index < 0) return -3;
    Get_Neigh get_neigh = (Get_Neigh)(*this)[get_full_neigh_index].data;
     KIM_API_model *pkim = this;
    if (baseConvertKey==0) {
        int erkey = (*get_neigh)((void **)&pkim,&locmode, &locrequest, atom, numnei, nei1atom, Rij );
        if(*numnei > KIM_API_MAX_NEIGHBORS) {
            cout<<endl<< "KIM_API::get_full_neigh: numnei > MAX_NEIGHBORS : ";
            cout<<" "<<*numnei <<" > "<< KIM_API_MAX_NEIGHBORS<<endl;
            return -4;
        }
        return erkey;
    }else if (baseConvertKey == 1 || baseConvertKey == -1){
        int req=request;
        if (mode ==1) req = request - baseConvertKey;
        int at = *atom;
        if (mode==0 && request == 0) {
            return (*get_neigh)((void **)&pkim,&locmode, &req, &at, numnei, nei1atom, Rij );
        }else{
            int erkey = (*get_neigh)((void **)&pkim,&locmode, &req, &at, numnei, nei1atom, Rij );
            if(*numnei > KIM_API_MAX_NEIGHBORS) {
                cout<<endl<< "KIM_API::get_full_neigh: numnei > MAX_NEIGHBORS : ";
                cout<<" "<<*numnei <<" > "<< KIM_API_MAX_NEIGHBORS<<endl;
                return -4;
            }
            if (erkey ==0 || erkey == 1){
                *atom = at + baseConvertKey;
                for (int i = 0; i<(*numnei) - 1;i++){
                    neiOfAnAtom_full[i] = (*nei1atom)[i] + baseConvertKey;
                }
                *nei1atom = &(neiOfAnAtom_full[0]);
            }
            return erkey;
        }
    }else{
        cout<<endl<< "KIM_API::get_full_neigh: wrong base convert key,baseConvertKey =";
        cout<< baseConvertKey <<"  (must be 0,1 or -1)"<<endl;
        return -3;
    }
}

int KIM_API_model::get_half_neigh(int mode, int request, int *atom,
        int *numnei, int** nei1atom, double** Rij){
    int locrequest=request;
    int locmode = mode;

    if(mode!=0 && mode!=1) return -2;
    if(this == NULL) return -3;
    typedef int (*Get_Neigh)(void **, int *, int *, int *, int *, int **,double **);

    if (get_half_neigh_index < 0) return -3;
    Get_Neigh get_neigh = (Get_Neigh)(*this)[get_half_neigh_index].data;
    KIM_API_model *pkim = this;

    if (baseConvertKey==0) {
        int erkey = (*get_neigh)((void **)&pkim,&locmode, &locrequest, atom, numnei, nei1atom, Rij );
        if(*numnei > KIM_API_MAX_NEIGHBORS) {
            cout<<endl<< "KIM_API::get_half_neigh: numnei > MAX_NEIGHBORS : ";
            cout<<" "<<*numnei <<" > "<< KIM_API_MAX_NEIGHBORS<<endl;
            return -4;
        }
        return erkey;
    }else if (baseConvertKey == 1 || baseConvertKey == -1){

 	int req=request;
        if (mode ==1) req = request - baseConvertKey;
 	int at = *atom;

        if (mode==0 && request == 0) {	    
            return (*get_neigh)((void **)&pkim,&locmode, &req, &at, numnei, nei1atom, Rij );
        }else{
            int erkey = (*get_neigh)((void **)&pkim,&locmode, &req, &at, numnei, nei1atom, Rij );
            if(*numnei > KIM_API_MAX_NEIGHBORS) {
                cout<<endl<< "KIM_API::get_full_neigh: numnei > MAX_NEIGHBORS : ";
                cout<<" "<<*numnei <<" > "<< KIM_API_MAX_NEIGHBORS<<endl;
                return -4;
            }
            if (erkey == 1){
                *atom= at + baseConvertKey;
                for (int i = 0; i<(*numnei);i++){
                    neiOfAnAtom_half[i] = (*nei1atom)[i] + baseConvertKey;
                }
                *nei1atom = &(neiOfAnAtom_half[0]);
            }
            return erkey;
        }
    }else{
        cout<<endl<< "KIM_API::get_half_neigh: wrong base convert key,baseConvertKey =";
        cout<< baseConvertKey <<"  (must be 0,1 or -1)"<<endl;
        return -3;
    }
}

void KIM_API_model::irrelevantVars2donotcompute(KIM_API_model & test, KIM_API_model & mdl){
    if(! is_it_match_noDummyCount(test,mdl.inlines,mdl.numlines)) {
        cout<<"irrelevantVars2donotcompute: not a test-model match"<<endl;
        exit(133);
    }
    for(int i=0; i<mdl.numlines;i++){
        if(mdl.inlines[i].isitoptional()) {
            mdl[i].flag->calculate = 0;
            for (int j=0;j<test.model.size;j++){
                if(test[j] ==mdl.inlines[i]) mdl[i].flag->calculate = 1;
            }

        }
    }
}
void KIM_API_model::allocateinitialized(KIM_API_model * mdl, intptr_t natoms, int ntypes, int * error){
    // in process
    if ( mdl->model.data == NULL) {
        cout<<"KIM_API_model::allocateinitialized: model is not preinitialized"<<endl;
        *error =0;
        return;
    }
    for(int i=0; i<mdl->model.size;i++){
        intptr_t rank = (intptr_t)mdl->inlines[i].get_rank();
        int *shape = mdl->inlines[i].get_shape();
        int calculate = (*mdl)[i].flag->calculate;
 
        intptr_t sz=0;
        int c=1;
        if (shape!=NULL) {
            for(int k=1;k<rank;k++) c=c*shape[k];
 
            if(mdl->inlines[i].isitperatom()) {

                sz=natoms*c;
                shape[0]=natoms;
  
            }else if(mdl->inlines[i].isitpernatomtypes()){
  
                sz=ntypes*c;
                shape[0]=ntypes;
            }else if(mdl->inlines[i].isitsizedefined()){
   
                sz=shape[0]*c;
            }else{
                cout<<"KIM_API_model::allocateinitialized: shape[0] of";
                cout<< mdl->inlines[i].name<<"  is "<< shape[0]<<endl;
            }
        }else{
            sz = 1;
            if (strcmp((*mdl)[i].type,"pointer")==0 || strcmp((*mdl)[i].type,"method")==0) sz=0;
            if (strcmp((*mdl)[i].type,"dummy")==0 ) sz=0;
        }
        if(mdl->inlines[i].isitoptional() && calculate == 0) {
            sz=0;
            if(shape!=0) shape[0]=0;
        }
        (*mdl)[i].free();
        (*mdl)[i].init(mdl->inlines[i].name,mdl->inlines[i].type,sz,rank,shape);
        strncpy((*mdl)[i].unit->dim,mdl->inlines[i].dim,strlen(mdl->inlines[i].dim)+1);
        (*mdl)[i].flag->calculate=calculate;
         (*mdl)[i].flag->peratom = 1;
        if(mdl->inlines[i].isitperatom()) (*mdl)[i].flag->peratom = 0;
        delete [] shape;
    }
    mdl->set_units(mdl->originalUnits);
    *error=1;
}
bool KIM_API_model::  is_unitsfixed(){
    return unitsFixed;
}
void KIM_API_model::set_unitsfixed(bool f){
    unitsFixed=f;
}
void KIM_API_model::transform_units_to(char * unitS,int *error){
    *error=0;
    for(int i=0;i<model.size;i++){
        if(strcmp((*this)[i].type,"real")==0 || strcmp((*this)[i].type,"real*8")==0 ){
            int uindexcur,uindexnext;
            float acurrent,anext;
            if (strcmp(this->currentUnits,this->originalUnits)==0){
                acurrent = (*this)[i].unit->scale;
            }else{
                uindexcur=get_indexOfsupportedUnits(currentUnits);
                acurrent = UnitsSet[uindexcur][(*this)[i].unit->dim];
            }
            if (strcmp(unitS,this->originalUnits)==0){
                anext = (*this)[i].unit->scale;
            }else{
                uindexnext=get_indexOfsupportedUnits(unitS);
                anext = UnitsSet[uindexnext][(*this)[i].unit->dim];
            }
            data_multiply_a((*this)[i].data, (*this)[i].type,(*this)[i].size,acurrent/anext);
            
        }
    }
    strncpy(currentUnits,unitS,strlen(unitS)+1);
    *error=1;
}
bool KIM_API_model::set_units(char * unitS){
    if(!among_supported_units(unitS)  && strcmp(unitS,"custom")!=0 ) return false;
    if(strcmp(unitS,"custom")==0){
        int ind = 0;
        int ii=0;
        for (int i=0;i<model.size;i++){

             strncpy((*this)[i].unit->dim,inlines[i].dim,KEY_CHAR_LENGTH);

            if(UnitsSet[ind].isitunit(inlines[i].dim)){
                strncpy((*this)[i].unit->units,"custom",strlen("custom")+1);
                (*this)[i].unit->scale = inlines[i].get_unitscale();
            }else{
                strncpy((*this)[i].unit->units,"none",strlen("custom")+1);
                (*this)[i].unit->scale = 1.0;
            }

        }
        strncpy(this->originalUnits,"custom",strlen("custom")+1);
        strncpy(this->currentUnits,"custom",strlen("custom")+1);
    }else{
        int ind = this->get_indexOfsupportedUnits(unitS);
        for (int i=0;i<model.size;i++){

            strncpy((*this)[i].unit->dim,inlines[i].dim,KEY_CHAR_LENGTH);

            if(UnitsSet[ind].isitunit(inlines[i].dim)){
                strncpy((*this)[i].unit->units,unitS,strlen(unitS)+1);
            }else{
                strncpy((*this)[i].unit->units,"none",strlen("custom")+1);
            }

        }
        strncpy(this->originalUnits,unitS,strlen(unitS)+1);
        strncpy(this->currentUnits,unitS,strlen(unitS)+1);
        setScale(ind);
    }
    strncpy(this->model.unit->units,originalUnits,strlen(originalUnits)+1);
    return true;
}
void KIM_API_model::get_units(char * units,int *error){
    *error=0;
    strncpy(units, this->currentUnits,strlen(this->currentUnits)+1);
    *error=1;
}
void KIM_API_model::get_originalUnits(char * unitS,int *error){
    *error=0;
    strncpy(unitS, this->originalUnits, strlen(this->originalUnits)+1);
    *error=1;
}

float KIM_API_model::get_unit_scalefactor(char*nm,int *error){
    int ind= get_index(nm);
    *error =1;
    if(ind < 0){
        *error =-3;
        return -3.0;
    }
    if(strcmp((*this)[ind].unit->dim,"none")==0) {
        *error = -1;
        return -1.0;
    }
    if(strcmp((*this)[ind].unit->units,"none")==0) {
        *error =-2;
        return -2.0;
    }
    if(strcmp(this->originalUnits,this->currentUnits)==0){
        return (*this)[ind].unit->scale;
    }else{
        int i = get_indexOfsupportedUnits(currentUnits);
        return UnitsSet[i][(*this)[ind].unit->dim];
    }
}
void KIM_API_model::supported_units_init(){
        numUnitsSet =2; // change when available  needed
        char cnf_files[numUnitsSet][161] ;
        //standard units
        char standard_units_file[161]=KIM_DIR_API;
        strcat(standard_units_file,"standard_units.cfg");
        strncpy(& cnf_files[0][0],standard_units_file,161);
        // SI units
        char SI_units_file[161]=KIM_DIR_API;
        strcat(SI_units_file,"si_units.cfg");
        strncpy(& cnf_files[1][0],SI_units_file,161);
        
        // add here more when availabel and change the total name
        //strncpy(&cnf_files[1][0] ...

        (*this).UnitsSet = new SystemOfUnit [numUnitsSet];
 
        for(int i=0; i<numUnitsSet; i++){
            (*this).UnitsSet[i].init(& cnf_files[i][0]);

        }
}
bool KIM_API_model::are_infileunits_consistent(){
        char unittmp[KEY_CHAR_LENGTH];
        char coordinates_str [] ="coordinates";
        int ind= get_index(coordinates_str);
        strncpy(& unittmp[0],(*this)[ind].unit->units, KEY_CHAR_LENGTH);
      
        for(int i=0;i<model.size;i++){
            if(strcmp(&unittmp[0],(*this)[i].unit->units)!=0){
                if(strcmp((*this)[i].unit->units,"none")!=0) return false;
                if(strcmp((*this)[i].unit->dim,"none")!=0) return false;
            }
        }
        return true;
}
bool KIM_API_model::among_supported_units(char *unitS){
        for(int i=0;i<numUnitsSet;i++){
           if( strcmp(&(UnitsSet[i].UnitsSystemName[0]),unitS)==0) return true ;
        }
        return false;
}
int KIM_API_model::get_indexOfsupportedUnits(char * unitS){
        for(int i=0;i<numUnitsSet;i++){
           if( strcmp(&(UnitsSet[i].UnitsSystemName[0]),unitS)==0) return i ;
        }
        cout<<"get_indexOfsupportedUnits, no unitsystem "<<unitS<<" found"<<endl;
        exit( 3033);
}
void KIM_API_model::setScale(int index){
        for(int i=0; i< model.size;i++){
            if (strcmp((*this)[i].unit->dim,"none")!=0){
                (*this)[i].unit->scale =  (UnitsSet[index])[(*this)[i].unit->dim];
            }
        }
 }
void KIM_API_model::data_multiply_a(void *dt,char* type,intptr_t sz,float a){
        if(sz < 1) return;
        if(strcmp(type,"real")==0){
            float *d = (float *) dt;
            for(int i=0;i<sz;i++) d[i]=d[i]*a;
        }else if(strcmp(type,"real*8")==0) {
            double *d = (double *)dt;
            for(int i=0;i<sz;i++) d[i]=d[i]*a;
        }
}
ostream &operator<<(ostream &stream, KIM_API_model &a){
    stream<<"*************************************"<<endl;
    stream << a.model;
    stream<<"-------------------------------------"<<endl;
    KIMBaseElement **pel =  (KIMBaseElement **)  a.model.data;
    for(int i=0;i<a.model.size;i++)   stream<< *(pel[i]);
    stream<<"-------------------------------------"<<endl;
    stream<<"currentUnits: "<<a.currentUnits<<",     originalUnits: "<<a.originalUnits<<endl;
    stream<<"*************************************"<<endl;

    return stream;
}
bool KIM_API_model::init_AtomsTypes(){
    nAtomsTypes=0;
    for(int i=0;i < numlines;i++){
        if (strcmp(inlines[i].type, "spec")==0) nAtomsTypes++;
    }
    if (nAtomsTypes==0) return true;

    AtomsTypes = new Atom_Map[nAtomsTypes];
    int ii=0;
    for(int i=0;i < numlines;i++){
        if (strcmp(inlines[i].type, "spec")==0){
            strncpy(AtomsTypes[ii].symbol,inlines[i].name,strlen(inlines[i].name)+1);
            if(inlines[i].get_rank() !=1){
                ErrorCode = -30;
                cout <<" atom code error";
                return false;
            }
            int * shp = inlines[i].get_shape();
            AtomsTypes[ii].code = shp[0];
            delete [] shp;
            ii++;
        }
    }
    qsort((void *) AtomsTypes,(size_t) nAtomsTypes,sizeof(Atom_Map),&(Atom_Map::comparator));
    ErrorCode=1;
    return true;
}
void * KIM_API_model::get_listAtomsTypes(int* nATypes, int* error){
    *error=0;
    if (nAtomsTypes==0){
        *nATypes = 0;
        *error =1; //success but no atoms type specified
        return NULL;
    }
    if (nAtomsTypes < 0){
        *error =-20;//was internal error in init nAtomsTypes
        return NULL;
    }
    *nATypes = nAtomsTypes;
    char * listatypes= new char[nAtomsTypes*KEY_CHAR_LENGTH];
    for (int i=0;i<nAtomsTypes*KEY_CHAR_LENGTH;i++) listatypes[i] = '\0';
    for(int i=0; i<nAtomsTypes; i++){
        strncpy(listatypes + i*KEY_CHAR_LENGTH, AtomsTypes[i].symbol,strlen( AtomsTypes[i].symbol)+1);
    }
    *error =1;//success
    return (void *) listatypes;
}
void * KIM_API_model::get_NBC_method(int* error){
    *error=0;
    if(strcmp(this->NBC_method_current,"none")==0) {
        // no NBC methods are specified
        return NULL;
    }
    char * method = new char[KEY_CHAR_LENGTH];
    for (int i=0;i<KEY_CHAR_LENGTH;i++) method[i] = '\0';
    strcpy(method,this->NBC_method_current);
    *error=1; //success
    return (void *)method;
}

void * KIM_API_model::get_listParams(int* nVpar, int* error){
    int count;
    count=0;
    char * listvpar;
    for(int i=0; i< model.size; i++){
        if(is_it_par((*this)[i].name)) count++;
    }
    if (count==0) {
        *nVpar=0;
        *error=1;  //success but no parameters
        return NULL;
    }
    *nVpar=count;
    listvpar= new char[KEY_CHAR_LENGTH * count];
    for (int i=0;i<count*KEY_CHAR_LENGTH;i++) listvpar[i] = '\0';
    count=0;
    for (int i=0;i<model.size;i++){
         if(is_it_par((*this)[i].name)){
             strncpy(listvpar+count*KEY_CHAR_LENGTH, (*this)[i].name, strlen((*this)[i].name) +1 );
             count++;
         }
    }
    *error =1;//success
    return (void *) listvpar;
}
void * KIM_API_model::get_listFreeParams(int* nVpar, int* error){
    int count;
    count=0;
    char * listvpar;
    for(int i=0; i< model.size; i++){
        if(is_it_free_par((*this)[i].name)) count++;
    }
    if (count==0) {
        *nVpar=0;
        *error=1;  //success but no parameters
        return NULL;
    }
    *nVpar=count;
    listvpar= new char[KEY_CHAR_LENGTH * count];
    for (int i=0;i<count*KEY_CHAR_LENGTH;i++) listvpar[i] = '\0';
    count=0;
    for (int i=0;i<model.size;i++){
         if(is_it_free_par((*this)[i].name)){
             strncpy(listvpar+count*KEY_CHAR_LENGTH, (*this)[i].name, strlen((*this)[i].name) +1 );
             count++;
         }
    }
    *error =1;//success
    return (void *) listvpar;
}
void * KIM_API_model::get_listFixedParams(int* nVpar, int* error){
    int count;
    count=0;
    char * listvpar;
    for(int i=0; i< model.size; i++){
        if(is_it_fixed_par((*this)[i].name)) count++;
    }
    if (count==0) {
        *nVpar=0;
        *error=1;  //success but no parameters
        return NULL;
    }
    *nVpar=count;
    listvpar= new char[KEY_CHAR_LENGTH * count];
    for (int i=0;i<count*KEY_CHAR_LENGTH;i++) listvpar[i] = '\0';
    count=0;
    for (int i=0;i<model.size;i++){
         if(is_it_fixed_par((*this)[i].name)){
             strncpy(listvpar+count*KEY_CHAR_LENGTH, (*this)[i].name, strlen((*this)[i].name) +1 );
             count++;
         }
    }
    *error =1;//success
    return (void *) listvpar;
}

int KIM_API_model::get_aTypeCode(char* atom, int * error){
    *error =0;
    if (atom == NULL) return -2; //no atom symbol provided
    Atom_Map key, *res=NULL;
    strcpy(key.symbol,atom);
    res = (Atom_Map *)bsearch((void *)&key,AtomsTypes,nAtomsTypes,sizeof(Atom_Map),&(Atom_Map::comparator));
    if (res == NULL) {
        *error = -1;
        return  -1; //did not find atom symbol among atom types
    }
    *error=1;
    return res->code;
}

bool KIM_API_model::NBC_methods_match(KIM_API_model& test, KIM_API_model& mdl){
    
    
    
    bool NBC_method_mdl[number_NBC_methods];
    bool NBC_method_test[number_NBC_methods];
    for (int i=0;i<number_NBC_methods; i++){
        NBC_method_mdl[i] = is_it_in_and_is_it_dummy(mdl,NBC_methods[i]);
        NBC_method_test[i] = is_it_in_and_is_it_dummy(test,NBC_methods[i]);
    }

    int indexes[number_NBC_methods];
    for (int i=0;i<number_NBC_methods; i++) indexes[i]=1000;
    bool match=false;
    for (int i=0;i<number_NBC_methods; i++) {
        if(NBC_method_test[i]&&NBC_method_mdl[i]){
            match=true;
            indexes[i]=test.get_index(NBC_methods[i]);
        }
    }
    if(!match) return false;
    int min=1000;
    for (int i=0;i<number_NBC_methods; i++) if(indexes[i]<min) min=indexes[i];
    strcpy(test.NBC_method_current,test[min].name);
    strcpy(mdl.NBC_method_current,test[min].name);
    return true;
}
bool KIM_API_model::check_consistance_NBC_method(){
    //will check current NBC method if all arguments are in the object
    bool match=false;
    int i;
    for (i=0; i<n_NBC_methods; i++){
        if(strcmp(NBC_methods[i],NBC_method_current)==0){
            match =true;
            break;
        }
    }
    if (!match) {
        cout<<"check_consistance_NBC_method :"<<NBC_method_current
                <<" is not among the methods"<<endl;
        return false;
    }
    for (int j=0;j<nnarg_NBC[i]; j++){
        if (get_index(arg_NBC_methods[i][j]) == -1){
            cout<<"check_consistance_NBC_method : argument "<< arg_NBC_methods[i][j];
            cout<<" is not in  KIM API object"<<endl;
            return false;
        }
    }
    return true;
}
