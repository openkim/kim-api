//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
//
// Contributors:
//    Valeriu Smirichinski
//

//
// Release: This file is part of the openkim-api.git repository.
//


#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string.h>
#include <sstream>


#ifdef KIM_DYNAMIC
#include <dlfcn.h>
#endif

using namespace std;
#include "KIM_API.h"

#define KIM_KEY_STRING_LENGTH 64
#define KIM_LINE_LENGTH 512

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
    strncpy(dim,"none",KIM_KEY_STRING_LENGTH);
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

            init2empty();
            //check for comments part and get it removed
            tmp = strpbrk(inString,"#");
            if(tmp !=NULL) {
                strncpy(comments,tmp,strlen(tmp)+1);
                tmp[0]='\0';

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

            if(strcmp(type,"flag")==0) {
                strncpy(dim,"none",5);
                strncpy(shape,"[]",3);
                return true;
            }

            if(strcmp(type,"spec")==0) {
                strncpy(dim,"none",5);
                tmp = strtok(NULL," \t");if(tmp == NULL) return false;
                strcat(shape,"[");
                strcat(shape,tmp);
                strcat(shape,"]");
                return true;
            }else if(strcmp(type,"flex")==0){
                //cout<<"KIM_IOline::getFields:flex type for preprocessor only..."<<endl;
                cout<<"* Error (KIM_IOline::getFields): Particle name `flex' in SUPPORTED_ATOM/PARTICLES_TYPES section is only supported by the KIM Processing Pipeline."<<endl;
                KIM_API_model::fatal_error_print();
                exit (333);
                //return false;
            }

            tmp = strtok(NULL," \t");if(tmp == NULL) return false;
            strncpy(dim,tmp,strlen(tmp)+1);

            tmp = strtok(NULL," \t");if(tmp == NULL) return false;
            strncpy(shape,tmp,strlen(tmp)+1);

            tmp = strtok(NULL," \t");if(tmp == NULL) return true;
            strncpy(requirements,tmp,strlen(tmp)+1);

            return true;
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
            cout<<"* Error (KIM_IOline::get_rank): bad shape format"<<endl;
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
int * KIM_IOline::get_shape(int natoms, int ntypes){
            char shapetmp[strlen(shape)+1];
            char tmpstring[128];
            strncpy(shapetmp,shape,strlen(shape)+1);
            int rnk = get_rank();
            if (rnk < 1) return NULL;
            int *shp = new int[rnk];
            int i=0;
            char *tmp =strtok(shapetmp,"[,]");
            while(tmp!=NULL){
                double dd = strtod(tmp,&tmp);
                shp[i]=(int)dd;
                if(shp[i]==0){
                    strcpy(tmpstring,tmp);
                    this->strip(tmpstring);
                    if(strcmp(tmpstring,"numberParticleTypes")==0) shp[i]=ntypes;
                    if(strcmp(tmpstring,"numberOfParticles")==0) shp[i]=(int)natoms;
                }
                tmp = strtok(NULL,"[,]");
                i++;
            }
            return shp;
}
 bool KIM_IOline::isitpernatomtypes(){
             char shapetmp[strlen(shape)+1];
             char tmpstring[128];
             strncpy(shapetmp,shape,strlen(shape)+1);
             char *tmp =strtok(shapetmp,"[,]");
             if(tmp==NULL)return false;
             while(tmp!=NULL){
                strcpy(tmpstring,tmp);
                this->strip(tmpstring);
                if(strcmp(tmpstring,"numberParticleTypes")==0) return true;
                tmp = strtok(NULL,"[,]");
             }
             return false;
 }
bool KIM_IOline::isitsizedefined(){

             int rnk =this->get_rank();
             if (rnk < 1) return false;
             int * shp = this->get_shape();
             if (shp==NULL) return false;

             int c=1;
             for (int i=0; i<rnk;i++) c=c*shp[i];

             delete [] shp;

             if (c > 0) return true;
             return false;
}
bool KIM_IOline:: isitperatom(){
             char shapetmp[strlen(shape)+1];
             char tmpstring[128];
             strncpy(shapetmp,shape,strlen(shape)+1);
             char *tmp =strtok(shapetmp,"[,]");
             if(tmp==NULL)return false;
             while(tmp!=NULL){
                strcpy(tmpstring,tmp);
                this->strip(tmpstring);
                if(strcmp(tmpstring,"numberOfParticles")==0) return true;
                tmp = strtok(NULL,"[,]");
             }
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
            strip(this->shape);
            strip(this->requirements);
 }
void KIM_IOline:: init2empty(){
            name[0]='\0';
            type[0]='\0';
            dim[0]='\0';
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
        stream<<a.shape<<" "<<a.requirements;
        stream << endl;
        return stream;
};
istream &operator>>(istream &stream, KIM_IOline &a){
        char inputline[KIM_LINE_LENGTH];
        stream.getline(inputline,KIM_LINE_LENGTH-1);
        if(stream.fail() && !stream.eof()){
           cerr << "* Error (operator>> KIM_IOline): Input line in .kim file longer than KIM_LINE_LENGTH (default 512) characters.\n"
                << "         The line is: `"
                << inputline << "'" << "\nExiting..." << endl;
           KIM_API_model::fatal_error_print();
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
    for(int i=0; i<121;i++) comment[i]='\0';
    for(int i=0; i<101;i++) name[i]='\0';
    for(int i=0; i<101;i++) value[i]='\0';
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

                for(i=i;i<=(int)strlen(inputString); i++){
                        if(*(inputString+i)=='#'){value[j]='\0';i+=2;break;};
                        value[j]=*(inputString+i);
                        j++;
                        value[j]='\0';
                }

                j=0;
                if(i>=(int)strlen(inputString)){comment[0]='\0';strip();return true;};
                for(i=i;i<=(int)strlen(inputString); i++){
                        comment[j]=*(inputString + i);
                        comment[j+1]='\0';
                        j++;
                }

                strip();

                return true;
}
int  IOline::readlines(char * infile, IOline **inlines){
            int counter=0;
        IOline inlne;
        *inlines=NULL;
        ifstream myfile;
         myfile.open(infile);
         if(!myfile){
             cout<<"* Error (IOline::readlines): can not open file:"<<infile<<":"<<endl;
             KIM_API_model::fatal_error_print();
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
int IOline::readlines_str(char* instrn, IOline** inlines){
    int counter=0;
        IOline inlne;
        *inlines=NULL;
        string in_instrn=instrn;
        stringstream myfile (in_instrn,stringstream::in|stringstream::out) ;
        stringstream myfile1 (in_instrn,stringstream::in|stringstream::out) ;
         if(!myfile){
             cout<<"* Error (IOline::readlines_str): can not parse instrn."<<endl
                 <<"        Offending line is: `" << in_instrn << "'" << endl;
             KIM_API_model::fatal_error_print();
             exit(327);
         }
        myfile.seekp(stringstream::beg);
          while(!myfile.eof()){
                myfile>> inlne;
                if(inlne.goodformat) counter++;
        }


        if (counter < 1) return counter;


        (*inlines) =  new IOline [counter] ;


        myfile1.seekp(stringstream::beg);

        counter=0;
        while(!myfile1.eof()){
                myfile1 >> inlne;

                if(inlne.goodformat) {

                    (*inlines)[counter]=inlne;

                    counter++;
                }
        }
        return counter;
}

ostream &operator<<(ostream &stream, IOline a){
        stream<<a.name<<":= "<<a.value<<" #"<<a.comment;
        return stream;
}
istream &operator>>(istream &stream, IOline &a){
        char inputline[KIM_LINE_LENGTH];
        stream.getline(inputline,KIM_LINE_LENGTH-1);
        if(a.getFields(inputline)){
                a.goodformat=true;
        }else{
                a.goodformat=false;
        }
        return stream;
}

KIMBaseElement:: KIMBaseElement(){
            nullefy();
}
KIMBaseElement::~KIMBaseElement(){

}
void KIMBaseElement:: init(char *nm,char * tp,intptr_t sz, intptr_t rnk, int *shp,void * pdata){
            flag = new KIMBaseElementFlag;

            unit = new KIMBaseElementUnit;
            name = new char[KIM_KEY_STRING_LENGTH];

            type = new char[KIM_KEY_STRING_LENGTH];
            strncpy(name,nm,KIM_KEY_STRING_LENGTH);
            strncpy(type,tp,KIM_KEY_STRING_LENGTH);


            if(rnk < 0) {
                cout << "* Error (KIMBaseElement::init): KIMBaseElement_init:rnk < 0"<<endl;
                KIM_API_model::fatal_error_print();
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
                  cout << "* Error (KIMBaseElement::init): KIMBaseElement_init:shp==NULL"<<endl;
                  KIM_API_model::fatal_error_print();
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
            //switch off check for virial and process_dnEdr related things
            if(strcmp(name,"virial") == 0  ||
               strcmp(name,"particleVirial")==0 ||
               strcmp(name,"hessian")    ==0 ||
               strcmp(name,"process_dEdr")    ==0 ||
               strcmp(name,"process_d2Edr2")    ==0  ) return true;
            //compare the rest
            if(strcmp(kimioline.name,this->name)==0){
                if(strcmp(kimioline.type,this->type)==0){
                    if(strcmp(kimioline.dim,this->unit->dim)==0){
                        if(kimioline.get_rank()==(int)(this->rank)) {
                        int *shp = kimioline.get_shape();
                        for(int i=0; i< (int)(this->rank);i++){
                            if(shp[i]!=this->shape[i]){
                                delete [] shp;
                                return false;
                               }
                             }
                            delete [] shp;
                            return true;
                        }
                    }
                }
            }
            return false;
}
bool KIMBaseElement::equiv(KIM_IOline& kimioline){
    if(strcmp(kimioline.name,this->name)==0)
        if(strcmp(kimioline.type,this->type)==0){
            if(strcmp(kimioline.type,"flag")==0) return true;
            if(strcmp(kimioline.dim,this->unit->dim)==0)
                if(kimioline.get_rank()==(int)(this->rank)) {
                    int *shp = kimioline.get_shape();
                    for(int i=0; i< (int)(this->rank);i++){
                        if(shp[i]!=this->shape[i]){
                            delete [] shp;
                            return false;
                        }
                    }
                    delete [] shp;
                    return true;
                }
        }
    return false;
}

int KIMBaseElement::getelemsize(char *tp){
            char realkey[KIM_KEY_STRING_LENGTH]="real";      //key defenitions
            char real8key[KIM_KEY_STRING_LENGTH]="real*8";
            char integerkey[KIM_KEY_STRING_LENGTH]="integer";
            char integer8key[KIM_KEY_STRING_LENGTH]="integer*8";
            char ptrkey[KIM_KEY_STRING_LENGTH]="pointer";
            char methodkey[KIM_KEY_STRING_LENGTH]="method";
            char flagkey[KIM_KEY_STRING_LENGTH]="flag";// add here to expand...

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
             }else if (strcmp(flagkey,tp)==0){
                return 0;
            }else{// add here more in else if block...
               cout << "* Error (KIMBaseElement::getelemsize): Unknown Type in KIM descriptor file line." << endl
                    << "         `" << tp <<"' is not one of: " << realkey << ", "
                    << real8key << ", " << integerkey << ", " << ptrkey << ", "
                    << integer8key << ", " << flagkey << endl;
            KIM_API_model::fatal_error_print();
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
    stream<<" phys.dimension: "<<a.unit->dim<<endl;
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
       //method_A init
       strcpy(NBC_method_A,"CLUSTER");
       strcpy(&arg_NBC_method_A[0][0],"coordinates");
       narg_NBC_method_A=1;

       //method_B init
       strcpy(NBC_method_B,"MI_OPBC_H");
       strcpy(&arg_NBC_method_B[0][0],"coordinates");
       strcpy(&arg_NBC_method_B[1][0],"boxSideLengths");
       strcpy(&arg_NBC_method_B[2][0],"numberContributingParticles");
       strcpy(&arg_NBC_method_B[3][0],"neighObject");
       strcpy(&arg_NBC_method_B[4][0],"get_neigh");
       narg_NBC_method_B=5;

       //method_C init
       strcpy(NBC_method_C,"MI_OPBC_F");
       strcpy(&arg_NBC_method_C[0][0],"coordinates");
       strcpy(&arg_NBC_method_C[1][0],"boxSideLengths");
       strcpy(&arg_NBC_method_C[2][0],"neighObject");
       strcpy(&arg_NBC_method_C[3][0],"get_neigh");
       narg_NBC_method_C=4;

       //method_D init
       strcpy(NBC_method_D,"NEIGH_RVEC_F");
       strcpy(&arg_NBC_method_D[0][0],"coordinates");
       strcpy(&arg_NBC_method_D[1][0],"neighObject");
       strcpy(&arg_NBC_method_D[2][0],"get_neigh");
       narg_NBC_method_D=3;

       //method_E init
       strcpy(NBC_method_E,"NEIGH_PURE_H");
       strcpy(&arg_NBC_method_E[0][0],"coordinates");
       strcpy(&arg_NBC_method_E[1][0],"numberContributingParticles");
       strcpy(&arg_NBC_method_E[2][0],"neighObject");
       strcpy(&arg_NBC_method_E[3][0],"get_neigh");
       narg_NBC_method_E=4;

       //method_F init
       strcpy(NBC_method_F,"NEIGH_PURE_F");
       strcpy(&arg_NBC_method_F[0][0],"coordinates");
       strcpy(&arg_NBC_method_F[1][0],"neighObject");
       strcpy(&arg_NBC_method_F[2][0],"get_neigh");
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
           arg_NBC_methods[i] = new char * [nnarg_NBC[i]];
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


       this->model_index_shift=0;
       this->AUX_index_shift =0;
       ErrorCode=1;
       AtomsTypes = NULL;
       nAtomsTypes = 0;
       locator_neigh_mode=false;
       iterator_neigh_mode=false;
       both_neigh_mode=false;
       model_buffer=NULL;

       this->virial_ind=-1;
       this->particleVirial_ind=-1;
       this->hessian_ind=-1;
       this->process_dEdr_ind=-1;
       this->process_d2Edr2_ind=-1;

       this->virial_need2add=false;
       this->particleVirial_need2add=false;
       this->hessian_need2add=false;
}
KIM_API_model:: ~KIM_API_model(){
      // free();
      for(int i=0;i<n_NBC_methods;i++){
         delete [] arg_NBC_methods[i];
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

        model.init(modelname,pointer_str,(intptr_t)(numlines-nAtomsTypes+3),1,shape);
        model.size = (intptr_t)(numlines-nAtomsTypes);

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
                //here to add checking is it derived or is it base units
                strncpy(el->unit->dim,inlines[i].dim,strlen(inlines[i].dim)+1);

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
        KIM_IOline * inlinesnew = new KIM_IOline[numlines - nAtomsTypes +3];
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

        //extra input
        IOline *extrainput;

        int nextra = IOline::readlines(initfile,&extrainput);
        for (int i=0;i<nextra;i++){

            if(strcmp(extrainput[i].name,"TEST_NAME")==0){
                strcpy(this->model.name,extrainput[i].value);
            }
            if(strcmp(extrainput[i].name,"MODEL_NAME")==0){
                strcpy(this->model.name,extrainput[i].value);
            }
        }
        delete [] extrainput;

        unit_h.init(initfile,&ErrorCode);
        if(ErrorCode < KIM_STATUS_OK) {
            return false;
        }
        return true;

 }

bool KIM_API_model::prestring_init(char *instrn){
        read_file_str_testname(instrn,&inlines,&numlines);


        int *shape=NULL;
        char pointer_str [] = "pointer";
        char modelname [] ="flag_name";
        //get Atoms Types and nAtomsTypes
        if (! init_AtomsTypes()) {
            ErrorCode=KIM_STATUS_FAIL;
            return false;
        }
        model.init(modelname,pointer_str,(intptr_t)(numlines-nAtomsTypes+3),1,shape);
        model.size =(intptr_t)(numlines-nAtomsTypes);

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
                //here to add checking is it derived or is it base units
                strncpy(el->unit->dim,inlines[i].dim,strlen(inlines[i].dim)+1);

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
        KIM_IOline * inlinesnew = new KIM_IOline[numlines - nAtomsTypes+3];
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


        //extra input like unitFixed flag and,later may be, authors
        IOline *extrainput;
        int nextra = IOline::readlines_str(instrn,&extrainput);
        for (int i=0;i<nextra;i++){
            /*
            if(strcmp(extrainput[i].name,"SystemOfUnitsFix")==0){
                if(strcmp(extrainput[i].value,"fixed")==0) this->unitsFixed=true;
            }
            */
            if(strcmp(extrainput[i].name,"TEST_NAME")==0){
                strcpy(this->model.name,extrainput[i].value);
            }
            if(strcmp(extrainput[i].name,"MODEL_NAME")==0){
                strcpy(this->model.name,extrainput[i].value);
            }
        }
        delete [] extrainput;
        unit_h.init_str(instrn,&ErrorCode);
        if(ErrorCode < KIM_STATUS_OK) return false;

        return true;
}

 void KIM_API_model::free_e(int *error){
        //
        *error = KIM_STATUS_FAIL;
        if(this==NULL) return;
        this->free();
        *error=KIM_STATUS_OK;
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


        numlines=0;

        if(AtomsTypes !=NULL) {
            delete [] AtomsTypes;
            AtomsTypes=NULL;
            nAtomsTypes=0;
        }
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
            if(c!=0) (*this)[ind].shape[0] = size/c;
        }
        (*this)[ind].flag->freeable = 1;
        return true;
}
bool KIM_API_model::set_data_by_index(int ind, intptr_t size, void* dt){
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
        if ((*this)[ind].rank==1){
            (*this)[ind].shape[0] = size;
        }
        (*this)[ind].flag->freeable = 1;
        return true;
}
void * KIM_API_model::get_data(char *nm,int *error){
        int i=get_index(nm);
        *error = KIM_STATUS_FAIL;
        if (i<0) return NULL;
        *error =KIM_STATUS_OK;
        return (*this)[i].data;
}

void * KIM_API_model::get_data_by_index(int ind, int* error){
        *error = KIM_STATUS_FAIL;
        if (ind<0) return NULL;
        *error =KIM_STATUS_OK;
        return (*this)[ind].data;
}

void * KIM_API_model::get_data(char *nm){
        int i=get_index(nm);
        if (i<0) return NULL;
        return (*this)[i].data;
}

int KIM_API_model::get_index(char *nm,int *error){
        for(int i=0; i< model.size;i++){
            if(strcmp((*this)[i].name,nm)==0) {
                *error =KIM_STATUS_OK;
                return i;
            }
        }
        *error = KIM_STATUS_FAIL;
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
        *error=KIM_STATUS_FAIL;
        if (ind < 0) return -1;
        *error=KIM_STATUS_OK;
        return (*this)[ind].size;
}
intptr_t KIM_API_model::get_shape(char *nm,int * shape, int *error){
        int ind=get_index(nm,error);
        *error =KIM_STATUS_ARG_UNKNOWN;
        if (ind < 0) return -1;
        *error =KIM_STATUS_OK;
        if((*this)[ind].rank == 0){
            return 0;
        }else if((*this)[ind].rank ==1){
            shape[0] = (int)(*this)[ind].size;
            return 1;
        }else if((*this)[ind].rank>1){
            for (int i=0; i< (*this)[ind].rank; i++) shape[i] =(*this)[ind].shape[i];
            return (*this)[ind].rank;
        }else{
            *error = KIM_STATUS_ARG_UNKNOWN;
            return -1;
        }
}

void KIM_API_model::set_shape(char* nm, int* shape, int rank, int* error){
    //size will be calculated and set too
        *error =KIM_STATUS_ARG_UNKNOWN;
        int ind=get_index(nm,error);
        if (ind < 0) return;
        if((intptr_t)(rank) != (*this)[ind].rank) {
            *error= KIM_STATUS_ARG_INVALID_RANK; //rank do not match
            return;
        }

        if((*this)[ind].rank == 0){
            (*this)[ind].size=1;
            *error = KIM_STATUS_OK; //success
            return;
        }else if((*this)[ind].rank ==1){
            (*this)[ind].shape[0]=shape[0];
            (*this)[ind].size=(intptr_t)shape[0];
            if (shape[0] < 0) {
                *error=KIM_STATUS_ARG_INVALID_SHAPE; //negative index
            }else{
                *error = KIM_STATUS_OK; //success
            }
            return;
        }else if((*this)[ind].rank>1){
            int sz=1;
            for (int i=0;i<rank;i++) {
                if (shape[i]<0){
                    *error = KIM_STATUS_ARG_INVALID_SHAPE; //negative index
                    return;
                }
                sz=sz*shape[i];
            }
            (*this)[ind].size=(intptr_t)sz;
            for (int i=0; i< rank; i++) (*this)[ind].shape[i]=shape[i];
            *error=KIM_STATUS_OK;//success
            return;
        }else{
            *error=KIM_STATUS_ARG_UNKNOWN;
            return;
        }
}
void KIM_API_model::set_compute(char *nm, int flag, int *error){
   *error = KIM_STATUS_FAIL;
   int ind = (*this).get_index(nm, error);
   if (*error != KIM_STATUS_OK) return;

   if ((flag == 1) || (flag == 0)){
      (*this)[ind].flag->calculate = flag;
      *error = KIM_STATUS_OK;
   }
   else
      *error = KIM_STATUS_FAIL;

   return;
}
void KIM_API_model::set_compute_by_index(int ind, int flag, int *error){
   *error = KIM_STATUS_FAIL;
   if ((flag == 1) || (flag == 0)){
      (*this)[ind].flag->calculate = flag;
      *error = KIM_STATUS_OK;
   }
   else
      *error = KIM_STATUS_FAIL;

   return;
}
bool KIM_API_model::get_compute(char *nm){
        if ((*this)[nm].flag->calculate == 1) return true;
        return false;
}
KIMBaseElement & KIM_API_model::operator[](int i){
        if ((i > (*this).model.size) || (i < 0)){
           cout<<"* Error (KIM_API_model::operator[](int i): invalid index." <<endl;
           KIM_API_model::fatal_error_print();
           exit(326);
        }
        KIMBaseElement **pel =(KIMBaseElement**) model.data;
        return *pel[i];
}
KIMBaseElement & KIM_API_model::operator[](char *nm){
        int error;
        int ind=get_index(nm,&error);
        if (error == KIM_STATUS_FAIL){
           cout<<"* Error (KIM_API_model::operator[](char *nm): name not found." <<endl;
           KIM_API_model::fatal_error_print();
           exit(325);
        }
        KIMBaseElement **pel =(KIMBaseElement**) model.data;
        return *pel[ind];
}

void KIM_API_model::read_file(char * initfile,KIM_IOline ** lns, int * numlns){
        int counter=0;
        KIM_IOline inln;
        ifstream myfile;
        myfile.open(initfile);
        if(!myfile){
            cout<<"* Error (KIM_API_model::read_file): can not open file: "<<initfile<<endl;
            KIM_API_model::fatal_error_print();
            exit (327);
        }
        while(!myfile.eof()){
                myfile >> inln;
                if(inln.goodformat) counter++;
        }
        myfile.close();
        *numlns = counter;

        *lns = new KIM_IOline[counter+3];

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

void KIM_API_model::read_file_str_testname(char* strstream, KIM_IOline** lns, int* numlns){
        int counter=0;
        KIM_IOline inln;

        //open string as stream from char *
        string in_strstream=strstream ;
        stringstream myfile (in_strstream, stringstream::in|stringstream::out);
        stringstream myfile1 (in_strstream, stringstream::in|stringstream::out);
        if(!myfile){
            cout<<"* Error (KIM_API_model::read_file_str_testname): can not access KIM descriptor file as input string."<<endl;
            KIM_API_model::fatal_error_print();
            exit (327);
        }


        myfile.seekp(stringstream::beg);//set to the begining
        while(!myfile.eof()){
                myfile >> inln;
                if(inln.goodformat) counter++;
        }

        myfile1.seekp(stringstream::beg);//set to the begining

        *numlns = counter;
        *lns = new KIM_IOline[counter+3];

        //myfile.open(initfile);
        counter=0;


        while(!myfile1.eof()){
                myfile1 >> inln;

                if(inln.goodformat) {
                    (*lns)[counter]=inln;
                    counter++;
                }
        }

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
            }//else if(is_it_par(mdtst[j].name)){
             //   match = true;
            //    break;
            //}
        }
        if(!match) {
           cout << "* Info (KIM_API_model::is_it_match): The following descriptor file line may not match with " << mdtst.model.name << "'s descriptor file."<<endl;
            cout<<IOlines[i]<<endl;
            return match;
        }
    }
    return match;
}//will be private

bool KIM_API_model::is_it_std_match(KIM_API_model& mdtst, KIM_IOline* IOlines, int nlns){
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

            if(mdtst[j].equiv(IOlines[i])) {
                match = true;
                break;
            }
        }



        if(!match) {
           cout << "* Info (KIM_API_model::is_it_std_match): The following descriptor file line may not match with " << mdtst.model.name << "'s descriptor file."<<endl;
            cout<<IOlines[i]<<endl;
            return match;
        }
    }

    return match;
}

bool KIM_API_model::is_it_match_noFlagCount(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns){
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

        if(strcmp(IOlines[i].type,"flag")==0){
            match=true;
        }
        for(int j=0;j<mdtst.model.size;j++){
            if(mdtst[j]== IOlines[i]) {
                match = true;
                break;
            }else if(strcmp(mdtst[j].type,"flag")==0){
                match = true;
                break;
            }else if(is_it_par(mdtst[j].name)){
                match = true;
                break;
            }
        }
        if(!match) {
            cout << "* Warning (KIM_API_model::is_it_match_noFlagCount): The following line in the Model descriptor file does not match."<<endl;
            cout<<IOlines[i]<<endl;
            return match;
        }
    }
    return match;
}//will be private

bool KIM_API_model::is_it_match(KIM_API_model &test,KIM_API_model & mdl){
    //preinit model from standard template kim file
   KIM_API_model stdmdl;

    char * inStandard_kim_str = standard_kim_str();
    if(!stdmdl.prestring_init(inStandard_kim_str)){
        cout<<" preinit of :"<<"standard.kim"<<" failed"<<endl;
        stdmdl.free();
        return false;
    }

    // test and mdl must be preinit.
    bool test2modelmatch= is_it_match(test,mdl.inlines,mdl.numlines);
    bool model2testmatch= is_it_match(mdl,test.inlines,test.numlines);

    bool test2modelmatch_noDC= is_it_match_noFlagCount(test,mdl.inlines,mdl.numlines);
    bool model2testmatch_noDC= is_it_match_noFlagCount(mdl,test.inlines,test.numlines);

    bool test2standardmatch = is_it_std_match(stdmdl,test.inlines,test.numlines);

    bool model2standardmatch = is_it_std_match(stdmdl,mdl.inlines,mdl.numlines);

    bool test2standardAtomsTypesMatch = do_AtomsTypes_match(test,stdmdl);
    bool model2standardAtomsTypesMatch = do_AtomsTypes_match(mdl,stdmdl);
    bool test2modelAtomsTypesMatch = do_AtomsTypes_match(test,mdl);
    bool AtomsTypesMatch=test2standardAtomsTypesMatch&&model2standardAtomsTypesMatch&&test2modelAtomsTypesMatch;

    stdmdl.free();

    bool NBC_methodsmatch = this->NBC_methods_match(test,mdl);
    NBC_methodsmatch=NBC_methodsmatch&&test.check_consistance_NBC_method();
    NBC_methodsmatch=NBC_methodsmatch&&mdl.check_consistance_NBC_method();
    bool process_fij_related = this->fij_related_things_match(test,mdl);
    bool units_match = Unit_Handling::do_unit_match(test.unit_h,mdl.unit_h);

    if(!test2standardmatch) cout<<"* Error (KIM_API_model::is_it_match): There are non-standard variables in Test descriptor file:"<<endl;
    if(!model2standardmatch) cout<<"* Error (KIM_API_model::is_it_match): There are non-standard variables in Model descriptor file:"<<endl;
    if(!test2standardAtomsTypesMatch) cout<<"* Error (KIM_API_model::is_it_match): There are non-standard AtomsTypes in Test descriptor file:"<<endl;
    if(!model2standardAtomsTypesMatch) cout<<"* Error (KIM_API_model::is_it_match):there are non-standard AtomsTypes in Model descriptor file:"<<endl;
    if(!test2modelAtomsTypesMatch) cout<<"* Error (KIM_API_model::is_it_match): Test-Model AtomsTypes do not match:"<<endl;
    if(!NBC_methodsmatch) cout<<"* Error (KIM_API_model::is_it_match): NBC methods do not match:"<<endl;
    if(!process_fij_related) cout<<
       "* Error (KIM_API_model::is_it_match): (virial,particleVirial,hessian,process_d1/2Edr) do not match:"<<endl;
    if(!units_match){
       cout<<"* Error (KIM_API_model::is_it_match): units do not match:"<<endl;
    }else{
        this->unit_h = mdl.unit_h;
    }

    do_flag_match(test,mdl);

    if (test2modelmatch && model2testmatch && test2standardmatch && process_fij_related &&
            model2standardmatch && AtomsTypesMatch && NBC_methodsmatch && units_match) return true;
    if (test2modelmatch_noDC && model2testmatch_noDC && test2standardmatch && process_fij_related &&
             model2standardmatch && AtomsTypesMatch && NBC_methodsmatch && units_match){
        return this->do_flag_match(test,mdl);
    }
    return false;
}

bool KIM_API_model::is_it_in_and_is_it_flag(KIM_API_model& mdl,char * name){
    int i = mdl.get_index(name);
    if (i<0) return false;
    if (strcmp(mdl[i].type,"flag")!=0) return false;
    return true;
}
bool KIM_API_model::is_it_in(KIM_API_model& mdl, char* name){
    int i = mdl.get_index(name);
    if (i<0) return false;
    return true;
}
bool KIM_API_model::do_flag_match(KIM_API_model& tst, KIM_API_model& mdl){
    // here the assumption : besides flag type , everything is a match

    // check flag for tst
    bool ZeroBasedLists_tst =is_it_in_and_is_it_flag(tst, "ZeroBasedLists");
    bool OneBasedLists_tst =is_it_in_and_is_it_flag(tst, "OneBasedLists");



    bool Neigh_IterAccess_tst=is_it_in_and_is_it_flag(tst, "Neigh_IterAccess");
    bool Neigh_LocaAccess_tst=is_it_in_and_is_it_flag(tst, "Neigh_LocaAccess");
    bool Neigh_BothAccess_tst=is_it_in_and_is_it_flag(tst, "Neigh_BothAccess");

    bool Neigh_CalcRij_tst=is_it_in_and_is_it_flag(tst, "Neigh_CalcRij");

    // check flag for mdl
    bool ZeroBasedLists_mdl =is_it_in_and_is_it_flag(mdl, "ZeroBasedLists");
    bool OneBasedLists_mdl =is_it_in_and_is_it_flag(mdl, "OneBasedLists");

    bool Neigh_IterAccess_mdl=is_it_in_and_is_it_flag(mdl, "Neigh_IterAccess");

    bool Neigh_LocaAccess_mdl=is_it_in_and_is_it_flag(mdl, "Neigh_LocaAccess");

    bool Neigh_BothAccess_mdl=is_it_in_and_is_it_flag(mdl, "Neigh_BothAccess");


    bool Neigh_CalcRij_mdl=is_it_in_and_is_it_flag(mdl, "Neigh_CalcRij");


    //logic for Zero or One base list handling
    if ((!ZeroBasedLists_tst && !OneBasedLists_tst)||(ZeroBasedLists_tst && OneBasedLists_tst) ) {
        cout<< "* Error (KIM_API_model::do_flag_match): Test descriptor file must have only ONE of ZeroBasedLists or OneBasedLists."<<endl;
        return false;
    }
     if ((!ZeroBasedLists_mdl && !OneBasedLists_mdl)||(ZeroBasedLists_mdl && OneBasedLists_mdl)) {
        cout<< "* Error (KIM_API_model::do_flag_match): Model descriptor file must have only ONE of ZeroBasedLists or OneBasedLists."<<endl;
        return false;
    }
    model_index_shift = 0;
    if (ZeroBasedLists_tst && OneBasedLists_mdl) model_index_shift = 1;
    if (OneBasedLists_tst && ZeroBasedLists_mdl) model_index_shift = -1;
    if (OneBasedLists_mdl) AUX_index_shift =1;
    int ind_LocaAccess_mdl = mdl.get_index("Neigh_LocaAccess");
    int ind_IterAcces_mdl = mdl.get_index("Neigh_IterAccess");


    //logic for checking Both/Loca/Iter
    // checking if test o.k. when model requires both

    if (Neigh_BothAccess_mdl){

        if(!(Neigh_BothAccess_tst || (Neigh_LocaAccess_tst && Neigh_IterAccess_tst))){
            cout<< "* Error (KIM_API_model::do_flag_match): Model descriptor file requres Neigh_BothAccess."<<endl;
            return false;
        }
        mdl.both_neigh_mode=true;
     //checking if test o.k. when model may work with loca or iter
     }else if (Neigh_LocaAccess_mdl && Neigh_IterAccess_mdl){

        if(!(Neigh_LocaAccess_tst || (Neigh_IterAccess_tst || Neigh_BothAccess_tst))){
            cout<< "* Error (KIM_API_model::do_flag_match): Model descriptor file requres IterAccess or LocaAccess."<<endl;
            return false;
        }
        if ((Neigh_LocaAccess_tst && Neigh_IterAccess_tst) || Neigh_BothAccess_tst) {
            if (ind_LocaAccess_mdl < ind_IterAcces_mdl) {
                mdl.locator_neigh_mode =true;
            }else {
                mdl.iterator_neigh_mode = true;
            }
        }else if (Neigh_LocaAccess_tst) {
            mdl.locator_neigh_mode=true;
        } else if(Neigh_IterAccess_tst){
            mdl.iterator_neigh_mode=true;
        }


     //checking if test o.k. with loca
     }else if(Neigh_LocaAccess_mdl){
         if(!(Neigh_LocaAccess_tst || Neigh_BothAccess_tst)) {
             cout<< "* Error (KIM_API_model::do_flag_match): Model descriptor file requres Neigh_LocaAccess."<<endl;
             return false;
         }

         mdl.locator_neigh_mode = true;
     //checking if test o.k. with iter
     }else if(Neigh_IterAccess_mdl){
         if(!(Neigh_IterAccess_tst || Neigh_BothAccess_tst)) {
             cout<< "* Error (KIM_API_model::do_flag_match): Model descriptor file requres Neigh_IterAccess."<<endl;
             return false;
         }

         mdl.iterator_neigh_mode = true;
     }

    //logic for Neigh_CalcRij
    if (Neigh_CalcRij_mdl && !Neigh_CalcRij_tst){
             cout<< "model .kim requres Neigh_CalcRij"<<endl;
             return false;
    }

    if(!(mdl.locator_neigh_mode||mdl.iterator_neigh_mode||mdl.both_neigh_mode)) return false;

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
            cout <<"* Error (KIM_API_model::do_AtomsTypes_match): The following symbol: "<<test.AtomsTypes[i].symbol<<" in ";
            cout<< test.model.name << " is not found in "<<mdl.model.name<<endl;
            return false;
        }
    }
    return true;
}

bool KIM_API_model::is_it_fixed_par(char* name){
     char tmpname[KIM_KEY_STRING_LENGTH]="";
     strncpy(&tmpname[0],name,strlen(name)+1);
     char * tmp = strtok(tmpname,"_");if(tmp == NULL) return false;
     if(strcmp(tmp,"PARAM")==0) {
         tmp = strtok(NULL,"_");if(tmp == NULL) return false;
         if(strcmp(tmp,"FIXED")==0) return true;
     }
     return false;
}
bool KIM_API_model::is_it_free_par(char* name){
     char tmpname[KIM_KEY_STRING_LENGTH]="";
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
        this->unit_h=test.unit_h;
        this->irrelevantVars2donotcompute(test,*this);

        strcpy(this->NBC_method_current, mdl.NBC_method_current);
        locator_neigh_mode=mdl.locator_neigh_mode;
        iterator_neigh_mode=mdl.iterator_neigh_mode;
        both_neigh_mode=mdl.both_neigh_mode;
        test.free(); mdl.free();
        char computestr [] = "compute";
        compute_index = get_index(computestr);
        get_neigh_index = get_index("get_neigh");
        this->fij_related_things_add_set_index();
        support_Rij=false;
        if (strcmp(NBC_method_current,"NEIGH_RVEC_F")==0) support_Rij=true;

        return true;
    }else{
        test.free(); mdl.free();
 cout<<"Do not match  " << modelname << " and "<< testname<<endl;
        return false;
    }

}

#ifndef KIM_DYNAMIC
extern "C"{
  #include "model_kim_str_include.h"
}

char * KIM_API_model::get_model_kim_str(char* modelname,int * kimerr){
   // void * model_lib_handle;
     //redirecting cout > kimlog
    *kimerr=KIM_STATUS_FAIL;
    char kimlog[2048] = KIM_DIR; strcat(kimlog,"kim.log");
    streambuf * psbuf, * backup;ofstream filekimlog;
    filekimlog.open(kimlog);
    backup = cout.rdbuf();psbuf = filekimlog.rdbuf();cout.rdbuf(psbuf);

    char * in_mdlstr=NULL;

    #include "model_kim_str_include.cpp"

    if (in_mdlstr == NULL){
       cout<<"* Error (KIM_API_model::get_model_kim_str): Unknown KIM Model name " << modelname << "." << endl;
        exit(367);
    }
     //redirecting back to > cout
    cout.rdbuf(backup); filekimlog.close();
    *kimerr= KIM_STATUS_OK;
    return in_mdlstr;
}

bool KIM_API_model::init(char* testname, char* modelname){

    //redirecting cout > kimlog
    char kimlog[2048] = KIM_DIR; strcat(kimlog,"kim.log");
    streambuf * psbuf, * backup;ofstream filekimlog;
    filekimlog.open(kimlog);
    backup = cout.rdbuf();psbuf = filekimlog.rdbuf();cout.rdbuf(psbuf);

    char * in_mdlstr=NULL;

#include "model_kim_str_include.cpp"

    if (in_mdlstr == NULL){
       cout<<"* Error (KIM_API_model::init): Unknown KIM Model name " << modelname << "." << endl;
        exit(367);
    }
    bool result_init= this->init_str_modelname(testname,in_mdlstr);
   //redirecting back to > cout
    cout.rdbuf(backup); filekimlog.close();

    return result_init;
}

#else

char * KIM_API_model::get_model_kim_str(char* modelname,int * kimerr){
    void * model_lib_handle;
    *kimerr= KIM_STATUS_FAIL;
    char model_slib_file[2048];
    char model_kim_str_name[2048];
    sprintf(model_slib_file,"%s%s/%s.so",KIM_DIR_MODELS,modelname,modelname);
    sprintf(model_kim_str_name,"%s_kim_str",modelname);

    //redirecting cout > kimlog
    char kimlog[2048] = KIM_DIR; strcat(kimlog,"kim.log");
    streambuf * psbuf, * backup;ofstream filekimlog;
    filekimlog.open(kimlog);
    backup = cout.rdbuf();psbuf = filekimlog.rdbuf();cout.rdbuf(psbuf);

    model_lib_handle = dlopen(model_slib_file,RTLD_NOW);
    if(!model_lib_handle) {
         cout<< "* Error (KIM_API_model::get_model_kim_str): Cannot find Model shared library file for Model name: ";
         cout<<modelname<<endl<<dlerror()<<endl;
         fprintf(stderr,"%s not found...\n",model_slib_file);

          //redirecting back to > cout
          cout.rdbuf(backup); filekimlog.close();
         return false;
    }

    typedef char* (*Model_kim_str)(void);
    Model_kim_str get_kim_str = (Model_kim_str)dlsym(model_lib_handle,model_kim_str_name);
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        cerr << "Cannot load symbol: " << dlsym_error <<endl;
        dlclose(model_lib_handle);

        //redirecting back to > cout
        cout.rdbuf(backup); filekimlog.close();

        return false;
    }

    char * in_mdlstr=NULL;

    in_mdlstr = (*get_kim_str)();

    if (in_mdlstr == NULL){
        cout<<"* Error (KIM_API_get_model_kim_str: Unknown KIM Model name " << modelname << "." << endl;
        exit(367);
    }


    dlclose(model_lib_handle);
   //redirecting back to > cout
    cout.rdbuf(backup); filekimlog.close();
    *kimerr= KIM_STATUS_OK;
    return in_mdlstr;
}


bool KIM_API_model::init(char* testname, char* modelname){

    char model_slib_file[2048];
    char model_kim_str_name[2048];
    sprintf(model_slib_file,"%s%s/%s.so",KIM_DIR_MODELS,modelname,modelname);
    sprintf(model_kim_str_name,"%s_kim_str",modelname);

    //redirecting cout > kimlog
    char kimlog[2048] = KIM_DIR; strcat(kimlog,"kim.log");
    streambuf * psbuf, * backup;ofstream filekimlog;
    filekimlog.open(kimlog);
    backup = cout.rdbuf();psbuf = filekimlog.rdbuf();cout.rdbuf(psbuf);

    model_lib_handle = dlopen(model_slib_file,RTLD_NOW);
    if(!model_lib_handle) {
         cout<< "* Error (KIM_API_model::init): Cannot find Model shared library file for Model name: ";
         cout<<modelname<<endl<<dlerror()<<endl;
         fprintf(stderr,"%s not found...\n",model_slib_file);

          //redirecting back to > cout
          cout.rdbuf(backup); filekimlog.close();
         return false;
    }

    typedef char* (*Model_kim_str)(void);
    Model_kim_str get_kim_str = (Model_kim_str)dlsym(model_lib_handle,model_kim_str_name);
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        cerr << "Cannot load symbol: " << dlsym_error <<endl;
        dlclose(model_lib_handle);

        //redirecting back to > cout
        cout.rdbuf(backup); filekimlog.close();

        return false;
    }

    char * in_mdlstr=NULL;

    in_mdlstr = (*get_kim_str)();

    if (in_mdlstr == NULL){
        cout<<"* Error (KIM_API_model_init): Unknown KIM Model name " << modelname << "." << endl;
        exit(367);
    }
    bool result_init= this->init_str_modelname(testname,in_mdlstr);

    dlclose(model_lib_handle);
   //redirecting back to > cout
    cout.rdbuf(backup); filekimlog.close();

    return result_init;
}
#endif

void KIM_API_model::fatal_error_print(){
    printf("* KIM FATAL ERROR: See kim.log file for details\n");
}


bool KIM_API_model::init_str_modelname(char* testname, char* inmdlstr){
    char testinputfile[2048] = KIM_DIR_TESTS;
    strcat(testinputfile,testname);strcat(testinputfile,"/");strcat(testinputfile,testname);
    strcat(testinputfile,".kim");



    //check test-model match and preinit test-model-API
    KIM_API_model test,mdl;
    //preinit test and model API object

    if(!mdl.prestring_init(inmdlstr)){
        cout<<"prestring_init  failed with error status: "<<this->get_status_msg(ErrorCode)<<endl;
        return false;
    }

    if(!test.preinit(testinputfile,testname)){
        cout<<"preinit  failed with error status: "<<this->get_status_msg(ErrorCode)<<endl;
        return false;
    }

    //check if they match
    if (is_it_match(test,mdl)){
        this->prestring_init(inmdlstr);
        this->unit_h=test.unit_h;
        this->irrelevantVars2donotcompute(test,*this);

        strcpy(this->NBC_method_current, mdl.NBC_method_current);
        locator_neigh_mode=mdl.locator_neigh_mode;
        iterator_neigh_mode=mdl.iterator_neigh_mode;
        both_neigh_mode=mdl.both_neigh_mode;
        test.free(); mdl.free();
        char computestr [] = "compute";
        compute_index = get_index(computestr);
        get_neigh_index = get_index("get_neigh");
        this->fij_related_things_add_set_index();
        support_Rij=false;
        if (strcmp(NBC_method_current,"NEIGH_RVEC_F")==0) support_Rij=true;



        return true;
    }else{

 cout<<"Do not match  " << mdl.model.name  << " and "<< testname <<endl;
       mdl.free();
       test.free();


        return false;
    }

}

bool KIM_API_model::preinit(char* modelname){
    //redirecting cout > kimlog
    char kimlog[2048] = KIM_DIR; strcat(kimlog,"kim.log");
    streambuf * psbuf, * backup;ofstream filekimlog;
    filekimlog.open(kimlog);
    backup = cout.rdbuf();psbuf = filekimlog.rdbuf();cout.rdbuf(psbuf);
    //preinit model
   #ifndef KIM_DYNAMIC
    //get string .kim for the model----------
     char * in_mdlstr=NULL;
    #include "model_kim_str_include.cpp"
     if (in_mdlstr == NULL){
        cout<<"* Error (KIM_API_model::preinit): Unknown KIM Model name " << modelname << "." << endl;
         //redirecting back to > cout
          cout.rdbuf(backup); filekimlog.close();
         return false;
    }
     //                         -------------
    #else

    char model_slib_file[2048];
    char model_kim_str_name[2048];
    sprintf(model_slib_file,"%s%s/%s.so",KIM_DIR_MODELS,modelname,modelname);
    sprintf(model_kim_str_name,"%s_kim_str",modelname);
     model_lib_handle = dlopen(model_slib_file,RTLD_NOW);
    if(!model_lib_handle) {
         cout<< "* Error (KIM_API_model::preinit): Cannot find Model shared library file for Model name: ";
         cout<<modelname<<endl<<dlerror()<<endl;

          //redirecting back to > cout
          cout.rdbuf(backup); filekimlog.close();
         return false;
    }

    typedef char* (*Model_kim_str)(void);
    Model_kim_str get_kim_str = (Model_kim_str)dlsym(model_lib_handle,model_kim_str_name);
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        cout << "* Error (KIM_API_model::preinit): Model descriptor file function not found in shared library for Model: " << modelname << "." << endl;
        dlclose(model_lib_handle);

        //redirecting back to > cout
        cout.rdbuf(backup); filekimlog.close();

        return false;
    }

    char * in_mdlstr=NULL;

    in_mdlstr = (*get_kim_str)();

    if (in_mdlstr == NULL){
        cout<< "* Error (KIM_API_model::preinit): Model descriptor string not found in shared library for Model: " << modelname << "." << endl;
        //redirecting back to > cout
        cout.rdbuf(backup); filekimlog.close();
        return false;
    }

 #endif
    bool result= this->prestring_init(in_mdlstr);
    //redirecting back to > cout
    cout.rdbuf(backup); filekimlog.close();
    return result;
}

bool KIM_API_model::init_str_testname(char * intststr,char * modelname){
    return string_init(intststr, modelname);
}

bool KIM_API_model::string_init(char* in_tststr, char* modelname){
    //char modelinputfile[2048] = KIM_DIR_MODELS;
    //strcat(modelinputfile,modelname);strcat(modelinputfile,"/");strcat(modelinputfile,modelname);
    //strcat(modelinputfile,".kim");

    //redirecting cout > kimlog
    char kimlog[2048] = KIM_DIR; strcat(kimlog,"kim.log");
    streambuf * psbuf, * backup;ofstream filekimlog;
    filekimlog.open(kimlog);
    backup = cout.rdbuf();psbuf = filekimlog.rdbuf();cout.rdbuf(psbuf);

    //check test-model match and preinit test-model-API
    KIM_API_model test,mdl;
#ifndef KIM_DYNAMIC
    //get string .kim for the model----------
     char * in_mdlstr=NULL;
#include "model_kim_str_include.cpp"

    if (in_mdlstr == NULL){
        cout<<"* Error (KIM_API_model::string_init): Unknown KIM Model name " << modelname << "." << endl;
        exit(368);
    }
     //                         -------------
 #else

    char model_slib_file[2048];
    char model_kim_str_name[2048];
    sprintf(model_slib_file,"%s%s/%s.so",KIM_DIR_MODELS,modelname,modelname);
    sprintf(model_kim_str_name,"%s_kim_str",modelname);
     model_lib_handle = dlopen(model_slib_file,RTLD_NOW);
    if(!model_lib_handle) {
         cout<< "* Error (KIM_API_model::string_init): Cannot find Model shared library file for Model name: ";
         cout<<modelname<<endl<<dlerror()<<endl;
         fprintf(stderr,"%s not found..\n",model_slib_file);

          //redirecting back to > cout
          cout.rdbuf(backup); filekimlog.close();
         return false;
    }

    typedef char* (*Model_kim_str)(void);
    Model_kim_str get_kim_str = (Model_kim_str)dlsym(model_lib_handle,model_kim_str_name);
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        cerr << "* Error (KIM_API_model_string_init): Model descriptor file function not found in shared library for Model: " << modelname << "." << endl;
        dlclose(model_lib_handle);

        //redirecting back to > cout
        cout.rdbuf(backup); filekimlog.close();

        return false;
    }

    char * in_mdlstr=NULL;

    in_mdlstr = (*get_kim_str)();

    if (in_mdlstr == NULL){
        cout<< "* Error (KIM_API_model::string_init): Model descriptor string not found in shared library for Model: " << modelname << "." << endl;
        exit(367);
    }

 #endif
//preinit test and model API object
    if(!test.prestring_init(in_tststr))
        cout<<"test.prestring_init failed with error status:"<<this->get_status_msg(test.ErrorCode)<<endl;

    if(!mdl.prestring_init(in_mdlstr))
        cout<<"mdl.prestring_init failed with error status:"<<this->get_status_msg(mdl.ErrorCode)<<endl;

    //check if they match
    if (is_it_match(test,mdl)){
        this->prestring_init(in_mdlstr);
        this->unit_h=test.unit_h;
        this->irrelevantVars2donotcompute(test,*this);

        strcpy(this->NBC_method_current, mdl.NBC_method_current);
        locator_neigh_mode=mdl.locator_neigh_mode;
        iterator_neigh_mode=mdl.iterator_neigh_mode;
        both_neigh_mode=mdl.both_neigh_mode;
        test.free(); mdl.free();
        char computestr [] = "compute";
        compute_index = get_index(computestr);
        get_neigh_index = get_index("get_neigh");
        this->fij_related_things_add_set_index();
        support_Rij=false;
        if (strcmp(NBC_method_current,"NEIGH_RVEC_F")==0) support_Rij=true;

#ifdef KIM_DYNAMIC
       dlclose(model_lib_handle);
#endif
        //redirecting back to > cout

        cout.rdbuf(backup); filekimlog.close();

        return true;
    }else{
        mdl.free();
 cout<<"Do not match  " << modelname << " and "<< test.model.name <<endl;
       test.free();

#ifdef KIM_DYNAMIC
       dlclose(model_lib_handle);
#endif
       //redirecting back to > cout
        cout.rdbuf(backup); filekimlog.close();

        return false;
    }
}
bool KIM_API_model::model_reinit(){
   int reinit_ind = get_index("reinit");
   if (reinit_ind < 0) return false;

   KIM_API_model *pkim = this;
   typedef void (*Model_Reinit)(void *);//prototype for model_reinit
   Model_Reinit mdl_reinit = (Model_Reinit)(*this)[reinit_ind].data;
   if (mdl_reinit == NULL) return false;
   (*mdl_reinit)(&pkim) ;
   return true;
}

#ifndef KIM_DYNAMIC
extern "C" {
#include "model_init_include.h"
}
bool KIM_API_model::model_init(){
    char modelname[KIM_KEY_STRING_LENGTH]="";
    KIM_API_model * kim;
    void ** pkim;
    strcpy(modelname,this->model.name);
    kim=this;
    pkim =(void**) &kim;

    //redirecting cout > kimlog
    char kimlog[2048] = KIM_DIR; strcat(kimlog,"kim.log");
    streambuf * psbuf, * backup;ofstream filekimlog;
    filekimlog.open(kimlog,ofstream::app);
    backup = cout.rdbuf();psbuf = filekimlog.rdbuf();cout.rdbuf(psbuf);

cout<< "* Info: KIM_API_model::model_init: call statically linked initialize routine for::"<<modelname<<endl;
    //redirecting back to > cout
    cout.rdbuf(backup); filekimlog.close();

#include "model_init_include.cpp"
#include "Unit_Handling.h"

    //redirecting cout > kimlog
    filekimlog.open(kimlog,ofstream::app);
    backup = cout.rdbuf();psbuf = filekimlog.rdbuf();cout.rdbuf(psbuf);

    cout<< "* Info: KIM_API_model::model_init: model initiliser failed for ";
    cout<<modelname<<endl;

     //redirecting back to > cout
    cout.rdbuf(backup); filekimlog.close();

    return false;
}
#else
bool KIM_API_model::model_init(){
    char modelname[KIM_KEY_STRING_LENGTH]="";
    KIM_API_model * kim;
    void ** pkim;
    char model_slib_file[2048];
    char model_init_routine_name[2048];
    strcpy(modelname,this->model.name);
    kim=this;
    pkim =(void**) &kim;
    sprintf(model_slib_file,"%s%s/%s.so",KIM_DIR_MODELS,modelname,modelname);

//redirecting cout > kimlog
    char kimlog[2048] = KIM_DIR; strcat(kimlog,"kim.log");
    streambuf * psbuf, * backup;ofstream filekimlog;
    filekimlog.open(kimlog, ofstream::app);
    backup = cout.rdbuf();psbuf = filekimlog.rdbuf();cout.rdbuf(psbuf);

cout<<"* Info: KIM_API_model::model_init: call dynamically linked initialize routine for:"<<modelname<<endl;
cout<<"               from the shared library:"<<model_slib_file<<endl;
    sprintf(model_init_routine_name,"%s_init_",modelname);
    for(int i=0;i<(int)strlen(model_init_routine_name);i++){
         model_init_routine_name[i]=tolower(model_init_routine_name[i]);
    }

    model_lib_handle = dlopen(model_slib_file,RTLD_NOW);
    if(!model_lib_handle) {
         cout<< "* Info: KIM_API_model::model_init: model initiliser failed for ";
         cout<<modelname<<endl<<dlerror()<<endl;
         fprintf(stderr,"%s not found...\n",model_slib_file);

          //redirecting back to > cout
          cout.rdbuf(backup); filekimlog.close();

         return false;
    }

    typedef void (*Model_Init)(void **);//prototype for model_init
    Model_Init mdl_init = (Model_Init)dlsym(model_lib_handle,model_init_routine_name);
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        cerr << "* Error (KIM_API_model::model_init): Cannot load symbol: " << dlsym_error <<endl;
        dlclose(model_lib_handle);

        //redirecting back to > cout
        cout.rdbuf(backup); filekimlog.close();

        return false;
    }

    //redirecting back to > cout
    cout.rdbuf(backup); filekimlog.close();


    (*mdl_init)(pkim);

    return true;
}
#endif

void KIM_API_model::model_destroy(int *error){
  typedef void (*Model_Destroy)(void *,int *);//prototype for model_destroy
  *error =KIM_STATUS_FAIL;
  Model_Destroy mdl_destroy = (Model_Destroy) (*this)["destroy"].data;
  //call model_destroy
  KIM_API_model *pkim = this;

  if (mdl_destroy != NULL) {
      (*mdl_destroy)((void *)&pkim, error);
  }

#ifdef KIM_DYNAMIC
  dlclose(model_lib_handle);
#endif
 *error =KIM_STATUS_OK;
}
void KIM_API_model::model_compute(int *error){
  // set model_compute pointer
  typedef void (*Model_Compute)(void *,int *);//prototype for model_compute
  *error = KIM_STATUS_FAIL;
  Model_Compute mdl_compute = (Model_Compute) (*this)[compute_index].data;
  if (mdl_compute == NULL) return;
  *error = KIM_STATUS_OK;

  //initialize virials if needed

  if (process_dEdr_ind >=0 || process_d2Edr2_ind >= 0) KIM_AUX::Process_DE::init2zero(this,error);
  if(*error != KIM_STATUS_OK) return;

  //call model_compute
  KIM_API_model *pkim = this;
  (*mdl_compute)((void *)&pkim, error);
}

int KIM_API_model::get_neigh(int mode, int request, int *atom,
        int *numnei, int** nei1atom, double** Rij){
    int locrequest=request;
    int locmode = mode;

    if(mode!=0 && mode!=1) return KIM_STATUS_NEIGH_INVALID_MODE;
    if(this == NULL) return KIM_STATUS_API_OBJECT_INVALID;
    typedef int (*Get_Neigh)(void **, int *, int *, int *, int *, int **,double **);

    if (get_neigh_index < 0) return KIM_STATUS_API_OBJECT_INVALID;
    Get_Neigh get_neigh = (Get_Neigh)(*this)[get_neigh_index].data;
    KIM_API_model *pkim = this;

    if (model_index_shift==0) {
        if (mode==0 && request == 0) {
            return (*get_neigh)((void **)&pkim,&locmode, &locrequest, atom, numnei, nei1atom, Rij ) ;
        }else{

            int erkey = (*get_neigh)((void **)&pkim,&locmode, &locrequest, atom, numnei, nei1atom, Rij );
            if (erkey <= 0) return erkey; // return with error from  supplied by test get_neigh
            if(*numnei > KIM_API_MAX_NEIGHBORS) {
                cout<<endl<< "* Error (KIM_API_model::get_neigh): numnei > MAX_NEIGHBORS : ";
                cout<<" "<<*numnei <<" > "<< KIM_API_MAX_NEIGHBORS<<endl;
                return KIM_STATUS_NEIGH_TOO_MANY_NEIGHBORS;
            }
            return erkey;
        }
    }else if (model_index_shift == 1 || model_index_shift == -1){

        int req=request;
        if (mode ==1) req = request - model_index_shift;
        int at = *atom;

        if (mode==0 && request == 0) {
            return (*get_neigh)((void **)&pkim,&locmode, &req, &at, numnei, nei1atom, Rij );
        }else{
            int erkey = (*get_neigh)((void **)&pkim,&locmode, &req, &at, numnei, nei1atom, Rij );
            if (erkey <= 0) return erkey; // return with error from  supplied by test get_neigh
            if(*numnei > KIM_API_MAX_NEIGHBORS) {
                cout<<endl<< "* Error (KIM_API_model::get_neigh): numnei > MAX_NEIGHBORS : ";
                cout<<" "<<*numnei <<" > "<< KIM_API_MAX_NEIGHBORS<<endl;
                return KIM_STATUS_NEIGH_TOO_MANY_NEIGHBORS;
            }
            if (erkey == 1){
                *atom= at + model_index_shift;
                for (int i = 0; i<(*numnei);i++){
                    neiOfAnAtom_half[i] = (*nei1atom)[i] + model_index_shift;
                }
                *nei1atom = &(neiOfAnAtom_half[0]);
            }
            return erkey;
        }
    }else{
        cout<<endl<< "* Error (KIM_API_model::get_neigh): wrong base convert key,model_index_shift =";
        cout<< model_index_shift <<"  (must be 0,1 or -1)"<<endl;
        return KIM_STATUS_API_OBJECT_INVALID;
    }
}

void KIM_API_model::irrelevantVars2donotcompute(KIM_API_model & test, KIM_API_model & mdl){
    if(! is_it_match_noFlagCount(test,mdl.inlines,mdl.numlines)) {
        cout<<"* Error (KIM_API_model::irrelevantVars2donotcompute): Test and Model descriptor files are incompatible (do not match)."<<endl;
        KIM_API_model::fatal_error_print();
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

void KIM_API_model::allocate(KIM_API_model * mdl, int natoms, int ntypes, int * error){
    // in process
    if ( mdl->model.data == NULL) {
        cout<<"* Error (KIM_API_model::allocate): KIM API object not initialized with KIM_API_init()."<<endl;
        *error = KIM_STATUS_FAIL;
        return;
    }
    for(int i=0; i<mdl->model.size;i++){
        intptr_t rank = (intptr_t)mdl->inlines[i].get_rank();
        int *shape = mdl->inlines[i].get_shape(natoms,ntypes);
        int calculate = (*mdl)[i].flag->calculate;
        bool isitparam = mdl->is_it_par((*mdl)[i].name);
        intptr_t sz=0;
        int c=1;
        if (shape!=NULL) {
            for(int k=0;k<rank;k++) c=c*shape[k];
            sz=c;
        }else{
            sz = 1;
            if (strcmp((*mdl)[i].type,"pointer")==0 || strcmp((*mdl)[i].type,"method")==0) sz=0;
            if (strcmp((*mdl)[i].type,"flag")==0 ) sz=0;
        }
        if((mdl->inlines[i].isitoptional() && (calculate == 0)) || isitparam) {
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
    *error=KIM_STATUS_OK;
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
    stream<<a.unit_h;
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

char * KIM_API_model::get_partcl_types(int* nATypes, int* error){
    *error=KIM_STATUS_FAIL;
    if (nAtomsTypes==0){
        *nATypes = 0;
        *error =KIM_STATUS_OK; //success but no atoms type specified
        return NULL;
    }
    if (nAtomsTypes < 0){
        *error =KIM_STATUS_FAIL;//was internal error in init nAtomsTypes
        return NULL;
    }
    *nATypes = nAtomsTypes;
    char * listatypes=(char *)malloc(nAtomsTypes*KIM_KEY_STRING_LENGTH);

    for (int i=0;i<nAtomsTypes*KIM_KEY_STRING_LENGTH;i++) listatypes[i] = '\0';
    for(int i=0; i<nAtomsTypes; i++){
        strncpy(listatypes + i*KIM_KEY_STRING_LENGTH, AtomsTypes[i].symbol,strlen( AtomsTypes[i].symbol)+1);
    }
    *error =KIM_STATUS_OK;//success
    return  listatypes;
}
char * KIM_API_model::get_NBC_method(int* error){
    *error=KIM_STATUS_FAIL;
    if(strcmp(this->NBC_method_current,"none")==0) {
        // no NBC methods are specified
        return NULL;
    }
    char *method = (char *)malloc(KIM_KEY_STRING_LENGTH);
    for (int i=0;i<KIM_KEY_STRING_LENGTH;i++) method[i] = '\0';
    strcpy(method,this->NBC_method_current);
    *error=KIM_STATUS_OK; //success
    return method;
}

static void c_free(void *p){
    free(p);
}
bool KIM_API_model::requiresFullNeighbors(){
    int kimerr;
    char * method = NULL;
    method = (char *) get_NBC_method(&kimerr);

    if(kimerr!=1){
        if (method!=NULL) c_free((void *)method);
        return false;
    }

    bool answer = false;
    if (strcmp(method,"NEIGH_PURE_F")==0) answer = true;
    if (strcmp(method,"NEIGH_RVEC_F")==0) answer = true;
    if (strcmp(method,"MI_OPBC_F")==0) answer = true;
    if (method!=NULL) c_free((void *)method);
    return answer;
}

bool KIM_API_model::is_half_neighbors(){
    int kimerr;
    char * method = NULL;
    method = (char *) get_NBC_method(&kimerr);

    if(kimerr!=1){
        if (method!=NULL) c_free((void *)method);
        return true;
    }

    bool answer = true;
    if (strcmp(method,"NEIGH_PURE_F")==0) answer = false;
    if (strcmp(method,"NEIGH_RVEC_F")==0) answer = false;
    if (strcmp(method,"MI_OPBC_F")==0) answer = false;
    if (method!=NULL) c_free((void *)method);
    return answer;
}

char * KIM_API_model::get_params(int* nVpar, int* error){
    int count;
    count=0;
    *error=KIM_STATUS_FAIL;
    char * listvpar;
    for(int i=0; i< model.size; i++){
        if(is_it_par((*this)[i].name)) count++;
    }
    if (count==0) {
        *nVpar=0;
        *error=KIM_STATUS_OK;  //success but no parameters
        return NULL;
    }
    *nVpar=count;
    listvpar = (char *)malloc(KIM_KEY_STRING_LENGTH * count);
    for (int i=0;i<count*KIM_KEY_STRING_LENGTH;i++) listvpar[i] = '\0';
    count=0;
    for (int i=0;i<model.size;i++){
         if(is_it_par((*this)[i].name)){
             strncpy(listvpar+count*KIM_KEY_STRING_LENGTH, (*this)[i].name, strlen((*this)[i].name) +1 );
             count++;
         }
    }
    *error =KIM_STATUS_OK;//success
    return  listvpar;
}
char * KIM_API_model::get_free_params(int* nVpar, int* error){
    int count;
    count=0;
    *error=KIM_STATUS_FAIL;
    char * listvpar;
    for(int i=0; i< model.size; i++){
        if(is_it_free_par((*this)[i].name)) count++;
    }
    if (count==0) {
        *nVpar=0;
        *error=KIM_STATUS_OK;  //success but no parameters
        return NULL;
    }
    *nVpar=count;
    //listvpar= new char[KIM_KEY_STRING_LENGTH * count];
    listvpar = (char *)malloc(KIM_KEY_STRING_LENGTH * count);
    for (int i=0;i<count*KIM_KEY_STRING_LENGTH;i++) listvpar[i] = '\0';
    count=0;
    for (int i=0;i<model.size;i++){
         if(is_it_free_par((*this)[i].name)){
             strncpy(listvpar+count*KIM_KEY_STRING_LENGTH, (*this)[i].name, strlen((*this)[i].name) +1 );
             count++;
         }
    }
    *error =KIM_STATUS_OK;//success
    return  listvpar;
}
char * KIM_API_model::get_fixed_params(int* nVpar, int* error){
    int count;
    count=0;
    *error = KIM_STATUS_FAIL;
    char * listvpar;
    for(int i=0; i< model.size; i++){
        if(is_it_fixed_par((*this)[i].name)) count++;
    }
    if (count==0) {
        *nVpar=0;
        *error=KIM_STATUS_OK;  //success but no parameters
        return NULL;
    }
    *nVpar=count;
    //listvpar= new char[KIM_KEY_STRING_LENGTH * count];
    listvpar = (char *)malloc(KIM_KEY_STRING_LENGTH * count);
    for (int i=0;i<count*KIM_KEY_STRING_LENGTH;i++) listvpar[i] = '\0';
    count=0;
    for (int i=0;i<model.size;i++){
         if(is_it_fixed_par((*this)[i].name)){
             strncpy(listvpar+count*KIM_KEY_STRING_LENGTH, (*this)[i].name, strlen((*this)[i].name) +1 );
             count++;
         }
    }
    *error =KIM_STATUS_OK;//success
    return  listvpar;
}
int  KIM_API_model::get_neigh_mode(int*kimerr){
    *kimerr=KIM_STATUS_OK;

    if(locator_neigh_mode && !iterator_neigh_mode && !both_neigh_mode){
        return 2;
    }else if(!locator_neigh_mode && iterator_neigh_mode && !both_neigh_mode){
        return 1;
    }else if(!locator_neigh_mode && !iterator_neigh_mode && both_neigh_mode){
        return 3;
    }else if(locator_neigh_mode && iterator_neigh_mode && !both_neigh_mode){
        return 1;
    }else{
        *kimerr = KIM_STATUS_FAIL;
        return -1;
    }
}

int KIM_API_model::get_partcl_type_code(char* atom, int * error){
    *error =KIM_STATUS_FAIL;
    if (atom == NULL)  {
        *error = KIM_STATUS_PARTICLE_TYPES_UNDEFINED;
        return KIM_STATUS_PARTICLE_TYPES_UNDEFINED; //no atom symbol provided
    }
    Atom_Map key, *res=NULL;
    strcpy(key.symbol,atom);
    res = (Atom_Map *)bsearch((void *)&key,AtomsTypes,nAtomsTypes,sizeof(Atom_Map),&(Atom_Map::comparator));
    if (res == NULL) {
        *error = KIM_STATUS_PARTICLE_INVALID_TYPE;
        return  KIM_STATUS_PARTICLE_INVALID_TYPE; //did not find atom symbol among atom types
    }
    *error=KIM_STATUS_OK;
    return res->code;
}

bool KIM_API_model::NBC_methods_match(KIM_API_model& test, KIM_API_model& mdl){



    bool NBC_method_mdl[number_NBC_methods];
    bool NBC_method_test[number_NBC_methods];
    for (int i=0;i<number_NBC_methods; i++){
        NBC_method_mdl[i] = is_it_in_and_is_it_flag(mdl,NBC_methods[i]);
        NBC_method_test[i] = is_it_in_and_is_it_flag(test,NBC_methods[i]);
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
        cout<<"* Error (KIM_API_model::check_consistance_NBC_method):"<<NBC_method_current
                <<" is unknown."<<endl;
        return false;
    }
    for (int j=0;j<nnarg_NBC[i]; j++){
        if (get_index(arg_NBC_methods[i][j]) == -1){
            cout<<"* Error (KIM_API_model::check_consistance_NBC_method): Argument "<< arg_NBC_methods[i][j];
            cout<<" required for NBC method " << NBC_method_current;
            cout<<" is not in KIM API object."<<endl;
            return false;
        }
    }
    return true;
}
char * KIM_API_model::get_status_msg(int status_code) {
    int mincode=-23,maxcode=3,offset=23;

    char KIM_STATUS_MSG[][KIM_KEY_STRING_LENGTH]=
   {
    {"base units: are not supported or not the same phys.dimensions"},
    {" unsupported Unit_time  "},
    {" unsupported Unit_temperature  "},
    {" unsupported Unit_charge  "},
    {" unsupported Unit_energy  "},
    {" unsupported Unit_length  "},
    {"Unit_Handling must be \"flexible\" or \"fixed\" "},
    {"group argument must be 1 or 0(in KIM_API...multiple routine)"},//
    { "numargs is not divisiable by 4(in KIM_API...multiple routine)"},
    { "wrong optional arguments (in a kim_api_...multiple_f routine)"},
    { "numargs is not divisible by 2 (in KIM_API...multiple routine)"},
    { "numargs is not divisiable by 3(in KIM_API...multiple routine)"},
    { "invalid value for `request' provided"},
    { "get_neigh method in KIM API object is not set(NULL value)"},
    { "number of neighbors of an atom exceeds KIM_API_MAX_NEIGHBORS"},
    { "invalid KIM API object"},
    { "negative index in shape"},
    { "invalid mode value"},
    { "no atom/particle types have been specified by the Test or Model"},
    { "provided rank does not match KIM API argument rank"},
    { "invalid atom id requested (request out of range)"},
    { "symbol is not among supported atom symbols"},
    { "argument name provided is not in KIM API object"},
    { "unsuccessful completion"},
    { "successful completion"},
    { "iterator has been incremented past end of list"},
    { "iterator has been successfully initialized"}};

    if (status_code < mincode || status_code > maxcode) {
        char * retstr = (char *)malloc(KIM_KEY_STRING_LENGTH);
        strcpy(retstr,"the error code is not among KIM_STATUS codes");
        return retstr;
    }else{
        int ind = offset + status_code;
        char * retstr = (char *)malloc(KIM_KEY_STRING_LENGTH);
        for (int i=0;i<KIM_KEY_STRING_LENGTH;i++) retstr[i]='\0';
        strcpy(retstr,&(KIM_STATUS_MSG[ind][0]));
        return retstr;
    }

}

int KIM_API_model::report_error(int ln,char * fl,char * usermsg,int ier){
    if(ier <= 0){
        char * kimstatus =get_status_msg(ier);
        cout<<"* Error: at line "<<ln<<" in "<<fl<< endl<<"\tMessage: "<<usermsg<<endl;
        cout<<"\tKIM_STATUS_MSG: "<<kimstatus<<endl;
        c_free((void *) kimstatus);
        return KIM_STATUS_FAIL;
    }
    return KIM_STATUS_OK;
}

int KIM_API_model::get_model_index_shift(){
    return this->model_index_shift;
}

void KIM_API_model::set_model_buffer(void* o, int* ier){
    *ier = KIM_STATUS_OK;
    model_buffer = o;
}
void * KIM_API_model::get_model_buffer(int* ier){
    *ier = KIM_STATUS_FAIL;
    if (model_buffer == NULL) return NULL;
    *ier = KIM_STATUS_OK;
    return model_buffer;
}
void KIM_API_model::set_test_buffer(void* o, int* ier){
    *ier = KIM_STATUS_OK;
    test_buffer = o;
}

void * KIM_API_model::get_test_buffer(int* ier){
    *ier = KIM_STATUS_FAIL;
    if (test_buffer == NULL) return NULL;
    *ier = KIM_STATUS_OK;
    return test_buffer;
}


bool KIM_API_model::fij_related_things_match(KIM_API_model& test, KIM_API_model& mdl){

    bool tst_process_dEdr = is_it_in(test,"process_dEdr");
    bool tst_process_d2Edr2 = is_it_in(test,"process_d2Edr2");
    bool tst_virial_required  = is_it_in(test,"virial");
    bool tst_particleVirial_required = is_it_in(test,"particleVirial");
    bool tst_hessian_required =     is_it_in(test,"hessian");

    bool mdl_process_dEdr = is_it_in(mdl,"process_dEdr");
    bool mdl_process_d2Edr2 = is_it_in(mdl,"process_d2Edr2");
    bool mdl_virial  = is_it_in(mdl,"virial");
    bool mdl_particleVirial = is_it_in(mdl,"particleVirial");
    bool mdl_hessian =     is_it_in(mdl,"hessian");

    bool virial_comp_possible = mdl_virial || mdl_process_dEdr;
    bool particleVirial_comp_possible = mdl_particleVirial || mdl_process_dEdr;
    bool hessian_comp_possible =  mdl_hessian || (mdl_process_dEdr && mdl_process_d2Edr2);

    //do test & model match?
    bool match = true;
    if (tst_virial_required ) if( !virial_comp_possible) match=false;
    if (tst_particleVirial_required ) if( !particleVirial_comp_possible) match=false;
    if (tst_hessian_required ) if( !hessian_comp_possible) match=false;
    if (tst_process_dEdr) if(!mdl_process_dEdr) match=false;
    if (tst_process_d2Edr2) if(!mdl_process_d2Edr2) match=false;

    // the match is set, now set flaggs

    if (tst_virial_required ) if (!mdl_virial) virial_need2add = true;

    if (tst_particleVirial_required ) if (!mdl_particleVirial) particleVirial_need2add = true;

    if (tst_hessian_required ) if (!mdl_hessian) hessian_need2add = true;

    return match;

}

void KIM_API_model::add_element(char* instring){
        KIM_IOline inln;


        //open string as stream from char *
        string in_strstream=instring;
        stringstream myfile (in_strstream, stringstream::in|stringstream::out);
        if(!myfile){
            cout<<"* Error (KIM_API_model::add_element): can not access input string."<<endl;
            KIM_API_model::fatal_error_print();
            exit (329);
        }

        myfile.seekp(stringstream::beg);//set to the begining
        myfile >> inln;
        if(inln.goodformat) {
            this->inlines[numlines]=inln;
        }else{
            cout<<"* Error (KIM_API_model::add_element): bad format input string."<<endl;
            KIM_API_model::fatal_error_print();
            exit (330);
        }

        int *shape=NULL;

        KIMBaseElement *el = new KIMBaseElement ;
        int rank=inlines[numlines].get_rank();
        shape =inlines[numlines].get_shape();
        char * name =& (inlines[numlines].name[0]);
        char * type =& (inlines[numlines].type[0]);

        el->init(name,type,0,rank,shape); //preinit element with zero size
        strncpy(el->unit->dim,inlines[numlines].dim,strlen(inlines[numlines].dim)+1);

        el->flag->calculate = 1;
        el->flag->peratom = 1;//per something else
        if(inlines[numlines].isitperatom()) el->flag->peratom = 0; //per atom
        KIMBaseElement **pel =(KIMBaseElement**) model.data;
        pel[(int)model.size] =  el;
        delete [] shape;

        numlines ++;
        model.size++;
}

void KIM_API_model::fij_related_things_add_set_index(){
    //add part
    if(virial_need2add){
        char instr[512] = "virial            real*8       pressure     ";
        strcat(instr,"    [6]           # automatically generated");
        this->add_element(instr);
    }
    if(particleVirial_need2add){
        char instr[512] = "particleVirial            real*8       pressure     ";
        strcat(instr,"    [numberOfParticles,6]           # automatically generated");
        this->add_element(instr);
    }
    if(hessian_need2add){
        char instr[512] = "hessian            real*8       pressure     ";
        strcat(instr,"    [numberOfParticles,numberOfParticles,3,3]     # automatically generated");
        this->add_element(instr);
    }


    //get index
    this->virial_ind = get_index("virial");
    this->particleVirial_ind = get_index("particleVirial");
    this->hessian_ind =get_index("hessian");
    this->process_dEdr_ind =get_index("process_dEdr");
    this->process_d2Edr2_ind =get_index("process_d2Edr2");

    if (virial_need2add || particleVirial_need2add || hessian_need2add) (*this)[process_dEdr_ind].flag->calculate=1;
    if (hessian_need2add) (*this)[process_d2Edr2_ind].flag->calculate=1;
}
void KIM_API_model::process_dEdr(KIM_API_model** ppkim, double* dE, double* r,
        double** dx,int *i, int *j, int* ier){
    KIM_API_model * pkim= *ppkim;
    typedef void (*Process_d1Edr)(KIM_API_model **, double *, double *, double **,int *,int *, int *);

    Process_d1Edr process = (Process_d1Edr) (*pkim)[pkim->process_dEdr_ind].data;
    int process_flag =0;
    process_flag = (*pkim)[pkim->process_dEdr_ind].flag->calculate;

    if (process != NULL && process_flag == 1 && pkim->model_index_shift == 0) {
        (*process)(ppkim,dE,r,dx,i,j,ier);
     }else if (process != NULL && process_flag == 1){
        int i2send = *i-pkim->model_index_shift;
        int j2send = *j-pkim->model_index_shift;
        (*process)(ppkim,dE,r,dx,&i2send,&j2send,ier);
    }else if (process_flag == 1 && pkim->AUX_index_shift == 0){
        KIM_AUX::Process_DE::process_dEdr(ppkim,dE,r,dx,i,j,ier);
    } else if(process_flag == 1){
        int i2send = *i-1;
        int j2send = *j-1;
        KIM_AUX::Process_DE::process_dEdr(ppkim,dE,r,dx,&i2send,&j2send,ier);
    }
}

void KIM_API_model::process_d2Edr2(KIM_API_model **ppkim,double *de,double **r,double ** pdx,int **i,int **j,int *ier){
    KIM_API_model * pkim= *ppkim;
    typedef void (*Process_d2Edr)(KIM_API_model **, double *, double **, double **,int **,int **, int *);

    Process_d2Edr process = (Process_d2Edr) (*pkim)[pkim->process_d2Edr2_ind].data;
    int process_flag =0;
    process_flag = (*pkim)[pkim->process_d2Edr2_ind].flag->calculate;

    if (process != NULL && process_flag == 1 && pkim->model_index_shift == 0) {
        (*process)(ppkim,de,r,pdx,i,j,ier);
    }else if (process != NULL && process_flag == 1) {
        int k=pkim->model_index_shift;
        int i2send[2];   i2send[0]=(*i)[0]-k; i2send[1]=(*i)[1]-k;
        int j2send[2];   j2send[0]=(*j)[0]-k; j2send[1]=(*j)[1]-k;
        int *pi = &i2send[0];
        int *pj = &j2send[0];
        (*process)(ppkim,de,r,pdx,&pi,&pj,ier);
    } else if(process_flag == 1 && pkim->AUX_index_shift == 0){
        KIM_AUX::Process_DE::process_d2Edr2(ppkim,de,r,pdx,i,j,ier);
    }else if(process_flag == 1 ){
        int i2send[2];   i2send[0]=(*i)[0]-1; i2send[1]=(*i)[1]-1;
        int j2send[2];   j2send[0]=(*j)[0]-1; j2send[1]=(*j)[1]-1;
        int *pi = &i2send[0];
        int *pj = &j2send[0];
        KIM_AUX::Process_DE::process_d2Edr2(ppkim,de,r,pdx,&pi,&pj,ier);
    }
}


//related to Unit_Handling
double KIM_API_model::get_scale_conversion( char *u_from,char *u_to, int *error){
    return Unit_Handling::get_scale_conversion(u_from,u_to,error);
}
int KIM_API_model::get_unit_handling(int *error){
    return unit_h.get_unit_handling(error);
}
char * KIM_API_model::get_unit_length(int *error){
    return unit_h.get_unit_length(error);
}
char * KIM_API_model::get_unit_energy(int *error){
    return unit_h.get_unit_energy(error);
}
char * KIM_API_model::get_unit_charge(int *error){
    return unit_h.get_unit_charge(error);
}
char * KIM_API_model::get_unit_temperature(int *error){
    return unit_h.get_unit_temperature(error);
}
char * KIM_API_model::get_unit_time(int *error){
    return unit_h.get_unit_time(error);
}

double KIM_API_model::convert_to_act_unit(
   char* length, char* energy, char* charge, char* temperature, char* time,
   double length_exponent, double energy_exponent, double charge_exponent,
   double temperature_exponent, double time_exponent, int* kimerror){
   return Unit_Handling::convert_to_act_unit((void *)this, length, energy, charge, temperature, time,
   length_exponent, energy_exponent, charge_exponent,  temperature_exponent, time_exponent, kimerror);
}

//multiple data set/get methods
//
void KIM_API_model::setm_data(int *err, int numargs, ... ){
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        cout<<"setm_data: numargs must be multiple of 4"<<endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/4; i++){
        char *nm      = va_arg(listPointer, char *);
        intptr_t size = va_arg(listPointer, intptr_t);
        void *dt      = va_arg(listPointer, void *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) cout<<"setm_data: WARNING: for "<<nm<<" data is NULL\n";
        if(!this->set_data(nm,size,dt)){
            cout<<"setm_data: set data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);

}
void KIM_API_model::setm_data_by_index(int *err, int numargs, ... ){
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        cout<<"setm_data_by_index: numargs must be multiple of 4"<<endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/4; i++){
        int ind      = va_arg(listPointer, int);
        intptr_t size = va_arg(listPointer, intptr_t);
        void *dt      = va_arg(listPointer, void *);
        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) cout<<"setm_data_by_index: WARNING: for argument group "<<i<<" data is NULL\n";

        if(!this->set_data_by_index(ind,size,dt)){
            cout<<"setm_data_by_index: set data for argument group"<<i<<" failed\n";
            va_end(listPointer);
            return;
        }
    }
    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::getm_data(int *err,int numargs, ...){

    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        cout<<"getm_data: numargs must be multiple of 3"<<endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        void **dt      = va_arg(listPointer, void **);
        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = this->get_data(nm,err);
        if(*err != KIM_STATUS_OK){
            cout<<"getm_data: get data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::getm_data_by_index(int *err,int numargs, ...){
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        cout<<"getm_data_by_index: numargs must be multiple of 3"<<endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        int ind      = va_arg(listPointer, int);
        void **dt      = va_arg(listPointer, void **);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = this->get_data_by_index(ind,err);
        if(*err != KIM_STATUS_OK){
            cout<<"getm_data_by_index: get data for argument group "<<i<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::getm_index(int *err, int numargs, ...){
     *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);

    if(numargs % 3 != 0) {
        cout<<"getm_index: numargs must be multiple of 3"<<endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int *ind      = va_arg(listPointer, int *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *ind = this->get_index(nm,err);
        if(*err != KIM_STATUS_OK){
            cout<<"getm_index: get index for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);

}

void KIM_API_model::setm_compute(int *err, int numargs, ...){
     *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        cout<<"setm_compute: numargs must be multiple of 3"<<endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int compute_flag = va_arg(listPointer, int);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        int index = this->get_index(nm,err);
        if (*err != KIM_STATUS_OK){
           cout<<"setm_compute:  name "<<nm<<" not in KIM\n";
           va_end(listPointer);
           return;
        }
        if (compute_flag ==1){
            (*this)[index].flag->calculate = 1;
        }else if (compute_flag ==0){
            (*this)[index].flag->calculate = 0;
        }else{
            cout<<"setm_compute:  for "<<nm<<" failed: compute_flag must be 0 or 1\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::setm_compute_by_index(int* err, int numargs, ...){
      *err=KIM_STATUS_OK;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        cout<<"setm_compute_by_index: numargs must be multiple of 3"<<endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        int index      = va_arg(listPointer, int);
        int compute_flag = va_arg(listPointer, int);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if (index < 0 || index >= this->model.size) *err=KIM_STATUS_FAIL;
        if (*err != KIM_STATUS_OK){
           cout<<"setm_compute_by_index:  for argument group "<<i<<" failed\n";
           va_end(listPointer);
           return;
        }
        if (compute_flag ==1){
            (*this)[index].flag->calculate = 1;
        }else if (compute_flag ==0){
            (*this)[index].flag->calculate = 0;
        }else{
            cout<<"setm_compute_by_index:  for argument group "<<i<<" failed: compute_flag must be 0 or 1\n";
            *err=KIM_STATUS_FAIL;
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::getm_compute(int *err,int numargs, ...){
     *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        cout<<"getm_compute: numargs must be multiple of 3"<<endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int *compute_flag = va_arg(listPointer, int*);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        int index = this->get_index(nm,err);
        if (*err != KIM_STATUS_OK){
           cout<<"getm_compute:  name "<<nm<<" not in KIM\n";
           va_end(listPointer);
           return;
        }
        *compute_flag =(*this)[index].flag->calculate;
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::getm_compute_by_index(int* err, int numargs, ...){
    *err=KIM_STATUS_OK;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        cout<<"getm_compute_by_index: numargs must be multiple of 3"<<endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        int index      = va_arg(listPointer, int);
        int *compute_flag = va_arg(listPointer, int*);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if (index < 0 || index >= this->model.size) *err=KIM_STATUS_FAIL;
        if (*err != KIM_STATUS_OK){
           cout<<"getm_compute_by_index:  for argument group "<<i<<" failed\n";
           va_end(listPointer);
           return;
        }
        *compute_flag = (*this)[index].flag->calculate;

    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_model::print(int* error){
    *error =KIM_STATUS_FAIL;
    if (this==NULL) return;
    cout<<(*this);
    *error=KIM_STATUS_OK;
}

intptr_t KIM_API_model::get_size_by_index(int I,int *error){
    *error =KIM_STATUS_FAIL;
    if (this == NULL) return 0;
    *error =KIM_STATUS_OK;
    return (*this)[I].size;
}

intptr_t KIM_API_model::get_shape_by_index(int I, int * shape,int *error){
     *error =KIM_STATUS_OK;
    if (this == NULL) return -2;
    *error =1;
     if((*this)[I].rank == 0){
            return 0;
        }else if((*this)[I].rank ==1){
            shape[0] = (int)(*this)[I].size;
            return 1;
        }else if((*this)[I].rank>1){
            for (int i=0; i< (*this)[I].rank; i++) shape[i] =(*this)[I].shape[i];
            return (*this)[I].rank;
        }else{
            *error =KIM_STATUS_FAIL;
            return -1;
        }
}

int KIM_API_model::get_compute_by_index(int I,int * error){
    *error =KIM_STATUS_FAIL;
    if (this == NULL) return 1;
    *error =KIM_STATUS_OK;
    return (*this)[I].flag->calculate;
 }
