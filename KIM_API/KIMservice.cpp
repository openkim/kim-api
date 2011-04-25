
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

using namespace std;
#include "KIMservice.h"
#ifndef _KIMSERVICE_H
#define	_KIMSERVICE_H
#endif	/* _KIMSERVICE_H */
#define KEY_CHAR_LENGTH 64


 #define FREEABLE 0
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

KIM_IOline::KIM_IOline(){
    goodformat=false;init2empty();
}
      
bool KIM_IOline:: getFields(char *inString){
            char *tmp;
            static int counter=0;
            init2empty();
            //check for comments part and get it removed
            tmp = strpbrk(inString,"#");
            if(tmp !=NULL) {strncpy(comments,tmp,strlen(tmp)+1);
            tmp[0]='\0';
            counter++;
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
           if(flag!=NULL) delete [] flag;
           if(unit!=NULL) delete  unit;
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
            char methodkey[KEY_CHAR_LENGTH]="method";// add here to expand...

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
            }else{// add here more in else if block...
            printf("KIMBaseElement_getelemsize:key type %s is not:\n %s %s %s, %s, %s \n",tp,realkey,real8key,integerkey,ptrkey,integer8key);
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
}
KIM_API_model:: ~KIM_API_model(){
       //delete [] UtitsSet;
}
bool KIM_API_model:: preinit(char * initfile,char *modelname){
        read_file(initfile,&inlines,&numlines);
        int *shape=NULL;
        char pointer_str [] = "pointer";
        model.init(modelname,pointer_str,(intptr_t)numlines,1,shape);
        this->supported_units_init();
        for (int i=0; i< numlines;i++){
            //neobhodimo napisat' KIM_IOline.get_rank / get_shape routine
            KIMBaseElement *el = new KIMBaseElement ;

            int rank=inlines[i].get_rank();
            shape =inlines[i].get_shape();
            char * name =& (inlines[i].name[0]);
            char * type =& (inlines[i].type[0]);
 
            el->init(name,type,0,rank,shape); //preinit element with zero size
            strncpy(el->unit->dim,inlines[i].dim,strlen(inlines[i].dim)+1);
            double scale=inlines[i].get_unitscale();
 
            strncpy(el->unit->units,inlines[i].dim,strlen(inlines[i].dim)+1);
            el->unit->scale = (float)scale;
            el->flag->calculate = 1;
            el->flag->peratom = 1;//per something else
            if(inlines[i].isitperatom()) el->flag->peratom = 0; //per atom
            KIMBaseElement **pel =(KIMBaseElement**) model.data;
            pel[i] =  el;
            delete [] shape;

        }
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
 void KIM_API_model::free(){
        //
         KIMBaseElement **pel =  (KIMBaseElement **)  model.data;
        if(model.data != NULL)  for (int i =0;i<model.size;i++) pel[i]->free();
        model.free();
        delete [] inlines;
        inlines=NULL;
        delete [] UnitsSet;
        numlines=0;
        UnitsSet=NULL;
        numUnitsSet=0;
        originalUnits[0]='\0';
        currentUnits[0]='\0';
        unitsFixed=false;
 }
bool KIM_API_model::set_data(char *nm, intptr_t size, void *dt){
        // set data into preinit element correctly calculates all
        int ind=get_index(nm);
        if (ind<0) {
            return false;
        } //no data in KIM_API_model
        int c=1;
        
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
void * KIM_API_model::get_data(char *nm){
        int i=get_index(nm);
        return (*this)[i].data;
}
int KIM_API_model::get_index(char *nm){
        for(int i=0; i< model.size;i++){
            if(strcmp((*this)[i].name,nm)==0) return i;
        }
        return -1;
}
intptr_t KIM_API_model::get_size(char *nm){
        int ind=get_index(nm);
        return (*this)[ind].size;
}
intptr_t KIM_API_model::get_rank_shape(char *nm,int * shape){
        int ind=get_index(nm);
        if((*this)[ind].rank == 0){
            return 0;
        }else if((*this)[ind].rank ==1){
            shape[0] = (int)(*this)[ind].size;
            return 1;
        }else if((*this)[ind].rank>1){
            for (int i=0; i< (*this)[ind].rank; i++) shape[i] =(*this)[ind].shape[i];
            return (*this)[ind].rank;
        }else{
            return -1;
        }
}
void KIM_API_model::set2_compute(char *nm){
        (*this)[nm].flag->calculate = 1;
}
void KIM_API_model::set2_uncompute(char *nm){
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
    return is_it_match(mdtst,IOlines,nlns);
 }//will be private
bool KIM_API_model::is_it_match(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns){
    bool match;
    //check if lines are match with Model api variable
    match =true;
    for (int i=0; i<nlns || match==false;i++){
        match=false;
 
        if(IOlines[i].isitoptional()){
            match=true;
            //break;
        }
        for(int j=0;j<mdtst.model.size;j++){
            if(mdtst[j]== IOlines[i]) {
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
    char modelfile[160] = KIM_DIR;
    char modelstdname[32]="standard";
    strcat(modelfile,"KIM_API/standard.kim");
    if(!stdmdl.preinit(modelfile,modelstdname)){
        cout<<" preinit of :"<<modelfile<<" failed"<<endl;
        stdmdl.free();
        return false;
    }

    // test and mdl must be preinit.
    bool test2modelmatch= is_it_match(test,mdl.inlines,mdl.numlines);
    bool model2testmatch= is_it_match(mdl,test.inlines,test.numlines);
    bool test2standardmatch = is_it_match(stdmdl,test.inlines,test.numlines);
    bool model2standardmatch = is_it_match(stdmdl,mdl.inlines,mdl.numlines);
    stdmdl.free();

    if(!test2standardmatch) cout<<"there are none standard variables in test:"<<endl;
    if(!model2standardmatch) cout<<"there are none standard variables in model"<<endl;

    if (test2modelmatch && model2testmatch && test2standardmatch && model2standardmatch) return true;
    return false;
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
        this->irrelevantVars2uncompute(test,*this);

        test.free(); mdl.free();
        char computestr [] = "compute";
        compute_index = get_index(computestr);
        return true;
    }else{
        test.free(); mdl.free();
 cout<<"Do not match  " << modelname << " and "<< testname<<endl;
        return false;
    }

}
bool KIM_API_model::init(char* testname, char* modelname){
    char testfile[160] = KIM_DIR;
    char modelfile[160] = KIM_DIR;
    strcat(testfile,KIM_DIR_TESTS);
    strcat(testfile,testname);strcat(testfile,"/");strcat(testfile,testname);
    strcat(testfile,".kim");
     strcat(modelfile,KIM_DIR_MODELS);
    strcat(modelfile,modelname);strcat(modelfile,"/");strcat(modelfile,modelname);
    strcat(modelfile,".kim");
    return this->init(testfile,testname,modelfile,modelname);
}

void KIM_API_model::model_compute(){
  // set model_compute pointer
  typedef void (*Model_Compute)(void *);//prototype for model_compute
  Model_Compute mdl_compute = (Model_Compute) (*this)[compute_index].data;
  //call model_compute
  KIM_API_model *pkim = this;
  (*mdl_compute)((void *)&pkim);
}



void KIM_API_model::irrelevantVars2uncompute(KIM_API_model & test, KIM_API_model & mdl){
    if(! is_it_match(test,mdl.inlines,mdl.numlines)) {
        cout<<"irrelevantVars2uncompute: not a test-model match"<<endl;
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
void KIM_API_model::allocateinitialized(KIM_API_model * mdl, intptr_t natoms, int ntypes){
    // in process
    if ( mdl->model.data == NULL) {
        cout<<"KIM_API_model::allocateinitialized: model is not preinitialized"<<endl;
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
}
bool KIM_API_model::  is_unitsfixed(){
    return unitsFixed;
}
void KIM_API_model::set_unitsfixed(bool f){
    unitsFixed=f;
}
void KIM_API_model::transform_units_to(char * unitS){
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
}
bool KIM_API_model::set_units(char * unitS){
    if(!among_supported_units(unitS)  && strcmp(unitS,"custom")!=0 ) return false;
    if(strcmp(unitS,"custom")==0){
        int ind = 0;
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
void KIM_API_model::get_units(char * units){
    strncpy(units, this->currentUnits,strlen(this->currentUnits)+1);
}
void KIM_API_model::get_originalUnits(char * unitS){
    strncpy(unitS, this->originalUnits, strlen(this->originalUnits)+1);
}
void KIM_API_model::supported_units_init(){
        numUnitsSet =1; // change when available  needed
        char cnf_files[numUnitsSet][161] ;

        strncpy(& cnf_files[0][0],"../../KIM_API/standard_units.cfg",161);
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
ostream &operator<<(ostream &stream, KIM_API_model a){
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
