//                                                                      
// Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
// All rights reserved.                                                 
//                                                                     
// Author: Valeriu Smirichinski                                         
// 


#include <stdlib.h>

#include <iostream>
#include <fstream>
//#include <cctype>
#include <string.h>

using namespace std;


#include "KIMservice.h"

/*
 * 
 */
int main(int argc, char** argv) {
    KIM_IOline inln;

    //testing KIM_IOline
    cout<<endl<<"*********  testing KIM_IOline ***********"<<endl;
    ifstream myfile;
    myfile.open("../test_val01.kim");
    int counter=0;
    while(!myfile.eof()){
       myfile >> inln; //cout<<"unparsed: "<<inln<<endl;
        if(inln.goodformat) {
            cout<< inln<<endl;
            cout<<"get_rank= "<<inln.get_rank()<<endl;
            cout<<" scale = "<<inln.get_unitscale();
            cout<<"get_shape= [";
            int *shp = inln.get_shape();
            if (shp!=NULL) for(int i =0;i<inln.get_rank();i++)cout<<" "<<shp[i];
            if (shp==NULL) cout<<"NULL";
            cout<<"]"<<endl;
           
            counter++;
        }
    }
    cout<<"counter="<<counter<<endl;
    myfile.close();

    KIM_IOline *inlines = new KIM_IOline[counter];
    myfile.open("../test_val01.kim");
    counter=0;
 
    while(!myfile.eof()){
       myfile >> inln; //cout<<"unparsed: "<<inln<<endl;
        if(inln.goodformat) {
            inlines[counter]=inln;
            counter++;
        }
    }
    cout<<"counter="<<counter<<endl;
    for (int i=0;i<counter;i++) cout<<inlines[i]<<endl;
    myfile.close();

    //testing KIMBaseElement
    cout<<endl<<"*********  testing KIMBaseElement ***********"<<endl;
   KIMBaseElement el;
   int shape[3];
   el.init(inlines[1].name,inlines[1].type,1,0,shape);
    ((long long*) (el.data))[0] = 25;
 
   cout<<el;
   el.free();

   shape[0]=2;shape[1]=3;
   double x[6]; double *xx;
   x[0*3+0]=0.5; x[0*3+1]=1.0; x[0*3+2]=1.5;
   x[1*3+0]=3.5; x[1*3+1]=4.0; x[1*3+2]=5.0;
   xx=&x[0];

   el.init(inlines[0].name,inlines[0].type,6,2,shape,&xx);
   cout<<el;

   el.free();
   cout<<el;

   //
   //testing KIM_API_model
   cout<<" *************** testing KIM_API_model *********** "<<endl;
   KIM_API_model kimlj;
cout<<"proba"<<endl;
   char inputfile [] ="../../KIM_API/model_val01.kim";
   char modelname [] = "First_test_model_api";
   kimlj.preinit(inputfile,modelname);
  
    cout<<" kimlj[coordinates].name "<<kimlj["coordinates"].name<<endl;
//cout<<kimlj;
cout<<" SystemOfUnitsFix ="<< kimlj.is_unitsfixed()<<" "<<false<<endl;
   


    // kimlj.free();

   //cout<<kimlj;
   // now get packed and unpucked??
   long long natoms = 2;
   int kimerr;
   kimlj.set_data("forces",6,&x[0]);
   kimlj.set_data("numberOfAtoms",1,&natoms);
   kimlj.transform_units_to("standard",&kimerr);
    kimlj.transform_units_to("custom",&kimerr);
   cout<<kimlj;


   kimlj.free();
   kimlj.free();
   cout<<kimlj;

   char  intest [] = "../../KIM_API/test_val01.kim";
   char  inmodel [] ="../../KIM_API/model_val01.kim";

  cout<<"  xxxx match?:"<< kimlj.init(intest,"test_val01",inmodel,"model_val01")<<endl;
  KIM_API_model::allocateinitialized(&kimlj,10,2,&kimerr);
 // kimlj.model_init();
    //cout<<kimlj;

    
   kimlj.free();


   
   return (EXIT_SUCCESS);
}

