
//                                                                      
// Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
// All rights reserved.                                                 
//                                                                     
// Author: Valeriu Smirichinski                                         
// 


//#include <stdlib.h>

//#include <iostream>
//#include <fstream>
////#include <cctype>
//#include <string.h>
#include <stdint.h>
using namespace std;
#ifndef _KIMSERVICE_H
#define	_KIMSERVICE_H



#endif	/* _KIMSERVICE_H */

#define KEY_CHAR_LENGTH 64

#ifndef KIM_DIR
#define KIM_DIR "../../"
#endif

#ifndef KIM_DIR_MODELS
#define KIM_DIR_MODELS "MODELs/"
#endif

#ifndef KIM_DIR_TESTS
#define KIM_DIR_TESTS "TESTs/"
#endif

//#ifndef intptr_t
//#define intptr_t long long  // for 64 bit machines
//#endif
#ifndef KIM_DIR
#define KIM_DIR "../../"
#endif
#define FREEABLE 0

//#define intptr_t int  // for 32 bit machines

class KIMBaseElementFlag{
public:
        int peratom; //0 -- peratom, 1--per something else
        int freeable; // 0--freeable , 1 is not freeable, ... - error
        int pointerchanged;// 0 -- changed therefore update, 1 don't update, ... =error
        int calculate; // 0 -- do not calculate, 1 -- calculate
        KIMBaseElementFlag();
};

class KIMBaseElementUnit{
public:
        char units[KEY_CHAR_LENGTH]; //name of units system, "none" if not set
        char  dim [KEY_CHAR_LENGTH];  // dimension (length, energy, time, mass, ore derivatives...
        float scale;// scale to the standard unit: " 1.0 if it is standard unit"
      KIMBaseElementUnit();
      void init();
};

class KIM_IOline{
public:
	char name[KEY_CHAR_LENGTH];
	char type[KEY_CHAR_LENGTH];
	char dim[KEY_CHAR_LENGTH];
        char units[KEY_CHAR_LENGTH];
        char shape[KEY_CHAR_LENGTH];
        char requirements[KEY_CHAR_LENGTH];
        char comments[181];
	bool goodformat,input,output;

	KIM_IOline();
      
	bool getFields(char *inString);
        double get_unitscale();
        int get_rank();
        int * get_shape();
        bool isitpernatomtypes();
        bool isitsizedefined();
        bool isitperatom();
        bool isitoptional();
 private:
        void strip(char * strv);
        void strip();
        void init2empty();
        bool isitinput(char*str);
        bool isitoutput(char*str);
};//main input line handler
ostream &operator<<(ostream &stream, KIM_IOline a);
istream &operator>>(istream &stream, KIM_IOline &a);

class IOline{
public:
	char name[101];
	char value[101];
	char comment[101];
	bool goodformat;
	void strip();

        void strip(char *nm);
	IOline();
	bool getFields(const char *inputString);
}; //secondary input line handler
ostream &operator<<(ostream &stream, IOline a);
istream &operator>>(istream &stream, IOline &a);

class SystemOfUnit {
public:
    char UnitsSystemName[KEY_CHAR_LENGTH];
    IOline * inlines;
    int numlines;
    float mass,distance,time,energy,velocity;
    float force,torque,temperature,pressure;
    float dynamic_viscosity,charge,dipole,electric_field,density;
    float garbage;

    SystemOfUnit();
    ~SystemOfUnit();
    void init(char *infile);
    float &operator[](char * unit);
 static int readlines(char * infile, IOline **inlines);
    bool isitunit(char * unit);
};

class KIMBaseElement{
public:
        void *data;
        intptr_t size; //Size in words defined by type
        intptr_t rank; // number of indexes
        int * shape; //1d array of integer showing the size of each index
        void * ptrptr; // Auxilary structure for holding pointer to pointers
        char *name;
        char *type;
        KIMBaseElementUnit * unit;
        KIMBaseElementFlag *flag;
        void *reserved;
        KIMBaseElement();
        ~KIMBaseElement();
        void init(char *nm,char * tp,intptr_t sz, intptr_t rnk, int *shp,void * pdata);
        void init(char *nm,char * tp,intptr_t sz, intptr_t rnk, int *shp);
        void free();
        void nullefy();
        bool operator==(KIM_IOline& kimioline);

static  int getelemsize(char *tp);
};
ostream &operator<<(ostream &stream, KIMBaseElement a);

class KIM_API_model{
public:
    KIMBaseElement model;
    KIM_API_model();
    ~KIM_API_model();
    bool preinit(char * initfile,char *modelname);
    void free();

    bool set_data(char *nm, intptr_t size, void *dt);
    bool set_data_byi(int ind,intptr_t size, void *dt);

    void * get_data(char *nm);
    int get_index(char *nm);
    intptr_t get_size(char *nm);
    intptr_t get_rank_shape(char *nm,int * shape);
    void set2_compute(char *nm);
    void set2_uncompute(char *nm);
    bool isit_compute(char *nm);
    KIMBaseElement &operator[](int i);
    KIMBaseElement &operator[](char *nm);

static   void read_file(char * initfile,KIM_IOline ** lns, int * numlns);
static bool is_it_match(KIM_API_model & mdtst,char * inputinitfile);
static bool is_it_match(KIM_API_model & mdtst,KIM_IOline * IOlines,int nlns);
static bool is_it_match(KIM_API_model &test,KIM_API_model & mdl);
    bool init(char * testinputfile,char* testname, char * modelinputfile,char *modelname);
    bool init(char * testname,char * modelname);
    void model_compute();
    void model_init();
static void irrelevantVars2uncompute(KIM_API_model & test, KIM_API_model & mdl);
static void allocateinitialized(KIM_API_model * mdl, intptr_t natoms, int ntypes);
bool is_unitsfixed();
void set_unitsfixed(bool f);
void transform_units_to(char * unitS);
bool set_units(char * unitS);
void get_units(char * units);
void get_originalUnits(char * unitS);

    KIM_IOline *inlines;
    int numlines;
    SystemOfUnit * UnitsSet;
    int numUnitsSet;
    char originalUnits[KEY_CHAR_LENGTH];
    char currentUnits[KEY_CHAR_LENGTH];
    bool unitsFixed;

private:
    int compute_index;
    void supported_units_init();
    bool are_infileunits_consistent();
    bool among_supported_units(char *unitS);
    int get_indexOfsupportedUnits(char * unitS);
    void setScale(int index);
    void data_multiply_a(void *dt,char* type,intptr_t sz,float a);
};
ostream &operator<<(ostream &stream, KIM_API_model a);
