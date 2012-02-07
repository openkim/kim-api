#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string.h>
//#include <sstream>



using namespace std;

#include "KIM_AUX.h";
#include "KIMservice.h"

KIM_AUX::Process_DE::Process_DE(){
    virialGlobal=NULL;
    virialPerAtom=NULL;
    stiffness=NULL;
    virialGlobal_flag=0;
    virialPerAtom_flag=0;
    stiffness_flag=0;
    numberOfAtoms=NULL;
    halfNeighbors=true;
}

void KIM_AUX::Process_DE::init2zero(KIM_API_model* pkim, int* kimerr){
            //get instance of Process_DE from kim object
            Process_DE *prDE=pkim->get_process_DE_instance();

            prDE->virialGlobal_flag = 0;
            prDE->virialPerAtom_flag =0;
            int p1_ind=-1;
            int p2_ind=-1;
            int ierGlobal,ierPerAtom,ierStiffness;
            prDE->virialGlobal = (double *) pkim->get_data("virialGlobal",&ierGlobal);
            prDE->virialPerAtom = (double *) pkim->get_data("virialPerAtom",&ierPerAtom);
            prDE->stiffness = (double *)pkim->get_data("stiffness",&ierStiffness);
            prDE->numberOfAtoms = (int *) pkim->get_data("numberOfAtoms",kimerr);
            p1_ind = pkim->get_index("process_d1Edr");
            p2_ind = pkim->get_index("process_d2Edr");
           // halfNeighbors = !pkim->requiresFullNeighbors();
            bool process_d1=false, process_d2=false;

            if (*kimerr !=KIM_STATUS_OK) return;
            if (ierGlobal == KIM_STATUS_OK && prDE->virialGlobal != NULL) {
                prDE->virialGlobal_flag = pkim->isit_compute("virialGlobal");
                if (prDE->virialGlobal_flag==1 && pkim->virialGlobal_need2add) {
		  prDE->virialGlobal[0] =0.0;  prDE->virialGlobal[1] =0.0;  prDE->virialGlobal[2] =0.0;
                  prDE->virialGlobal[3] =0.0;  prDE->virialGlobal[4] =0.0;  prDE->virialGlobal[5] =0.0;
                  process_d1=true;
                }
            }

            if (ierPerAtom == KIM_STATUS_OK && prDE->virialPerAtom != NULL) {
                prDE->virialPerAtom_flag = pkim->isit_compute("virialPerAtom");
                if (prDE->virialPerAtom_flag==1 && pkim->virialPerAtom_need2add) {
                    for (int i =0;i<(*(prDE->numberOfAtoms))*6 ;i++) prDE->virialPerAtom[i]=0.0;
                    process_d1=true;
                }
            }

            if (ierStiffness == KIM_STATUS_OK && prDE->stiffness != NULL) {
                prDE->stiffness_flag = pkim->isit_compute("stiffness");
                if (prDE->stiffness_flag==1 && pkim->stiffness_need2add) {
                    for (int i =0;i<(*(prDE->numberOfAtoms))*(*(prDE->numberOfAtoms))*9 ;i++) prDE->stiffness[i]=0.0;
                    process_d1=true;
                    process_d2=true;
                }
            }

            if (p1_ind >=0){
                if (process_d1) {
                    pkim->set2_compute("process_d1Edr");
                } else {
                    pkim->set2_donotcompute("process_d1Edr");
                }
            }
            if (p2_ind >=0){
                if (process_d2) {
                    pkim->set2_compute("process_d2Edr");
                } else {
                    pkim->set2_donotcompute("process_d2Edr");
                }
            }

}

void KIM_AUX::Process_DE::process_d1Edr(KIM_API_model** ppkim, double* de, double* r, double** pdx, int* i, int* j, int* ier){
    //get instance of Process_DE from kim object
    KIM_API_model *pkim = *ppkim;
    Process_DE *prDE=pkim->get_process_DE_instance();

    *ier=KIM_STATUS_FAIL;
    double vir[6],v;
    double *dx = *pdx;
    v=(*de)/(*r);
   if (prDE->virialGlobal_flag ==1 && pkim->virialGlobal_need2add) {
       vir[0] = v * dx[0] * dx[0];
       vir[1] = v * dx[1] * dx[1];
       vir[2] = v * dx[2] * dx[2];
       vir[3] = v * dx[1] * dx[2];
       vir[4] = v * dx[0] * dx[2];
       vir[5] = v * dx[0] * dx[1];
       prDE->virialGlobal[0] += vir[0];
       prDE->virialGlobal[1] += vir[1];
       prDE->virialGlobal[2] += vir[2];
       prDE->virialGlobal[3] += vir[3];
       prDE->virialGlobal[4] += vir[4];
       prDE->virialGlobal[5] += vir[5];
    }
    if (prDE->virialPerAtom_flag==1 && pkim->virialPerAtom_need2add ){

       vir[0] =0.5 * v * dx[0] * dx[0];
       vir[1] =0.5 * v * dx[1] * dx[1];
       vir[2] =0.5 * v * dx[2] * dx[2];
       vir[3] =0.5 * v * dx[1] * dx[2];
       vir[4] =0.5 * v * dx[0] * dx[2];
       vir[5] =0.5 * v * dx[0] * dx[1];
       prDE->virialPerAtom[(*i)*6 + 0] += vir[0];
       prDE->virialPerAtom[(*i)*6 + 1] += vir[1];
       prDE->virialPerAtom[(*i)*6 + 2] += vir[2];
       prDE->virialPerAtom[(*i)*6 + 3] += vir[3];
       prDE->virialPerAtom[(*i)*6 + 4] += vir[4];
       prDE->virialPerAtom[(*i)*6 + 5] += vir[5];

       prDE->virialPerAtom[(*j)*6 + 0] += vir[0];
       prDE->virialPerAtom[(*j)*6 + 1] += vir[1];
       prDE->virialPerAtom[(*j)*6 + 2] += vir[2];
       prDE->virialPerAtom[(*j)*6 + 3] += vir[3];
       prDE->virialPerAtom[(*j)*6 + 4] += vir[4];
       prDE->virialPerAtom[(*j)*6 + 5] += vir[5];
     }

       if (prDE->stiffness_flag ==1 && pkim->stiffness_need2add) {
           double rm_half = 1.0/(*r);
           double rm3_half = rm_half*rm_half*rm_half;
           double stiff[3][3];

           rm_half  *= *de;
           rm3_half *= *de;
           stiff[0][0] = -rm3_half * dx[0] * dx[0] + rm_half;
           stiff[1][1] = -rm3_half * dx[1] * dx[1] + rm_half;
           stiff[2][2] = -rm3_half * dx[2] * dx[2] + rm_half;
           stiff[1][2] =stiff[2][1]= -rm3_half * dx[1] * dx[2] ;
           stiff[0][2] = stiff[2][0]= -rm3_half * dx[0] * dx[2] ;
           stiff[0][1] =  stiff[1][0]= -rm3_half * dx[0] * dx[1] ;


           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->stiffness[(*i)*(*(prDE->numberOfAtoms))*9 + (*i)*9 + k*3 + m] += stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->stiffness[(*i)*(*(prDE->numberOfAtoms))*9 + (*j)*9 + k*3 + m] -= stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->stiffness[(*j)*(*(prDE->numberOfAtoms))*9 + (*i)*9 + k*3 + m] -= stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->stiffness[(*j)*(*(prDE->numberOfAtoms))*9 + (*j)*9 + k*3 + m] += stiff[k][m];

       }
       *ier = KIM_STATUS_OK;
}

void KIM_AUX::Process_DE::process_d2Edr(KIM_API_model** ppkim, double* de, double** rr, double** pdx, int** ii, int** jj, int* ier){
         //get instance of Process_DE from kim object
         KIM_API_model *pkim = *ppkim;
         Process_DE *prDE=pkim->get_process_DE_instance();//=??
         
          *ier=KIM_STATUS_FAIL;
          double *r = *rr;
          double rm = (*de)/(r[0]*r[1]);
          double *dx = *pdx;
          double stiff[3][3];

           stiff[0][0] = rm * dx[0] * dx[3+0];
           stiff[1][1] = rm * dx[1] * dx[3+1];
           stiff[2][2] = rm * dx[2] * dx[3+2];
           stiff[1][2] = stiff[2][1]= 0.5 * rm * (dx[1] * dx[3+2] + dx[3+1] * dx[0+2]);
           stiff[0][1] = stiff[1][0]= 0.5 * rm * (dx[0] * dx[3+1] + dx[3+0] * dx[0+1]);
           stiff[0][2] = stiff[2][0]= 0.5 * rm * (dx[0] * dx[3+2] + dx[3+0] * dx[0+2]);

           int *i=*ii;
           int *j=*jj;
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->stiffness[(i[0])*(*(prDE->numberOfAtoms))*9 + (i[1])*9 + k*3 + m] += stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->stiffness[(i[0])*(*(prDE->numberOfAtoms))*9 + (j[1])*9 + k*3 + m] -= stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->stiffness[(j[0])*(*(prDE->numberOfAtoms))*9 + (i[1])*9 + k*3 + m] -= stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->stiffness[(j[0])*(*(prDE->numberOfAtoms))*9 + (j[1])*9 + k*3 + m] += stiff[k][m];

           if (!((i[0] == i[1]) && (j[0] == j[1])) &&
               !((i[0] == j[1]) && (j[0] == i[1])))
           {
              for(int k=0;k<3; k++) for(int m=0;m<3;m++)
              prDE->stiffness[(i[1])*(*(prDE->numberOfAtoms))*9 + (i[0])*9 + k*3 + m] += stiff[k][m];
              for(int k=0;k<3; k++) for(int m=0;m<3;m++)
              prDE->stiffness[(i[1])*(*(prDE->numberOfAtoms))*9 + (j[0])*9 + k*3 + m] -= stiff[k][m];
              for(int k=0;k<3; k++) for(int m=0;m<3;m++)
              prDE->stiffness[(j[1])*(*(prDE->numberOfAtoms))*9 + (i[0])*9 + k*3 + m] -= stiff[k][m];
              for(int k=0;k<3; k++) for(int m=0;m<3;m++)
              prDE->stiffness[(j[1])*(*(prDE->numberOfAtoms))*9 + (j[0])*9 + k*3 + m] += stiff[k][m];
           }

           *ier = KIM_STATUS_OK;
}