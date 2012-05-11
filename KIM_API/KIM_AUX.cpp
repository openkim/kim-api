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
//    Ryan S. Elliott
//    Ellad B. Tadmor
//

//
// Release: This file is part of the openkim-api.git repository.
//


#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string.h>
//#include <sstream>



using namespace std;

#include "KIM_AUX.h"
#include "KIM_API.h"

KIM_AUX::Process_DE::Process_DE(){
    virial=NULL;
    particleVirial=NULL;
    hessian=NULL;
    virial_flag=0;
    particleVirial_flag=0;
    hessian_flag=0;
    numberOfParticles=NULL;
    halfNeighbors=true;
}

void KIM_AUX::Process_DE::init2zero(KIM_API_model* pkim, int* kimerr){
            //get instance of Process_DE from kim object
            Process_DE *prDE=pkim->get_process_DE_instance();

            prDE->virial_flag = 0;
            prDE->particleVirial_flag =0;
            int p1_ind=-1;
            int p2_ind=-1;
            int ierGlobal,ierPerAtom,ierStiffness;
            p1_ind = pkim->get_index("process_dEdr",kimerr);
            p2_ind = pkim->get_index("process_d2Edr2",kimerr);
            prDE->virial = (double *) pkim->get_data("virial",&ierGlobal);
            prDE->particleVirial = (double *) pkim->get_data("particleVirial",&ierPerAtom);
            prDE->hessian = (double *)pkim->get_data("hessian",&ierStiffness);
            prDE->numberOfParticles = (int *) pkim->get_data("numberOfParticles",kimerr);
           // halfNeighbors = !pkim->requiresFullNeighbors();
            bool process_d1=false, process_d2=false;

            if (*kimerr !=KIM_STATUS_OK) return;
            if (ierGlobal == KIM_STATUS_OK && prDE->virial != NULL) {
                prDE->virial_flag = pkim->get_compute("virial");
                if (prDE->virial_flag==1 && pkim->virial_need2add) {
                  prDE->virial[0] =0.0;  prDE->virial[1] =0.0;  prDE->virial[2] =0.0;
                  prDE->virial[3] =0.0;  prDE->virial[4] =0.0;  prDE->virial[5] =0.0;
                  process_d1=true;
                }
            }

            if (ierPerAtom == KIM_STATUS_OK && prDE->particleVirial != NULL) {
                prDE->particleVirial_flag = pkim->get_compute("particleVirial");
                if (prDE->particleVirial_flag==1 && pkim->particleVirial_need2add) {
                    for (int i =0;i<(*(prDE->numberOfParticles))*6 ;i++) prDE->particleVirial[i]=0.0;
                    process_d1=true;
                }
            }

            if (ierStiffness == KIM_STATUS_OK && prDE->hessian != NULL) {
                prDE->hessian_flag = pkim->get_compute("hessian");
                if (prDE->hessian_flag==1 && pkim->hessian_need2add) {
                    for (int i =0;i<(*(prDE->numberOfParticles))*(*(prDE->numberOfParticles))*9 ;i++) prDE->hessian[i]=0.0;
                    process_d1=true;
                    process_d2=true;
                }
            }

            if ((p1_ind >=0) && (!pkim->test_doing_process_dEdr)){
                if (process_d1) {
                   pkim->set_compute("process_dEdr", 1, kimerr);
                } else {
                   pkim->set_compute("process_dEdr", 0, kimerr);
                }
            }
            if ((p2_ind >=0) && (!pkim->test_doing_process_d2Edr2)){
                if (process_d2) {
                   pkim->set_compute("process_d2Edr2", 1, kimerr);
                } else {
                   pkim->set_compute("process_d2Edr2", 0, kimerr);
                }
            }

}

void KIM_AUX::Process_DE::process_dEdr(KIM_API_model** ppkim, double* de, double* r, double** pdx, int* i, int* j, int* ier){
    //get instance of Process_DE from kim object
    KIM_API_model *pkim = *ppkim;
    Process_DE *prDE=pkim->get_process_DE_instance();

    *ier=KIM_STATUS_FAIL;
    double vir[6],v;
    double *dx = *pdx;
    v=(*de)/(*r);
   if (prDE->virial_flag ==1 && pkim->virial_need2add) {
       vir[0] = v * dx[0] * dx[0];
       vir[1] = v * dx[1] * dx[1];
       vir[2] = v * dx[2] * dx[2];
       vir[3] = v * dx[1] * dx[2];
       vir[4] = v * dx[0] * dx[2];
       vir[5] = v * dx[0] * dx[1];
       prDE->virial[0] += vir[0];
       prDE->virial[1] += vir[1];
       prDE->virial[2] += vir[2];
       prDE->virial[3] += vir[3];
       prDE->virial[4] += vir[4];
       prDE->virial[5] += vir[5];
    }
    if (prDE->particleVirial_flag==1 && pkim->particleVirial_need2add ){

       vir[0] =0.5 * v * dx[0] * dx[0];
       vir[1] =0.5 * v * dx[1] * dx[1];
       vir[2] =0.5 * v * dx[2] * dx[2];
       vir[3] =0.5 * v * dx[1] * dx[2];
       vir[4] =0.5 * v * dx[0] * dx[2];
       vir[5] =0.5 * v * dx[0] * dx[1];
       prDE->particleVirial[(*i)*6 + 0] += vir[0];
       prDE->particleVirial[(*i)*6 + 1] += vir[1];
       prDE->particleVirial[(*i)*6 + 2] += vir[2];
       prDE->particleVirial[(*i)*6 + 3] += vir[3];
       prDE->particleVirial[(*i)*6 + 4] += vir[4];
       prDE->particleVirial[(*i)*6 + 5] += vir[5];

       prDE->particleVirial[(*j)*6 + 0] += vir[0];
       prDE->particleVirial[(*j)*6 + 1] += vir[1];
       prDE->particleVirial[(*j)*6 + 2] += vir[2];
       prDE->particleVirial[(*j)*6 + 3] += vir[3];
       prDE->particleVirial[(*j)*6 + 4] += vir[4];
       prDE->particleVirial[(*j)*6 + 5] += vir[5];
     }

       if (prDE->hessian_flag ==1 && pkim->hessian_need2add) {
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
               prDE->hessian[(*i)*(*(prDE->numberOfParticles))*9 + (*i)*9 + k*3 + m] += stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->hessian[(*i)*(*(prDE->numberOfParticles))*9 + (*j)*9 + k*3 + m] -= stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->hessian[(*j)*(*(prDE->numberOfParticles))*9 + (*i)*9 + k*3 + m] -= stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->hessian[(*j)*(*(prDE->numberOfParticles))*9 + (*j)*9 + k*3 + m] += stiff[k][m];

       }
       *ier = KIM_STATUS_OK;
}

void KIM_AUX::Process_DE::process_d2Edr2(KIM_API_model** ppkim, double* de, double** rr, double** pdx, int** ii, int** jj, int* ier){
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
               prDE->hessian[(i[0])*(*(prDE->numberOfParticles))*9 + (i[1])*9 + k*3 + m] += stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->hessian[(i[0])*(*(prDE->numberOfParticles))*9 + (j[1])*9 + k*3 + m] -= stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->hessian[(j[0])*(*(prDE->numberOfParticles))*9 + (i[1])*9 + k*3 + m] -= stiff[k][m];
           for(int k=0;k<3; k++) for(int m=0;m<3;m++)
               prDE->hessian[(j[0])*(*(prDE->numberOfParticles))*9 + (j[1])*9 + k*3 + m] += stiff[k][m];

           if (!((i[0] == i[1]) && (j[0] == j[1])) &&
               !((i[0] == j[1]) && (j[0] == i[1])))
           {
              for(int k=0;k<3; k++) for(int m=0;m<3;m++)
              prDE->hessian[(i[1])*(*(prDE->numberOfParticles))*9 + (i[0])*9 + k*3 + m] += stiff[k][m];
              for(int k=0;k<3; k++) for(int m=0;m<3;m++)
              prDE->hessian[(i[1])*(*(prDE->numberOfParticles))*9 + (j[0])*9 + k*3 + m] -= stiff[k][m];
              for(int k=0;k<3; k++) for(int m=0;m<3;m++)
              prDE->hessian[(j[1])*(*(prDE->numberOfParticles))*9 + (i[0])*9 + k*3 + m] -= stiff[k][m];
              for(int k=0;k<3; k++) for(int m=0;m<3;m++)
              prDE->hessian[(j[1])*(*(prDE->numberOfParticles))*9 + (j[0])*9 + k*3 + m] += stiff[k][m];
           }

           *ier = KIM_STATUS_OK;
}
