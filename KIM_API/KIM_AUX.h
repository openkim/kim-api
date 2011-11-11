//
//

#ifndef _KIM_AUX_H
#define	_KIM_AUX_H

#ifndef _KIMSERVICE_H
#include "KIMservice.h"
#endif

namespace KIM_Standard_Virials{

        double *virialGlobal;
        double *virialPerAtom;
        int virialGlobal_flag;
        int virialPerAtom_flag;
        int *numberOfAtoms;
        bool halfNeighbors;

        void init2zero(KIM_API_model *pkim,int *kimerr){
            virialGlobal_flag = 0;
            virialPerAtom_flag =0;
            int ierGlobal,ierPerAtom;
            virialGlobal = (double *) pkim->get_data("virialGlobal",&ierGlobal);
            virialPerAtom = (double *) pkim->get_data("virialPerAtom",&ierPerAtom);
            numberOfAtoms = (int *) pkim->get_data("numberOfAtoms",kimerr);
           // halfNeighbors = !pkim->requiresFullNeighbors();
            
            if (*kimerr !=KIM_STATUS_OK) return;
            if (ierGlobal == KIM_STATUS_OK && virialGlobal != NULL) {
                virialGlobal_flag = pkim->isit_compute("virialGlobal");
                if (virialGlobal_flag==1 && pkim->virialGlobal_need2add) *virialGlobal =0.0;
            }

            if (ierPerAtom == KIM_STATUS_OK && virialPerAtom != NULL) {
                virialPerAtom_flag = pkim->isit_compute("virialPerAtom");
                if (virialPerAtom_flag==1 && pkim->virialPerAtom_need2add) {
                    for (int i =0;i<(*numberOfAtoms)*6 ;i++) virialPerAtom[i]=0.0;
                }
            }


        }

        void process_d1Edr(KIM_API_model **ppkim,double *de,double *r,double ** pdx,int *i,int *j,int *ier){
            KIM_API_model *pkim = *ppkim;
            *ier=KIM_STATUS_FAIL;
            double vir[6],v;
            double *dx = *pdx;
            //v=(*de)/((*r)*(*r))/3.0;
            //v=(*de)/((*r)*(*r));
            v=(*de)/(*r);
  if (virialGlobal_flag ==1 && pkim->virialGlobal_need2add) {
       vir[0] = v * dx[0] * dx[0];
       vir[1] = v * dx[1] * dx[1];
       vir[2] = v * dx[2] * dx[2];
       vir[3] = v * dx[1] * dx[2];
       vir[4] = v * dx[0] * dx[2];
       vir[5] = v * dx[0] * dx[1];
       virialGlobal[0] += vir[0];
       virialGlobal[1] += vir[1];
       virialGlobal[2] += vir[2];
       virialGlobal[3] += vir[3];
       virialGlobal[4] += vir[4];
       virialGlobal[5] += vir[5];
    }
    if (virialPerAtom_flag==1 && pkim->virialPerAtom_need2add ){

       vir[0] =0.5 * v * dx[0] * dx[0];
       vir[1] =0.5 * v * dx[1] * dx[1];
       vir[2] =0.5 * v * dx[2] * dx[2];
       vir[3] =0.5 * v * dx[1] * dx[2];
       vir[4] =0.5 * v * dx[0] * dx[2];
       vir[5] =0.5 * v * dx[0] * dx[1];
       virialPerAtom[(*i)*6 + 0] += vir[0];
       virialPerAtom[(*i)*6 + 1] += vir[1];
       virialPerAtom[(*i)*6 + 2] += vir[2];
       virialPerAtom[(*i)*6 + 3] += vir[3];
       virialPerAtom[(*i)*6 + 4] += vir[4];
       virialPerAtom[(*i)*6 + 5] += vir[5];

       virialPerAtom[(*j)*6 + 0] += vir[0];
       virialPerAtom[(*j)*6 + 1] += vir[1];
       virialPerAtom[(*j)*6 + 2] += vir[2];
       virialPerAtom[(*j)*6 + 3] += vir[3];
       virialPerAtom[(*j)*6 + 4] += vir[4];
       virialPerAtom[(*j)*6 + 5] += vir[5];
     }

            *ier = KIM_STATUS_OK;  
    }
}

#endif	/* _KIM_AUX_H */
