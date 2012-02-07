//
//

#ifndef _KIM_AUX_H
#define	_KIM_AUX_H




class KIM_API_model;

namespace KIM_AUX{
    class Process_DE{
    public:
        Process_DE();
        static void init2zero(KIM_API_model *pkim,int *kimerr);
        static void process_d1Edr(KIM_API_model **ppkim,double *de,
                                 double *r,double ** pdx,int *i,int *j,int *ier);
        static void process_d2Edr(KIM_API_model **ppkim,double *de,
                                 double **rr,double ** pdx,int **ii,int **jj,int *ier);
    private:
        double *virialGlobal;
        double *virialPerAtom;
        double *stiffness;
        int virialGlobal_flag;
        int virialPerAtom_flag;
        int stiffness_flag;
        int *numberOfAtoms;
        bool halfNeighbors;
    };
       
}

#endif	/* _KIM_AUX_H */
