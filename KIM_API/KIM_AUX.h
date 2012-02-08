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
        static void process_dEdr(KIM_API_model **ppkim,double *de,
                                 double *r,double ** pdx,int *i,int *j,int *ier);
        static void process_d2Edr2(KIM_API_model **ppkim,double *de,
                                 double **rr,double ** pdx,int **ii,int **jj,int *ier);
    private:
        double *virial;
        double *particleVirial;
        double *hessian;
        int virial_flag;
        int particleVirial_flag;
        int hessian_flag;
        int *numberOfParticles;
        bool halfNeighbors;
    };
       
}

#endif	/* _KIM_AUX_H */
