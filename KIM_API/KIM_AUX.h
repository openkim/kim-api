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
// Copyright (c) 2013--2014, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Valeriu Smirichinski
//    Ryan S. Elliott
//    Ellad B. Tadmor
//

//
// Release: This file is part of the openkim-api.git repository.
//


#ifndef KIMHDR_KIM_AUX_H
#define KIMHDR_KIM_AUX_H




class KIM_API_model;

namespace KIM_AUX{
    class Process_DE{
    public:
        Process_DE();
        static void init2zero(KIM_API_model *pkim,int *kimerr);
        static int process_dEdr(KIM_API_model **ppkim,double *de,
                                 double *r,double ** pdx,int *i,int *j);
        static int process_d2Edr2(KIM_API_model **ppkim,double *de,
                                 double **rr,double ** pdx,int **ii,int **jj);
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

#endif  /* KIMHDR_KIM_AUX_H */
