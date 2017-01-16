//                                                                              //123
// CDDL HEADER START                                                            //123
//                                                                              //123
// The contents of this file are subject to the terms of the Common Development //123
// and Distribution License Version 1.0 (the "License").                        //123
//                                                                              //123
// You can obtain a copy of the license at                                      //123
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the        //123
// specific language governing permissions and limitations under the License.   //123
//                                                                              //123
// When distributing Covered Code, include this CDDL HEADER in each file and    //123
// include the License file in a prominent location with the name LICENSE.CDDL. //123
// If applicable, add the following below this CDDL HEADER, with the fields     //123
// enclosed by brackets "[]" replaced with your own identifying information:    //123
//                                                                              //123
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.//123
//                                                                              //123
// CDDL HEADER END                                                              //123
//                                                                              //123
//                                                                              //123
//                                                                              //123
// Copyright (c) 2012--2017, Regents of the University of Minnesota.            //123
// All rights reserved.                                                         //123
//                                                                              //123
// Contributors:                                                                //123
//    Ryan S. Elliott                                                           //123
//                                                                              //123
                                                                                //123
//******************************************************************************//123
//                                                                              //123
// TEST_NAME_STR                                                                //123
//                                                                              //123
// Test for Ar to compute properties of an fcc cluster                          //123
//                                                                              //123
// Language: C++                                                                //123
//                                                                              //123
// Release: This file is part of the kim-api.git repository.                    //123
//                                                                              //123
//******************************************************************************//123
                                                                                //123
#include <cstdlib>                                                              //123
#include <iostream>                                                             //123
#include <iomanip>                                                              //123
#include "KIM_API.h"                                                            //123
#include "KIM_API_status.h"                                                     //123
                                                                                //123
#define WIDTH 25                                                                //123
#define PRECISION  15                                                           //123
                                                                                //123
#define FCCSPACING    5.260                                                     //123
#define NCELLSPERSIDE 2                                                         //123
#define DIM           3                                                         //123
#define SPECIESTYPES  1                                                         //123
#define NCLUSTERPARTICLES (4*(NCELLSPERSIDE*NCELLSPERSIDE*NCELLSPERSIDE) \      //123
                           + 6*(NCELLSPERSIDE*NCELLSPERSIDE)             \      //123
                           + 3*(NCELLSPERSIDE) + 1)                             //123
                                                                                //123
using namespace std;                                                            //123
                                                                                //123
// Define prototypes                                                            //123
static void create_FCC_configuration(                                           //123
    const double FCCspacing, const int nCellsPerSide, const int periodic,       //123
    double* const coords, int* const MiddleParticleId);                         //123
                                                                                //123
                                                                                //123
// Main program                                                                 //123
int main(int argc, char* argv[])                                                //123
{                                                                               //123
  // Get KIM Model name to use                                                  //123
  cout << "Please enter a valid KIM model name: " << endl;                      //123
  char modelname[1024];                                                         //123
  cin >> modelname;                                                             //123
                                                                                //123
  // Initialize the KIM Model                                                   //123
  KIM_API_model KIM_API_object;                                                 //123
  int status = KIM_API_object.file_init("descriptor.kim", modelname);           //123
  if (KIM_STATUS_OK > status)                                                   //123
  {                                                                             //123
    KIM_API_object.report_error(__LINE__, __FILE__,                             //123
                                "KIM_API_file_init", status);                   //123
    exit(1);                                                                    //123
  }                                                                             //123
                                                                                //123
  // Allocate memory via the KIM system                                         //123
  KIM_API_object.allocate(NCLUSTERPARTICLES, SPECIESTYPES, &status);            //123
  if (KIM_STATUS_OK > status)                                                   //123
  {                                                                             //123
    KIM_API_object.report_error(__LINE__, __FILE__, "KIM_API_allocate", status);//123
    exit(1);                                                                    //123
  }                                                                             //123
                                                                                //123
  // call Model's init routine                                                  //123
  status = KIM_API_object.model_init();                                         //123
  if (KIM_STATUS_OK > status)                                                   //123
  {                                                                             //123
    KIM_API_object.report_error(__LINE__, __FILE__,                             //123
                                "KIM_API_model_init", status);                  //123
    exit(1);                                                                    //123
  }                                                                             //123
                                                                                //123
  // Unpack data from KIM object                                                //123
  //                                                                            //123
  // model inputs                                                               //123
  int* numberOfParticles;                                                       //123
  int* numberOfSpecies;                                                         //123
  int* particleSpecies;                                                         //123
  double* coords;                                                               //123
  // model outputs                                                              //123
  double* cutoff;                                                               //123
  double* energy;                                                               //123
  double* forces;                                                               //.23
  double* particleEnergy;                                                       //.23
  double* virial;                                                               //..3
  double* particleVirial;                                                       //..3
  double* hessian;                                                              //..3
  //                                                                            //123
  KIM_API_object.getm_data(&status, 11*3,                                       //..3
  KIM_API_object.getm_data(&status, 8*3,                                        //.2.
  KIM_API_object.getm_data(&status, 6*3,                                        //1..
                           "numberOfParticles", &numberOfParticles,   1,        //123
                           "numberOfSpecies",   &numberOfSpecies,     1,        //123
                           "particleSpecies",   &particleSpecies,     1,        //123
                           "coordinates",       &coords,              1,        //123
                           "cutoff",            &cutoff,              1,        //123
                           "energy",            &energy,              1,        //.23
                           "energy",            &energy,              1);       //1..
                           "forces",            &forces,              1,        //.23
                           "particleEnergy",    &particleEnergy,      1,        //..3
                           "particleEnergy",    &particleEnergy,      1);       //.2.
                           "virial",            &virial,              1,        //..3
                           "particleVirial",    &particleVirial,      1,        //..3
                           "hessian",           &hessian,             1);       //..3
  if (KIM_STATUS_OK > status)                                                   //123
  {                                                                             //123
    KIM_API_object.report_error(__LINE__, __FILE__,                             //123
                                "KIM_API_getm_data", status);                   //123
    exit(1);                                                                    //123
  }                                                                             //123
                                                                                //123
  // Set values                                                                 //123
  *numberOfParticles = NCLUSTERPARTICLES;                                       //123
  *numberOfSpecies = SPECIESTYPES;                                              //123
  const int species_code = KIM_API_object.get_species_code("Ar", &status);      //123
  if (KIM_STATUS_OK > status)                                                   //123
  {                                                                             //123
    KIM_API_object.report_error(__LINE__, __FILE__,                             //123
                                "KIM_API_get_species_code", status);            //123
    exit(1);                                                                    //123
  }                                                                             //123
  for (int i = 0; i < *numberOfParticles; ++i)                                  //123
  {                                                                             //123
    particleSpecies[i] = species_code;                                          //123
  }                                                                             //123
                                                                                //123
  // set up the cluster particle positions                                      //123
  int middleDum;                                                                //123
  create_FCC_configuration(FCCSPACING, NCELLSPERSIDE, 0, coords, &middleDum);   //123
                                                                                //123
  // Call model compute                                                         //123
  status = KIM_API_object.model_compute();                                      //123
  if (KIM_STATUS_OK > status)                                                   //123
  {                                                                             //123
    KIM_API_object.report_error(__LINE__, __FILE__,                             //123
                                "KIM_API_model_compute", status);               //123
    exit(1);                                                                    //123
  }                                                                             //123
                                                                                //123
  // print results to screen                                                    //123
  cout << setiosflags(ios::scientific) << setprecision(PRECISION);              //123
  cout << "-----------------------------------------------"                     //123
      "---------------------------------\n";                                    //123
  cout << "This is Test          : TEST_NAME_STR" << endl;                      //123
  cout << "Results for KIM Model : " << modelname << endl;                      //123
  cout << "Energy        = " << setw(WIDTH) << *energy << endl;                 //123
  cout << "Virial        = "                                                    //..3
       << setw(WIDTH) << virial[0]                                              //..3
       << setw(WIDTH) << virial[1]                                              //..3
       << setw(WIDTH) << virial[2]                                              //..3
       << endl << "                "                                            //..3
       << setw(WIDTH) << virial[3]                                              //..3
       << setw(WIDTH) << virial[4]                                              //..3
       << setw(WIDTH) << virial[5]                                              //..3
       << endl;                                                                 //..3
  cout << endl << "Here we will only print data for the first 10 particles..."  //.23
       << endl;                                                                 //.23
  for (int i = 0; i < 10; ++i)                                                  //.23
  {                                                                             //.23
    cout << "Particle " << setw(3) << i << endl                                 //.23
         << "  Energy = " << setw(WIDTH) << particleEnergy[i] << endl           //.23
         << "  Force  = "                                                       //.23
         << setw(WIDTH) << forces[i*DIM + 0]                                    //.23
         << setw(WIDTH) << forces[i*DIM + 1]                                    //.23
         << setw(WIDTH) << forces[i*DIM + 2]                                    //.23
         << endl                                                                //..3
         << "  Virial = "                                                       //..3
         << setw(WIDTH) << particleVirial[i*6 + 0]                              //..3
         << setw(WIDTH) << particleVirial[i*6 + 1]                              //..3
         << setw(WIDTH) << particleVirial[i*6 + 2]                              //..3
         << endl                                                                //..3
         << "           "                                                       //..3
         << setw(WIDTH) << particleVirial[i*6 + 3]                              //..3
         << setw(WIDTH) << particleVirial[i*6 + 4]                              //..3
         << setw(WIDTH) << particleVirial[i*6 + 5]                              //..3
         << endl;                                                               //.23
  }                                                                             //.23
  cout << endl                                                                  //..3
       << "Here we will only print data for the first 3"                        //..3
      " particle interactions..."                                               //..3
       << endl;                                                                 //..3
  for (int i = 0; i < 3; ++i)                                                   //..3
  {                                                                             //..3
    for (int j = 0; j < 3; ++j)                                                 //..3
    {                                                                           //..3
      cout << "Hessian[" << i << "][" << j << "][0..2][0..2] = " << endl;       //..3
      for (int k = 0; k < DIM; ++k)                                             //..3
      {                                                                         //..3
        for (int m = 0; m < DIM; ++m)                                           //..3
        {                                                                       //..3
          cout << setw(WIDTH) << hessian[i*(*numberOfParticles)*DIM*DIM +       //..3
                                         j*DIM*DIM + k*DIM + m];                //..3
        }                                                                       //..3
        cout << endl;                                                           //..3
      }                                                                         //..3
    }                                                                           //..3
  }                                                                             //..3
                                                                                //123
  /* don't forget to destroy and deallocate */                                  //123
  status = KIM_API_object.model_destroy();                                      //123
  if (KIM_STATUS_OK > status)                                                   //123
  {                                                                             //123
    KIM_API_object.report_error(__LINE__, __FILE__,                             //123
                                "KIM_API_model_destory", status);               //123
    exit(1);                                                                    //123
  }                                                                             //123
  KIM_API_object.free(&status);                                                 //123
                                                                                //123
  // everything is great                                                        //123
  return 0;                                                                     //123
}                                                                               //123
                                                                                //123
//***************************************************************************** //123
//                                                                              //123
// create_FCC_configuration function                                            //123
//                                                                              //123
//  creates a cubic configuration of FCC particles with lattice spacing         //123
//  `FCCspacing' and `nCellsPerSide' cells along each direction.                //123
//                                                                              //123
//  With periodic==0 this will result in a total number of particles equal to   //123
//  4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1         //123
//                                                                              //123
//  With periodic==1 this will result in a total number of particles equal to   //123
//  4*(nCellsPerSide)**3                                                        //123
//                                                                              //123
//  Returns the Id of the particle situated in the middle of the configuration  //123
//  (this particle will have the most neighbors).                               //123
//                                                                              //123
//******************************************************************************//123
static void create_FCC_configuration(                                           //123
    const double FCCspacing, const int nCellsPerSide, const int periodic,       //123
    double* const coords, int* const MiddleParticleId)                          //123
{                                                                               //123
  // local variables                                                            //123
  double FCCshifts[4][3];                                                       //123
  double latVec[3];                                                             //123
  int a;                                                                        //123
  int i;                                                                        //123
  int j;                                                                        //123
  int k;                                                                        //123
  int m;                                                                        //123
  int n;                                                                        //123
                                                                                //123
  // create a cubic FCC cluster                                                 //123
  FCCshifts[0][0] = 0.0;                                                        //123
  FCCshifts[0][1] = 0.0;                                                        //123
  FCCshifts[0][2] = 0.0;                                                        //123
                                                                                //123
  FCCshifts[1][0] = 0.5*FCCspacing;                                             //123
  FCCshifts[1][1] = 0.5*FCCspacing;                                             //123
  FCCshifts[1][2] = 0.0;                                                        //123
                                                                                //123
  FCCshifts[2][0] = 0.5*FCCspacing;                                             //123
  FCCshifts[2][1] = 0.0;                                                        //123
  FCCshifts[2][2] = 0.5*FCCspacing;                                             //123
                                                                                //123
  FCCshifts[3][0] = 0.0;                                                        //123
  FCCshifts[3][1] = 0.5*FCCspacing;                                             //123
  FCCshifts[3][2] = 0.5*FCCspacing;                                             //123
                                                                                //123
  *MiddleParticleId = 0; // Always put middle particle as #0                    //123
  a = 1;  // leave space for middle particle as particle #0                     //123
  for (i = 0; i < nCellsPerSide; ++i)                                           //123
  {                                                                             //123
    latVec[0] = ((double) i)*FCCspacing;                                        //123
    for (j = 0; j < nCellsPerSide; ++j)                                         //123
    {                                                                           //123
      latVec[1] = ((double) j)*FCCspacing;                                      //123
      for (k = 0; k < nCellsPerSide; ++k)                                       //123
      {                                                                         //123
        latVec[2] = ((double) k)*FCCspacing;                                    //123
        for (m = 0; m < 4; ++m)                                                 //123
        {                                                                       //123
          for (n = 0; n < DIM; ++n)                                             //123
          {                                                                     //123
            coords[a*DIM + n] = latVec[n] + FCCshifts[m][n];                    //123
          }                                                                     //123
          if ((nCellsPerSide/2 == i) && (nCellsPerSide/2 == j) &&               //123
              (nCellsPerSide/2 == k) && (1 == m))                               //123
          {                                                                     //123
            // put middle particle as particle #0                               //123
            for (n = 0; n < DIM; ++n)                                           //123
            {                                                                   //123
              coords[n] = latVec[n] + FCCshifts[m][n];                          //123
            }                                                                   //123
            a--;                                                                //123
          }                                                                     //123
          a++;                                                                  //123
        }                                                                       //123
      }                                                                         //123
      if (!periodic)                                                            //123
      {                                                                         //123
        // add in the remaining three faces                                     //123
        // pos-x face                                                           //123
        latVec[0] = NCELLSPERSIDE*FCCspacing;                                   //123
        latVec[1] = ((double) i)*FCCspacing;                                    //123
        latVec[2] = ((double) j)*FCCspacing;                                    //123
        for (n = 0; n < DIM; ++n)                                               //123
        {                                                                       //123
          coords[a*DIM + n] = latVec[n];                                        //123
        }                                                                       //123
        a++;                                                                    //123
        for (n = 0; n < DIM; ++n)                                               //123
        {                                                                       //123
          coords[a*DIM + n] = latVec[n] + FCCshifts[3][n];                      //123
        }                                                                       //123
        a++;                                                                    //123
        // pos-y face                                                           //123
        latVec[0] = ((double) i)*FCCspacing;                                    //123
        latVec[1] = NCELLSPERSIDE*FCCspacing;                                   //123
        latVec[2] = ((double) j)*FCCspacing;                                    //123
        for (n = 0; n < DIM; ++n)                                               //123
        {                                                                       //123
          coords[a*DIM + n] = latVec[n];                                        //123
        }                                                                       //123
        a++;                                                                    //123
        for (n = 0; n < DIM; ++n)                                               //123
        {                                                                       //123
          coords[a*DIM + n] = latVec[n] + FCCshifts[2][n];                      //123
        }                                                                       //123
        a++;                                                                    //123
        // pos-z face                                                           //123
        latVec[0] = ((double) i)*FCCspacing;                                    //123
        latVec[1] = ((double) j)*FCCspacing;                                    //123
        latVec[2] = NCELLSPERSIDE*FCCspacing;                                   //123
        for (n = 0; n < DIM; ++n)                                               //123
        {                                                                       //123
          coords[a*DIM + n] = latVec[n];                                        //123
        }                                                                       //123
        a++;                                                                    //123
        for (n = 0; n < DIM; ++n)                                               //123
        {                                                                       //123
          coords[a*DIM + n] = latVec[n] + FCCshifts[1][n];                      //123
        }                                                                       //123
        a++;                                                                    //123
      }                                                                         //123
    }                                                                           //123
    if (!periodic)                                                              //123
    {                                                                           //123
      // add in the remaining three edges                                       //123
      latVec[0] = ((double) i)*FCCspacing;                                      //123
      latVec[1] = NCELLSPERSIDE*FCCspacing;                                     //123
      latVec[2] = NCELLSPERSIDE*FCCspacing;                                     //123
      for (n = 0; n < DIM; ++n)                                                 //123
      {                                                                         //123
        coords[a*DIM + n] = latVec[n];                                          //123
      }                                                                         //123
      a++;                                                                      //123
      latVec[0] = NCELLSPERSIDE*FCCspacing;                                     //123
      latVec[1] = ((double) i)*FCCspacing;                                      //123
      latVec[2] = NCELLSPERSIDE*FCCspacing;                                     //123
      for (n = 0; n < DIM; ++n)                                                 //123
      {                                                                         //123
        coords[a*DIM + n] = latVec[n];                                          //123
      }                                                                         //123
      a++;                                                                      //123
      latVec[0] = NCELLSPERSIDE*FCCspacing;                                     //123
      latVec[1] = NCELLSPERSIDE*FCCspacing;                                     //123
      latVec[2] = ((double) i)*FCCspacing;                                      //123
      for (n = 0; n < DIM; ++n)                                                 //123
      {                                                                         //123
        coords[a*DIM + n] = latVec[n];                                          //123
      }                                                                         //123
      a++;                                                                      //123
    }                                                                           //123
  }                                                                             //123
  if (!periodic)                                                                //123
  {                                                                             //123
    // add in the remaining corner                                              //123
    for (n = 0; n < DIM; ++n)                                                   //123
    {                                                                           //123
      coords[a*DIM + n] = NCELLSPERSIDE*FCCspacing;                             //123
    }                                                                           //123
    a++;                                                                        //123
  }                                                                             //123
                                                                                //123
  return;                                                                       //123
}                                                                               //123
