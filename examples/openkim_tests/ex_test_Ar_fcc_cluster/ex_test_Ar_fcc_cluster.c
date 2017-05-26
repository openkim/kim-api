/*
 *
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License Version 1.0 (the "License").
 *
 * You can obtain a copy of the license at
 * http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each file and
 * include the License file in a prominent location with the name LICENSE.CDDL.
 * If applicable, add the following below this CDDL HEADER, with the fields
 * enclosed by brackets "[]" replaced with your own identifying information:
 *
 * Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
 *
 * CDDL HEADER END
 *

 *
 * Copyright (c) 2013--2017, Regents of the University of Minnesota.
 * All rights reserved.
 *
 * Contributors:
 *    Ryan S. Elliott
 *    Stephen M. Whalen
 *
 */

/*                                                                      */
/* Release: This file is part of the kim-api.git repository.            */
/*                                                                      */


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "KIM_LanguageName.h"
#include "KIM_SpeciesName.h"
#include "KIM_Numbering.h"
#include "KIM_Model.h"
#include "KIM_Attribute.h"
#include "KIM_ArgumentName.h"
#include "KIM_CallBackName.h"
#include "KIM_UnitSystem.h"

#define TRUE 1
#define FALSE 0

#define NAMESTRLEN    128

#define FCCSPACING    5.260
#define DIM           3
#define NCELLSPERSIDE 2
#define NCLUSTERPARTS (4*(NCELLSPERSIDE*NCELLSPERSIDE*NCELLSPERSIDE) + \
                       6*(NCELLSPERSIDE*NCELLSPERSIDE)                 \
                       + 3*(NCELLSPERSIDE) + 1)


#define MY_ERROR(message)                                       \
  {                                                             \
    printf("* Error : \"%s\" %d:%s\n", message,                 \
           __LINE__, __FILE__);                                 \
    exit(1);                                                    \
  }

#define MY_WARNING(message)                                             \
  {                                                                     \
    printf("* Error : \"%s\" %d:%s\n", message,                         \
           __LINE__, __FILE__);                                         \
  }


/* Define neighborlist structure */
typedef struct
{
  int numberOfParticles;
  int iteratorId;
  int* NNeighbors;
  int* neighborList;
} NeighList;

/* Define prototypes */
void fcc_cluster_neighborlist(int allOrOne, int numberOfParticles,
                              double* coords, double cutoff, NeighList* nl);

int get_cluster_neigh(
    void const * const dataObject,
    int const neighborListIndex,
    int const particleNumber, int * const numberOfNeighbors,
    int const ** const neighborsOfParticle);

void create_FCC_cluster(double FCCspacing, int nCellsPerSide, double *coords);


/* Main program */
int main()
{
  /* Local variable declarations */
  double const MinSpacing = 0.8*FCCSPACING;
  double const MaxSpacing = 1.2*FCCSPACING;
  double const SpacingIncr = 0.025*FCCSPACING;
  double CurrentSpacing;
  double cutpad = 0.75; /* Angstroms */
  int i;
  int error;


  /* KIM variable declarations */
  KIM_Model * model;
  /* model inputs */
  int numberOfParticles_cluster = NCLUSTERPARTS;
  int speciesIsSupported;
  int particleSpecies_cluster_model[NCLUSTERPARTS];
  int particleContributing_cluster_model[NCLUSTERPARTS];
  int particleContributing_one_atom_model[NCLUSTERPARTS];
  double coords_cluster[NCLUSTERPARTS][DIM];
  NeighList nl_cluster_model;
  /* model outputs */
  int number_of_cutoffs_cluster_model;
  double influence_distance_cluster_model;
  double const * cutoff_cluster_model;
  double energy_cluster_model = 0.0;
  double energy_one_atom_model = 0.0;
  double energy = 0.0;

  char modelname[NAMESTRLEN];
  int requestedUnitsAccepted;
  int modelArCode;
  int numberOfAPI_Arguments;
  KIM_ArgumentName argumentName;
  KIM_Attribute attribute;
  int numberOfAPI_CallBacks;
  KIM_CallBackName callBackName;



  /* Get KIM Model names */
  printf("Please enter valid KIM Model name: \n");
  error = scanf("%s", modelname);
  if (1 != error)
  {
    MY_ERROR("Unable to read model name");
  }

  /* initialize the model */
  error = KIM_Model_create(KIM_NUMBERING_zeroBased,
                           KIM_LENGTH_UNIT_A,
                           KIM_ENERGY_UNIT_eV,
                           KIM_CHARGE_UNIT_e,
                           KIM_TEMPERATURE_UNIT_K,
                           KIM_TIME_UNIT_ps,
                           modelname,
                           &requestedUnitsAccepted,
                           &model);
  if (error) MY_ERROR("KIM_create_model_interface()");

  /* Check for compatibility with the model */
  if (!requestedUnitsAccepted) MY_ERROR("Must adapt to model units");

  /* check species */
  error = KIM_Model_get_species_support_and_code(
      model, KIM_SPECIES_NAME_Ar, &speciesIsSupported, &modelArCode);
  if ((error) || (!speciesIsSupported))
  {
    MY_ERROR("Species Ar not supported");
  }

  /* check arguments */
  KIM_ARGUMENT_NAME_get_number_of_arguments(&numberOfAPI_Arguments);
  for (i=0; i<numberOfAPI_Arguments; ++i)
  {
    error = KIM_ARGUMENT_NAME_get_argument_name(i, &argumentName);
    if (error) MY_ERROR("can't get argument name");
    error = KIM_Model_get_argument_attribute(model, argumentName, &attribute);
    if (error) MY_ERROR("can't get argument attribute");

    /* can only handle energy as a required arg */
    if (KIM_AttributeEqual(attribute, KIM_ATTRIBUTE_required))
    {
      if (KIM_ArgumentNameNotEqual(argumentName, KIM_ARGUMENT_NAME_energy))
      {
        MY_ERROR("unsupported required argument");
      }
    }

    /* must have energy */
    if (KIM_ArgumentNameEqual(argumentName, KIM_ARGUMENT_NAME_energy))
    {
      if (! ((KIM_AttributeEqual(attribute, KIM_ATTRIBUTE_required))
             ||
             (KIM_AttributeEqual(attribute, KIM_ATTRIBUTE_optional))))
      {
        MY_ERROR("energy not available");
      }
    }
  }

  /* check call backs */
  KIM_CALL_BACK_NAME_get_number_of_call_backs(&numberOfAPI_CallBacks);
  for (i=0; i<numberOfAPI_CallBacks; ++i)
  {
    error = KIM_CALL_BACK_NAME_get_call_back_name(i, &callBackName);
    if (error) MY_ERROR("can't get call back name");
    error = KIM_Model_get_call_back_attribute(model, callBackName, &attribute);
    if (error) MY_ERROR("can't get call back attribute");

    /* cannot handle any "required" call backs */
    if (KIM_AttributeEqual(attribute, KIM_ATTRIBUTE_required))
    {
      MY_ERROR("unsupported required call back");
    }
  }

  /* We're compatible with the model.  Let's do it. */

  error =
      KIM_Model_set_data_int(model, KIM_ARGUMENT_NAME_numberOfParticles,
                             &numberOfParticles_cluster)
      ||
      KIM_Model_set_data_int(model, KIM_ARGUMENT_NAME_particleSpecies,
                             particleSpecies_cluster_model)
      ||
      KIM_Model_set_data_double(model, KIM_ARGUMENT_NAME_coordinates,
                                (double*) &coords_cluster)
      ||
      KIM_Model_set_data_double(model, KIM_ARGUMENT_NAME_energy, &energy);
  if (error) MY_ERROR("KIM_setm_data");
  KIM_Model_set_call_back(model,
                          KIM_CALL_BACK_NAME_get_neigh,
                          KIM_LANGUAGE_NAME_C,
                          (func *) &get_cluster_neigh,
                          &nl_cluster_model);

  KIM_Model_get_influence_distance(model, &influence_distance_cluster_model);
  KIM_Model_get_cutoffs(model, &number_of_cutoffs_cluster_model, &cutoff_cluster_model);
  if (number_of_cutoffs_cluster_model != 1) MY_ERROR("too many cutoffs");

  /* setup particleSpecies */
  error = KIM_Model_get_species_support_and_code(
      model,
      KIM_SPECIES_NAME_Ar,
      &speciesIsSupported,
      &(particleSpecies_cluster_model[0]));
  if (error) MY_ERROR("KIM_get_species_code");
  for (i = 1; i < NCLUSTERPARTS; ++i)
    particleSpecies_cluster_model[i] = particleSpecies_cluster_model[0];

  /* setup particleContributing */
  for (i = 0; i < NCLUSTERPARTS; ++i)
  {
    particleContributing_cluster_model[i] = 1;  /* all particles contribute */
    particleContributing_one_atom_model[i] = 0;  /* none (but 30 contribute */
  }
  particleContributing_one_atom_model[30] = 1;

  /* setup neighbor lists */
  /* allocate memory for list */
  nl_cluster_model.numberOfParticles = NCLUSTERPARTS;
  nl_cluster_model.NNeighbors = (int*) malloc(NCLUSTERPARTS*sizeof(int));
  if (NULL==nl_cluster_model.NNeighbors) MY_ERROR("malloc unsuccessful");

  nl_cluster_model.neighborList = (int*) malloc(NCLUSTERPARTS*NCLUSTERPARTS*sizeof(int));
  if (NULL==nl_cluster_model.neighborList) MY_ERROR("malloc unsuccessful");

  /* ready to compute */
  printf("--------------------------------------------------------------------------------\n");
  printf("This is Test  : ex_test_Ar_fcc_cluster\n");
  printf("MODEL is : %s\n",   modelname);

  printf("There are %i particles.\n", NCLUSTERPARTS);
  printf("%20s, %20s, %20s\n", "Cluster Energy", "One Atom Energy", "Current Spacing");
  for (CurrentSpacing = MinSpacing; CurrentSpacing < MaxSpacing; CurrentSpacing += SpacingIncr)
  {
    /* update coordinates for cluster */
    create_FCC_cluster(CurrentSpacing, NCELLSPERSIDE, &(coords_cluster[0][0]));
    /* compute neighbor lists */
    fcc_cluster_neighborlist(-1, NCLUSTERPARTS, &(coords_cluster[0][0]),
                             (*cutoff_cluster_model + cutpad), &nl_cluster_model);

    /* call compute functions */
    error = KIM_Model_set_data_int(model, KIM_ARGUMENT_NAME_particleContributing, particleContributing_cluster_model);
    error = error || KIM_Model_compute(model);
    if (error) MY_ERROR("KIM_model_compute");
    energy_cluster_model = energy;

    error = KIM_Model_set_data_int(model, KIM_ARGUMENT_NAME_particleContributing, particleContributing_one_atom_model);
    error = error || KIM_Model_compute(model);
    if (error) MY_ERROR("KIM_model_compute");
    energy_one_atom_model = energy;

    /* print the results */
    printf("%20.10e, %20.10e, %20.10e\n",
           energy_cluster_model,
           energy_one_atom_model,
           CurrentSpacing);
  }


  /* free memory of neighbor lists */
  free(nl_cluster_model.NNeighbors);
  free(nl_cluster_model.neighborList);

  /* free pkim objects */
  KIM_Model_destroy(&model);

  /* everything is great */
  return 0;
}

void create_FCC_cluster(double FCCspacing, int nCellsPerSide, double *coords)
{
  /* local variables */
  double FCCshifts[4][DIM];
  double latVec[DIM];
  int a;
  int i;
  int j;
  int k;
  int m;
  int n;

  /* create a cubic FCC cluster of parts */
  FCCshifts[0][0] = 0.0;            FCCshifts[0][1] = 0.0;            FCCshifts[0][2] = 0.0;
  FCCshifts[1][0] = 0.5*FCCspacing; FCCshifts[1][1] = 0.5*FCCspacing; FCCshifts[1][2] = 0.0;
  FCCshifts[2][0] = 0.5*FCCspacing; FCCshifts[2][1] = 0.0;            FCCshifts[2][2] = 0.5*FCCspacing;
  FCCshifts[3][0] = 0.0;            FCCshifts[3][1] = 0.5*FCCspacing; FCCshifts[3][2] = 0.5*FCCspacing;

  a = 0;
  for (i = 0; i < nCellsPerSide; ++i)
  {
    latVec[0] = ((double) i)*FCCspacing;
    for (j = 0; j < nCellsPerSide; ++j)
    {
      latVec[1] = ((double) j)*FCCspacing;
      for (k = 0; k < nCellsPerSide; ++k)
      {
        latVec[2] = ((double) k)*FCCspacing;
        for (m = 0; m < 4; ++m)
        {
          for (n = 0; n < DIM; ++n)
          {
            coords[a*DIM + n] = latVec[n] + FCCshifts[m][n];
          }
          a++;
        }
      }
      /* add in the remaining three faces */
      /* pos-x face */
      latVec[0] = NCELLSPERSIDE*FCCspacing;
      latVec[1] = ((double) i)*FCCspacing;
      latVec[2] = ((double) j)*FCCspacing;
      for (n = 0; n < DIM; ++n)
      {
        coords[a*DIM + n] = latVec[n];
      }
      a++;
      for (n = 0; n < DIM; ++n)
      {
        coords[a*DIM + n] = latVec[n] + FCCshifts[3][n];
      }
      a++;
      /* pos-y face */
      latVec[0] = ((double) i)*FCCspacing;
      latVec[1] = NCELLSPERSIDE*FCCspacing;
      latVec[2] = ((double) j)*FCCspacing;
      for (n = 0; n < DIM; ++n)
      {
        coords[a*DIM + n] = latVec[n];
      }
      a++;
      for (n = 0; n < DIM; ++n)
      {
        coords[a*DIM + n] = latVec[n] + FCCshifts[2][n];
      }
      a++;
      /* pos-z face */
      latVec[0] = ((double) i)*FCCspacing;
      latVec[1] = ((double) j)*FCCspacing;
      latVec[2] = NCELLSPERSIDE*FCCspacing;
      for (n = 0; n < DIM; ++n)
      {
        coords[a*DIM + n] = latVec[n];
      }
      a++;
      for (n = 0; n < DIM; ++n)
      {
        coords[a*DIM + n] = latVec[n] + FCCshifts[1][n];
      }
      a++;
    }
    /* add in the remaining three edges */
    latVec[0] = ((double) i)*FCCspacing;
    latVec[1] = NCELLSPERSIDE*FCCspacing;
    latVec[2] = NCELLSPERSIDE*FCCspacing;
    for (n = 0; n < DIM; ++n)
    {
      coords[a*DIM + n] = latVec[n];
    }
    a++;
    latVec[0] = NCELLSPERSIDE*FCCspacing;
    latVec[1] = ((double) i)*FCCspacing;
    latVec[2] = NCELLSPERSIDE*FCCspacing;
    for (n = 0; n < DIM; ++n)
    {
      coords[a*DIM + n] = latVec[n];
    }
    a++;
    latVec[0] = NCELLSPERSIDE*FCCspacing;
    latVec[1] = NCELLSPERSIDE*FCCspacing;
    latVec[2] = ((double) i)*FCCspacing;
    for (n = 0; n < DIM; ++n)
    {
      coords[a*DIM + n] = latVec[n];
    }
    a++;
  }
  /* add in the remaining corner */
  for (n = 0; n < DIM; ++n)
  {
    coords[a*DIM + n] = NCELLSPERSIDE*FCCspacing;
  }
  a++;

  return;
}


void fcc_cluster_neighborlist(int allOrOne, int numberOfParticles, double* coords,
                              double cutoff, NeighList* nl)
{
  /* local variables */
  int i;
  int j;
  int k;
  int a;

  double dx[DIM];
  double r2;
  double cutoff2;

  cutoff2 = cutoff*cutoff;
  for (i = 0; i < numberOfParticles; ++i)
  {
    a = 0;
    if ((allOrOne == i) || (allOrOne < 0))
    {
      for (j = 0; j < numberOfParticles; ++j)
      {
        r2 = 0.0;
        for (k = 0; k < DIM; ++k)
        {
          dx[k] = coords[j*DIM + k] - coords[i*DIM + k];
          r2 += dx[k]*dx[k];
        }

        if ((r2 < cutoff2) && (i != j))
        {
          /* part j is a neighbor of part i */
          (*nl).neighborList[i*NCLUSTERPARTS + a] = j;
          a++;
        }
      }
    }
    /* part i has `a' neighbors */
    (*nl).NNeighbors[i] = a;
  }

  return;
}

int get_cluster_neigh(void const * const dataObject,
                      int const neighborListIndex,
                      int const particleNumber, int * const numberOfNeighbors,
                      int const ** const neighborsOfParticle)
{
  /* local variables */
  int error = TRUE;
  NeighList* nl = (NeighList*) dataObject;
  int numberOfParticles = nl->numberOfParticles;

  if (neighborListIndex != 0) return error;

  /* initialize numNeigh */
  *numberOfNeighbors = 0;

  if ((particleNumber >= numberOfParticles) || (particleNumber < 0)) /* invalid id */
  {
    MY_WARNING("Invalid part ID in get_cluster_neigh");
    return TRUE;
  }

  /* set the returned number of neighbors for the returned part */
  *numberOfNeighbors = (*nl).NNeighbors[particleNumber];

  /* set the location for the returned neighbor list */
  *neighborsOfParticle = &((*nl).neighborList[(particleNumber)*numberOfParticles]);

  return FALSE;
}
