/******************************************************************************
 *
 * create_FCC_configuration function
 *
 *  creates a cubic configuration of FCC particles with lattice spacing
 *  `FCCspacing' and `nCellsPerSide' cells along each direction.
 *
 *  With periodic==0. this will result in a total number of particles equal to
 *  4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
 *
 *  With periodic==1 this will result in a total number of particles equal to
 *  4*(nCellsPerSide)**3
 *
 *  Returns the Id of the particle situated in the middle of the configuration
 *  (this particle will have the most neighbors.)
 *
 ******************************************************************************/
static void create_FCC_configuration(double FCCspacing, int nCellsPerSide, int periodic,
                                     double *coords, int* MiddlePartId)
{
   /* local variables */
   double FCCshifts[4][3];
   double latVec[3];
   int a;
   int i;
   int j;
   int k;
   int m;
   int n;

   /* create a cubic FCC cluster */
   FCCshifts[0][0] = 0.0;            FCCshifts[0][1] = 0.0;            FCCshifts[0][2] = 0.0;
   FCCshifts[1][0] = 0.5*FCCspacing; FCCshifts[1][1] = 0.5*FCCspacing; FCCshifts[1][2] = 0.0;
   FCCshifts[2][0] = 0.5*FCCspacing; FCCshifts[2][1] = 0.0;            FCCshifts[2][2] = 0.5*FCCspacing;
   FCCshifts[3][0] = 0.0;            FCCshifts[3][1] = 0.5*FCCspacing; FCCshifts[3][2] = 0.5*FCCspacing;

   *MiddlePartId = 0; /* Always put middle particle as #0 */
   a = 1;            /* leave space for middle particle as particle #0 */
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
               if ((nCellsPerSide/2 == i) && (nCellsPerSide/2 == j) &&
                   (nCellsPerSide/2 == k) && (1 == m))
               {
                  /* put middle particle as particle #0 */
                  for (n = 0; n < DIM; ++n)
                  {
                     coords[n] = latVec[n] + FCCshifts[m][n];
                  }
                  a--;
               }
               a++;
            }
         }
         if (!periodic)
         {
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
      }
      if (!periodic)
      {
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
   }
   if (!periodic)
   {
      /* add in the remaining corner */
      for (n = 0; n < DIM; ++n)
      {
         coords[a*DIM + n] = NCELLSPERSIDE*FCCspacing;
      }
      a++;
   }

   return;
}
