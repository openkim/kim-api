C
C CDDL HEADER START
C
C The contents of this file are subject to the terms of the Common Development
C and Distribution License Version 1.0 (the "License").
C
C You can obtain a copy of the license at
C http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
C specific language governing permissions and limitations under the License.
C
C When distributing Covered Code, include this CDDL HEADER in each file and
C include the License file in a prominent location with the name LICENSE.CDDL.
C If applicable, add the following below this CDDL HEADER, with the fields
C enclosed by brackets "[]" replaced with your own identifying information:
C
C Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
C
C CDDL HEADER END
C

C
C Copyright (c) 2013--2018, Regents of the University of Minnesota.
C All rights reserved.
C
C Contributors:
C    Valeriu Smirichinski
C

C*******************************************************************************
C
C  Release: This file is part of the kim-api.git repository.
C
C*******************************************************************************

#include "KIM_API_status.h"

C*******************************************************************************
C
C  FORTRAN 77 SUBROUTINE for calculating Lennard-Jones potential
C
C*******************************************************************************
      SUBROUTINE ljpot_f77(sigma,epsilon,rr,v,dv)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE

C-------Transferred variables
      REAL(C_DOUBLE) sigma,epsilon,rr,v,dv

C-------Local variables
      REAL(C_DOUBLE) sor2,sor6,sor12

      IF (rr.LT.1.0E-14) STOP 'rr is zero'
      sor2  = (sigma*sigma)/rr
      sor6  = sor2*sor2*sor2
      sor12 = sor6*sor6
      v  = 4.D0*epsilon*(sor12 - sor6)
      dv = 24.D0*epsilon*(-2.d0*sor12 + sor6)/sqrt(rr)
      RETURN
      END

C*******************************************************************************
C
C  FORTRAN 77 SUBROUTINE to compute energy and forces on particles from the
C  positions.
C
C*******************************************************************************
      SUBROUTINE calculate(cutoff,sigma,epsilon,pkim,x,f,ea,npart,
     &                     ncontrib,en,e_flag,f_flag,eper_flag,
     &                     kimget_neigh,ier)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE

C-------Transferred variables
      REAL(C_DOUBLE) cutoff,sigma,epsilon
      type(C_PTR) pkim
      INTEGER(C_INT) npart,ier,e_flag,f_flag,eper_flag
      REAL(C_DOUBLE) x(3,npart),f(3,npart),ea(npart)
      REAL(C_DOUBLE) en
      INTEGER(C_INT) kimget_neigh
      EXTERNAL kimget_neigh

C-------Local variables
      REAL(C_DOUBLE) vij,dv,sumv,cut2,energycutoff
      INTEGER(C_INT) i,j,jj,numnei,ncontrib
      REAL(C_DOUBLE) r2
      REAL(C_DOUBLE) r
      REAL(C_DOUBLE) xi(3),xj(3),dx(3)
      REAL(C_DOUBLE) Rij(3,512)
      INTEGER(C_INT) nei1part(1)
      INTEGER(C_INT) retcode,mode,request,part

      part = 0
      numnei = 0
      IF (f_flag.EQ.1) THEN
         DO 100 i = 1,npart
            f(1,i) = 0.D0
            f(2,i) = 0.D0
            f(3,i) = 0.D0
 100     CONTINUE
      ENDIF
      IF (eper_flag.EQ.1) THEN
         DO 110 i = 1,npart
            ea(i) = 0.D0
 110     CONTINUE
      ENDIF
      sumv = 0.D0
      cut2 = cutoff*cutoff
      CALL ljpot_f77(sigma,epsilon,cut2,energycutoff,dv)

C     Iterator mode (reset iterator to beginning)
      mode = 0
      request = 0
      retcode=kimget_neigh(pkim,mode,request,part,
     &                     numnei,nei1part,Rij)
      IF (retcode.NE.KIM_STATUS_NEIGH_ITER_INIT_OK) THEN
         WRITE(*,'("model_Ne_pure_LJ_NEIGH_PURE_*.F error: ",I5)')
     &      retcode
         ier = retcode
         RETURN
      ENDIF
      retcode = KIM_STATUS_OK

C     Loop over particles and compute energy and forces
 120  CONTINUE
      IF (retcode.NE.KIM_STATUS_OK) GOTO 140

C     Increment iterator
      mode = 0
      request = 1
      retcode = kimget_neigh(pkim,mode,request,
     &                       part,numnei,nei1part,Rij)
      IF (retcode.EQ.KIM_STATUS_NEIGH_ITER_PAST_END) THEN
         ier = KIM_STATUS_OK
         GOTO 140
      ENDIF
      IF (retcode.LT.KIM_STATUS_OK) THEN
         WRITE(*,'("neigh iterator error:retcode",I5)') retcode
         ier = retcode
         RETURN
      ENDIF
      i = part
      xi(1) = x(1,i)
      xi(2) = x(2,i)
      xi(3) = x(3,i)
      DO 130 jj = 1,numnei
         j = nei1part(jj)
         xj(1) = x(1,j)
         xj(2) = x(2,j)
         xj(3) = x(3,j)
         dx(1) = xj(1)-xi(1)
         dx(2) = xj(2)-xi(2)
         dx(3) = xj(3)-xi(3)
         r2 = dx(1)*dx(1) + dx(2)*dx(2) + dx(3)*dx(3)
         IF (r2.LT.1.D-14)
     &      WRITE(*,'("i=",I5,", j=",I5)') i,j
         IF (r2.LE.cut2) THEN
            CALL ljpot_f77(sigma,epsilon,r2,vij,dv)
            IF (j.LE.ncontrib) THEN
               sumv = sumv + vij-energycutoff
            ELSE
               sumv = sumv + 0.5d0*(vij-energycutoff)
               dv = 0.5d0*dv;
            ENDIF
            IF (eper_flag.EQ.1) THEN
               ea(i)=ea(i)+(vij-energycutoff)/2.D0
               IF (j.LE.ncontrib) THEN
                  ea(j)=ea(j)+(vij-energycutoff)/2.D0
               ENDIF
            ENDIF
            IF (f_flag.EQ.1) THEN
               r = sqrt(r2)
               f(1,i) = f(1,i) + dv*dx(1)/r
               f(2,i) = f(2,i) + dv*dx(2)/r
               f(3,i) = f(3,i) + dv*dx(3)/r
               f(1,j) = f(1,j) - dv*dx(1)/r
               f(2,j) = f(2,j) - dv*dx(2)/r
               f(3,j) = f(3,j) - dv*dx(3)/r
            ENDIF
         ENDIF
 130  CONTINUE
      GOTO 120
 140  CONTINUE
      IF (e_flag.EQ.1) en=sumv
      RETURN
      END
