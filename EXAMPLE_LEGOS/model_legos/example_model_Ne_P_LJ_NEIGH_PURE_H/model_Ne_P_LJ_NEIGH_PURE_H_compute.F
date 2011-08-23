C*******************************************************************************
C
C  Release: This file is part of the openkim-api.git repository.
C
C  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
C  All rights reserved.
C
C  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
C
C*******************************************************************************


C*******************************************************************************
C
C  FORTRAN 77 SUBROUTINE for calculating Lennard-Jones potential
C
C*******************************************************************************
      SUBROUTINE ljpot_f77(sigma,epsilon,rr,v,dv)
      IMPLICIT NONE

C-------Transferred variables
      DOUBLE PRECISION sigma,epsilon,rr,v,dv

C-------Local variables
      DOUBLE PRECISION sor2,sor6,sor12
        
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
C  FORTRAN 77 SUBROUTINE to compute energy and forces on atoms from the
C  positions.
C
C*******************************************************************************
      SUBROUTINE calculate(cutoff,sigma,epsilon,pkim,x,f,ea,natom,en,
     &                     f_flag,e_flag,kimget_h_neigh,ier)
      IMPLICIT NONE

C-------Transferred variables
      DOUBLE PRECISION cutoff,sigma,epsilon
      INTEGER kim
      pointer (pkim,kim)
      DOUBLE PRECISION x(3,natom),f(3,natom),ea(natom)
      INTEGER natom,ier,f_flag,e_flag
      DOUBLE PRECISION en
      INTEGER kimget_h_neigh
      EXTERNAL kimget_h_neigh

C-------Local variables
      DOUBLE PRECISION vij,dv,sumv,cut2,energycutoff
      INTEGER i,j,jj,numnei
      DOUBLE PRECISION r2
      DOUBLE PRECISION r
      DOUBLE PRECISION xi(3),xj(3),dx(3)
      DOUBLE PRECISION Rij(3,512)
      pointer (pRij,Rij)
      INTEGER nei1atom(1)
      pointer (pnei1atom,nei1atom)
      INTEGER retcode,mode,request,atom

      atom = 0
      numnei = 0
      IF (f_flag.EQ.1) THEN
         DO 100 i = 1,natom
            f(1,i) = 0.D0
            f(2,i) = 0.D0
            f(3,i) = 0.D0
 100     CONTINUE
      ENDIF
      IF (e_flag.EQ.1) THEN
         DO 110 i = 1,natom
 110        ea(i) = 0.D0
      ENDIF
      sumv = 0.D0
      cut2 = cutoff*cutoff
      CALL ljpot_f77(sigma,epsilon,cut2,energycutoff,dv)

C     Iterator mode (reset iterator to beginning)
      mode = 0 
      request = 0
      retcode=kimget_h_neigh(pkim,mode,request,atom,
     &                       numnei,pnei1atom,pRij)
      IF (retcode.NE.2) THEN
         WRITE(*,'("model_Ne_pure_LJ_NEIGH_PURE_*.F error: ",I5)')
     &      retcode
         ier = retcode
         RETURN
      ENDIF
      retcode = 1

C     Loop over atoms and compute energy and forces
 120  CONTINUE
      IF (retcode.NE.1) GOTO 140 

C     Increment iterator
      mode = 0
      request = 1
      retcode = kimget_h_neigh(pkim,mode,request,
     &                         atom,numnei,pnei1atom,pRij)
      IF (retcode.LT.0) THEN
         WRITE(*,'("neigh iterator error:retcode",I5)'),retcode
         ier = retcode
         RETURN
      ENDIF
      IF (retcode.EQ.0) THEN
         ier = 1
         GOTO 140
      ENDIF
      i = atom
      xi(1) = x(1,i)
      xi(2) = x(2,i)
      xi(3) = x(3,i)
      DO 130 jj = 1,numnei
         j = nei1atom(jj)
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
            sumv = sumv + vij-energycutoff
            IF (e_flag.eq.1) THEN
               ea(i)=ea(i)+(vij-energycutoff)/2.D0
               ea(j)=ea(j)+(vij-energycutoff)/2.D0
            ENDIF
            IF (f_flag.eq.1) THEN
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
      en=sumv
      RETURN
      END
