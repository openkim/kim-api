C******************************************************************************************************
C
C  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
C  All rights reserved.
C 
C  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
C 
C*****************************************************************************************************
C*****************************************
C  f77 SUBROUTINE for calculating simple lj potential
C
C
C*******************************************
       SUBROUTINE ljpotr_f77(sigma,epsilon,rr,v,dvmr)
        IMPLICIT NONE
        DOUBLE PRECISION rr,v,dvmr, epsilon,sigma,r1,sor2,sor6,sor12
        
        IF (rr.LT.1.0E-14) STOP 'rr is zero'
        sor2= (sigma*sigma)/rr
        sor6= sor2*sor2*sor2
        sor12= sor6*sor6
        v    = 4.d0*epsilon*(sor12 - sor6)
        dvmr = 24.d0*epsilon*(-2.d0*sor12 + sor6)/sqrt(rr)
        RETURN
        END

      SUBROUTINE calculate(cutoff,sigma,epsilon,pkim,x,f,ea,natom,en,
     &                     f_flag,e_flag, kimget_h_neigh,kimerr)
        IMPLICIT NONE
        DOUBLE PRECISION cutoff
        DOUBLE PRECISION sigma
        DOUBLE PRECISION epsilon
        DOUBLE PRECISION x(3,natom),f(3,natom),ea(natom)
        INTEGER natom,kimerr,f_flag,e_flag
        DOUBLE PRECISION en
        INTEGER kimget_h_neigh
        EXTERNAL kimget_h_neigh
C       local variable
        DOUBLE PRECISION vij,dvmr,v,sumv,cut2,energycutoff
        INTEGER i,j,jj,numnei
        DOUBLE PRECISION r2,dv
        DOUBLE PRECISION xi(3),xj(3),dx(3),fij(3)
C need check IF cray pointers are acceptable in f77, or use next line
        DOUBLE PRECISION Rij(3,512)
        INTEGER nei1atom(1), kim
        pointer (pnei1atom,nei1atom)
        pointer (pkim,kim)
        pointer (pRij,Rij)
        INTEGER retcode,mode,request,atom
        atom =0
        numnei=0
        IF(f_flag.eq.1) THEN
        DO 100 i=1,natom
                f(1,i)=0.0
                f(2,i)=0.0
                f(3,i)=0.0
 100    CONTINUE
        ENDIF
        IF(e_flag.eq.1) THEN
        DO 108 i=1,natom
                ea(i)=0.0
 108    CONTINUE
        ENDIF
        sumv=0  
        cut2=cutoff*cutoff
        CALL  ljpotr_f77(sigma,epsilon,cut2,energycutoff,dvmr)
c       iterator mode
        mode=0 
C       reset neighbor iterator to beginning
        request=0
        retcode =kimget_h_neigh(pkim,mode,request,atom,
     &                          numnei,pnei1atom,pRij)
        IF(retcode .NE. 2) THEN
         WRITE(*,'("model_Ne_pure_LJ_NEIGH_PURE_*.F error: ",I5)')
     &         retcode
         kimerr = retcode
         return
        ENDIF
        retcode=1
 101    CONTINUE
        IF (retcode .NE. 1) GOTO 103 
                !increment iterator
                mode=0; request=1;
                retcode = kimget_h_neigh(pkim,mode,request,
     &          atom,numnei,pnei1atom,pRij)
                IF(retcode.LT.0) THEN
                 WRITE(*,'("neigh iterator error:retcode",I5)'),retcode
                 kimerr=retcode
                 return
                ENDIF

                IF(retcode.EQ.0) THEN
                 kimerr=1
                 GOTO 103
                ENDIF
                i=atom
                xi(1) = x(1,i)
                xi(2) = x(2,i)
                xi(3) = x(3,i)
                DO 102 jj=1, numnei
                        j=nei1atom(jj)
                        xj(1) = x(1,j)
                        xj(2) = x(2,j)
                        xj(3) = x(3,j)
                        dx(1) = xj(1)-xi(1)
                        dx(2) = xj(2)-xi(2)
                        dx(3) = xj(3)-xi(3)
                        r2=dx(1)*dx(1) + dx(2)*dx(2) + dx(3)*dx(3)
                        IF (r2.LT.1.0E-14) THEN
                         WRITE(*,'("i=",I5,", j=",I5)') i,j
                        ENDIF
                        IF (r2.LE.cut2) THEN
                              CALL ljpotr_f77(sigma,epsilon,r2,vij,dvmr)
                              sumv = sumv + vij-energycutoff
                              IF(e_flag.eq.1) THEN
                                ea(i)=ea(i)+(vij-energycutoff)/2.0
                                ea(j)=ea(j)+(vij-energycutoff)/2.0
                              ENDIF
                              IF(f_flag.eq.1) THEN
                               f(1,i) =f(1,i) + dvmr*dx(1)
                               f(2,i) =f(2,i) + dvmr*dx(2)
                               f(3,i) =f(3,i) + dvmr*dx(3)

                               f(1,j) =f(1,j) - dvmr*dx(1)
                               f(2,j) =f(2,j) - dvmr*dx(2)
                               f(3,j) =f(3,j) - dvmr*dx(3)
                              ENDIF
                        ENDIF
 102            CONTINUE
        GOTO 101 
 103    CONTINUE
        en=sumv
        RETURN
      END
