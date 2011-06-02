C*****************************************
C  f77 SUBROUTINE for calculating simple lj potential
C
C
C*******************************************
       SUBROUTINE ljpotr_f77(rr,v,dvmr)
	IMPLICIT NONE
	DOUBLE PRECISION rr,v,dvmr, a,b,r1,r2,rm6,rm8
	
c	LJ parameters	
	a=0.0002441
	b=0.03125
	IF (rr.LT. 1.0E-14) STOP 'rr is zero'
	r2=rr
	rm6=1.0/(r2*r2*r2)
	rm8=rm6/r2
	v=(a*rm6 - b)*rm6
	dvmr = 6.0*rm8*(-2*rm6*a + b)
	RETURN
	END

      SUBROUTINE LJ_f77_calculate(pkim,x,f,natom,en,cutof,
     &                             kimget_h_neigh,kimerr)
     	IMPLICIT NONE
      	DOUBLE PRECISION x(3,natom),f(3,natom)
	INTEGER natom,kimerr
	DOUBLE PRECISION en,cutof
	INTEGER kimget_h_neigh
	EXTERNAL kimget_h_neigh
C       local variable
	DOUBLE PRECISION vij,dvmr,v,sumv,cut2,energycutof
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
	DO 100 i=1,natom
		f(1,i)=0.0
		f(2,i)=0.0
		f(3,i)=0.0
 100	CONTINUE
	sumv=0	
	cut2=cutof*cutof
	CALL  ljpotr_f77(cut2,energycutof,dvmr)
c	iterator mode
	mode=0 
C       reset neighbor iterator to beginning
	request=0
	retcode =kimget_h_neigh(pkim,mode,request,atom,
     &					numnei,pnei1atom,pRij)
	IF(retcode .NE. 2) THEN
	 WRITE(*,'("sample_01_lj_cutoff_f77_calculate:",I5)') retcode
	 kimerr = retcode
	 return
	ENDIF
	retcode=1
 101	CONTINUE
	IF (retcode .NE. 1) GOTO 103 
		!increment iterator
		mode=0; request=1;
      		retcode = kimget_h_neigh(pkim,mode,request,
     & 		atom,numnei,pnei1atom,pRij)
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
			dx(1) = xi(1)-xj(1)
			dx(2) = xi(2)-xj(2)
			dx(3) = xi(3)-xj(3)
			r2=dx(1)*dx(1) + dx(2)*dx(2) + dx(3)*dx(3)
			IF (r2.LT.1.0E-14) THEN
			 WRITE(*,'("i=",I5,", j=",I5)') i,j
			ENDIF
			IF (r2.LE.cut2) THEN
				CALL ljpotr_f77(r2,vij,dvmr)
				sumv = sumv + vij-energycutof
				f(1,i) =f(1,i) - dvmr*dx(1)
 				f(2,i) =f(2,i) - dvmr*dx(2)
				f(3,i) =f(3,i) - dvmr*dx(3)

				f(1,j) =f(1,j) + dvmr*dx(1)
 				f(2,j) =f(2,j) + dvmr*dx(2)
				f(3,j) =f(3,j) + dvmr*dx(3)
					
			ENDIF
 102		CONTINUE
	GOTO 101 
 103	CONTINUE
	en=sumv
	RETURN
      END
