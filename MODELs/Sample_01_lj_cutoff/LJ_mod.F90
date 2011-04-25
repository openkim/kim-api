!                                                                      
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
! All rights reserved.                                                 
!                                                                     
! Author: Valeriu Smirichinski                                         
!


module lj_test_mod
	use  KIMservice
	implicit none
	!defining the structure that holds variables for calculation
	type lj_test_object
		integer :: numberatoms
		real*8,pointer :: x(:,:), f(:,:) ! position, forces and saved position
		!real*8 :: a= 1.0*0.5**(12),b=2.0*0.5**(6),cutof=2.3! parameters of lj potential
		real*8 :: a= 0.0002441,b=0.03125,cutof=2.3! parameters of lj potential
		real*8 :: energy ! holder for energy pointer
	
	end type lj_test_object
!	interface
!		subroutine neighborsiterate(pneiobj,pnei1atom,numnei,restart)
!			use KIMservice
!			integer(kind=kim_intptr) ::pneiobj,pnei1atom
!			integer :: numnei,restart! if restart = 0 set iterator in the begining
!						  ! if restart != 0 proseed next
!		end subroutine neighborsiterate
!	end interface

	!defining cray pointer to neighbor iterator
         external neighborsiterate
	 pointer (piterator,neighborsiterate)
	
	
	type (lj_test_object) :: lj_obj
	SAVE ::	lj_obj

contains
	! actual initialization routine
	subroutine lj_init(pkim )
		use KIMservice
		implicit none
		! KIM API related declaration
		integer(kind=kim_intptr) :: kim; pointer(pkim,kim) 
		real*8 :: xstub(3,1); pointer(px,xstub)  ! cray pointer to position
		real*8 :: fstub(3,1); pointer(pf,fstub)  ! cray pointer to forces
		integer(kind=kim_intptr) ::sz; integer(kind=8):: numatoms; pointer(patoms,numatoms) ! .. to number of atoms
		real*8::cutoff; pointer(pcutoff,cutoff)
		real*8::skin; pointer(pskin,skin)
		real*8::energy; pointer(penergy,energy)
		!getting pointers data from kim
		patoms = kim_api_get_data_f(pkim,"numberOfAtoms") ; lj_obj%numberatoms=numatoms
		px=kim_api_get_data_f(pkim,"coordinates"); call toRealArrayWithDescriptor2d(xstub,lj_obj%x,3,lj_obj%numberatoms)
		pf=kim_api_get_data_f(pkim,"forces");      call toRealArrayWithDescriptor2d(fstub,lj_obj%f,3,lj_obj%numberatoms)
		pcutoff = kim_api_get_data_f(pkim,"cutoff"); lj_obj%cutof = cutoff;
		
		!getting pointer to neighbor iterator from KIM API object
		 piterator=kim_api_get_data_f(pkim,"neighIterator") ! get pointer to iterator
		!setting pointer to compute method
		sz=1
		if(kim_api_set_data_f(pkim,"compute",sz,loc(lj_calculate)).ne.1)  then
			stop ' compute not found in kim'
		end if
	end subroutine lj_init
	
	
	
	
	!calculates forces per atom and total energy
	subroutine lj_calculate(pkim) ! compute routine with KIM interface
		implicit none
		integer(kind=kim_intptr) :: kim; pointer(pkim,kim)
		call lj_calculate2(pkim,lj_obj%x,lj_obj%f)
	end subroutine lj_calculate

	subroutine lj_calculate2(pkim,x,f) ! actual compute routine
		use KIMservice
		implicit none		
		
		!KIM related declaration
		integer(kind=kim_intptr) :: kim; pointer(pkim,kim)          
		real*8,pointer,dimension(:,:) :: x,f
		real*8 :: vij,dvmr,v,sumv,cutof,cut2,energycutof
		integer :: i,j,jj,numnei=0;      real*8 :: r2,dv;   real*8,dimension(3):: xi,xj,dx,fij
		real*8::energy; pointer(penergy,energy)
		integer(kind=kim_intptr) :: nei_obj; pointer(pnei_obj,nei_obj) !pointer to neighbor list object
		integer :: nnn(512)
		integer:: nei1atom(1); pointer (pnei1atom,nei1atom)
		integer restart;
		nnn=0
		pnei1atom = loc(nnn) !!!!!!!!
		penergy = kim_api_get_data_f(pkim,"energy")     !get pointer to energy
		pnei_obj=kim_api_get_data_f(pkim,"neighObject")	! get pointer to neighbor list object
                cutof = lj_obj%cutof	!get pointer to cutoff
	
        	sumv=0.0; 
		
                f(:,:)= 0.0; 
	        numnei = 0
		cut2 = cutof*cutof
		! store energy of LJ at rcut in 'energycutof'
		! to be used for shifting LJ so energy is zero at cutoff 
		call  ljpotr(cut2,energycutof,dvmr)              
	        
                restart = 0; !reset neighbor iterator to beginning
        	call neighborsiterate (pnei_obj,pnei1atom,numnei,restart)
                
                
		restart=1   !increment flag for neighbor iterator
        	do while (numnei .ge. 0)
			!increment iterator
			call neighborsiterate (pnei_obj,pnei1atom,numnei,restart)
			i = nei1atom(2)
			xi = x(:,i)
			do jj=3, nei1atom(1)
				
				j=nei1atom(jj)
				xj = x(:,j)
				dx = xi-xj
				r2=dx(1)*dx(1) + dx(2)*dx(2) + dx(3)*dx(3)
				if(r2.lt.0.000000000000001) print*,"xi,xj",xi,xj
				if(r2.lt.0.000000000000001) print*,"i,j",i,j
				if (r2.le.cut2) then
					call ljpotr(r2,vij,dvmr)
					sumv = sumv + vij-energycutof
					f(:,i) =f(:,i) - dvmr*dx 
					f(:,j) =f(:,j) + dvmr*dx
					
				end if
			end do
		end do
		v=sumv
		lj_obj%energy = v
		energy = v !set energy in KIM API object
		!forces are already stored in KIM API object
		return
		contains
		!Computational core of LJ potential
		subroutine ljpotr(rr,v,dvmr)
			implicit none
			real*8 rr,v,dvmr,       a,b,r1,r2,rm6,rm8
			a=lj_obj%a; b=lj_obj%b; ! LJ parameters
			if (rr.lt.0.000000000000001) stop 'rr is zero'
			r2=rr; rm6=1.0/(r2*r2*r2); rm8=rm6/r2
			v=(a*rm6 - b)*rm6; dvmr = 6.0*rm8*(-2*rm6*a + b)
		end subroutine ljpotr
	end subroutine lj_calculate2

end module lj_test_mod

!  Model Initiation routine (it calls actual initialization routine in the module lj_test_mod)
subroutine sample_01_lj_cutoff_init(pkim)
	use lj_test_mod
	implicit none
	integer(kind=kim_intptr) :: kim; pointer(pkim,kim)
	call lj_init(pkim)
end subroutine sample_01_lj_cutoff_init

