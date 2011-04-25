!                                                                      
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
! All rights reserved.                                                 
!                                                                     
! Author: Valeriu Smirichinski                                         
!

program ljtest
	use  neighborlistmod
	use KIMservice
	implicit none
	!input atomic configuration file
        character*80 :: infile = "./data/dumpval10.xyz"

	!KIM API related declaration pointer declarations 	
	character*80 :: testname ="Sample_01_compute_example_f"
	character*80 :: modelname ="Sample_01_lj_cutoff_c"   !the only place to change in the future as argument for the model
        real*8,pointer, dimension(:,:)::x,f
	
	type (neighborlist_object) :: neigh_obj   , nei_obj; pointer(pnei_obj,nei_obj)
	integer :: kim; pointer(pkim,kim)    !kim world object pointer
	real*8::xstub(3,1),fstub(3,1); pointer(px,xstub); pointer(pf,fstub) ! cray pointer to coordinates and forces
	real*8::energy; pointer(penergy,energy)		!cray pointer to energy
	real*8::cutoff; pointer(pcutoff,cutoff)		!		 cuttof
	integer*8 :: natoms; pointer(pnatoms,natoms)		!cray pointer to numberOfAtoms
	real*8 ::cutofeps
	integer ::i,id,ni4; integer(kind=kim_intptr) ::n,one
	! initialize KIM_api object
	if(kim_api_init_f(pkim,testname,modelname).ne.1) stop 'not a test-model match'

	!read number of atoms in configuration		
	open(33,FILE=infile)
	read(33,*) n
	
	ni4=n
	!Allocate memory and associated it with the KIM API object
	call kim_api_allocate_f(pkim,n,1)

	! Make local pointers point to allocated memory (in KIM API object)
	px=kim_api_get_data_f(pkim,"coordinates"); call toRealArrayWithDescriptor2d(xstub,x,3,ni4)
	pf=kim_api_get_data_f(pkim,"forces");      call toRealArrayWithDescriptor2d(fstub,f,3,ni4)
	penergy = kim_api_get_data_f(pkim,"energy")
	pcutoff = kim_api_get_data_f(pkim,"cutoff")
	
	!  Read in the atomic positions for all atoms	
	do i=1,n
		read(33,*) id, x(:,i)
	end do
	close(33)
	f=0.0
	! setup parameter for lj potential   
	cutoff=1.8        
	cutofeps=2.1 
	! calculate neighbor list for the configuration
        call neighborcoolistdomaindec(neigh_obj,x,cutofeps)

	!Inform KIM API object about neighbor list iterator and object
	one=1
	if(kim_api_set_data_f(pkim,"neighObject",one,loc(neigh_obj)).ne.1) stop' neighObjec not in kim'
	if(kim_api_set_data_f(pkim,"neighIterator",one,loc(neighiterator)).ne.1) stop' neighIterator not in kim'

	! READY to call Model Initiation routine
	call sample_01_lj_cutoff_c_init(pkim)
	!All setup finished -- ready to compute
	!compute the model -- e.g., compute energy & force
	call kim_api_model_compute(pkim)
	!output KIM API object to screen (optional)
	call kim_api_print_f(pkim)
	! print energy
	print *,"energy =", energy
	
	!clean up
	call kim_api_free_f(pkim)
	call neifree(neigh_obj)
	
end program ljtest


