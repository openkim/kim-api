
!
! Release: This file is part of the openkim-api.git repository.
!
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
! All rights reserved.
!
! Authors: Valeriu Smirichinski, Ellad B. Tadmor, Ryan S. Elliott
!

module kimservice
    implicit none
    integer,parameter :: KEY_CHAR_LENGTH = 64 !

#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
    
    interface
        !explicite inteface to C-side code
        integer function kim_api_init(kimmdl,testname,mdlname)
	    
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,testname,mdlname
        end function kim_api_init


 integer function kim_api_init_str_testname(kimmdl,testname,mdlname)
	    
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,testname,mdlname
        end function kim_api_init_str_testname

        function kim_api_status_msg_f(errcode)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
        integer ::errcode
        integer(kind=kim_intptr) :: kimmdl,kim_api_status_msg_f
	end function kim_api_status_msg_f

        integer function kim_api_init1(kimmdl,testinputf,testname,mdlinputf,mdlname)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,testinputf,testname,mdlinputf,mdlname
        end function kim_api_init1

        integer function kim_api_set_units(kimmdl,UnitsSystem)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
             integer(kind=kim_intptr) :: kimmdl,UnitsSystem
        end function kim_api_set_units

        integer function kim_api_isunits_fixed(kimmdl)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
             integer(kind=kim_intptr) :: kimmdl
        end function kim_api_isunits_fixed

        integer function kim_api_set_data(kimmdl,nm, size, dt)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,nm,  size, dt
        end function kim_api_set_data



        function kim_api_get_data(kimmdl,nm,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,nm,kim_api_get_data
	    integer::error
        end function kim_api_get_data

	real function kim_api_get_unit_scalefactor(kimmdl,nm,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,nm
	    integer::error
        end function kim_api_get_unit_scalefactor


#ifdef FORTRAN2003
        function kim_api_get_data_cptr(kimmdl,nm)
            use iso_c_binding
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
           type (c_ptr)::kim_api_get_data_cptr
            integer(kind=kim_intptr) :: kimmdl,nm
        end function kim_api_get_data_cptr
#endif

       	function kim_api_get_size(kimmdl,nm,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,nm,kim_api_get_size
	    integer::error
        end function kim_api_get_size

        function kim_api_get_neigh_mode_f(kimmdl,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_neigh_mode_f
	    integer::error
        end function kim_api_get_neigh_mode_f

        function kim_api_get_rank_shape(kimmdl,nm, shapea,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,nm, shapea,kim_api_get_rank_shape
	    integer::error
        end function kim_api_get_rank_shape

        subroutine kim_api_set_rank_shape(kimmdl,nm,shapea,rank,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif 
	integer(kind=kim_intptr) :: kimmdl,nm,shapea
	integer::rank,error
	end subroutine kim_api_set_rank_shape

	function kim_api_get_listatomtypes_f(kimmdl,natypes,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_listatomtypes_f
	    integer::natypes,error
        end function kim_api_get_listatomtypes_f

	function kim_api_get_listparams_f(kimmdl,nvpar,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_listparams_f
	    integer::nvpar,error
        end function kim_api_get_listparams_f

	function kim_api_get_listfreeparams_f(kimmdl,nvpar,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_listfreeparams_f
	    integer::nvpar,error
        end function kim_api_get_listfreeparams_f

	function kim_api_get_listfixedparams_f(kimmdl,nvpar,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_listfixedparams_f
	    integer::nvpar,error
        end function kim_api_get_listfixedparams_f

	function kim_api_get_nbc_method_f(kimmdl,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_nbc_method_f
	    integer::error
        end function kim_api_get_nbc_method_f

	function kim_api_get_atypecode(kimmdl,nm,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl, nm
	    integer::error,kim_api_get_atypecode
        end function kim_api_get_atypecode

	integer function kim_api_get_full_neigh_f(kimmdl,mode,request, atom, numnei, pnei1atom, pRij)
	integer :: mode,request,atom,numnei
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
	integer(kind=kim_intptr) :: kimmdl,pnei1atom,pRij
	end function kim_api_get_full_neigh_f

	integer function kim_api_get_half_neigh_f(kimmdl,mode,request, atom, numnei, pnei1atom, pRij)
	integer :: mode,request,atom,numnei
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
	integer(kind=kim_intptr) :: kimmdl,pnei1atom,pRij
	end function kim_api_get_half_neigh_f


        integer function kim_api_isit_compute(kimmdl,nm,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,nm
	    integer::error
        end function kim_api_isit_compute

        integer function kim_api_get_index(kimmdl,nm,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,nm
	    integer::error
        end function kim_api_get_index

        function kim_api_get_data_byi(kimmdl,I,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_data_byi
            integer :: I,error
        end function kim_api_get_data_byi

        function kim_api_get_size_byi(kimmdl,I,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_size_byi
            integer :: I,error
        end function kim_api_get_size_byi

        function kim_api_get_rank_shape_byi(kimmdl,I,shapea,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,shapea,kim_api_get_rank_shape_byi
            integer  :: I,error
        end function kim_api_get_rank_shape_byi

        integer function kim_api_isit_compute_byi(kimmdl,I,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
        end function kim_api_isit_compute_byi


        subroutine kim_api_allocate(kimmdl,natoms,ntypes,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl
            integer  :: natoms,ntypes,error
        end subroutine kim_api_allocate

        subroutine kim_api_free(kimmdl,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl
	integer::error
        end subroutine kim_api_free

        subroutine kim_api_print(kimmdl,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl
	    integer ::error
        end subroutine kim_api_print

        subroutine kim_api_model_compute_f(kimmdl,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl
	    integer::error
        end subroutine kim_api_model_compute_f

	subroutine kim_api_model_destroy_f(kimmdl,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl
	    integer::error
        end subroutine kim_api_model_destroy_f

	integer function kim_api_model_reinit_f(kimmdl)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl
        end function kim_api_model_reinit_f
        
	integer function kim_api_model_init_f(kimmdl)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
		integer(kind=kim_intptr) :: kimmdl
		
	end function kim_api_model_init_f

    !subroutines
        subroutine kim_api_get_units(kimmdl,UnitsSystem,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,UnitsSystem
	    integer::error
        end subroutine kim_api_get_units

        subroutine kim_api_get_originalunits(kimmdl,UnitsSystem,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,UnitsSystem
	    integer::error
        end subroutine kim_api_get_originalunits

        subroutine kim_api_transform_units_to(kimmdl,UnitsSystem,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,UnitsSystem
	    integer::error
        end subroutine  kim_api_transform_units_to

        subroutine kim_api_set2_compute(kimmdl,nm,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,nm
	integer::error
        end subroutine kim_api_set2_compute

        subroutine kim_api_set2_donotcompute(kimmdl,nm,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,nm
	integer::error
        end subroutine kim_api_set2_donotcompute

        subroutine kim_api_set_data_byi(kimmdl,I, size,dt,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer (kind=kim_intptr):: kimmdl,size,dt
            integer :: I,error
        end subroutine kim_api_set_data_byi

        subroutine kim_api_set2_compute_byi(kimmdl,I,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer (kind=kim_intptr):: kimmdl
            integer :: I,error
        end subroutine kim_api_set2_compute_byi

        subroutine kim_api_set2_donotcompute_byi(kimmdl,I,error)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
        end subroutine kim_api_set2_donotcompute_byi
    end interface

 contains
    !the following subs converts ctypeArray to fortran type array
    ! ( to pointer to array with descriptor) without 2003 features
    ! it is analog of c_f_pointer(...)
    subroutine toRealArrayWithDescriptor2d(ctypeArray,ArrayWithDescriptor,n,m)
        implicit none
        integer  :: n,m
        real*8,target :: ctypeArray(n,m)
        real*8,pointer ::ArrayWithDescriptor(:,:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine toRealArrayWithDescriptor2d

    subroutine toIntegerArrayWithDescriptor2d(ctypeArray,ArrayWithDescriptor,n,m)
        implicit none
        integer  :: n,m !! Check if I can make array big like size of integer*8
        integer,target :: ctypeArray(n,m)
        integer,pointer ::ArrayWithDescriptor(:,:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine toIntegerArrayWithDescriptor2d

    subroutine toIntegerArrayWithDescriptor1d(ctypeArray,ArrayWithDescriptor,n)
        implicit none
        integer :: n
        integer,target :: ctypeArray(n)
        integer,pointer ::ArrayWithDescriptor(:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine toIntegerArrayWithDescriptor1d

    subroutine toRealArrayWithDescriptor1d(ctypeArray,ArrayWithDescriptor,n)
        implicit none
        integer :: n
        real*8,target :: ctypeArray(n)
        real*8,pointer ::ArrayWithDescriptor(:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine toRealArrayWithDescriptor1d

    character (len=KEY_CHAR_LENGTH) function attachnull(str)
        character (LEN=*) :: str
        attachnull = str//CHAR(0)
    end function attachnull
    integer(kind=kim_intptr) function attachnull_c(str)
        character (LEN=*) :: str

  ! print*,"proba02"
        str = trim(str)//CHAR(0)
  ! print*,"proba03"
        attachnull_c = loc(str)
    end function attachnull_c
    
    character (len=KEY_CHAR_LENGTH) function cstring2fortran(pointercstring)
        character (len=KEY_CHAR_LENGTH) cstring ; pointer(pointercstring,cstring)
        cstring2fortran = cstring
    end function cstring2fortran
   
    ! fortran 90 wraper for KIM_API interface using cray pointers
    ! takes care of character string (null atachment) and dope vector for arrays
    !****************** f90 wraper ********************
    integer function kim_api_init_f(kimmdl,testname,mdlname)
            character (len=*) :: testname,mdlname
            integer(kind=kim_intptr) :: kimmdl
            character (len=KEY_CHAR_LENGTH)::testnamesnd,mdlnamesnd
            character (len=KEY_CHAR_LENGTH):: s1,s2
            pointer(ps1,s1);pointer(ps2,s2)
            testnamesnd=attachnull(trim(testname))
            mdlnamesnd=attachnull(trim(mdlname))
            ps1=loc(testnamesnd)
            ps2=loc(mdlnamesnd)
            kim_api_init_f =kim_api_init(kimmdl,ps1,ps2)
     end function kim_api_init_f

    integer function kim_api_init_str_testname_f(kimmdl,testname,mdlname)
            character (len=*) :: testname,mdlname
            integer(kind=kim_intptr) :: kimmdl
            character (len=KEY_CHAR_LENGTH)::testnamesnd,mdlnamesnd
            character (len=KEY_CHAR_LENGTH):: s1,s2
            pointer(ps1,s1);pointer(ps2,s2)
            !testnamesnd=attachnull(trim(testname))
            
            mdlnamesnd=attachnull(trim(mdlname))
            ps1=loc(testname)
            ps2=loc(mdlnamesnd)
            kim_api_init_str_testname_f =kim_api_init_str_testname(kimmdl,ps1,ps2)
     end function kim_api_init_str_testname_f

    integer function kim_api_init1_f(kimmdl,testinputf,testname,mdlinputf,mdlname)
            character (len=*) :: testinputf,testname,mdlinputf,mdlname
            integer(kind=kim_intptr) :: kimmdl
            character (len=KEY_CHAR_LENGTH)::testinputfsnd,testnamesnd,mdlinputfsnd,mdlnamesnd
            character (len=KEY_CHAR_LENGTH):: s1,s2,s3,s4
            pointer(ps1,s1);pointer(ps2,s2);pointer(ps3,s3);pointer(ps4,s4)
            testinputfsnd=attachnull(trim(testinputf))
            testnamesnd=attachnull(trim(testname))
            mdlinputfsnd=attachnull(trim(mdlinputf))
            mdlnamesnd=attachnull(trim(mdlname))
            ps1=loc(testinputfsnd)
            ps2=loc(testnamesnd)
            ps3=loc(mdlinputfsnd)
            ps4=loc(mdlnamesnd)
            kim_api_init1_f =kim_api_init1(kimmdl,ps1,ps2,ps3,ps4)
        end function kim_api_init1_f

        integer function kim_api_set_units_f(kimmdl,UnitsSystem)
             integer(kind=kim_intptr) :: kimmdl
             character (len=*) ::UnitsSystem
             character (len=KEY_CHAR_LENGTH) :: str2send
             character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
             str2send = attachnull(trim(UnitsSystem))
             pstr = loc(str2send)
             kim_api_set_units_f = kim_api_set_units(kimmdl,pstr)
        end function kim_api_set_units_f

        integer function kim_api_isunits_fixed_f(kimmdl)
             !returns 1 (true) if fixed units (unitits can't be reset)
             integer(kind=kim_intptr) :: kimmdl
             kim_api_isunits_fixed_f = kim_api_isunits_fixed(kimmdl)
        end function kim_api_isunits_fixed_f

        integer function kim_api_set_data_f(kimmdl,nm, size, dt)
            ! returns 1 (true) if successfull
            ! dt is address (cray pointer to acctual data
            integer(kind=kim_intptr) :: kimmdl,  size, dt
            character (len=*) ::nm

            character (len=KEY_CHAR_LENGTH) :: str2send
            character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)

            kim_api_set_data_f = kim_api_set_data(kimmdl,pstr,size,dt)
        end function kim_api_set_data_f

      
        integer(kind=kim_intptr) function kim_api_get_data_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) ::nm
            character (len=KEY_CHAR_LENGTH) :: str2send
            character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
	    integer::error
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            kim_api_get_data_f = kim_api_get_data(kimmdl,pstr,error)
        end function kim_api_get_data_f

	integer function kim_api_get_atypecode_f(kimmdl,nm,error)
		integer(kind=kim_intptr) :: kimmdl
 		character (len=*) ::nm
		character (len=KEY_CHAR_LENGTH) :: str2send
            	character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
		integer::error
            	str2send = attachnull(trim(nm))
           	pstr = loc(str2send)
		kim_api_get_atypecode_f = kim_api_get_atypecode(kimmdl,pstr,error)
	end function kim_api_get_atypecode_f

	real function kim_api_get_unit_scalefactor_f(kimmdl,nm,error)
		integer(kind=kim_intptr) :: kimmdl
 		character (len=*) ::nm
		character (len=KEY_CHAR_LENGTH) :: str2send
            	character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
		integer::error
            	str2send = attachnull(trim(nm))
           	pstr = loc(str2send)
		kim_api_get_unit_scalefactor_f = kim_api_get_unit_scalefactor(kimmdl,pstr,error)
	end function kim_api_get_unit_scalefactor_f

#ifdef FORTRAN2003
        function kim_api_get_data_fc(kimmdl,nm)
	    use iso_c_binding
            integer(kind=kim_intptr) :: kimmdl
	    type(c_ptr) :: kim_api_get_data_fc
            character (len=*) ::nm
            character (len=KEY_CHAR_LENGTH) :: str2send
            character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            kim_api_get_data_fc = kim_api_get_data_cptr(kimmdl,pstr)
        end function kim_api_get_data_fc
#endif

        integer(kind=kim_intptr) function kim_api_get_size_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) ::nm

            character (len=KEY_CHAR_LENGTH) :: str2send
            character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
	    integer::error
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)

            kim_api_get_size_f = kim_api_get_size(kimmdl,pstr,error)
        end function kim_api_get_size_f

        integer(kind=kim_intptr) function kim_api_get_rank_shape_f(kimmdl,nm, shape,error)
            ! returns rank and place correct shape (shape has to be allocated )
            ! if unssaccesfull returns -1
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) :: nm

            character (len=KEY_CHAR_LENGTH) :: str2send
            character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)   

            integer,pointer :: shape(:)
            integer :: shp(size(shape))
            integer :: shpst(1); pointer(pshpst,shpst)
            integer :: rank,i,error
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            pshpst = loc(shp)
            rank = kim_api_get_rank_shape(kimmdl,pstr,pshpst,error)
            if(rank .eq. 0) then
                kim_api_get_rank_shape_f=0
            else if(rank.eq.1) then
                kim_api_get_rank_shape_f=1
                shape(1)=shp(1)
            else if(rank.gt.1) then
                kim_api_get_rank_shape_f=rank
                do i=1, rank
                    shape(i)=shp(rank - i + 1)
                end do
            else
                kim_api_get_rank_shape_f=-1
            end if
        end function kim_api_get_rank_shape_f

        subroutine kim_api_set_rank_shape_f(kimmdl,nm, shapef,rankf,error)
            ! set rank, shape and size of the data in KIM API object
            ! error=1 if successful , error <= 0 if unsuccessful
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) :: nm

            character (len=KEY_CHAR_LENGTH) :: str2send
            character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)   

            !integer,pointer :: shape(:)
	    integer ::rankf,shapef(rankf)
            integer :: shpst(1); pointer(pshpst,shpst)
            integer :: i,error
	    integer :: shp(rankf)
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            pshpst = loc(shp)
	    if(rankf .gt. 1) then
		do i=1, rankf 
                    shp(i)=shapef(rankf - i + 1) !transpose shape
                end do
	    end if
            call kim_api_set_rank_shape(kimmdl,pstr,pshpst,rankf,error)
        end subroutine kim_api_set_rank_shape_f

        integer function kim_api_isit_compute_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) ::nm
	    integer::error
            character (len=KEY_CHAR_LENGTH) :: str2send
            character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)

            kim_api_isit_compute_f =kim_api_isit_compute(kimmdl,pstr,error)
        end function kim_api_isit_compute_f

        integer function kim_api_get_index_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) ::nm
            character (len=KEY_CHAR_LENGTH) :: str2send
            character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
	    integer::error
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            kim_api_get_index_f = kim_api_get_index(kimmdl,pstr,error)
        end function kim_api_get_index_f

        
        integer(kind=kim_intptr) function kim_api_get_data_byi_f(kimmdl,I,error)
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
            kim_api_get_data_byi_f = kim_api_get_data_byi(kimmdl,I,error)
        end function kim_api_get_data_byi_f

        integer(kind=kim_intptr) function kim_api_get_size_byi_f(kimmdl,I,error)
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
            kim_api_get_size_byi_f = kim_api_get_size_byi(kimmdl,I,error)
        end function kim_api_get_size_byi_f

        integer(kind=kim_intptr) function kim_api_get_rank_shape_byi_f(kimmdl,I,shape,error)
            integer(kind=kim_intptr) :: kimmdl
            integer  :: I,error
            integer,pointer :: shape(:)
            integer :: shp(size(shape))
            integer :: shpst(1); pointer(pshpst,shpst)
            integer :: rank,ii
            pshpst = loc(shp)
            rank = kim_api_get_rank_shape_byi(kimmdl,I,pshpst,error)
            if(rank .eq. 0) then
                kim_api_get_rank_shape_byi_f=0
            else if(rank.eq.1) then
                kim_api_get_rank_shape_byi_f=1
                shape(1)=shp(1)
            else if(rank.gt.1) then
                kim_api_get_rank_shape_byi_f=rank
                do ii=1, rank
                    shape(ii)=shp(rank - ii + 1)
                end do
            else
                kim_api_get_rank_shape_byi_f=-1
            end if
        end function kim_api_get_rank_shape_byi_f

        integer function kim_api_isit_compute_byi_f(kimmdl,I,error)
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
            kim_api_isit_compute_byi_f = kim_api_isit_compute_byi(kimmdl,I,error)
        end function kim_api_isit_compute_byi_f

        !subroutines 
        subroutine kim_api_allocate_f(kimmdl,natoms,ntypes,error)
            integer(kind=kim_intptr) :: kimmdl
            integer  :: natoms,ntypes,error
            call kim_api_allocate(kimmdl,natoms,ntypes,error)
        end subroutine kim_api_allocate_f

        subroutine kim_api_free_f(kimmdl,error)
            integer(kind=kim_intptr) :: kimmdl
	    integer::error
            call kim_api_free(kimmdl,error)
        end subroutine kim_api_free_f

        subroutine kim_api_print_f(kimmdl,error)
            integer(kind=kim_intptr) :: kimmdl
	    integer::error
            call kim_api_print(kimmdl,error)
        end subroutine kim_api_print_f
    !subroutine kim_api_model_calculate(subroutine *kimmdl) //method is not implemented yet
        subroutine kim_api_get_units_f(kimmdl,UnitsSystem,error)
            integer(kind=kim_intptr) :: kimmdl
           character (len=*) :: UnitsSystem
           character (len=KEY_CHAR_LENGTH) ::us
           character :: str(1); pointer(pstr,str)
	   integer::error
           pstr =loc(us)
           call kim_api_get_units(kimmdl,pstr,error)
           UnitsSystem = attachnull(us)
        end subroutine kim_api_get_units_f

        subroutine kim_api_get_originalunits_f(kimmdl,UnitsSystem,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) :: UnitsSystem
            character (len=KEY_CHAR_LENGTH) ::us
            character :: str(1); pointer(pstr,str)
	    integer::error
            pstr =loc(us)
            call kim_api_get_originalunits(kimmdl,pstr,error)
            UnitsSystem = attachnull(us)
        end subroutine kim_api_get_originalunits_f

        subroutine kim_api_transform_units_to_f(kimmdl,UnitsSystem,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) :: UnitsSystem
	    integer::error
            character (len=KEY_CHAR_LENGTH) ::us
            character :: str(1); pointer(pstr,str)
            us = attachnull(UnitsSystem)
            pstr =loc(us)
            call kim_api_transform_units_to(kimmdl,pstr,error)
        end subroutine  kim_api_transform_units_to_f

        subroutine kim_api_set2_compute_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) :: nm
	    integer :: error
            character (len=KEY_CHAR_LENGTH) ::us
            character :: str(1); pointer(pstr,str)
            us = attachnull(nm)
            pstr =loc(us)

            call kim_api_set2_compute(kimmdl,pstr,error)
        end subroutine kim_api_set2_compute_f

        subroutine kim_api_set2_donotcompute_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) :: nm
            integer :: error
            character (len=KEY_CHAR_LENGTH) ::us
            character :: str(1); pointer(pstr,str)
            us = attachnull(nm)
            pstr =loc(us)

            call kim_api_set2_donotcompute(kimmdl,pstr,error)
        end subroutine kim_api_set2_donotcompute_f

        subroutine kim_api_set_data_byi_f(kimmdl,I, size,dt,error)
            integer(kind=kim_intptr) :: kimmdl,size,dt
            integer :: I,error
            call kim_api_set_data_byi(kimmdl,I, size,dt,error)
        end subroutine kim_api_set_data_byi_f

        subroutine kim_api_set2_compute_byi_f(kimmdl,I,error)
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
            call kim_api_set2_compute_byi(kimmdl,I,error)
        end subroutine kim_api_set2_compute_byi_f

        subroutine kim_api_set2_donotcompute_byi_f(kimmdl,I,error)
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
            call kim_api_set2_donotcompute_byi(kimmdl,I,error)
        end subroutine kim_api_set2_donotcompute_byi_f

end module kimservice


