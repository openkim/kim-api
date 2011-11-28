
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
#include "KIMstatus.h"

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
        
        function kim_api_get_model_kim_str(modelname,lenstr,ier)

#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
	integer(kind=kim_intptr)::kim_api_get_model_kim_str,modelname
	integer :: ier,lenstr
	end function kim_api_get_model_kim_str
         
        function kim_api_get_model_buffer_f(kimmdl,ier)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
        integer ::ier
        integer(kind=kim_intptr) :: kimmdl,kim_api_get_model_buffer_f
        end function kim_api_get_model_buffer_f

 	function kim_api_isit_half_neighbors_f(kimmdl,ier)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
        integer ::ier
        integer(kind=kim_intptr) :: kimmdl,kim_api_isit_half_neighbors_f
        end function kim_api_isit_half_neighbors_f


        subroutine kim_api_set_model_buffer_f(kimmdl,ob,ier)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
        integer ::ier
        integer(kind=kim_intptr) :: kimmdl,ob
        end subroutine kim_api_set_model_buffer_f

        subroutine kim_api_process_d1edr_f(pkim,de,r,dx,i,j,ier)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
		integer(kind=kim_intptr) :: pkim,dx
		real*8 :: de,r
		integer ::i,j,ier
        end subroutine kim_api_process_d1edr_f

    integer function kim_api_report_error(ln,fl,usermsg,ier)
#ifdef SYSTEM32
integer, parameter :: kim_intptr=4
#else
integer,parameter :: kim_intptr = 8
#endif
      integer :: ln,ier
      integer(kind=kim_intptr)::fl,usermsg
    end function kim_api_report_error

        integer function kim_api_init1(kimmdl,testinputf,testname,mdlinputf,mdlname)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl,testinputf,testname,mdlinputf,mdlname
        end function kim_api_init1

         integer function kim_api_get_model_index_shift_f(kimmdl)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
            integer(kind=kim_intptr) :: kimmdl
        end function kim_api_get_model_index_shift_f

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

	subroutine toRealArrayWithDescriptor3d(ctypeArray,ArrayWithDescriptor,n,m,l)
        implicit none
        integer  :: n,m,l
        real*8,target :: ctypeArray(n,m,l)
        real*8,pointer ::ArrayWithDescriptor(:,:,:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine toRealArrayWithDescriptor3d


    subroutine toIntegerArrayWithDescriptor2d(ctypeArray,ArrayWithDescriptor,n,m)
        implicit none
        integer  :: n,m !! Check if I can make array big like size of integer*8
        integer,target :: ctypeArray(n,m)
        integer,pointer ::ArrayWithDescriptor(:,:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine toIntegerArrayWithDescriptor2d

	subroutine toIntegerArrayWithDescriptor3d(ctypeArray,ArrayWithDescriptor,n,m,l)
        implicit none
        integer  :: n,m,l !! Check if I can make array big like size of integer*8
        integer,target :: ctypeArray(n,m,l)
        integer,pointer ::ArrayWithDescriptor(:,:,:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine toIntegerArrayWithDescriptor3d

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
         
        integer(kind=kim_intptr) function kim_api_get_model_kim_str_f(nm,lenstr,error)
	    character (len=*) ::nm
            character (len=KEY_CHAR_LENGTH) :: str2send
            character (len=KEY_CHAR_LENGTH) :: str; pointer (pstr,str)
	    integer::error,lenstr
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            kim_api_get_model_kim_str_f = kim_api_get_model_kim_str(pstr,lenstr,error)
        end function kim_api_get_model_kim_str_f

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
        
        integer function kim_api_report_error_f(ln,fl,usermsg,ier)
            integer:: ln,ier
            character(len=*)::fl,usermsg
            character(len=128)::fltmp,umsgtmp
            fltmp=fl//CHAR(0)
            umsgtmp=usermsg//char(0)
            kim_api_report_error_f = kim_api_report_error(ln,loc(fltmp),loc(umsgtmp),ier)

        end function kim_api_report_error_f

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

        subroutine kim_api_set_data_multiple_f(kimmdl,error, nm1,sz1,dt1, nm2,sz2,dt2, nm3,sz3,dt3, nm4,sz4,dt4, nm5,sz5,dt5,&
         nm6,sz6,dt6, nm7,sz7,dt7, nm8,sz8,dt8, nm9,sz9,dt9, nm10,sz10,dt10, nm11,sz11,dt11, nm12,sz12,dt12, nm13,sz13,dt13,&
         nm14,sz14,dt14, nm15,sz15,dt15, nm16,sz16,dt16, nm17,sz17,dt17, nm18,sz18,dt18, nm19,sz19,dt19, nm20,sz20,dt20)
            integer(kind=kim_intptr) :: kimmdl;  integer error, grarg

            character(len=40) ::msg="kim_api_set_data_multiple_f"

            !    declare optional arguments
            character(len=*)          :: nm1;   integer(kind=kim_intptr)          :: sz1, dt1
            character(len=*),optional:: nm2;   integer(kind=kim_intptr),optional:: sz2, dt2
            character(len=*),optional:: nm3;   integer(kind=kim_intptr),optional:: sz3, dt3
            character(len=*),optional:: nm4;   integer(kind=kim_intptr),optional:: sz4, dt4
            character(len=*),optional:: nm5;   integer(kind=kim_intptr),optional:: sz5, dt5
            character(len=*),optional:: nm6;   integer(kind=kim_intptr),optional:: sz6, dt6
            character(len=*),optional:: nm7;   integer(kind=kim_intptr),optional:: sz7, dt7
            character(len=*),optional:: nm8;   integer(kind=kim_intptr),optional:: sz8, dt8
            character(len=*),optional:: nm9;   integer(kind=kim_intptr),optional:: sz9, dt9
            character(len=*),optional:: nm10;  integer(kind=kim_intptr),optional:: sz10,dt10
            character(len=*),optional:: nm11;  integer(kind=kim_intptr),optional:: sz11,dt11
            character(len=*),optional:: nm12;  integer(kind=kim_intptr),optional:: sz12,dt12
            character(len=*),optional:: nm13;  integer(kind=kim_intptr),optional:: sz13,dt13
            character(len=*),optional:: nm14;  integer(kind=kim_intptr),optional:: sz14,dt14
            character(len=*),optional:: nm15;  integer(kind=kim_intptr),optional:: sz15,dt15
            character(len=*),optional:: nm16;  integer(kind=kim_intptr),optional:: sz16,dt16
            character(len=*),optional:: nm17;  integer(kind=kim_intptr),optional:: sz17,dt17
            character(len=*),optional:: nm18;  integer(kind=kim_intptr),optional:: sz18,dt18
            character(len=*),optional:: nm19;  integer(kind=kim_intptr),optional:: sz19,dt19
            character(len=*),optional:: nm20;  integer(kind=kim_intptr),optional:: sz20,dt20
            !
            error = kim_api_set_data_f(kimmdl,nm1,sz1,dt1); 
            if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(sz2 ).or..not.present(dt2 ))) then 
            	if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(sz3 ).or..not.present(dt3 ))) then 
            	if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(sz4 ).or..not.present(dt4 ))) then 
            	if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(sz5 ).or..not.present(dt5 ))) then 
            	if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(sz6 ).or..not.present(dt6 ))) then 
            	if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(sz7 ).or..not.present(dt7 ))) then 
            	if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(sz8 ).or..not.present(dt8 ))) then 
            	if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(sz9 ).or..not.present(dt9 ))) then 
            	if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(sz10 ).or..not.present(dt10 ))) then 
            	if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(sz11 ).or..not.present(dt11 ))) then 
            	if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(sz12 ).or..not.present(dt12 ))) then 
            	if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(sz13 ).or..not.present(dt13 ))) then 
            	if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(sz14 ).or..not.present(dt14 ))) then 
            	if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(sz15 ).or..not.present(dt15 ))) then 
            	if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            elseif( present(nm16 ).and.(.not.present(sz16 ).or..not.present(dt16))) then 
            	if(errcheck_mltpl(error,msg,16, nm16 ).lt.KIM_STATUS_OK) return
            elseif( present(nm17 ).and.(.not.present(sz17 ).or..not.present(dt17 ))) then 
            	if(errcheck_mltpl(error,msg,17, nm17 ).lt.KIM_STATUS_OK) return
            elseif( present(nm18 ).and.(.not.present(sz18 ).or..not.present(dt18 ))) then 
            	if(errcheck_mltpl(error,msg,18, nm18 ).lt.KIM_STATUS_OK) return
            elseif( present(nm19 ).and.(.not.present(sz19 ).or..not.present(dt19 ))) then 
            	if(errcheck_mltpl(error,msg,19, nm19 ).lt.KIM_STATUS_OK) return
            elseif( present(nm20 ).and.(.not.present(sz20 ).or..not.present(dt20 ))) then 
            	if(errcheck_mltpl(error,msg,20, nm20 ).lt.KIM_STATUS_OK) return
            endif 
	    
	    !process arguments
 if(present(nm2)) error=kim_api_set_data_f(kimmdl,nm2,sz2,dt2);if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3)) error=kim_api_set_data_f(kimmdl,nm3,sz3,dt3);if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4)) error=kim_api_set_data_f(kimmdl,nm4,sz4,dt4);if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5)) error=kim_api_set_data_f(kimmdl,nm5,sz5,dt5);if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6)) error=kim_api_set_data_f(kimmdl,nm6,sz6,dt6);if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7)) error=kim_api_set_data_f(kimmdl,nm7,sz7,dt7);if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8)) error=kim_api_set_data_f(kimmdl,nm8,sz8,dt8);if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9)) error=kim_api_set_data_f(kimmdl,nm9,sz9,dt9);if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10)) error=kim_api_set_data_f(kimmdl,nm10,sz10,dt10);if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11)) error=kim_api_set_data_f(kimmdl,nm11,sz11,dt11);if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12)) error=kim_api_set_data_f(kimmdl,nm12,sz12,dt12);if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13)) error=kim_api_set_data_f(kimmdl,nm13,sz13,dt13);if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14)) error=kim_api_set_data_f(kimmdl,nm14,sz14,dt14);if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15)) error=kim_api_set_data_f(kimmdl,nm15,sz15,dt15);if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
 if(present(nm16)) error=kim_api_set_data_f(kimmdl,nm16,sz16,dt16);if(errcheck_mltpl(error,msg,16,nm16).lt.KIM_STATUS_OK) return
 if(present(nm17)) error=kim_api_set_data_f(kimmdl,nm17,sz17,dt17);if(errcheck_mltpl(error,msg,17,nm17).lt.KIM_STATUS_OK) return
 if(present(nm18)) error=kim_api_set_data_f(kimmdl,nm18,sz18,dt18);if(errcheck_mltpl(error,msg,18,nm18).lt.KIM_STATUS_OK) return
 if(present(nm19)) error=kim_api_set_data_f(kimmdl,nm19,sz19,dt19);if(errcheck_mltpl(error,msg,19,nm19).lt.KIM_STATUS_OK) return
 if(present(nm20)) error=kim_api_set_data_f(kimmdl,nm20,sz20,dt20);if(errcheck_mltpl(error,msg,20,nm20).lt.KIM_STATUS_OK) return         
	end subroutine kim_api_set_data_multiple_f
        
        subroutine kim_api_set_data_byI_multiple_f(kimmdl,error,nm1,sz1,dt1, nm2,sz2,dt2, nm3,sz3,dt3, nm4,sz4,dt4, nm5,sz5,dt5,&
         nm6,sz6,dt6, nm7,sz7,dt7, nm8,sz8,dt8, nm9,sz9,dt9, nm10,sz10,dt10, nm11,sz11,dt11, nm12,sz12,dt12, nm13,sz13,dt13,&
         nm14,sz14,dt14, nm15,sz15,dt15, nm16,sz16,dt16, nm17,sz17,dt17, nm18,sz18,dt18, nm19,sz19,dt19, nm20,sz20,dt20)
            integer(kind=kim_intptr) :: kimmdl;  integer error, grarg

            character(len=40) ::msg="kim_api_set_data_byI_multiple_f"

            !    declare optional arguments
            integer          :: nm1;   integer(kind=kim_intptr)          :: sz1, dt1
            integer,optional:: nm2;   integer(kind=kim_intptr),optional:: sz2, dt2
            integer,optional:: nm3;   integer(kind=kim_intptr),optional:: sz3, dt3
            integer,optional:: nm4;   integer(kind=kim_intptr),optional:: sz4, dt4
            integer,optional:: nm5;   integer(kind=kim_intptr),optional:: sz5, dt5
            integer,optional:: nm6;   integer(kind=kim_intptr),optional:: sz6, dt6
            integer,optional:: nm7;   integer(kind=kim_intptr),optional:: sz7, dt7
            integer,optional:: nm8;   integer(kind=kim_intptr),optional:: sz8, dt8
            integer,optional:: nm9;   integer(kind=kim_intptr),optional:: sz9, dt9
            integer,optional:: nm10;  integer(kind=kim_intptr),optional:: sz10,dt10
            integer,optional:: nm11;  integer(kind=kim_intptr),optional:: sz11,dt11
            integer,optional:: nm12;  integer(kind=kim_intptr),optional:: sz12,dt12
            integer,optional:: nm13;  integer(kind=kim_intptr),optional:: sz13,dt13
            integer,optional:: nm14;  integer(kind=kim_intptr),optional:: sz14,dt14
            integer,optional:: nm15;  integer(kind=kim_intptr),optional:: sz15,dt15
            integer,optional:: nm16;  integer(kind=kim_intptr),optional:: sz16,dt16
            integer,optional:: nm17;  integer(kind=kim_intptr),optional:: sz17,dt17
            integer,optional:: nm18;  integer(kind=kim_intptr),optional:: sz18,dt18
            integer,optional:: nm19;  integer(kind=kim_intptr),optional:: sz19,dt19
            integer,optional:: nm20;  integer(kind=kim_intptr),optional:: sz20,dt20
            !
            call kim_api_set_data_byi_f(kimmdl,nm1,sz1,dt1,error); 
            if (errcheck_mltpl(error,msg,1,"", nm1).lt.KIM_STATUS_OK) return
            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(sz2 ).or..not.present(dt2 ))) then 
            	if(errcheck_mltpl(error,msg,2,"", nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(sz3 ).or..not.present(dt3 ))) then 
            	if(errcheck_mltpl(error,msg,3,"", nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(sz4 ).or..not.present(dt4 ))) then 
            	if(errcheck_mltpl(error,msg,4,"", nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(sz5 ).or..not.present(dt5 ))) then 
            	if(errcheck_mltpl(error,msg,5,"", nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(sz6 ).or..not.present(dt6 ))) then 
            	if(errcheck_mltpl(error,msg,6,"", nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(sz7 ).or..not.present(dt7 ))) then 
            	if(errcheck_mltpl(error,msg,7,"", nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(sz8 ).or..not.present(dt8 ))) then 
            	if(errcheck_mltpl(error,msg,8,"", nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(sz9 ).or..not.present(dt9 ))) then 
            	if(errcheck_mltpl(error,msg,9,"", nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(sz10 ).or..not.present(dt10 ))) then 
            	if(errcheck_mltpl(error,msg,10,"", nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(sz11 ).or..not.present(dt11 ))) then 
            	if(errcheck_mltpl(error,msg,11,"", nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(sz12 ).or..not.present(dt12 ))) then 
            	if(errcheck_mltpl(error,msg,12,"", nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(sz13 ).or..not.present(dt13 ))) then 
            	if(errcheck_mltpl(error,msg,13,"",nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(sz14 ).or..not.present(dt14 ))) then 
            	if(errcheck_mltpl(error,msg,14,"", nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(sz15 ).or..not.present(dt15 ))) then 
            	if(errcheck_mltpl(error,msg,15,"", nm15 ).lt.KIM_STATUS_OK) return
            elseif( present(nm16 ).and.(.not.present(sz16 ).or..not.present(dt16))) then 
            	if(errcheck_mltpl(error,msg,16,"", nm16 ).lt.KIM_STATUS_OK) return
            elseif( present(nm17 ).and.(.not.present(sz17 ).or..not.present(dt17 ))) then 
            	if(errcheck_mltpl(error,msg,17,"", nm17 ).lt.KIM_STATUS_OK) return
            elseif( present(nm18 ).and.(.not.present(sz18 ).or..not.present(dt18 ))) then 
            	if(errcheck_mltpl(error,msg,18,"", nm18 ).lt.KIM_STATUS_OK) return
            elseif( present(nm19 ).and.(.not.present(sz19 ).or..not.present(dt19 ))) then 
            	if(errcheck_mltpl(error,msg,19,"", nm19 ).lt.KIM_STATUS_OK) return
            elseif( present(nm20 ).and.(.not.present(sz20 ).or..not.present(dt20 ))) then 
            	if(errcheck_mltpl(error,msg,20,"", nm20 ).lt.KIM_STATUS_OK) return
            endif 
	    
	    !process arguments
 if(present(nm2)) call kim_api_set_data_byi_f(kimmdl,nm2,sz2,dt2,error);if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3)) call kim_api_set_data_byi_f(kimmdl,nm3,sz3,dt3,error);if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4)) call kim_api_set_data_byi_f(kimmdl,nm4,sz4,dt4,error);if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5)) call kim_api_set_data_byi_f(kimmdl,nm5,sz5,dt5,error);if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6)) call kim_api_set_data_byi_f(kimmdl,nm6,sz6,dt6,error);if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7)) call kim_api_set_data_byi_f(kimmdl,nm7,sz7,dt7,error);if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8)) call kim_api_set_data_byi_f(kimmdl,nm8,sz8,dt8,error);if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9)) call kim_api_set_data_byi_f(kimmdl,nm9,sz9,dt9,error);if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10))call kim_api_set_data_byi_f(kimmdl,nm10,sz10,dt10,error);if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK)return
 if(present(nm11))call kim_api_set_data_byi_f(kimmdl,nm11,sz11,dt11,error);if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK)return
 if(present(nm12))call kim_api_set_data_byi_f(kimmdl,nm12,sz12,dt12,error);if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK)return
 if(present(nm13))call kim_api_set_data_byi_f(kimmdl,nm13,sz13,dt13,error);if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK)return
 if(present(nm14))call kim_api_set_data_byi_f(kimmdl,nm14,sz14,dt14,error);if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK)return
 if(present(nm15))call kim_api_set_data_byi_f(kimmdl,nm15,sz15,dt15,error);if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK)return
 if(present(nm16))call kim_api_set_data_byi_f(kimmdl,nm16,sz16,dt16,error);if(errcheck_mltpl(error,msg,16,"",nm16).lt.KIM_STATUS_OK)return
 if(present(nm17))call kim_api_set_data_byi_f(kimmdl,nm17,sz17,dt17,error);if(errcheck_mltpl(error,msg,17,"",nm17).lt.KIM_STATUS_OK)return
 if(present(nm18))call kim_api_set_data_byi_f(kimmdl,nm18,sz18,dt18,error);if(errcheck_mltpl(error,msg,18,"",nm18).lt.KIM_STATUS_OK)return
 if(present(nm19))call kim_api_set_data_byi_f(kimmdl,nm19,sz19,dt19,error);if(errcheck_mltpl(error,msg,19,"",nm19).lt.KIM_STATUS_OK)return
 if(present(nm20))call kim_api_set_data_byi_f(kimmdl,nm20,sz20,dt20,error);if(errcheck_mltpl(error,msg,20,"",nm20).lt.KIM_STATUS_OK)return 
       	end subroutine kim_api_set_data_byI_multiple_f
        
        subroutine kim_api_get_data_multiple_f(kimmdl,error, nm1,dt1, nm2,dt2, nm3,dt3, nm4,dt4, nm5,dt5,&
         nm6,dt6, nm7,dt7, nm8,dt8, nm9,dt9, nm10,dt10, nm11,dt11, nm12,dt12, nm13,dt13,&
         nm14,dt14, nm15,dt15, nm16,dt16, nm17,dt17, nm18,dt18, nm19,dt19, nm20,dt20)
            integer(kind=kim_intptr) :: kimmdl;  integer error, grarg

            character(len=40) ::msg="kim_api_get_data_multiple_f"

            !    declare optional arguments
            character(len=*)          :: nm1;   integer(kind=kim_intptr)          :: dt1
            character(len=*),optional:: nm2;   integer(kind=kim_intptr),optional:: dt2
            character(len=*),optional:: nm3;   integer(kind=kim_intptr),optional:: dt3
            character(len=*),optional:: nm4;   integer(kind=kim_intptr),optional:: dt4
            character(len=*),optional:: nm5;   integer(kind=kim_intptr),optional:: dt5
            character(len=*),optional:: nm6;   integer(kind=kim_intptr),optional:: dt6
            character(len=*),optional:: nm7;   integer(kind=kim_intptr),optional:: dt7
            character(len=*),optional:: nm8;   integer(kind=kim_intptr),optional:: dt8
            character(len=*),optional:: nm9;   integer(kind=kim_intptr),optional:: dt9
            character(len=*),optional:: nm10;  integer(kind=kim_intptr),optional:: dt10
            character(len=*),optional:: nm11;  integer(kind=kim_intptr),optional:: dt11
            character(len=*),optional:: nm12;  integer(kind=kim_intptr),optional:: dt12
            character(len=*),optional:: nm13;  integer(kind=kim_intptr),optional:: dt13
            character(len=*),optional:: nm14;  integer(kind=kim_intptr),optional:: dt14
            character(len=*),optional:: nm15;  integer(kind=kim_intptr),optional:: dt15
            character(len=*),optional:: nm16;  integer(kind=kim_intptr),optional:: dt16
            character(len=*),optional:: nm17;  integer(kind=kim_intptr),optional:: dt17
            character(len=*),optional:: nm18;  integer(kind=kim_intptr),optional:: dt18
            character(len=*),optional:: nm19;  integer(kind=kim_intptr),optional:: dt19
            character(len=*),optional:: nm20;  integer(kind=kim_intptr),optional:: dt20
            !
            dt1 = kim_api_get_data_f(kimmdl,nm1,error); 
            if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(dt2 ))) then 
            	if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(dt3 ))) then 
            	if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(dt4 ))) then 
            	if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(dt5 ))) then 
            	if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(dt6 ))) then 
            	if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(dt7 ))) then 
            	if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(dt8 ))) then 
            	if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(dt9 ))) then 
            	if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(dt10 ))) then 
            	if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(dt11 ))) then 
            	if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(dt12 ))) then 
            	if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(dt13 ))) then 
            	if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(dt14 ))) then 
            	if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(dt15 ))) then 
            	if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            elseif( present(nm16 ).and.(.not.present(dt16))) then 
            	if(errcheck_mltpl(error,msg,16, nm16 ).lt.KIM_STATUS_OK) return
            elseif( present(nm17 ).and.(.not.present(dt17 ))) then 
            	if(errcheck_mltpl(error,msg,17, nm17 ).lt.KIM_STATUS_OK) return
            elseif( present(nm18 ).and.(.not.present(dt18 ))) then 
            	if(errcheck_mltpl(error,msg,18, nm18 ).lt.KIM_STATUS_OK) return
            elseif( present(nm19 ).and.(.not.present(dt19 ))) then 
            	if(errcheck_mltpl(error,msg,19, nm19 ).lt.KIM_STATUS_OK) return
            elseif( present(nm20 ).and.(.not.present(dt20 ))) then 
            	if(errcheck_mltpl(error,msg,20, nm20 ).lt.KIM_STATUS_OK) return
            endif 
	    
	    !process arguments
 if(present(nm2)) dt2= kim_api_get_data_f(kimmdl,nm2,error);if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3)) dt3= kim_api_get_data_f(kimmdl,nm3,error);if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4)) dt4= kim_api_get_data_f(kimmdl,nm4,error);if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5)) dt5= kim_api_get_data_f(kimmdl,nm5,error);if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6)) dt6= kim_api_get_data_f(kimmdl,nm6,error);if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7)) dt7= kim_api_get_data_f(kimmdl,nm7,error);if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8)) dt8= kim_api_get_data_f(kimmdl,nm8,error);if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9)) dt9= kim_api_get_data_f(kimmdl,nm9,error);if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10)) dt10= kim_api_get_data_f(kimmdl,nm10,error);if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11)) dt11= kim_api_get_data_f(kimmdl,nm11,error);if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12)) dt12= kim_api_get_data_f(kimmdl,nm12,error);if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13)) dt13= kim_api_get_data_f(kimmdl,nm13,error);if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14)) dt14= kim_api_get_data_f(kimmdl,nm14,error);if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15)) dt15= kim_api_get_data_f(kimmdl,nm15,error);if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
 if(present(nm16)) dt16= kim_api_get_data_f(kimmdl,nm16,error);if(errcheck_mltpl(error,msg,16,nm16).lt.KIM_STATUS_OK) return
 if(present(nm17)) dt17= kim_api_get_data_f(kimmdl,nm17,error);if(errcheck_mltpl(error,msg,17,nm17).lt.KIM_STATUS_OK) return
 if(present(nm18)) dt18= kim_api_get_data_f(kimmdl,nm18,error);if(errcheck_mltpl(error,msg,18,nm18).lt.KIM_STATUS_OK) return
 if(present(nm19)) dt19= kim_api_get_data_f(kimmdl,nm19,error);if(errcheck_mltpl(error,msg,19,nm19).lt.KIM_STATUS_OK) return
 if(present(nm20)) dt20= kim_api_get_data_f(kimmdl,nm20,error);if(errcheck_mltpl(error,msg,20,nm20).lt.KIM_STATUS_OK) return 	
	end subroutine kim_api_get_data_multiple_f

	subroutine kim_api_get_data_byI_multiple_f(kimmdl,error, nm1,dt1, nm2,dt2, nm3,dt3, nm4,dt4, nm5,dt5,&
         nm6,dt6, nm7,dt7, nm8,dt8, nm9,dt9, nm10,dt10, nm11,dt11, nm12,dt12, nm13,dt13,&
         nm14,dt14, nm15,dt15, nm16,dt16, nm17,dt17, nm18,dt18, nm19,dt19, nm20,dt20)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_get_data_byI_multiple_f"

            !    declare optional arguments
            integer          :: nm1;   integer(kind=kim_intptr)          :: dt1
            integer,optional:: nm2;   integer(kind=kim_intptr),optional:: dt2
            integer,optional:: nm3;   integer(kind=kim_intptr),optional:: dt3
            integer,optional:: nm4;   integer(kind=kim_intptr),optional:: dt4
            integer,optional:: nm5;   integer(kind=kim_intptr),optional:: dt5
            integer,optional:: nm6;   integer(kind=kim_intptr),optional:: dt6
            integer,optional:: nm7;   integer(kind=kim_intptr),optional:: dt7
            integer,optional:: nm8;   integer(kind=kim_intptr),optional:: dt8
            integer,optional:: nm9;   integer(kind=kim_intptr),optional:: dt9
            integer,optional:: nm10;  integer(kind=kim_intptr),optional:: dt10
            integer,optional:: nm11;  integer(kind=kim_intptr),optional:: dt11
            integer,optional:: nm12;  integer(kind=kim_intptr),optional:: dt12
            integer,optional:: nm13;  integer(kind=kim_intptr),optional:: dt13
            integer,optional:: nm14;  integer(kind=kim_intptr),optional:: dt14
            integer,optional:: nm15;  integer(kind=kim_intptr),optional:: dt15
            integer,optional:: nm16;  integer(kind=kim_intptr),optional:: dt16
            integer,optional:: nm17;  integer(kind=kim_intptr),optional:: dt17
            integer,optional:: nm18;  integer(kind=kim_intptr),optional:: dt18
            integer,optional:: nm19;  integer(kind=kim_intptr),optional:: dt19
            integer,optional:: nm20;  integer(kind=kim_intptr),optional:: dt20
            !
            dt1 = kim_api_get_data_byi_f(kimmdl,nm1,error); 
            if (errcheck_mltpl(error,msg,1, "", nm1).lt.KIM_STATUS_OK) return
            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(dt2 ))) then 
            	if(errcheck_mltpl(error,msg,2, "",nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(dt3 ))) then 
            	if(errcheck_mltpl(error,msg,3, "",nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(dt4 ))) then 
            	if(errcheck_mltpl(error,msg,4, "",nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(dt5 ))) then 
            	if(errcheck_mltpl(error,msg,5, "",nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(dt6 ))) then 
            	if(errcheck_mltpl(error,msg,6, "",nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(dt7 ))) then 
            	if(errcheck_mltpl(error,msg,7, "",nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(dt8 ))) then 
            	if(errcheck_mltpl(error,msg,8, "",nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(dt9 ))) then 
            	if(errcheck_mltpl(error,msg,9, "",nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(dt10 ))) then 
            	if(errcheck_mltpl(error,msg,10, "",nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(dt11 ))) then 
            	if(errcheck_mltpl(error,msg,11, "",nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(dt12 ))) then 
            	if(errcheck_mltpl(error,msg,12, "",nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(dt13 ))) then 
            	if(errcheck_mltpl(error,msg,13, "",nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(dt14 ))) then 
            	if(errcheck_mltpl(error,msg,14, "",nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(dt15 ))) then 
            	if(errcheck_mltpl(error,msg,15, "",nm15 ).lt.KIM_STATUS_OK) return
            elseif( present(nm16 ).and.(.not.present(dt16))) then 
            	if(errcheck_mltpl(error,msg,16, "",nm16 ).lt.KIM_STATUS_OK) return
            elseif( present(nm17 ).and.(.not.present(dt17 ))) then 
            	if(errcheck_mltpl(error,msg,17, "",nm17 ).lt.KIM_STATUS_OK) return
            elseif( present(nm18 ).and.(.not.present(dt18 ))) then 
            	if(errcheck_mltpl(error,msg,18, "",nm18 ).lt.KIM_STATUS_OK) return
            elseif( present(nm19 ).and.(.not.present(dt19 ))) then 
            	if(errcheck_mltpl(error,msg,19, "",nm19 ).lt.KIM_STATUS_OK) return
            elseif( present(nm20 ).and.(.not.present(dt20 ))) then 
            	if(errcheck_mltpl(error,msg,20, "",nm20 ).lt.KIM_STATUS_OK) return
            endif 
	    
	    !process arguments
 if(present(nm2)) dt2= kim_api_get_data_byi_f(kimmdl,nm2,error);if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3)) dt3= kim_api_get_data_byi_f(kimmdl,nm3,error);if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4)) dt4= kim_api_get_data_byi_f(kimmdl,nm4,error);if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5)) dt5= kim_api_get_data_byi_f(kimmdl,nm5,error);if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6)) dt6= kim_api_get_data_byi_f(kimmdl,nm6,error);if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7)) dt7= kim_api_get_data_byi_f(kimmdl,nm7,error);if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8)) dt8= kim_api_get_data_byi_f(kimmdl,nm8,error);if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9)) dt9= kim_api_get_data_byi_f(kimmdl,nm9,error);if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10)) dt10= kim_api_get_data_byi_f(kimmdl,nm10,error);if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK) return
 if(present(nm11)) dt11= kim_api_get_data_byi_f(kimmdl,nm11,error);if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK) return
 if(present(nm12)) dt12= kim_api_get_data_byi_f(kimmdl,nm12,error);if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK) return
 if(present(nm13)) dt13= kim_api_get_data_byi_f(kimmdl,nm13,error);if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK) return
 if(present(nm14)) dt14= kim_api_get_data_byi_f(kimmdl,nm14,error);if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK) return
 if(present(nm15)) dt15= kim_api_get_data_byi_f(kimmdl,nm15,error);if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK) return
 if(present(nm16)) dt16= kim_api_get_data_byi_f(kimmdl,nm16,error);if(errcheck_mltpl(error,msg,16,"",nm16).lt.KIM_STATUS_OK) return
 if(present(nm17)) dt17= kim_api_get_data_byi_f(kimmdl,nm17,error);if(errcheck_mltpl(error,msg,17,"",nm17).lt.KIM_STATUS_OK) return
 if(present(nm18)) dt18= kim_api_get_data_byi_f(kimmdl,nm18,error);if(errcheck_mltpl(error,msg,18,"",nm18).lt.KIM_STATUS_OK) return
 if(present(nm19)) dt19= kim_api_get_data_byi_f(kimmdl,nm19,error);if(errcheck_mltpl(error,msg,19,"",nm19).lt.KIM_STATUS_OK) return
 if(present(nm20)) dt20= kim_api_get_data_byi_f(kimmdl,nm20,error);if(errcheck_mltpl(error,msg,20,"",nm20).lt.KIM_STATUS_OK) return 			
	end subroutine kim_api_get_data_byI_multiple_f

	subroutine kim_api_get_compute_multiple_f(kimmdl,error, nm1,dt1, nm2,dt2, nm3,dt3, nm4,dt4, nm5,dt5,&
         nm6,dt6, nm7,dt7, nm8,dt8, nm9,dt9, nm10,dt10, nm11,dt11, nm12,dt12, nm13,dt13,&
         nm14,dt14, nm15,dt15, nm16,dt16, nm17,dt17, nm18,dt18, nm19,dt19, nm20,dt20)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_get_compute_multiple_f"

            !    declare optional arguments
            character(len=*)          :: nm1;   integer          :: dt1
            character(len=*),optional:: nm2;   integer,optional:: dt2
            character(len=*),optional:: nm3;   integer,optional:: dt3
            character(len=*),optional:: nm4;   integer,optional:: dt4
            character(len=*),optional:: nm5;   integer,optional:: dt5
            character(len=*),optional:: nm6;   integer,optional:: dt6
            character(len=*),optional:: nm7;   integer,optional:: dt7
            character(len=*),optional:: nm8;   integer,optional:: dt8
            character(len=*),optional:: nm9;   integer,optional:: dt9
            character(len=*),optional:: nm10;  integer,optional:: dt10
            character(len=*),optional:: nm11;  integer,optional:: dt11
            character(len=*),optional:: nm12;  integer,optional:: dt12
            character(len=*),optional:: nm13;  integer,optional:: dt13
            character(len=*),optional:: nm14;  integer,optional:: dt14
            character(len=*),optional:: nm15;  integer,optional:: dt15
            character(len=*),optional:: nm16;  integer,optional:: dt16
            character(len=*),optional:: nm17;  integer,optional:: dt17
            character(len=*),optional:: nm18;  integer,optional:: dt18
            character(len=*),optional:: nm19;  integer,optional:: dt19
            character(len=*),optional:: nm20;  integer,optional:: dt20
            !
            dt1 = kim_api_isit_compute_f(kimmdl,nm1,error); 
            if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(dt2 ))) then 
            	if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(dt3 ))) then 
            	if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(dt4 ))) then 
            	if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(dt5 ))) then 
            	if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(dt6 ))) then 
            	if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(dt7 ))) then 
            	if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(dt8 ))) then 
            	if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(dt9 ))) then 
            	if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(dt10 ))) then 
            	if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(dt11 ))) then 
            	if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(dt12 ))) then 
            	if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(dt13 ))) then 
            	if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(dt14 ))) then 
            	if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(dt15 ))) then 
            	if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            elseif( present(nm16 ).and.(.not.present(dt16))) then 
            	if(errcheck_mltpl(error,msg,16, nm16 ).lt.KIM_STATUS_OK) return
            elseif( present(nm17 ).and.(.not.present(dt17 ))) then 
            	if(errcheck_mltpl(error,msg,17, nm17 ).lt.KIM_STATUS_OK) return
            elseif( present(nm18 ).and.(.not.present(dt18 ))) then 
            	if(errcheck_mltpl(error,msg,18, nm18 ).lt.KIM_STATUS_OK) return
            elseif( present(nm19 ).and.(.not.present(dt19 ))) then 
            	if(errcheck_mltpl(error,msg,19, nm19 ).lt.KIM_STATUS_OK) return
            elseif( present(nm20 ).and.(.not.present(dt20 ))) then 
            	if(errcheck_mltpl(error,msg,20, nm20 ).lt.KIM_STATUS_OK) return
            endif 
	    
	    !process arguments
 if(present(nm2)) dt2= kim_api_isit_compute_f(kimmdl,nm2,error);if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3)) dt3= kim_api_isit_compute_f(kimmdl,nm3,error);if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4)) dt4= kim_api_isit_compute_f(kimmdl,nm4,error);if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5)) dt5= kim_api_isit_compute_f(kimmdl,nm5,error);if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6)) dt6= kim_api_isit_compute_f(kimmdl,nm6,error);if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7)) dt7= kim_api_isit_compute_f(kimmdl,nm7,error);if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8)) dt8= kim_api_isit_compute_f(kimmdl,nm8,error);if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9)) dt9= kim_api_isit_compute_f(kimmdl,nm9,error);if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10)) dt10= kim_api_isit_compute_f(kimmdl,nm10,error);if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11)) dt11= kim_api_isit_compute_f(kimmdl,nm11,error);if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12)) dt12= kim_api_isit_compute_f(kimmdl,nm12,error);if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13)) dt13= kim_api_isit_compute_f(kimmdl,nm13,error);if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14)) dt14= kim_api_isit_compute_f(kimmdl,nm14,error);if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15)) dt15= kim_api_isit_compute_f(kimmdl,nm15,error);if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
 if(present(nm16)) dt16= kim_api_isit_compute_f(kimmdl,nm16,error);if(errcheck_mltpl(error,msg,16,nm16).lt.KIM_STATUS_OK) return
 if(present(nm17)) dt17= kim_api_isit_compute_f(kimmdl,nm17,error);if(errcheck_mltpl(error,msg,17,nm17).lt.KIM_STATUS_OK) return
 if(present(nm18)) dt18= kim_api_isit_compute_f(kimmdl,nm18,error);if(errcheck_mltpl(error,msg,18,nm18).lt.KIM_STATUS_OK) return
 if(present(nm19)) dt19= kim_api_isit_compute_f(kimmdl,nm19,error);if(errcheck_mltpl(error,msg,19,nm19).lt.KIM_STATUS_OK) return
 if(present(nm20)) dt20= kim_api_isit_compute_f(kimmdl,nm20,error);if(errcheck_mltpl(error,msg,20,nm20).lt.KIM_STATUS_OK) return 
	end subroutine kim_api_get_compute_multiple_f
        
        subroutine kim_api_get_compute_byI_mltpl_f(kimmdl,error, nm1,dt1, nm2,dt2, nm3,dt3, nm4,dt4, nm5,dt5,&
         nm6,dt6, nm7,dt7, nm8,dt8, nm9,dt9, nm10,dt10, nm11,dt11, nm12,dt12, nm13,dt13,&
         nm14,dt14, nm15,dt15, nm16,dt16, nm17,dt17, nm18,dt18, nm19,dt19, nm20,dt20)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_get_compute_byI_mltpl_f"

            !    declare optional arguments
            integer          :: nm1;   integer          :: dt1
            integer,optional:: nm2;   integer,optional:: dt2
            integer,optional:: nm3;   integer,optional:: dt3
            integer,optional:: nm4;   integer,optional:: dt4
            integer,optional:: nm5;   integer,optional:: dt5
            integer,optional:: nm6;   integer,optional:: dt6
            integer,optional:: nm7;   integer,optional:: dt7
            integer,optional:: nm8;   integer,optional:: dt8
            integer,optional:: nm9;   integer,optional:: dt9
            integer,optional:: nm10;  integer,optional:: dt10
            integer,optional:: nm11;  integer,optional:: dt11
            integer,optional:: nm12;  integer,optional:: dt12
            integer,optional:: nm13;  integer,optional:: dt13
            integer,optional:: nm14;  integer,optional:: dt14
            integer,optional:: nm15;  integer,optional:: dt15
            integer,optional:: nm16;  integer,optional:: dt16
            integer,optional:: nm17;  integer,optional:: dt17
            integer,optional:: nm18;  integer,optional:: dt18
            integer,optional:: nm19;  integer,optional:: dt19
            integer,optional:: nm20;  integer,optional:: dt20
            !
            dt1 = kim_api_isit_compute_byi_f(kimmdl,nm1,error); 
            if (errcheck_mltpl(error,msg,1, "", nm1).lt.KIM_STATUS_OK) return
            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(dt2 ))) then 
            	if(errcheck_mltpl(error,msg,2, "",nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(dt3 ))) then 
            	if(errcheck_mltpl(error,msg,3, "",nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(dt4 ))) then 
            	if(errcheck_mltpl(error,msg,4, "",nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(dt5 ))) then 
            	if(errcheck_mltpl(error,msg,5, "",nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(dt6 ))) then 
            	if(errcheck_mltpl(error,msg,6, "",nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(dt7 ))) then 
            	if(errcheck_mltpl(error,msg,7, "",nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(dt8 ))) then 
            	if(errcheck_mltpl(error,msg,8, "",nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(dt9 ))) then 
            	if(errcheck_mltpl(error,msg,9, "",nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(dt10 ))) then 
            	if(errcheck_mltpl(error,msg,10, "",nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(dt11 ))) then 
            	if(errcheck_mltpl(error,msg,11, "",nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(dt12 ))) then 
            	if(errcheck_mltpl(error,msg,12, "",nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(dt13 ))) then 
            	if(errcheck_mltpl(error,msg,13, "",nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(dt14 ))) then 
            	if(errcheck_mltpl(error,msg,14, "",nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(dt15 ))) then 
            	if(errcheck_mltpl(error,msg,15, "",nm15 ).lt.KIM_STATUS_OK) return
            elseif( present(nm16 ).and.(.not.present(dt16))) then 
            	if(errcheck_mltpl(error,msg,16, "",nm16 ).lt.KIM_STATUS_OK) return
            elseif( present(nm17 ).and.(.not.present(dt17 ))) then 
            	if(errcheck_mltpl(error,msg,17, "",nm17 ).lt.KIM_STATUS_OK) return
            elseif( present(nm18 ).and.(.not.present(dt18 ))) then 
            	if(errcheck_mltpl(error,msg,18, "",nm18 ).lt.KIM_STATUS_OK) return
            elseif( present(nm19 ).and.(.not.present(dt19 ))) then 
            	if(errcheck_mltpl(error,msg,19, "",nm19 ).lt.KIM_STATUS_OK) return
            elseif( present(nm20 ).and.(.not.present(dt20 ))) then 
            	if(errcheck_mltpl(error,msg,20, "",nm20 ).lt.KIM_STATUS_OK) return
            endif 
	    
	    !process arguments
 if(present(nm2)) dt2= kim_api_isit_compute_byi_f(kimmdl,nm2,error);if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3)) dt3= kim_api_isit_compute_byi_f(kimmdl,nm3,error);if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4)) dt4= kim_api_isit_compute_byi_f(kimmdl,nm4,error);if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5)) dt5= kim_api_isit_compute_byi_f(kimmdl,nm5,error);if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6)) dt6= kim_api_isit_compute_byi_f(kimmdl,nm6,error);if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7)) dt7= kim_api_isit_compute_byi_f(kimmdl,nm7,error);if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8)) dt8= kim_api_isit_compute_byi_f(kimmdl,nm8,error);if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9)) dt9= kim_api_isit_compute_byi_f(kimmdl,nm9,error);if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10)) dt10= kim_api_isit_compute_byi_f(kimmdl,nm10,error);if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK) return
 if(present(nm11)) dt11= kim_api_isit_compute_byi_f(kimmdl,nm11,error);if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK) return
 if(present(nm12)) dt12= kim_api_isit_compute_byi_f(kimmdl,nm12,error);if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK) return
 if(present(nm13)) dt13= kim_api_isit_compute_byi_f(kimmdl,nm13,error);if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK) return
 if(present(nm14)) dt14= kim_api_isit_compute_byi_f(kimmdl,nm14,error);if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK) return
 if(present(nm15)) dt15= kim_api_isit_compute_byi_f(kimmdl,nm15,error);if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK) return
 if(present(nm16)) dt16= kim_api_isit_compute_byi_f(kimmdl,nm16,error);if(errcheck_mltpl(error,msg,16,"",nm16).lt.KIM_STATUS_OK) return
 if(present(nm17)) dt17= kim_api_isit_compute_byi_f(kimmdl,nm17,error);if(errcheck_mltpl(error,msg,17,"",nm17).lt.KIM_STATUS_OK) return
 if(present(nm18)) dt18= kim_api_isit_compute_byi_f(kimmdl,nm18,error);if(errcheck_mltpl(error,msg,18,"",nm18).lt.KIM_STATUS_OK) return
 if(present(nm19)) dt19= kim_api_isit_compute_byi_f(kimmdl,nm19,error);if(errcheck_mltpl(error,msg,19,"",nm19).lt.KIM_STATUS_OK) return
 if(present(nm20)) dt20= kim_api_isit_compute_byi_f(kimmdl,nm20,error);if(errcheck_mltpl(error,msg,20,"",nm20).lt.KIM_STATUS_OK) return 
        end subroutine kim_api_get_compute_byI_mltpl_f
 
        subroutine kim_api_get_index_multiple_f(kimmdl,error, nm1,dt1, nm2,dt2, nm3,dt3, nm4,dt4, nm5,dt5,&
         nm6,dt6, nm7,dt7, nm8,dt8, nm9,dt9, nm10,dt10, nm11,dt11, nm12,dt12, nm13,dt13,&
         nm14,dt14, nm15,dt15, nm16,dt16, nm17,dt17, nm18,dt18, nm19,dt19, nm20,dt20)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_get_index_multiple_f"

            !    declare optional arguments
            character(len=*)          :: nm1;   integer          :: dt1
            character(len=*),optional:: nm2;   integer,optional:: dt2
            character(len=*),optional:: nm3;   integer,optional:: dt3
            character(len=*),optional:: nm4;   integer,optional:: dt4
            character(len=*),optional:: nm5;   integer,optional:: dt5
            character(len=*),optional:: nm6;   integer,optional:: dt6
            character(len=*),optional:: nm7;   integer,optional:: dt7
            character(len=*),optional:: nm8;   integer,optional:: dt8
            character(len=*),optional:: nm9;   integer,optional:: dt9
            character(len=*),optional:: nm10;  integer,optional:: dt10
            character(len=*),optional:: nm11;  integer,optional:: dt11
            character(len=*),optional:: nm12;  integer,optional:: dt12
            character(len=*),optional:: nm13;  integer,optional:: dt13
            character(len=*),optional:: nm14;  integer,optional:: dt14
            character(len=*),optional:: nm15;  integer,optional:: dt15
            character(len=*),optional:: nm16;  integer,optional:: dt16
            character(len=*),optional:: nm17;  integer,optional:: dt17
            character(len=*),optional:: nm18;  integer,optional:: dt18
            character(len=*),optional:: nm19;  integer,optional:: dt19
            character(len=*),optional:: nm20;  integer,optional:: dt20
            !
            dt1 = kim_api_get_index_f(kimmdl,nm1,error); 
            if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(dt2 ))) then 
            	if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(dt3 ))) then 
            	if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(dt4 ))) then 
            	if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(dt5 ))) then 
            	if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(dt6 ))) then 
            	if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(dt7 ))) then 
            	if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(dt8 ))) then 
            	if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(dt9 ))) then 
            	if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(dt10 ))) then 
            	if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(dt11 ))) then 
            	if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(dt12 ))) then 
            	if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(dt13 ))) then 
            	if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(dt14 ))) then 
            	if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(dt15 ))) then 
            	if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            elseif( present(nm16 ).and.(.not.present(dt16))) then 
            	if(errcheck_mltpl(error,msg,16, nm16 ).lt.KIM_STATUS_OK) return
            elseif( present(nm17 ).and.(.not.present(dt17 ))) then 
            	if(errcheck_mltpl(error,msg,17, nm17 ).lt.KIM_STATUS_OK) return
            elseif( present(nm18 ).and.(.not.present(dt18 ))) then 
            	if(errcheck_mltpl(error,msg,18, nm18 ).lt.KIM_STATUS_OK) return
            elseif( present(nm19 ).and.(.not.present(dt19 ))) then 
            	if(errcheck_mltpl(error,msg,19, nm19 ).lt.KIM_STATUS_OK) return
            elseif( present(nm20 ).and.(.not.present(dt20 ))) then 
            	if(errcheck_mltpl(error,msg,20, nm20 ).lt.KIM_STATUS_OK) return
            endif 
	    
	    !process arguments
 if(present(nm2)) dt2= kim_api_get_index_f(kimmdl,nm2,error);if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3)) dt3= kim_api_get_index_f(kimmdl,nm3,error);if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4)) dt4= kim_api_get_index_f(kimmdl,nm4,error);if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5)) dt5= kim_api_get_index_f(kimmdl,nm5,error);if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6)) dt6= kim_api_get_index_f(kimmdl,nm6,error);if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7)) dt7= kim_api_get_index_f(kimmdl,nm7,error);if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8)) dt8= kim_api_get_index_f(kimmdl,nm8,error);if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9)) dt9= kim_api_get_index_f(kimmdl,nm9,error);if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10)) dt10= kim_api_get_index_f(kimmdl,nm10,error);if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11)) dt11= kim_api_get_index_f(kimmdl,nm11,error);if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12)) dt12= kim_api_get_index_f(kimmdl,nm12,error);if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13)) dt13= kim_api_get_index_f(kimmdl,nm13,error);if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14)) dt14= kim_api_get_index_f(kimmdl,nm14,error);if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15)) dt15= kim_api_get_index_f(kimmdl,nm15,error);if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
 if(present(nm16)) dt16= kim_api_get_index_f(kimmdl,nm16,error);if(errcheck_mltpl(error,msg,16,nm16).lt.KIM_STATUS_OK) return
 if(present(nm17)) dt17= kim_api_get_index_f(kimmdl,nm17,error);if(errcheck_mltpl(error,msg,17,nm17).lt.KIM_STATUS_OK) return
 if(present(nm18)) dt18= kim_api_get_index_f(kimmdl,nm18,error);if(errcheck_mltpl(error,msg,18,nm18).lt.KIM_STATUS_OK) return
 if(present(nm19)) dt19= kim_api_get_index_f(kimmdl,nm19,error);if(errcheck_mltpl(error,msg,19,nm19).lt.KIM_STATUS_OK) return
 if(present(nm20)) dt20= kim_api_get_index_f(kimmdl,nm20,error);if(errcheck_mltpl(error,msg,20,nm20).lt.KIM_STATUS_OK) return 
	end subroutine kim_api_get_index_multiple_f
         
         subroutine kim_api_set_compute_multiple_f(kimmdl,error, nm1,dt1, nm2,dt2, nm3,dt3, nm4,dt4, nm5,dt5,&
         nm6,dt6, nm7,dt7, nm8,dt8, nm9,dt9, nm10,dt10, nm11,dt11, nm12,dt12, nm13,dt13,&
         nm14,dt14, nm15,dt15, nm16,dt16, nm17,dt17, nm18,dt18, nm19,dt19, nm20,dt20)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_set_compute_multiple_f"

            !    declare optional arguments
            character(len=*)          :: nm1;   integer          :: dt1
            character(len=*),optional:: nm2;   integer,optional:: dt2
            character(len=*),optional:: nm3;   integer,optional:: dt3
            character(len=*),optional:: nm4;   integer,optional:: dt4
            character(len=*),optional:: nm5;   integer,optional:: dt5
            character(len=*),optional:: nm6;   integer,optional:: dt6
            character(len=*),optional:: nm7;   integer,optional:: dt7
            character(len=*),optional:: nm8;   integer,optional:: dt8
            character(len=*),optional:: nm9;   integer,optional:: dt9
            character(len=*),optional:: nm10;  integer,optional:: dt10
            character(len=*),optional:: nm11;  integer,optional:: dt11
            character(len=*),optional:: nm12;  integer,optional:: dt12
            character(len=*),optional:: nm13;  integer,optional:: dt13
            character(len=*),optional:: nm14;  integer,optional:: dt14
            character(len=*),optional:: nm15;  integer,optional:: dt15
            character(len=*),optional:: nm16;  integer,optional:: dt16
            character(len=*),optional:: nm17;  integer,optional:: dt17
            character(len=*),optional:: nm18;  integer,optional:: dt18
            character(len=*),optional:: nm19;  integer,optional:: dt19
            character(len=*),optional:: nm20;  integer,optional:: dt20
            !
            call kim_api_set_compute_f(kimmdl,nm1,dt1,error); 
            if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(dt2 ))) then 
            	if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(dt3 ))) then 
            	if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(dt4 ))) then 
            	if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(dt5 ))) then 
            	if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(dt6 ))) then 
            	if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(dt7 ))) then 
            	if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(dt8 ))) then 
            	if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(dt9 ))) then 
            	if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(dt10 ))) then 
            	if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(dt11 ))) then 
            	if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(dt12 ))) then 
            	if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(dt13 ))) then 
            	if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(dt14 ))) then 
            	if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(dt15 ))) then 
            	if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            elseif( present(nm16 ).and.(.not.present(dt16))) then 
            	if(errcheck_mltpl(error,msg,16, nm16 ).lt.KIM_STATUS_OK) return
            elseif( present(nm17 ).and.(.not.present(dt17 ))) then 
            	if(errcheck_mltpl(error,msg,17, nm17 ).lt.KIM_STATUS_OK) return
            elseif( present(nm18 ).and.(.not.present(dt18 ))) then 
            	if(errcheck_mltpl(error,msg,18, nm18 ).lt.KIM_STATUS_OK) return
            elseif( present(nm19 ).and.(.not.present(dt19 ))) then 
            	if(errcheck_mltpl(error,msg,19, nm19 ).lt.KIM_STATUS_OK) return
            elseif( present(nm20 ).and.(.not.present(dt20 ))) then 
            	if(errcheck_mltpl(error,msg,20, nm20 ).lt.KIM_STATUS_OK) return
            endif 
	    
	    !process arguments
 if(present(nm2)) call kim_api_set_compute_f(kimmdl,nm2,dt2,error);if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3)) call kim_api_set_compute_f(kimmdl,nm3,dt3,error);if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4)) call kim_api_set_compute_f(kimmdl,nm4,dt4,error);if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5)) call kim_api_set_compute_f(kimmdl,nm5,dt5,error);if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6)) call kim_api_set_compute_f(kimmdl,nm6,dt6,error);if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7)) call kim_api_set_compute_f(kimmdl,nm7,dt7,error);if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8)) call kim_api_set_compute_f(kimmdl,nm8,dt8,error);if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9)) call kim_api_set_compute_f(kimmdl,nm9,dt9,error);if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10)) call kim_api_set_compute_f(kimmdl,nm10,dt10,error);if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11)) call kim_api_set_compute_f(kimmdl,nm11,dt11,error);if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12)) call kim_api_set_compute_f(kimmdl,nm12,dt12,error);if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13)) call kim_api_set_compute_f(kimmdl,nm13,dt13,error);if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14)) call kim_api_set_compute_f(kimmdl,nm14,dt14,error);if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15)) call kim_api_set_compute_f(kimmdl,nm15,dt15,error);if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
 if(present(nm16)) call kim_api_set_compute_f(kimmdl,nm16,dt16,error);if(errcheck_mltpl(error,msg,16,nm16).lt.KIM_STATUS_OK) return
 if(present(nm17)) call kim_api_set_compute_f(kimmdl,nm17,dt17,error);if(errcheck_mltpl(error,msg,17,nm17).lt.KIM_STATUS_OK) return
 if(present(nm18)) call kim_api_set_compute_f(kimmdl,nm18,dt18,error);if(errcheck_mltpl(error,msg,18,nm18).lt.KIM_STATUS_OK) return
 if(present(nm19)) call kim_api_set_compute_f(kimmdl,nm19,dt19,error);if(errcheck_mltpl(error,msg,19,nm19).lt.KIM_STATUS_OK) return
 if(present(nm20)) call kim_api_set_compute_f(kimmdl,nm20,dt20,error);if(errcheck_mltpl(error,msg,20,nm20).lt.KIM_STATUS_OK) return 
	 end subroutine kim_api_set_compute_multiple_f
         
        subroutine kim_api_set_compute_byI_mltpl_f(kimmdl,error, nm1,dt1, nm2,dt2, nm3,dt3, nm4,dt4, nm5,dt5,&
         nm6,dt6, nm7,dt7, nm8,dt8, nm9,dt9, nm10,dt10, nm11,dt11, nm12,dt12, nm13,dt13,&
         nm14,dt14, nm15,dt15, nm16,dt16, nm17,dt17, nm18,dt18, nm19,dt19, nm20,dt20)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_set_compute_byi_mltpl_f"

            !    declare optional arguments
            integer          :: nm1;   integer          :: dt1
            integer,optional:: nm2;   integer,optional:: dt2
            integer,optional:: nm3;   integer,optional:: dt3
            integer,optional:: nm4;   integer,optional:: dt4
            integer,optional:: nm5;   integer,optional:: dt5
            integer,optional:: nm6;   integer,optional:: dt6
            integer,optional:: nm7;   integer,optional:: dt7
            integer,optional:: nm8;   integer,optional:: dt8
            integer,optional:: nm9;   integer,optional:: dt9
            integer,optional:: nm10;  integer,optional:: dt10
            integer,optional:: nm11;  integer,optional:: dt11
            integer,optional:: nm12;  integer,optional:: dt12
            integer,optional:: nm13;  integer,optional:: dt13
            integer,optional:: nm14;  integer,optional:: dt14
            integer,optional:: nm15;  integer,optional:: dt15
            integer,optional:: nm16;  integer,optional:: dt16
            integer,optional:: nm17;  integer,optional:: dt17
            integer,optional:: nm18;  integer,optional:: dt18
            integer,optional:: nm19;  integer,optional:: dt19
            integer,optional:: nm20;  integer,optional:: dt20
            !
            call kim_api_set_compute_byi_f(kimmdl,nm1,dt1,error); 
            if (errcheck_mltpl(error,msg,1, "", nm1).lt.KIM_STATUS_OK) return
            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(dt2 ))) then 
            	if(errcheck_mltpl(error,msg,2, "",nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(dt3 ))) then 
            	if(errcheck_mltpl(error,msg,3, "",nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(dt4 ))) then 
            	if(errcheck_mltpl(error,msg,4, "",nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(dt5 ))) then 
            	if(errcheck_mltpl(error,msg,5, "",nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(dt6 ))) then 
            	if(errcheck_mltpl(error,msg,6, "",nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(dt7 ))) then 
            	if(errcheck_mltpl(error,msg,7, "",nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(dt8 ))) then 
            	if(errcheck_mltpl(error,msg,8, "",nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(dt9 ))) then 
            	if(errcheck_mltpl(error,msg,9, "",nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(dt10 ))) then 
            	if(errcheck_mltpl(error,msg,10, "",nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(dt11 ))) then 
            	if(errcheck_mltpl(error,msg,11, "",nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(dt12 ))) then 
            	if(errcheck_mltpl(error,msg,12, "",nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(dt13 ))) then 
            	if(errcheck_mltpl(error,msg,13, "",nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(dt14 ))) then 
            	if(errcheck_mltpl(error,msg,14, "",nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(dt15 ))) then 
            	if(errcheck_mltpl(error,msg,15, "",nm15 ).lt.KIM_STATUS_OK) return
            elseif( present(nm16 ).and.(.not.present(dt16))) then 
            	if(errcheck_mltpl(error,msg,16, "",nm16 ).lt.KIM_STATUS_OK) return
            elseif( present(nm17 ).and.(.not.present(dt17 ))) then 
            	if(errcheck_mltpl(error,msg,17, "",nm17 ).lt.KIM_STATUS_OK) return
            elseif( present(nm18 ).and.(.not.present(dt18 ))) then 
            	if(errcheck_mltpl(error,msg,18, "",nm18 ).lt.KIM_STATUS_OK) return
            elseif( present(nm19 ).and.(.not.present(dt19 ))) then 
            	if(errcheck_mltpl(error,msg,19, "",nm19 ).lt.KIM_STATUS_OK) return
            elseif( present(nm20 ).and.(.not.present(dt20 ))) then 
            	if(errcheck_mltpl(error,msg,20, "",nm20 ).lt.KIM_STATUS_OK) return
            endif 
	               
	    !process arguments
 if(present(nm2)) call kim_api_set_compute_byi_f(kimmdl,nm2,dt2,error);if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3)) call kim_api_set_compute_byi_f(kimmdl,nm3,dt3,error);if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4)) call kim_api_set_compute_byi_f(kimmdl,nm4,dt4,error);if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5)) call kim_api_set_compute_byi_f(kimmdl,nm5,dt5,error);if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6)) call kim_api_set_compute_byi_f(kimmdl,nm6,dt6,error);if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7)) call kim_api_set_compute_byi_f(kimmdl,nm7,dt7,error);if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8)) call kim_api_set_compute_byi_f(kimmdl,nm8,dt8,error);if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9)) call kim_api_set_compute_byi_f(kimmdl,nm9,dt9,error);if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10)) call kim_api_set_compute_byi_f(kimmdl,nm10,dt10,error);if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK) return
 if(present(nm11)) call kim_api_set_compute_byi_f(kimmdl,nm11,dt11,error);if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK) return
 if(present(nm12)) call kim_api_set_compute_byi_f(kimmdl,nm12,dt12,error);if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK) return
 if(present(nm13)) call kim_api_set_compute_byi_f(kimmdl,nm13,dt13,error);if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK) return
 if(present(nm14)) call kim_api_set_compute_byi_f(kimmdl,nm14,dt14,error);if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK) return
 if(present(nm15)) call kim_api_set_compute_byi_f(kimmdl,nm15,dt15,error);if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK) return
 if(present(nm16)) call kim_api_set_compute_byi_f(kimmdl,nm16,dt16,error);if(errcheck_mltpl(error,msg,16,"",nm16).lt.KIM_STATUS_OK) return
 if(present(nm17)) call kim_api_set_compute_byi_f(kimmdl,nm17,dt17,error);if(errcheck_mltpl(error,msg,17,"",nm17).lt.KIM_STATUS_OK) return
 if(present(nm18)) call kim_api_set_compute_byi_f(kimmdl,nm18,dt18,error);if(errcheck_mltpl(error,msg,18,"",nm18).lt.KIM_STATUS_OK) return
 if(present(nm19)) call kim_api_set_compute_byi_f(kimmdl,nm19,dt19,error);if(errcheck_mltpl(error,msg,19,"",nm19).lt.KIM_STATUS_OK) return
 if(present(nm20)) call kim_api_set_compute_byi_f(kimmdl,nm20,dt20,error);if(errcheck_mltpl(error,msg,20,"",nm20).lt.KIM_STATUS_OK) return 
	end subroutine kim_api_set_compute_byI_mltpl_f


       integer function errcheck_mltpl(error,msgfrom,grarg,nm,ind)
 	    integer ::error,grarg; character(len=*)::msgfrom
            character(len=*), optional::nm
            integer,optional ::ind
            errcheck_mltpl = KIM_STATUS_OK
	    if(error.ge.KIM_STATUS_OK) return 
            errcheck_mltpl = KIM_STATUS_FAIL
            if(present(nm).and.present(ind))then
		print*,"failed:", msgfrom,", for group argument ", grarg, " and kim_name ",nm,", kim_index", ind
            else if(present(nm)) then
		print*,"failed:", msgfrom,", for group argument ", grarg, " and kim_name ",nm
	    else if(present(ind)) then
		print*,"failed:", msgfrom,", for group argument ", grarg, " and kim_index",ind
            else
		print*,"failed:", msgfrom,", for group argument ", grarg
	    endif
            errcheck_mltpl = KIM_STATUS_FAIL
       end function errcheck_mltpl

       subroutine kim_api_set_compute_f(kimmdl,nm,compute,error)
	   integer(kind=kim_intptr) :: kimmdl; character(len=*)::nm
           integer::compute,error
           error=KIM_STATUS_FAIL
           if(compute.ne.1 .and. compute.ne.0) then
		print*,"compute flag must be 1 or 0";return
	   endif
           if(compute.eq.1) call kim_api_set2_compute_f(kimmdl,nm,error)
           if(compute.eq.0) call kim_api_set2_donotcompute_f(kimmdl,nm,error)
       end subroutine kim_api_set_compute_f

	subroutine kim_api_set_compute_byi_f(kimmdl,i,compute,error)
	   integer(kind=kim_intptr) :: kimmdl;  integer::i,compute,error
           error=KIM_STATUS_FAIL
           if(compute.ne.1 .and. compute.ne.0) then
		print*,"compute flag must be 1 or 0"
		return
	   endif
           if(compute.eq.1) call kim_api_set2_compute_byi_f(kimmdl,i,error)
           if(compute.eq.0) call kim_api_set2_donotcompute_byi_f(kimmdl,i,error)
       end subroutine kim_api_set_compute_byi_f

end module kimservice


