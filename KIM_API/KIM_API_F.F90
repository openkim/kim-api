!
! CDDL HEADER START
!
! The contents of this file are subject to the terms of the Common Development
! and Distribution License Version 1.0 (the "License").
!
! You can obtain a copy of the license at
! http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
! specific language governing permissions and limitations under the License.
!
! When distributing Covered Code, include this CDDL HEADER in each file and
! include the License file in a prominent location with the name LICENSE.CDDL.
! If applicable, add the following below this CDDL HEADER, with the fields
! enclosed by brackets "[]" replaced with your own identifying information:
!
! Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
!
! CDDL HEADER END
!

!
! Copyright (c) 2013, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Valeriu Smirichinski
!    Ryan S. Elliott
!    Ellad B. Tadmor
!

!
! Release: This file is part of the openkim-api.git repository.
!


#include "KIM_API_status.h"

module kim_kinds
  implicit none
#if KIM_ARCHTYPE == KIM_ARCH32BIT
  integer, parameter :: kim_intptr = 4
#else
  integer, parameter :: kim_intptr = 8
#endif
end module kim_kinds

module kim_api
  use kim_kinds
    implicit none
    integer,parameter :: KIM_KEY_STRING_LENGTH = 64 !

    interface
        !explicite inteface to C-side code
        integer function kim_api_init(kimmdl,testname,mdlname)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,testname,mdlname
        end function kim_api_init

        integer function kim_api_model_info(kimmdl,mdlname)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,mdlname
        end function kim_api_model_info

        integer function kim_api_string_init(kimmdl,testname,mdlname)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,testname,mdlname
        end function kim_api_string_init

        function kim_api_get_status_msg_f(errcode)
          use kim_kinds
        integer ::errcode
        integer(kind=kim_intptr) :: kimmdl,kim_api_get_status_msg_f
        end function kim_api_get_status_msg_f

        function kim_api_get_model_kim_str(modelname,lenstr,ier)
          use kim_kinds
        integer(kind=kim_intptr)::kim_api_get_model_kim_str,modelname
        integer :: ier,lenstr
        end function kim_api_get_model_kim_str

        function kim_api_get_model_buffer_f(kimmdl,ier)
          use kim_kinds
        integer ::ier
        integer(kind=kim_intptr) :: kimmdl,kim_api_get_model_buffer_f
        end function kim_api_get_model_buffer_f


        function kim_api_get_test_buffer_f(kimmdl,ier)
          use kim_kinds
        integer ::ier
        integer(kind=kim_intptr) :: kimmdl,kim_api_get_test_buffer_f
        end function kim_api_get_test_buffer_f


        function kim_api_is_half_neighbors_f(kimmdl,ier)
          use kim_kinds
        integer ::ier,kim_api_is_half_neighbors_f
        integer(kind=kim_intptr) :: kimmdl
        end function kim_api_is_half_neighbors_f


        subroutine kim_api_set_model_buffer_f(kimmdl,ob,ier)
          use kim_kinds
        integer ::ier
        integer(kind=kim_intptr) :: kimmdl,ob
        end subroutine kim_api_set_model_buffer_f

        subroutine kim_api_set_test_buffer_f(kimmdl,ob,ier)
          use kim_kinds
        integer ::ier
        integer(kind=kim_intptr) :: kimmdl,ob
        end subroutine kim_api_set_test_buffer_f


        integer function kim_api_process_dedr_f(pkim,de,r,dx,i,j)
          use kim_kinds
                integer(kind=kim_intptr) :: pkim,dx
                double precision :: de,r
                integer ::i,j
        end function kim_api_process_dedr_f

        integer function kim_api_process_d2edr2_f(pkim,de,r,dx,i,j)
          use kim_kinds
                integer(kind=kim_intptr) :: pkim,dx,r,i,j
                double precision :: de
        end function kim_api_process_d2edr2_f


    integer function kim_api_report_error(ln,fl,usermsg,ier)
      use kim_kinds
      integer :: ln,ier
      integer(kind=kim_intptr)::fl,usermsg
    end function kim_api_report_error


         integer function kim_api_get_model_index_shift_f(kimmdl)
           use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
        end function kim_api_get_model_index_shift_f





        integer function kim_api_set_data(kimmdl,nm, size, dt)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,nm,  size, dt
        end function kim_api_set_data
        integer function kim_api_set_method_data(kimmdl,nm, size, dt)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,nm,  size, dt
        end function kim_api_set_method_data



        function kim_api_get_data(kimmdl,nm,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,nm,kim_api_get_data
            integer::error
        end function kim_api_get_data
        function kim_api_get_method_data(kimmdl,nm,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,nm,kim_api_get_method_data
            integer::error
        end function kim_api_get_method_data




        function kim_api_get_size(kimmdl,nm,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,nm,kim_api_get_size
            integer::error
        end function kim_api_get_size

        function kim_api_get_neigh_mode_f(kimmdl,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
            integer::error,kim_api_get_neigh_mode_f
        end function kim_api_get_neigh_mode_f

        function kim_api_get_shape(kimmdl,nm, shapea,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,nm, shapea,kim_api_get_shape
            integer::error
        end function kim_api_get_shape

        subroutine kim_api_set_shape(kimmdl,nm,shapea,rank,error)
          use kim_kinds
        integer(kind=kim_intptr) :: kimmdl,nm,shapea
        integer::rank,error
        end subroutine kim_api_set_shape

        function kim_api_get_model_partcl_typs_f(kimmdl,natypes,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_model_partcl_typs_f
            integer::natypes,error
        end function kim_api_get_model_partcl_typs_f

        function kim_api_get_test_partcl_typs_f(kimmdl,natypes,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_test_partcl_typs_f
            integer::natypes,error
        end function kim_api_get_test_partcl_typs_f

        function kim_api_get_params_f(kimmdl,nvpar,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_params_f
            integer::nvpar,error
        end function kim_api_get_params_f

        function kim_api_get_free_params_f(kimmdl,nvpar,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_free_params_f
            integer::nvpar,error
        end function kim_api_get_free_params_f

        function kim_api_get_fixed_params_f(kimmdl,nvpar,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_fixed_params_f
            integer::nvpar,error
        end function kim_api_get_fixed_params_f

        function kim_api_get_nbc_method_f(kimmdl,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_nbc_method_f
            integer::error
        end function kim_api_get_nbc_method_f

        function kim_api_get_partcl_type_code(kimmdl,nm,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl, nm
            integer::error,kim_api_get_partcl_type_code
        end function kim_api_get_partcl_type_code

        subroutine kim_api_set_partcl_type_code(kimmdl,nm,code,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl, nm
            integer::code, error
        end subroutine kim_api_set_partcl_type_code



        integer function kim_api_get_neigh_f(kimmdl,mode,request, atom, numnei, pnei1atom, pRij)
          use kim_kinds
        integer :: mode,request,atom,numnei
        integer(kind=kim_intptr) :: kimmdl,pnei1atom,pRij
        end function kim_api_get_neigh_f


        integer function kim_api_get_compute(kimmdl,nm,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,nm
            integer::error
        end function kim_api_get_compute

        integer function kim_api_get_index(kimmdl,nm,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,nm
            integer::error
        end function kim_api_get_index

        function kim_api_get_data_by_index(kimmdl,I,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_data_by_index
            integer :: I,error
        end function kim_api_get_data_by_index
        function kim_api_get_method_data_by_index(kimmdl,I,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_method_data_by_index
            integer :: I,error
        end function kim_api_get_method_data_by_index

        function kim_api_get_size_by_index(kimmdl,I,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,kim_api_get_size_by_index
            integer :: I,error
        end function kim_api_get_size_by_index

        function kim_api_get_shape_by_index(kimmdl,I,shapea,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,shapea,kim_api_get_shape_by_index
            integer  :: I,error
        end function kim_api_get_shape_by_index

        integer function kim_api_get_compute_by_index(kimmdl,I,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
        end function kim_api_get_compute_by_index


        subroutine kim_api_allocate(kimmdl,natoms,ntypes,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
            integer  :: natoms,ntypes,error
        end subroutine kim_api_allocate

        subroutine kim_api_free(kimmdl,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
        integer::error
        end subroutine kim_api_free

        subroutine kim_api_print(kimmdl,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
            integer ::error
        end subroutine kim_api_print

        integer function kim_api_model_compute_f(kimmdl)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
        end function kim_api_model_compute_f

        integer function kim_api_model_destroy_f(kimmdl)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
        end function kim_api_model_destroy_f

        integer function kim_api_model_reinit_f(kimmdl)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
        end function kim_api_model_reinit_f

        integer function kim_api_model_init_f(kimmdl)
          use kim_kinds
                integer(kind=kim_intptr) :: kimmdl
        end function kim_api_model_init_f

        double precision function kim_api_get_scale_conversion(u_from,u_to, &
                                                               error)
          use kim_kinds
                integer(kind=kim_intptr) :: u_from,u_to
                integer:: error
        end function kim_api_get_scale_conversion

        integer function kim_api_get_unit_handling_f(kimmdl,error)
          use kim_kinds
                integer(kind=kim_intptr) :: kimmdl
                integer ::error
        end function kim_api_get_unit_handling_f

        function kim_api_get_unit_length_f(kimmdl,error)
          use kim_kinds
                integer(kind=kim_intptr) :: kimmdl,kim_api_get_unit_length_f
                integer ::error
        end function kim_api_get_unit_length_f

        function kim_api_get_unit_energy_f(kimmdl,error)
          use kim_kinds
                integer(kind=kim_intptr) :: kimmdl,kim_api_get_unit_energy_f
                integer ::error
        end function kim_api_get_unit_energy_f

        function kim_api_get_unit_charge_f(kimmdl,error)
          use kim_kinds
                integer(kind=kim_intptr) :: kimmdl,kim_api_get_unit_charge_f
                integer ::error
        end function kim_api_get_unit_charge_f

        function kim_api_get_unit_temperature_f(kimmdl,error)
          use kim_kinds
                integer(kind=kim_intptr) :: kimmdl, &
                     kim_api_get_unit_temperature_f
                integer ::error
        end function kim_api_get_unit_temperature_f

        function kim_api_get_unit_time_f(kimmdl,error)
          use kim_kinds
                integer(kind=kim_intptr) :: kimmdl,kim_api_get_unit_time_f
                integer ::error
        end function kim_api_get_unit_time_f

        double precision function kim_api_convert_to_act_unit(kimmdl,length, &
             energy,charge,temperature,time, length_exponent, energy_exponent, &
             charge_exponent, temperature_exponent, time_exponent, error)
          use kim_kinds
             integer(kind=kim_intptr) ::kimmdl,length,energy,charge, &
                  temperature,time
             double precision :: length_exponent, energy_exponent, &
                  charge_exponent, temperature_exponent, time_exponent
             integer::error
        end function kim_api_convert_to_act_unit

    !subroutines





        subroutine kim_api_set_compute(kimmdl,nm,flag,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl,nm
        integer::flag,error
        end subroutine kim_api_set_compute

        subroutine kim_api_set_compute_by_index_f(kimmdl,ind,flag,error)
          use kim_kinds
            integer(kind=kim_intptr) :: kimmdl
        integer::ind,flag,error
        end subroutine kim_api_set_compute_by_index_f

        integer function kim_api_set_data_by_index(kimmdl,I, size,dt)
          use kim_kinds
            integer (kind=kim_intptr):: kimmdl,size,dt
            integer :: I
        end function kim_api_set_data_by_index
        integer function kim_api_set_method_data_by_index(kimmdl,I, size,dt)
          use kim_kinds
            integer (kind=kim_intptr):: kimmdl,size,dt
            integer :: I
        end function kim_api_set_method_data_by_index

    end interface

 contains
    !the following subs converts ctypeArray to fortran type array
    ! ( to pointer to array with descriptor) without 2003 features
    ! it is analog of c_f_pointer(...)
    subroutine KIM_to_F90_real_array_2d(ctypeArray,ArrayWithDescriptor,n,m)
        implicit none
        integer  :: n,m
        double precision, target :: ctypeArray(n,m)
        double precision, pointer ::ArrayWithDescriptor(:,:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine KIM_to_F90_real_array_2d

        subroutine KIM_to_F90_real_array_3d(ctypeArray,ArrayWithDescriptor,n,m,l)
        implicit none
        integer  :: n,m,l
        double precision, target :: ctypeArray(n,m,l)
        double precision, pointer ::ArrayWithDescriptor(:,:,:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine KIM_to_F90_real_array_3d


    subroutine KIM_to_F90_int_array_2d(ctypeArray,ArrayWithDescriptor,n,m)
        implicit none
        integer  :: n,m !! Check if I can make array big like size of integer*8
        integer,target :: ctypeArray(n,m)
        integer,pointer ::ArrayWithDescriptor(:,:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine KIM_to_F90_int_array_2d

        subroutine KIM_to_F90_int_array_3d(ctypeArray,ArrayWithDescriptor,n,m,l)
        implicit none
        integer  :: n,m,l !! Check if I can make array big like size of integer*8
        integer,target :: ctypeArray(n,m,l)
        integer,pointer ::ArrayWithDescriptor(:,:,:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine KIM_to_F90_int_array_3d

    subroutine KIM_to_F90_int_array_1d(ctypeArray,ArrayWithDescriptor,n)
        implicit none
        integer :: n
        integer,target :: ctypeArray(n)
        integer,pointer ::ArrayWithDescriptor(:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine KIM_to_F90_int_array_1d

    subroutine KIM_to_F90_real_array_1d(ctypeArray,ArrayWithDescriptor,n)
        implicit none
        integer :: n
        double precision, target :: ctypeArray(n)
        double precision, pointer ::ArrayWithDescriptor(:)
        ArrayWithDescriptor=>ctypeArray
    end subroutine KIM_to_F90_real_array_1d

    character (len=KIM_KEY_STRING_LENGTH) function attachnull(str)
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

    character (len=KIM_KEY_STRING_LENGTH) function cstring2fortran(pointercstring)
        character (len=KIM_KEY_STRING_LENGTH) cstring ; pointer(pointercstring,cstring)
        cstring2fortran = cstring
    end function cstring2fortran

    ! fortran 90 wraper for KIM_API interface using cray pointers
    ! takes care of character string (null atachment) and dope vector for arrays
    !****************** f90 wraper ********************
    integer function kim_api_init_f(kimmdl,testname,mdlname)
            character (len=*) :: testname,mdlname
            integer(kind=kim_intptr) :: kimmdl
            character (len=KIM_KEY_STRING_LENGTH)::testnamesnd,mdlnamesnd
            character (len=KIM_KEY_STRING_LENGTH):: s1,s2
            pointer(ps1,s1);pointer(ps2,s2)
            testnamesnd=attachnull(trim(testname))
            mdlnamesnd=attachnull(trim(mdlname))
            ps1=loc(testnamesnd)
            ps2=loc(mdlnamesnd)
            kim_api_init_f =kim_api_init(kimmdl,ps1,ps2)
     end function kim_api_init_f

    integer function kim_api_model_info_f(kimmdl,mdlname)
            character (len=*) :: mdlname
            integer(kind=kim_intptr) :: kimmdl
            character (len=KIM_KEY_STRING_LENGTH)::mdlnamesnd
            character (len=KIM_KEY_STRING_LENGTH):: s2
            pointer(ps2,s2)
            mdlnamesnd=attachnull(trim(mdlname))
            ps2=loc(mdlnamesnd)
            kim_api_model_info_f =kim_api_model_info(kimmdl,ps2)
     end function kim_api_model_info_f

    integer function kim_api_string_init_f(kimmdl,testname,mdlname)
            character (len=*) :: testname,mdlname
            integer(kind=kim_intptr) :: kimmdl
            character (len=KIM_KEY_STRING_LENGTH)::mdlnamesnd
            character (len=KIM_KEY_STRING_LENGTH):: s1,s2
            pointer(ps1,s1);pointer(ps2,s2)
            !testnamesnd=attachnull(trim(testname))

            mdlnamesnd=attachnull(trim(mdlname))
            ps1=loc(testname)
            ps2=loc(mdlnamesnd)
            kim_api_string_init_f =kim_api_string_init(kimmdl,ps1,ps2)
     end function kim_api_string_init_f





        integer function kim_api_set_data_f(kimmdl,nm, size, dt)
            ! returns 1 (true) if successfull
            ! dt is address (cray pointer to acctual data
            integer(kind=kim_intptr) :: kimmdl,  size, dt
            character (len=*) ::nm

            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)

            kim_api_set_data_f = kim_api_set_data(kimmdl,pstr,size,dt)
        end function kim_api_set_data_f
        integer function kim_api_set_method_data_f(kimmdl,nm, size, dt)
            ! returns 1 (true) if successfull
            ! dt is address (cray pointer to acctual data
            integer(kind=kim_intptr) :: kimmdl,  size, dt
            character (len=*) ::nm

            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)

            kim_api_set_method_data_f = kim_api_set_method_data(kimmdl,pstr,size,dt)
        end function kim_api_set_method_data_f


        integer(kind=kim_intptr) function kim_api_get_data_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) ::nm
            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
            integer::error
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            kim_api_get_data_f = kim_api_get_data(kimmdl,pstr,error)
        end function kim_api_get_data_f
        integer(kind=kim_intptr) function kim_api_get_method_data_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) ::nm
            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
            integer::error
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            kim_api_get_method_data_f = kim_api_get_method_data(kimmdl,pstr,error)
        end function kim_api_get_method_data_f

        double precision function kim_api_get_scale_conversion_f(from,to,error)
                integer:: error
                character (len=*) ::from,to
                character (len=KIM_KEY_STRING_LENGTH) ::sfrom,sto
                sfrom = attachnull(trim(from))
                sto = attachnull(trim(to))
                kim_api_get_scale_conversion_f = kim_api_get_scale_conversion(loc(sfrom),loc(sto),error)
        end function kim_api_get_scale_conversion_f

        double precision function kim_api_convert_to_act_unit_f(kimmdl,length, &
             energy,charge,temperature,time, length_exponent, energy_exponent, &
             charge_exponent, temperature_exponent, time_exponent, error)
                integer::error
                integer(kind=kim_intptr) :: kimmdl
                double precision ::length_exponent, energy_exponent, &
                     charge_exponent, temperature_exponent, time_exponent
                character (len=*) ::length,energy,charge,temperature,time
                character (len=KIM_KEY_STRING_LENGTH) ::slength,senergy, &
                     scharge,stemperature,stime
                slength = attachnull(trim(length))
                senergy = attachnull(trim(energy))
                scharge = attachnull(trim(charge))
                stemperature = attachnull(trim(temperature))
                stime = attachnull(trim(time))

                kim_api_convert_to_act_unit_f =kim_api_convert_to_act_unit(kimmdl,loc(slength),loc(senergy),loc(scharge),&
                loc(stemperature), loc(stime),length_exponent, energy_exponent, charge_exponent, &
                   temperature_exponent, time_exponent, error)
        end function kim_api_convert_to_act_unit_f

        integer(kind=kim_intptr) function kim_api_get_model_kim_str_f(nm,lenstr,error)
            character (len=*) ::nm
            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
            integer::error,lenstr
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            kim_api_get_model_kim_str_f = kim_api_get_model_kim_str(pstr,lenstr,error)
        end function kim_api_get_model_kim_str_f

        integer function kim_api_get_partcl_type_code_f(kimmdl,nm,error)
                integer(kind=kim_intptr) :: kimmdl
                character (len=*) ::nm
                character (len=KIM_KEY_STRING_LENGTH) :: str2send
                character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
                integer::error
                str2send = attachnull(trim(nm))
                pstr = loc(str2send)
                kim_api_get_partcl_type_code_f = kim_api_get_partcl_type_code(kimmdl,pstr,error)
        end function kim_api_get_partcl_type_code_f

        subroutine kim_api_set_partcl_type_code_f(kimmdl,nm,code,error)
                integer(kind=kim_intptr) :: kimmdl
                character (len=*) ::nm
                character (len=KIM_KEY_STRING_LENGTH) :: str2send
                character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
                integer::code, error
                str2send = attachnull(trim(nm))
                pstr = loc(str2send)
                call kim_api_set_partcl_type_code(kimmdl,pstr,code,error)
        end subroutine kim_api_set_partcl_type_code_f



        integer(kind=kim_intptr) function kim_api_get_size_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) ::nm

            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
            integer::error
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)

            kim_api_get_size_f = kim_api_get_size(kimmdl,pstr,error)
        end function kim_api_get_size_f

        integer(kind=kim_intptr) function kim_api_get_shape_f(kimmdl,nm, shape,error)
            ! returns rank and place correct shape (shape has to be allocated )
            ! if unssaccesfull returns -1
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) :: nm

            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)

            integer,pointer :: shape(:)
            integer :: shp(size(shape))
            integer :: shpst(1); pointer(pshpst,shpst)
            integer :: error
            integer(kind=kim_intptr) :: rank, i
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            pshpst = loc(shp)
            rank = kim_api_get_shape(kimmdl,pstr,pshpst,error)
            if(rank .eq. 0) then
                kim_api_get_shape_f=0
            else if(rank.eq.1) then
                kim_api_get_shape_f=1
                shape(1)=shp(1)
            else if(rank.gt.1) then
                kim_api_get_shape_f=rank
                do i=1, rank
                    shape(i)=shp(rank - i + 1)
                end do
            else
                kim_api_get_shape_f=-1
            end if
        end function kim_api_get_shape_f

        subroutine kim_api_set_shape_f(kimmdl,nm, shapef,rankf,error)
            ! set rank, shape and size of the data in KIM API object
            ! error=1 if successful , error <= 0 if unsuccessful
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) :: nm

            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)

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
            call kim_api_set_shape(kimmdl,pstr,pshpst,rankf,error)
        end subroutine kim_api_set_shape_f

        integer function kim_api_get_compute_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) ::nm
            integer::error
            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)

            kim_api_get_compute_f =kim_api_get_compute(kimmdl,pstr,error)
        end function kim_api_get_compute_f

        integer function kim_api_get_index_f(kimmdl,nm,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) ::nm
            character (len=KIM_KEY_STRING_LENGTH) :: str2send
            character (len=KIM_KEY_STRING_LENGTH) :: str; pointer (pstr,str)
            integer::error
            str2send = attachnull(trim(nm))
            pstr = loc(str2send)
            kim_api_get_index_f = kim_api_get_index(kimmdl,pstr,error)
        end function kim_api_get_index_f


        integer(kind=kim_intptr) function kim_api_get_data_by_index_f(kimmdl,I,error)
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
            kim_api_get_data_by_index_f = kim_api_get_data_by_index(kimmdl,I,error)
        end function kim_api_get_data_by_index_f
        integer(kind=kim_intptr) function kim_api_get_method_data_by_index_f(kimmdl,I,error)
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
            kim_api_get_method_data_by_index_f = kim_api_get_method_data_by_index(kimmdl,I,error)
        end function kim_api_get_method_data_by_index_f

        integer(kind=kim_intptr) function kim_api_get_size_by_index_f(kimmdl,I,error)
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
            kim_api_get_size_by_index_f = kim_api_get_size_by_index(kimmdl,I,error)
        end function kim_api_get_size_by_index_f

        integer(kind=kim_intptr) function kim_api_get_shape_by_index_f(kimmdl,I,shape,error)
            integer(kind=kim_intptr) :: kimmdl
            integer  :: I,error
            integer,pointer :: shape(:)
            integer :: shp(size(shape))
            integer :: shpst(1); pointer(pshpst,shpst)
            integer(kind=kim_intptr) :: rank, ii
            pshpst = loc(shp)
            rank = kim_api_get_shape_by_index(kimmdl,I,pshpst,error)
            if(rank .eq. 0) then
                kim_api_get_shape_by_index_f=0
            else if(rank.eq.1) then
                kim_api_get_shape_by_index_f=1
                shape(1)=shp(1)
            else if(rank.gt.1) then
                kim_api_get_shape_by_index_f=rank
                do ii=1, rank
                    shape(ii)=shp(rank - ii + 1)
                end do
            else
                kim_api_get_shape_by_index_f=-1
            end if
        end function kim_api_get_shape_by_index_f

        integer function kim_api_get_compute_by_index_f(kimmdl,I,error)
            integer(kind=kim_intptr) :: kimmdl
            integer :: I,error
            kim_api_get_compute_by_index_f = kim_api_get_compute_by_index(kimmdl,I,error)
        end function kim_api_get_compute_by_index_f

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




        integer function kim_api_report_error_f(ln,fl,usermsg,ier)
            integer:: ln,ier
            character(len=*)::fl,usermsg
            character(len=128)::fltmp,umsgtmp
            fltmp=fl//CHAR(0)
            umsgtmp=usermsg//char(0)
            kim_api_report_error_f = kim_api_report_error(ln,loc(fltmp),loc(umsgtmp),ier)

        end function kim_api_report_error_f

        subroutine kim_api_set_compute_f(kimmdl,nm,flag,error)
            integer(kind=kim_intptr) :: kimmdl
            character (len=*) :: nm
            integer :: flag, error
            character (len=KIM_KEY_STRING_LENGTH) ::us
            character :: str(1); pointer(pstr,str)
            us = attachnull(nm)
            pstr =loc(us)

            call kim_api_set_compute(kimmdl,pstr,flag,error)
        end subroutine kim_api_set_compute_f

        integer function kim_api_set_data_by_index_f(kimmdl,I, size,dt)
            integer(kind=kim_intptr) :: kimmdl,size,dt
            integer :: I
            kim_api_set_data_by_index_f = kim_api_set_data_by_index(kimmdl,I, size,dt)
        end function kim_api_set_data_by_index_f
        integer function kim_api_set_method_data_by_index_f(kimmdl,I, size,dt)
            integer(kind=kim_intptr) :: kimmdl,size,dt
            integer :: I
            kim_api_set_method_data_by_index_f = kim_api_set_method_data_by_index(kimmdl,I, size,dt)
        end function kim_api_set_method_data_by_index_f

        subroutine kim_api_setm_data_f(kimmdl,error, nm1,sz1,dt1,k1, nm2,sz2,dt2,k2, nm3,sz3,dt3,k3, nm4,sz4,dt4,k4,&
         nm5,sz5,dt5,k5,   nm6,sz6,dt6,k6,  nm7,sz7,dt7,k7, nm8,sz8,dt8,k8, nm9,sz9,dt9,k9, nm10,sz10,dt10,k10,&
         nm11,sz11,dt11,k11, nm12,sz12,dt12,k12, nm13,sz13,dt13,k13,  nm14,sz14,dt14,k14, nm15,sz15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_setm_data_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1, nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
               error = kim_api_set_data_f(kimmdl,nm1,sz1,dt1);
               if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            endif

            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(sz2 ).or..not.present(dt2 ).or..not.present(k2))) then
                if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(sz3 ).or..not.present(dt3 ).or..not.present(k3))) then
                if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(sz4 ).or..not.present(dt4 ).or..not.present(k4))) then
                if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(sz5 ).or..not.present(dt5 ).or..not.present(k5))) then
                if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(sz6 ).or..not.present(dt6 ).or..not.present(k6))) then
                if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(sz7 ).or..not.present(dt7 ).or..not.present(k7))) then
                if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(sz8 ).or..not.present(dt8 ).or..not.present(k8))) then
                if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(sz9 ).or..not.present(dt9 ).or..not.present(k9))) then
                if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(sz10 ).or..not.present(dt10 ).or..not.present(k10))) then
                if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(sz11 ).or..not.present(dt11 ).or..not.present(k11))) then
                if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(sz12 ).or..not.present(dt12 ).or..not.present(k12))) then
                if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(sz13 ).or..not.present(dt13 ).or..not.present(k13))) then
                if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(sz14 ).or..not.present(dt14 ).or..not.present(k14))) then
                if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(sz15 ).or..not.present(dt15 ).or..not.present(k15))) then
                if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error = KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) error=kim_api_set_data_f(kimmdl,nm2,sz2,dt2);
            if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) error=kim_api_set_data_f(kimmdl,nm3,sz3,dt3);
            if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) error=kim_api_set_data_f(kimmdl,nm4,sz4,dt4);
            if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) error=kim_api_set_data_f(kimmdl,nm5,sz5,dt5);
            if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) error=kim_api_set_data_f(kimmdl,nm6,sz6,dt6);
            if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) error=kim_api_set_data_f(kimmdl,nm7,sz7,dt7);
            if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) error=kim_api_set_data_f(kimmdl,nm8,sz8,dt8);
            if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) error=kim_api_set_data_f(kimmdl,nm9,sz9,dt9);
            if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) error=kim_api_set_data_f(kimmdl,nm10,sz10,dt10);
            if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) error=kim_api_set_data_f(kimmdl,nm11,sz11,dt11);
            if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) error=kim_api_set_data_f(kimmdl,nm12,sz12,dt12);
            if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) error=kim_api_set_data_f(kimmdl,nm13,sz13,dt13);
            if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) error=kim_api_set_data_f(kimmdl,nm14,sz14,dt14);
            if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) error=kim_api_set_data_f(kimmdl,nm15,sz15,dt15);
            if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_setm_data_f
        subroutine kim_api_setm_method_data_f(kimmdl,error, nm1,sz1,dt1,k1, nm2,sz2,dt2,k2, nm3,sz3,dt3,k3, nm4,sz4,dt4,k4,&
         nm5,sz5,dt5,k5,   nm6,sz6,dt6,k6,  nm7,sz7,dt7,k7, nm8,sz8,dt8,k8, nm9,sz9,dt9,k9, nm10,sz10,dt10,k10,&
         nm11,sz11,dt11,k11, nm12,sz12,dt12,k12, nm13,sz13,dt13,k13,  nm14,sz14,dt14,k14, nm15,sz15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_setm_method_data_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1, nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
               error = kim_api_set_method_data_f(kimmdl,nm1,sz1,dt1);
               if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            endif

            !check rest of the arguments
            error = KIM_STATUS_WRONG_MULTIPLE_ARGS
            if( present(nm2 ).and.(.not.present(sz2 ).or..not.present(dt2 ).or..not.present(k2))) then
                if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif( present(nm3 ).and.(.not.present(sz3 ).or..not.present(dt3 ).or..not.present(k3))) then
                if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif( present(nm4 ).and.(.not.present(sz4 ).or..not.present(dt4 ).or..not.present(k4))) then
                if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif( present(nm5 ).and.(.not.present(sz5 ).or..not.present(dt5 ).or..not.present(k5))) then
                if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif( present(nm6 ).and.(.not.present(sz6 ).or..not.present(dt6 ).or..not.present(k6))) then
                if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif( present(nm7 ).and.(.not.present(sz7 ).or..not.present(dt7 ).or..not.present(k7))) then
                if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif( present(nm8 ).and.(.not.present(sz8 ).or..not.present(dt8 ).or..not.present(k8))) then
                if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif( present(nm9 ).and.(.not.present(sz9 ).or..not.present(dt9 ).or..not.present(k9))) then
                if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif( present(nm10 ).and.(.not.present(sz10 ).or..not.present(dt10 ).or..not.present(k10))) then
                if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif( present(nm11 ).and.(.not.present(sz11 ).or..not.present(dt11 ).or..not.present(k11))) then
                if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif( present(nm12 ).and.(.not.present(sz12 ).or..not.present(dt12 ).or..not.present(k12))) then
                if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif( present(nm13 ).and.(.not.present(sz13 ).or..not.present(dt13 ).or..not.present(k13))) then
                if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif( present(nm14 ).and.(.not.present(sz14 ).or..not.present(dt14 ).or..not.present(k14))) then
                if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif( present(nm15 ).and.(.not.present(sz15 ).or..not.present(dt15 ).or..not.present(k15))) then
                if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error = KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) error=kim_api_set_method_data_f(kimmdl,nm2,sz2,dt2);
            if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) error=kim_api_set_method_data_f(kimmdl,nm3,sz3,dt3);
            if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) error=kim_api_set_method_data_f(kimmdl,nm4,sz4,dt4);
            if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) error=kim_api_set_method_data_f(kimmdl,nm5,sz5,dt5);
            if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) error=kim_api_set_method_data_f(kimmdl,nm6,sz6,dt6);
            if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) error=kim_api_set_method_data_f(kimmdl,nm7,sz7,dt7);
            if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) error=kim_api_set_method_data_f(kimmdl,nm8,sz8,dt8);
            if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) error=kim_api_set_method_data_f(kimmdl,nm9,sz9,dt9);
            if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) error=kim_api_set_method_data_f(kimmdl,nm10,sz10,dt10);
            if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) error=kim_api_set_method_data_f(kimmdl,nm11,sz11,dt11);
            if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) error=kim_api_set_method_data_f(kimmdl,nm12,sz12,dt12);
            if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) error=kim_api_set_method_data_f(kimmdl,nm13,sz13,dt13);
            if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) error=kim_api_set_method_data_f(kimmdl,nm14,sz14,dt14);
            if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) error=kim_api_set_method_data_f(kimmdl,nm15,sz15,dt15);
            if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_setm_method_data_f

        subroutine kim_api_setm_data_by_index_f(kimmdl,error,nm1,sz1,dt1,k1, nm2,sz2,dt2,k2, nm3,sz3,dt3,k3, nm4,sz4,dt4,k4,&
         nm5,sz5,dt5,k5,   nm6,sz6,dt6,k6,  nm7,sz7,dt7,k7, nm8,sz8,dt8,k8, nm9,sz9,dt9,k9, nm10,sz10,dt10,k10,&
         nm11,sz11,dt11,k11, nm12,sz12,dt12,k12, nm13,sz13,dt13,k13,  nm14,sz14,dt14,k14, nm15,sz15,dt15,k15)

            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_setm_data_by_index_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1,"",nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
               error = kim_api_set_data_by_index_f(kimmdl,nm1,sz1,dt1);
               if (errcheck_mltpl(error,msg,1,"", nm1).lt.KIM_STATUS_OK) return
            endif

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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2,"", nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3,"", nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4,"", nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5,"", nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6,"", nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7,"", nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8,"", nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9,"", nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10,"", nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11,"", nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12,"", nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13,"", nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14,"", nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15,"", nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error=KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm2,sz2,dt2);
            if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm3,sz3,dt3);
            if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm4,sz4,dt4);
            if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm5,sz5,dt5);
            if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm6,sz6,dt6);
            if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm7,sz7,dt7);
            if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm8,sz8,dt8);
            if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm9,sz9,dt9);
            if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm10,sz10,dt10);
            if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK)return
 if(present(nm11).and.k11.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm11,sz11,dt11);
            if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK)return
 if(present(nm12).and.k12.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm12,sz12,dt12);
            if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK)return
 if(present(nm13).and.k13.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm13,sz13,dt13);
            if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK)return
 if(present(nm14).and.k14.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm14,sz14,dt14);
            if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK)return
 if(present(nm15).and.k15.eq.1) error = kim_api_set_data_by_index_f(kimmdl,nm15,sz15,dt15);
            if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK)return
        end subroutine kim_api_setm_data_by_index_f
        subroutine kim_api_setm_method_data_by_index_f(kimmdl,error,nm1,sz1,dt1,k1, nm2,sz2,dt2,k2, nm3,sz3,dt3,k3, nm4,sz4,dt4,k4,&
         nm5,sz5,dt5,k5,   nm6,sz6,dt6,k6,  nm7,sz7,dt7,k7, nm8,sz8,dt8,k8, nm9,sz9,dt9,k9, nm10,sz10,dt10,k10,&
         nm11,sz11,dt11,k11, nm12,sz12,dt12,k12, nm13,sz13,dt13,k13,  nm14,sz14,dt14,k14, nm15,sz15,dt15,k15)

            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_setm_method_data_by_index_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1,"",nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
               error = kim_api_set_method_data_by_index_f(kimmdl,nm1,sz1,dt1);
               if (errcheck_mltpl(error,msg,1,"", nm1).lt.KIM_STATUS_OK) return
            endif

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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2,"", nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3,"", nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4,"", nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5,"", nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6,"", nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7,"", nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8,"", nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9,"", nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10,"", nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11,"", nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12,"", nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13,"", nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14,"", nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15,"", nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error=KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm2,sz2,dt2);
            if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm3,sz3,dt3);
            if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm4,sz4,dt4);
            if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm5,sz5,dt5);
            if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm6,sz6,dt6);
            if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm7,sz7,dt7);
            if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm8,sz8,dt8);
            if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm9,sz9,dt9);
            if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm10,sz10,dt10);
            if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK)return
 if(present(nm11).and.k11.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm11,sz11,dt11);
            if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK)return
 if(present(nm12).and.k12.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm12,sz12,dt12);
            if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK)return
 if(present(nm13).and.k13.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm13,sz13,dt13);
            if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK)return
 if(present(nm14).and.k14.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm14,sz14,dt14);
            if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK)return
 if(present(nm15).and.k15.eq.1) error = kim_api_set_method_data_by_index_f(kimmdl,nm15,sz15,dt15);
            if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK)return
        end subroutine kim_api_setm_method_data_by_index_f

        subroutine kim_api_getm_data_f(kimmdl,error, nm1,dt1,k1, nm2,dt2,k2, nm3,dt3,k3, nm4,dt4,k4, nm5,dt5,k5,&
         nm6,dt6,k6, nm7,dt7,k7, nm8,dt8,k8, nm9,dt9,k9, nm10,dt10,k10, nm11,dt11,k11, nm12,dt12,k12, nm13,dt13,k13,&
         nm14,dt14,k14, nm15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_getm_data_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1, nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
                dt1 = kim_api_get_data_f(kimmdl,nm1,error);
                if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            endif

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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error=KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) dt2= kim_api_get_data_f(kimmdl,nm2,error);
            if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) dt3= kim_api_get_data_f(kimmdl,nm3,error);
            if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) dt4= kim_api_get_data_f(kimmdl,nm4,error);
            if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) dt5= kim_api_get_data_f(kimmdl,nm5,error);
            if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) dt6= kim_api_get_data_f(kimmdl,nm6,error);
            if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) dt7= kim_api_get_data_f(kimmdl,nm7,error);
            if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) dt8= kim_api_get_data_f(kimmdl,nm8,error);
            if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) dt9= kim_api_get_data_f(kimmdl,nm9,error);
            if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) dt10= kim_api_get_data_f(kimmdl,nm10,error);
            if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) dt11= kim_api_get_data_f(kimmdl,nm11,error);
            if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) dt12= kim_api_get_data_f(kimmdl,nm12,error);
            if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) dt13= kim_api_get_data_f(kimmdl,nm13,error);
            if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) dt14= kim_api_get_data_f(kimmdl,nm14,error);
            if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) dt15= kim_api_get_data_f(kimmdl,nm15,error);
            if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_getm_data_f
        subroutine kim_api_getm_method_data_f(kimmdl,error, nm1,dt1,k1, nm2,dt2,k2, nm3,dt3,k3, nm4,dt4,k4, nm5,dt5,k5,&
         nm6,dt6,k6, nm7,dt7,k7, nm8,dt8,k8, nm9,dt9,k9, nm10,dt10,k10, nm11,dt11,k11, nm12,dt12,k12, nm13,dt13,k13,&
         nm14,dt14,k14, nm15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_getm_method_data_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1, nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
                dt1 = kim_api_get_method_data_f(kimmdl,nm1,error);
                if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            endif

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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error=KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) dt2= kim_api_get_method_data_f(kimmdl,nm2,error);
            if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) dt3= kim_api_get_method_data_f(kimmdl,nm3,error);
            if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) dt4= kim_api_get_method_data_f(kimmdl,nm4,error);
            if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) dt5= kim_api_get_method_data_f(kimmdl,nm5,error);
            if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) dt6= kim_api_get_method_data_f(kimmdl,nm6,error);
            if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) dt7= kim_api_get_method_data_f(kimmdl,nm7,error);
            if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) dt8= kim_api_get_method_data_f(kimmdl,nm8,error);
            if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) dt9= kim_api_get_method_data_f(kimmdl,nm9,error);
            if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) dt10= kim_api_get_method_data_f(kimmdl,nm10,error);
            if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) dt11= kim_api_get_method_data_f(kimmdl,nm11,error);
            if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) dt12= kim_api_get_method_data_f(kimmdl,nm12,error);
            if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) dt13= kim_api_get_method_data_f(kimmdl,nm13,error);
            if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) dt14= kim_api_get_method_data_f(kimmdl,nm14,error);
            if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) dt15= kim_api_get_method_data_f(kimmdl,nm15,error);
            if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_getm_method_data_f

        subroutine kim_api_getm_data_by_index_f(kimmdl,error, nm1,dt1,k1, nm2,dt2,k2, nm3,dt3,k3, nm4,dt4,k4, nm5,dt5,k5,&
         nm6,dt6,k6, nm7,dt7,k7, nm8,dt8,k8, nm9,dt9,k9, nm10,dt10,k10, nm11,dt11,k11, nm12,dt12,k12, nm13,dt13,k13,&
         nm14,dt14,k14, nm15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_getm_data_by_index_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
              if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1,"", nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
                dt1 = kim_api_get_data_by_index_f(kimmdl,nm1,error);
                if (errcheck_mltpl(error,msg,1, "", nm1).lt.KIM_STATUS_OK) return
            endif
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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2,"", nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3,"", nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4,"", nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5,"", nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6,"", nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7,"", nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8,"", nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9,"", nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10,"", nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11,"", nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12,"", nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13,"", nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14,"", nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15,"", nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error =KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) dt2= kim_api_get_data_by_index_f(kimmdl,nm2,error);
            if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) dt3= kim_api_get_data_by_index_f(kimmdl,nm3,error);
            if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) dt4= kim_api_get_data_by_index_f(kimmdl,nm4,error);
            if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) dt5= kim_api_get_data_by_index_f(kimmdl,nm5,error);
            if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) dt6= kim_api_get_data_by_index_f(kimmdl,nm6,error);
            if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) dt7= kim_api_get_data_by_index_f(kimmdl,nm7,error);
            if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) dt8= kim_api_get_data_by_index_f(kimmdl,nm8,error);
            if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) dt9= kim_api_get_data_by_index_f(kimmdl,nm9,error);
            if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) dt10= kim_api_get_data_by_index_f(kimmdl,nm10,error);
            if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) dt11= kim_api_get_data_by_index_f(kimmdl,nm11,error);
            if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) dt12= kim_api_get_data_by_index_f(kimmdl,nm12,error);
            if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) dt13= kim_api_get_data_by_index_f(kimmdl,nm13,error);
            if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) dt14= kim_api_get_data_by_index_f(kimmdl,nm14,error);
            if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) dt15= kim_api_get_data_by_index_f(kimmdl,nm15,error);
            if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_getm_data_by_index_f
        subroutine kim_api_getm_method_data_by_index_f(kimmdl,error, nm1,dt1,k1, nm2,dt2,k2, nm3,dt3,k3, nm4,dt4,k4, nm5,dt5,k5,&
         nm6,dt6,k6, nm7,dt7,k7, nm8,dt8,k8, nm9,dt9,k9, nm10,dt10,k10, nm11,dt11,k11, nm12,dt12,k12, nm13,dt13,k13,&
         nm14,dt14,k14, nm15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_getm_method_data_by_index_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
              if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1,"", nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
                dt1 = kim_api_get_method_data_by_index_f(kimmdl,nm1,error);
                if (errcheck_mltpl(error,msg,1, "", nm1).lt.KIM_STATUS_OK) return
            endif
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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2,"", nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3,"", nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4,"", nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5,"", nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6,"", nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7,"", nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8,"", nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9,"", nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10,"", nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11,"", nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12,"", nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13,"", nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14,"", nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15,"", nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error =KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) dt2= kim_api_get_method_data_by_index_f(kimmdl,nm2,error);
            if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) dt3= kim_api_get_method_data_by_index_f(kimmdl,nm3,error);
            if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) dt4= kim_api_get_method_data_by_index_f(kimmdl,nm4,error);
            if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) dt5= kim_api_get_method_data_by_index_f(kimmdl,nm5,error);
            if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) dt6= kim_api_get_method_data_by_index_f(kimmdl,nm6,error);
            if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) dt7= kim_api_get_method_data_by_index_f(kimmdl,nm7,error);
            if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) dt8= kim_api_get_method_data_by_index_f(kimmdl,nm8,error);
            if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) dt9= kim_api_get_method_data_by_index_f(kimmdl,nm9,error);
            if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) dt10= kim_api_get_method_data_by_index_f(kimmdl,nm10,error);
            if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) dt11= kim_api_get_method_data_by_index_f(kimmdl,nm11,error);
            if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) dt12= kim_api_get_method_data_by_index_f(kimmdl,nm12,error);
            if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) dt13= kim_api_get_method_data_by_index_f(kimmdl,nm13,error);
            if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) dt14= kim_api_get_method_data_by_index_f(kimmdl,nm14,error);
            if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) dt15= kim_api_get_method_data_by_index_f(kimmdl,nm15,error);
            if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_getm_method_data_by_index_f

        subroutine kim_api_getm_compute_f(kimmdl,error, nm1,dt1,k1, nm2,dt2,k2, nm3,dt3,k3, nm4,dt4,k4, nm5,dt5,k5,&
         nm6,dt6,k6, nm7,dt7,k7, nm8,dt8,k8, nm9,dt9,k9, nm10,dt10,k10, nm11,dt11,k11, nm12,dt12,k12, nm13,dt13,k13,&
         nm14,dt14,k14, nm15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_getm_compute_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
           !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
              if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1, nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
                dt1 = kim_api_get_compute_f(kimmdl,nm1,error);
                if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            endif

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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error=KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) dt2= kim_api_get_compute_f(kimmdl,nm2,error);
            if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) dt3= kim_api_get_compute_f(kimmdl,nm3,error);
            if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) dt4= kim_api_get_compute_f(kimmdl,nm4,error);
            if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) dt5= kim_api_get_compute_f(kimmdl,nm5,error);
            if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) dt6= kim_api_get_compute_f(kimmdl,nm6,error);
            if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) dt7= kim_api_get_compute_f(kimmdl,nm7,error);
            if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) dt8= kim_api_get_compute_f(kimmdl,nm8,error);
            if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) dt9= kim_api_get_compute_f(kimmdl,nm9,error);
            if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) dt10= kim_api_get_compute_f(kimmdl,nm10,error);
            if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) dt11= kim_api_get_compute_f(kimmdl,nm11,error);
            if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) dt12= kim_api_get_compute_f(kimmdl,nm12,error);
            if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) dt13= kim_api_get_compute_f(kimmdl,nm13,error);
            if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) dt14= kim_api_get_compute_f(kimmdl,nm14,error);
            if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) dt15= kim_api_get_compute_f(kimmdl,nm15,error);
            if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_getm_compute_f

        subroutine kim_api_getm_compute_by_index_f(kimmdl,error, nm1,dt1,k1, nm2,dt2,k2, nm3,dt3,k3, nm4,dt4,k4, nm5,dt5,k5,&
         nm6,dt6,k6, nm7,dt7,k7, nm8,dt8,k8, nm9,dt9,k9, nm10,dt10,k10, nm11,dt11,k11, nm12,dt12,k12, nm13,dt13,k13,&
         nm14,dt14,k14, nm15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_getm_compute_by_index_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !

            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(error,msg,1,"", nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
                dt1 = kim_api_get_compute_by_index_f(kimmdl,nm1,error);
                if (errcheck_mltpl(error,msg,1, "", nm1).lt.KIM_STATUS_OK) return
            endif

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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2,"", nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3,"", nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4,"", nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5,"", nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6,"", nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7,"", nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8,"", nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9,"", nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10,"", nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11,"", nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12,"", nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13,"", nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14,"", nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15,"", nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error =KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) dt2= kim_api_get_compute_by_index_f(kimmdl,nm2,error);
            if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) dt3= kim_api_get_compute_by_index_f(kimmdl,nm3,error);
            if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) dt4= kim_api_get_compute_by_index_f(kimmdl,nm4,error);
            if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) dt5= kim_api_get_compute_by_index_f(kimmdl,nm5,error);
            if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) dt6= kim_api_get_compute_by_index_f(kimmdl,nm6,error);
            if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) dt7= kim_api_get_compute_by_index_f(kimmdl,nm7,error);
            if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) dt8= kim_api_get_compute_by_index_f(kimmdl,nm8,error);
            if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) dt9= kim_api_get_compute_by_index_f(kimmdl,nm9,error);
            if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) dt10= kim_api_get_compute_by_index_f(kimmdl,nm10,error);
            if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) dt11= kim_api_get_compute_by_index_f(kimmdl,nm11,error);
            if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) dt12= kim_api_get_compute_by_index_f(kimmdl,nm12,error);
            if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) dt13= kim_api_get_compute_by_index_f(kimmdl,nm13,error);
            if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) dt14= kim_api_get_compute_by_index_f(kimmdl,nm14,error);
            if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) dt15= kim_api_get_compute_by_index_f(kimmdl,nm15,error);
            if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_getm_compute_by_index_f

        subroutine kim_api_getm_index_f(kimmdl,error,  nm1,dt1,k1, nm2,dt2,k2, nm3,dt3,k3, nm4,dt4,k4, nm5,dt5,k5,&
         nm6,dt6,k6, nm7,dt7,k7, nm8,dt8,k8, nm9,dt9,k9, nm10,dt10,k10, nm11,dt11,k11, nm12,dt12,k12, nm13,dt13,k13,&
         nm14,dt14,k14, nm15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_getm_index_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1, nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
                dt1 = kim_api_get_index_f(kimmdl,nm1,error);
                if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            endif

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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error = KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) dt2= kim_api_get_index_f(kimmdl,nm2,error);
            if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) dt3= kim_api_get_index_f(kimmdl,nm3,error);
            if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) dt4= kim_api_get_index_f(kimmdl,nm4,error);
            if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) dt5= kim_api_get_index_f(kimmdl,nm5,error);
            if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) dt6= kim_api_get_index_f(kimmdl,nm6,error);
            if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) dt7= kim_api_get_index_f(kimmdl,nm7,error);
            if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) dt8= kim_api_get_index_f(kimmdl,nm8,error);
            if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) dt9= kim_api_get_index_f(kimmdl,nm9,error);
            if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) dt10= kim_api_get_index_f(kimmdl,nm10,error);
            if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) dt11= kim_api_get_index_f(kimmdl,nm11,error);
            if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) dt12= kim_api_get_index_f(kimmdl,nm12,error);
            if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) dt13= kim_api_get_index_f(kimmdl,nm13,error);
            if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) dt14= kim_api_get_index_f(kimmdl,nm14,error);
            if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) dt15= kim_api_get_index_f(kimmdl,nm15,error);
            if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_getm_index_f

         subroutine kim_api_setm_compute_f(kimmdl,error, nm1,dt1,k1, nm2,dt2,k2, nm3,dt3,k3, nm4,dt4,k4, nm5,dt5,k5,&
         nm6,dt6,k6, nm7,dt7,k7, nm8,dt8,k8, nm9,dt9,k9, nm10,dt10,k10, nm11,dt11,k11, nm12,dt12,k12, nm13,dt13,k13,&
         nm14,dt14,k14, nm15,dt15,k15)
            integer(kind=kim_intptr) :: kimmdl;  integer error

            character(len=40) ::msg="kim_api_setm_compute_f"

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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
                error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1, nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
                call kim_api_set_compute_f(kimmdl,nm1,dt1,error);
                if (errcheck_mltpl(error,msg,1, nm1).lt.KIM_STATUS_OK) return
            endif

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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2, nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3, nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4, nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5, nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6, nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7, nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8, nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9, nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10, nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11, nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12, nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13, nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14, nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15, nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error = KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) call kim_api_set_compute_f(kimmdl,nm2,dt2,error);
            if(errcheck_mltpl(error,msg,2,nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) call kim_api_set_compute_f(kimmdl,nm3,dt3,error);
            if(errcheck_mltpl(error,msg,3,nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) call kim_api_set_compute_f(kimmdl,nm4,dt4,error);
            if(errcheck_mltpl(error,msg,4,nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) call kim_api_set_compute_f(kimmdl,nm5,dt5,error);
            if(errcheck_mltpl(error,msg,5,nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) call kim_api_set_compute_f(kimmdl,nm6,dt6,error);
            if(errcheck_mltpl(error,msg,6,nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) call kim_api_set_compute_f(kimmdl,nm7,dt7,error);
            if(errcheck_mltpl(error,msg,7,nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) call kim_api_set_compute_f(kimmdl,nm8,dt8,error);
            if(errcheck_mltpl(error,msg,8,nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) call kim_api_set_compute_f(kimmdl,nm9,dt9,error);
            if(errcheck_mltpl(error,msg,9,nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) call kim_api_set_compute_f(kimmdl,nm10,dt10,error);
            if(errcheck_mltpl(error,msg,10,nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) call kim_api_set_compute_f(kimmdl,nm11,dt11,error);
            if(errcheck_mltpl(error,msg,11,nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) call kim_api_set_compute_f(kimmdl,nm12,dt12,error);
            if(errcheck_mltpl(error,msg,12,nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) call kim_api_set_compute_f(kimmdl,nm13,dt13,error);
            if(errcheck_mltpl(error,msg,13,nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) call kim_api_set_compute_f(kimmdl,nm14,dt14,error);
            if(errcheck_mltpl(error,msg,14,nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) call kim_api_set_compute_f(kimmdl,nm15,dt15,error);
            if(errcheck_mltpl(error,msg,15,nm15).lt.KIM_STATUS_OK) return
         end subroutine kim_api_setm_compute_f

        subroutine kim_api_setm_compute_by_index_f(kimmdl,error, nm1,dt1,k1, nm2,dt2,k2, nm3,dt3,k3, nm4,dt4,k4, nm5,dt5,k5,&
         nm6,dt6,k6, nm7,dt7,k7, nm8,dt8,k8, nm9,dt9,k9, nm10,dt10,k10, nm11,dt11,k11, nm12,dt12,k12, nm13,dt13,k13,&
         nm14,dt14,k14, nm15,dt15,k15)
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
            integer ::k1;
            integer,optional::k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15
            !
            if(k1.ne.0.and.k1.ne.1) then
               error=KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
               if(errcheck_mltpl(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY,msg,1,"", nm1 ).lt.KIM_STATUS_OK) return
            endif
            if(k1.eq.1) then
                call kim_api_set_compute_by_index_f(kimmdl,nm1,dt1,error);
                if (errcheck_mltpl(error,msg,1, "", nm1).lt.KIM_STATUS_OK) return
            endif

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
            endif

            error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY
            if(present(k2).and.(k2.ne.0.and.k2.ne.1))then
                if(errcheck_mltpl(error,msg,2,"", nm2 ).lt.KIM_STATUS_OK) return
            elseif(present(k3).and.(k3.ne.0.and.k3.ne.1))then
                if(errcheck_mltpl(error,msg,3,"", nm3 ).lt.KIM_STATUS_OK) return
            elseif(present(k4).and.(k4.ne.0.and.k4.ne.1))then
                if(errcheck_mltpl(error,msg,4,"", nm4 ).lt.KIM_STATUS_OK) return
            elseif(present(k5).and.(k5.ne.0.and.k5.ne.1))then
                if(errcheck_mltpl(error,msg,5,"", nm5 ).lt.KIM_STATUS_OK) return
            elseif(present(k6).and.(k6.ne.0.and.k6.ne.1))then
                if(errcheck_mltpl(error,msg,6,"", nm6 ).lt.KIM_STATUS_OK) return
            elseif(present(k7).and.(k7.ne.0.and.k7.ne.1))then
                if(errcheck_mltpl(error,msg,7,"", nm7 ).lt.KIM_STATUS_OK) return
            elseif(present(k8).and.(k8.ne.0.and.k8.ne.1))then
                if(errcheck_mltpl(error,msg,8,"", nm8 ).lt.KIM_STATUS_OK) return
            elseif(present(k9).and.(k9.ne.0.and.k9.ne.1))then
                if(errcheck_mltpl(error,msg,9,"", nm9 ).lt.KIM_STATUS_OK) return
            elseif(present(k10).and.(k10.ne.0.and.k10.ne.1))then
                if(errcheck_mltpl(error,msg,10,"", nm10 ).lt.KIM_STATUS_OK) return
            elseif(present(k11).and.(k11.ne.0.and.k11.ne.1))then
                if(errcheck_mltpl(error,msg,11,"", nm11 ).lt.KIM_STATUS_OK) return
            elseif(present(k12).and.(k12.ne.0.and.k12.ne.1))then
                if(errcheck_mltpl(error,msg,12,"", nm12 ).lt.KIM_STATUS_OK) return
            elseif(present(k13).and.(k13.ne.0.and.k13.ne.1))then
                if(errcheck_mltpl(error,msg,13,"", nm13 ).lt.KIM_STATUS_OK) return
            elseif(present(k14).and.(k14.ne.0.and.k14.ne.1))then
                if(errcheck_mltpl(error,msg,14,"", nm14 ).lt.KIM_STATUS_OK) return
            elseif(present(k15).and.(k15.ne.0.and.k15.ne.1))then
                if(errcheck_mltpl(error,msg,15,"", nm15 ).lt.KIM_STATUS_OK) return
            endif

            !process arguments
            error = KIM_STATUS_OK
 if(present(nm2).and.k2.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm2,dt2,error);
            if(errcheck_mltpl(error,msg,2,"",nm2).lt.KIM_STATUS_OK) return
 if(present(nm3).and.k3.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm3,dt3,error);
            if(errcheck_mltpl(error,msg,3,"",nm3).lt.KIM_STATUS_OK) return
 if(present(nm4).and.k4.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm4,dt4,error);
            if(errcheck_mltpl(error,msg,4,"",nm4).lt.KIM_STATUS_OK) return
 if(present(nm5).and.k5.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm5,dt5,error);
            if(errcheck_mltpl(error,msg,5,"",nm5).lt.KIM_STATUS_OK) return
 if(present(nm6).and.k6.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm6,dt6,error);
            if(errcheck_mltpl(error,msg,6,"",nm6).lt.KIM_STATUS_OK) return
 if(present(nm7).and.k7.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm7,dt7,error);
            if(errcheck_mltpl(error,msg,7,"",nm7).lt.KIM_STATUS_OK) return
 if(present(nm8).and.k8.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm8,dt8,error);
            if(errcheck_mltpl(error,msg,8,"",nm8).lt.KIM_STATUS_OK) return
 if(present(nm9).and.k9.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm9,dt9,error);
            if(errcheck_mltpl(error,msg,9,"",nm9).lt.KIM_STATUS_OK) return
 if(present(nm10).and.k10.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm10,dt10,error);
            if(errcheck_mltpl(error,msg,10,"",nm10).lt.KIM_STATUS_OK) return
 if(present(nm11).and.k11.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm11,dt11,error);
            if(errcheck_mltpl(error,msg,11,"",nm11).lt.KIM_STATUS_OK) return
 if(present(nm12).and.k12.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm12,dt12,error);
            if(errcheck_mltpl(error,msg,12,"",nm12).lt.KIM_STATUS_OK) return
 if(present(nm13).and.k13.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm13,dt13,error);
            if(errcheck_mltpl(error,msg,13,"",nm13).lt.KIM_STATUS_OK) return
 if(present(nm14).and.k14.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm14,dt14,error);
            if(errcheck_mltpl(error,msg,14,"",nm14).lt.KIM_STATUS_OK) return
 if(present(nm15).and.k15.eq.1) call kim_api_set_compute_by_index_f(kimmdl,nm15,dt15,error);
            if(errcheck_mltpl(error,msg,15,"",nm15).lt.KIM_STATUS_OK) return
        end subroutine kim_api_setm_compute_by_index_f


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

end module kim_api
