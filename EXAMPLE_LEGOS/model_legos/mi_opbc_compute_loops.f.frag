    ! determine whether half or full lists are being used
    pNBC_Method = kim_api_get_nbc_method_f(pkim, ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_nbc_method_f", ier)
       return
    endif
    if (index(NBC_Method,"MI-OPBC-H").eq.1) then
       HalfOrFull = 1
    elseif (index(NBC_Method,"MI-OPBC-F").eq.1) then
       HalfOrFull = 2
    else
       ier = KIM_STATUS_FAIL
       call kim_api_report_error_f(__LINE__, __FILE__, "Unsupported NBC type", ier)
       return
    endif
    call free(pNBC_Method) ! don't forget to release the memory...

    ! get boxlength
    pboxlength = kim_api_get_data_f(pkim,"boxlength",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif
    ! get numberContributingAtoms
    pnumContrib = kim_api_get_data_f(pkim,"numberContributingAtoms",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       return
    endif


    !  Compute energy and forces
    !
    do i = 1,numberOfAtoms
       
       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i
       
       if (HalfOrFull.eq.1) then
          ier = kim_api_get_half_neigh_f(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_dummy)
       else
          ier = kim_api_get_full_neigh_f(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_dummy)
       endif
       if (ier.lt.KIM_STATUS_OK) then
          call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_*_neigh", ier)
          return
       endif
       
       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          j = nei1atom(jj)
          Rij(:) = coor(:,j) - coor(:,i)                ! distance vector between i j
          where ( abs(Rij) > 0.5d0*boxlength )          ! periodic boundary conditions
             Rij = Rij - sign(boxlength,Rij)            ! applied where needed.
          end where                                     ! 
          Rsqij = dot_product(Rij,Rij)                  ! compute square distance
          if ( Rsqij < model_cutsq ) then               ! particles are interacting?
             r = sqrt(Rsqij)                            ! compute distance
             call pair(model_epsilon,model_sigma,model_A,model_B, model_C, &
                  r,phi,dphi,d2phi)                     ! compute pair potential
             if ((HalfOrFull.eq.1) .and. &
                 (j .le. numContrib)) then              ! HALF mode
                dEidr = dphi                            !      double contribution
             else                                       ! FULL mode
                dEidr = 0.5d0*dphi                      !      regular contribution
             endif
             if (comp_enepot.eq.1) then                 !
                ene_pot(i) = ene_pot(i) + 0.5d0*phi     ! accumulate energy
                if ((HalfOrFull.eq.1) .and. &
                    (j .le. numContrib)) then           ! HALF mode
                   ene_pot(j) = ene_pot(j) + 0.5d0*phi  ! (i and j share it)
                endif                                   !
             elseif (comp_energy.eq.1) then             !
                if ((HalfOrFull.eq.1) .and. &
                    (j .le. numContrib)) then           ! HALF mode
                   energy = energy + phi                ! half neigh case
                else                                    !
                   energy = energy + 0.5d0*phi          ! full neigh case
                endif                                   !
             endif
             if (comp_virial.eq.1) then                 ! accumul. virial
                virial_global(1) = virial_global(1) + Rij(1)*Rij(1)*dEidr/r
                virial_global(2) = virial_global(2) + Rij(2)*Rij(2)*dEidr/r
                virial_global(3) = virial_global(3) + Rij(3)*Rij(3)*dEidr/r
                virial_global(4) = virial_global(4) + Rij(2)*Rij(3)*dEidr/r
                virial_global(5) = virial_global(5) + Rij(1)*Rij(3)*dEidr/r
                virial_global(6) = virial_global(6) + Rij(1)*Rij(2)*dEidr/r
             endif                                      !
             if (comp_force.eq.1) then                  !
                force(:,i) = force(:,i) + dEidr*Rij/r   ! accumulate force on atom i
                force(:,j) = force(:,j) - dEidr*Rij/r   ! accumulate force on atom j
             endif
          endif
       enddo
    enddo
