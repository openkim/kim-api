    ! determine whether half or full neighbor lists are being used
    pNBC_Method = kim_api_get_nbc_method_f(pkim, Compute_Energy_Forces)
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME,   &
                                     "kim_api_get_nbc_method_f", &
                                     Compute_Energy_Forces)
       return
    endif
    if (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
       HalfOrFull = 1
       ! get numberContributingParticles
       pnumContrib = kim_api_get_data_f(pkim,"numberContributingParticles",Compute_Energy_Forces)
       if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
          idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                        "kim_api_get_data", Compute_Energy_Forces)
          return
       endif
    elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
       pnumContrib = 0 ! initialize to avoid warning
       HalfOrFull = 2
    else
       pnumCongrib = 0 ! initialize to avoid warning
       Compute_Energy_Forces = KIM_STATUS_FAIL
       idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                     "Unsupported NBC type", Compute_Energy_Forces)
       return
    endif
    call free(pNBC_Method) ! don't forget to release the memory...

    !  Compute energy and forces
    !
    do i = 1,numberOfParticles

       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i

       Compute_Energy_Forces = kim_api_get_neigh_f(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_dummy)
       if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
          idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                        "kim_api_get_neigh", Compute_Energy_Forces)
          return
       endif

       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          j = nei1atom(jj)
          Rij(:) = coor(:,j) - coor(:,i)                ! distance vector between i j
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
                     (j .le. numContrib)) then
                   ene_pot(j) = ene_pot(j) + 0.5d0*phi  ! (i and j share it)
                endif                                   !
             endif                                      !
             if (comp_energy.eq.1) then                 !
               if ((HalfOrFull.eq.1) .and. &
                   (j .le. numContrib)) then            ! HALF mode
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
                force(:,i) = force(:,i) + dEidr*Rij/r   ! accumulate forces on i
                force(:,j) = force(:,j) - dEidr*Rij/r   ! accumulate forces on j
             endif
          endif
       enddo
    enddo
