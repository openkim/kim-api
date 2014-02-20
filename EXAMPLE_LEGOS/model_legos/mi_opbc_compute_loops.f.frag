    ! determine whether half or full lists are being used
    pNBC_Method = kim_api_get_nbc_method(pkim, Compute_Energy_Forces)
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_get_nbc_method", Compute_Energy_Forces)
       return
    endif
    call c_f_pointer(pNBC_Method, NBC_Method)
    if (index(NBC_Method,"MI_OPBC_H").eq.1) then
       HalfOrFull = 1
    elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then
       HalfOrFull = 2
    else
       Compute_Energy_Forces = KIM_STATUS_FAIL
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "Unsupported NBC type", Compute_Energy_Forces)
       return
    endif
    call KIM_API_c_free(pNBC_Method) ! don't forget to release the memory...
    NBC_Method => null()

    ! get boxSideLengths & numberContributingParticles
    call kim_api_getm_data(pkim, Compute_Energy_Forces,      &
         "boxSideLengths",              pboxSideLengths,  1, &
         "numberContributingParticles", pnumContrib,      1)
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", Compute_Energy_Forces)
       return
    endif
    call c_f_pointer(pboxSideLengths, boxSideLengths, [DIM])
    call c_f_pointer(pnumContrib, numContrib)

    !  Compute energy and forces
    !
    do i = 1,numberOfParticles

       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i

       Compute_Energy_Forces = kim_api_get_neigh(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij)
       if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
          idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_get_neigh", Compute_Energy_Forces)
          return
       endif
       call c_f_pointer(pnei1atom, nei1atom, [numnei])

       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          j = nei1atom(jj)
          Rij(:) = coor(:,j) - coor(:,i)                ! distance vector between i j
          where ( abs(Rij) > 0.5_cd*boxSideLengths )    ! periodic boundary conditions
             Rij = Rij - sign(boxSideLengths,Rij)            ! applied where needed.
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
                dEidr = 0.5_cd*dphi                     !      regular contribution
             endif
             if (comp_enepot.eq.1) then                 !
                enepot(i) = enepot(i) + 0.5_cd*phi      ! accumulate energy
                if ((HalfOrFull.eq.1) .and. &
                    (j .le. numContrib)) then           ! HALF mode
                   enepot(j) = enepot(j) + 0.5_cd*phi   ! (i and j share it)
                endif                                   !
             endif                                      !
             if (comp_energy.eq.1) then                 !
                if ((HalfOrFull.eq.1) .and. &
                    (j .le. numContrib)) then           ! HALF mode
                   energy = energy + phi                ! half neigh case
                else                                    !
                   energy = energy + 0.5_cd*phi         ! full neigh case
                endif                                   !
             endif
             if (comp_virial.eq.1) then                 ! accumul. virial
                virial(1) = virial(1) + Rij(1)*Rij(1)*dEidr/r
                virial(2) = virial(2) + Rij(2)*Rij(2)*dEidr/r
                virial(3) = virial(3) + Rij(3)*Rij(3)*dEidr/r
                virial(4) = virial(4) + Rij(2)*Rij(3)*dEidr/r
                virial(5) = virial(5) + Rij(1)*Rij(3)*dEidr/r
                virial(6) = virial(6) + Rij(1)*Rij(2)*dEidr/r
             endif                                      !
             if (comp_force.eq.1) then                  !
                force(:,i) = force(:,i) + dEidr*Rij/r   ! accumulate force on atom i
                force(:,j) = force(:,j) - dEidr*Rij/r   ! accumulate force on atom j
             endif
          endif
       enddo
    enddo
