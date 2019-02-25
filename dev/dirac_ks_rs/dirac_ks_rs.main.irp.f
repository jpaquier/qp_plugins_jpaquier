program dirac_ks_rs
 implicit none
 integer :: istate,i,j
 double precision :: r(3)
 double precision :: mu,weight,x
 double precision :: dm(N_states),tr_dm(N_states),tr_gamma_2(N_states)
 
!do istate=1,N_states
! do j = 2*large_ao_num+small_ao_num+1, 2*dirac_ao_num
!  do i = 1, large_ao_num
!   print*,i,j,dirac_one_body_dm_ao_for_dft(i,j,istate)
!  enddo
!  print*,'***********************************'
! enddo
!enddo

!r =0.1d0
!call dirac_tr_dm_dft_at_r(r,tr_dm)
!print*,tr_dm(1)

 do istate = 1, N_states
  do i = 1, 10 
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   tr_gamma_2(istate) = dirac_one_body_tr_dm_at_r(i,istate)
   print*,r(1),r(2),r(3),tr_gamma_2(istate)
  enddo
 enddo

!do j = 1,2*dirac_ao_num
! do i = 1,2*dirac_ao_num
!  print*,i,j,dirac_one_body_dm_ao_for_dft(i,j,1),dirac_one_body_dm_ao_for_dft(j,i,1)
! enddo
! print*,'**********'
!enddo

!do j = 1,small_ao_num
! do i = 1,small_ao_num
!  print*,i,j,dirac_one_body_dm_ao_for_dft(2*large_ao_num+i,2*large_ao_num+small_ao_num+j,1),dirac_one_body_dm_ao_for_dft(2*large_ao_num+small_ao_num+j,2*large_ao_num+i,1) 
! enddo
! print*,'**********'
!enddo


!do i = 36,54
! do j = 1, n_points_radial_grid-1
! !x = grid_points_radial(j)
!   
! !print*, i,x,m_knwoles, knowles_function(alpha_knowles(i),m_knowles,x) 
! enddo
!enddo
end
