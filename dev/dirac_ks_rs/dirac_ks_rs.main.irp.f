program dirac_ks_rs
 implicit none
 integer :: istate,i,j,k
 double precision :: r(3)
 double precision :: mu,weight,e_x,v_x
 double precision :: dm(N_states),tr_dm(N_states),tr_gamma_2(N_states),rho(N_states)
 complex*16 :: large_aos_array(large_ao_num),large_aos_array_bis(large_ao_num)
 complex*16 :: small_aos_array(small_ao_num),small_aos_array_bis(small_ao_num),u_dotc_v
 
!do istate=1,N_states
! do j = 2*large_ao_num+small_ao_num+1, 2*dirac_ao_num
!  do i = 1, large_ao_num
!   print*,i,j,dirac_one_body_dm_ao_for_dft(i,j,istate)
!  enddo
!  print*,'***********************************'
! enddo
!enddo

 r =0.1d0
 call dirac_tr_dm_dft_at_r(r,tr_dm)
 print*,tr_dm(1)

 do j = 1, elec_num
  do i = 1,2*dirac_ao_num
   write(33,*),i,j,dirac_mo_coef_electronic_for_dft(i,j)
  enddo
  write(33,*),'******************'
 enddo

 do j = 1,2*dirac_ao_num
  do i = 1, 2*dirac_ao_num
   write(34,*),i,j,dirac_one_body_dm_ao_for_dft(i,j,1)
  enddo
  write(34,*),'******************'
 enddo

 do istate = 1, N_states
  do i = 1, 5000 
   r(1) = final_grid_points(1,i)
   r(2) = final_grid_points(2,i)
   r(3) = final_grid_points(3,i)
   tr_gamma_2(istate) = dirac_one_body_tr_dm_at_r(i,istate)
   rho(istate) = dirac_one_body_dm_at_r(i,istate)
   call dirac_n2x_sr(mu_erf,tr_gamma_2,e_x,v_x)
   write(35,*),r(1),r(2),r(3),rho(istate),tr_gamma_2(istate)
  
  !call give_all_large_aos_at_r(r,large_aos_array)
  !call give_all_small_aos_at_r(r,small_aos_array)
  !do j = 1, small_ao_num
  ! write(35,*),r(1),r(2),r(3),j,small_aos_array(j)
  !enddo
 
  !do k = 1, large_ao_num
  ! write(34,*),r(1),r(2),r(3),k,large_aos_array(k)
  !enddo

 !print*,r(1),r(2),r(3),tr_gamma_2(istate),e_x
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


end
