 double precision function erf_mu_of_r_dirac_ao(i,j,k,l)
 implicit none
 BEGIN_DOC
 ! computes the following integral :
 ! int dr1 dr2 AO_i(r1) AO_j(r1) erf(mu(r1) |r1-r2|) / |r1-r2| AO_k(r2) AO_l(r2)
 END_DOC
 integer, intent(in) :: i,j,k,l
 integer :: i_point
 double precision, allocatable :: dirac_aos_array(:)
 double precision :: r(3),NAI_pol_mult_erf_dirac_ao
 !First, remove from the start the ints that have no physical meaning
!if ((i .le. large_ao_num .and. j .le. large_ao_num .and. k .le. large_ao_num .and. l .gt. large_ao_num) .or.  &
!    (i .le. large_ao_num .and. j .le. large_ao_num .and. k .gt. large_ao_num .and. l .le. large_ao_num) .or.  &
!    (i .le. large_ao_num .and. j .gt. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num) .or.  &
!    (i .gt. large_ao_num .and. j .le. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num) .or.  &
!    (i .le. large_ao_num .and. j .gt. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num) .or.  &
!    (i .gt. large_ao_num .and. j .le. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num) .or.  &
!    (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .le. large_ao_num .and. l .gt. large_ao_num) .or.  &
!    (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .gt. large_ao_num .and. l .le. large_ao_num)) then
! erf_mu_of_r_dirac_ao = 0.d0
!else 
  allocate(dirac_aos_array(dirac_ao_num))
  erf_mu_of_r_dirac_ao = 0.d0
  do i_point = 1, n_points_final_grid
   r(1) = final_grid_points(1,i_point)
   r(2) = final_grid_points(2,i_point)
   r(3) = final_grid_points(3,i_point)
   call give_all_dirac_aos_at_r(r,dirac_aos_array)
   erf_mu_of_r_dirac_ao += 0.5d0 * dirac_aos_array(i) * dirac_aos_array(j) *NAI_pol_mult_erf_dirac_ao(k,l,mu_of_r_for_ints_vector(i_point),r) *final_weight_at_r_vector(i_point)
   erf_mu_of_r_dirac_ao += 0.5d0 * dirac_aos_array(k) * dirac_aos_array(l) *NAI_pol_mult_erf_dirac_ao(j,i,mu_of_r_for_ints_vector(i_point),r) *final_weight_at_r_vector(i_point)
  enddo
  deallocate(dirac_aos_array)
!endif 
 end

 BEGIN_PROVIDER [ double precision, erf_mu_of_r_dirac_ao_test, (dirac_ao_num,dirac_ao_num,dirac_ao_num,dirac_ao_num) ]
 implicit none
 BEGIN_DOC
 ! computes the following integral :
 ! int dr1 dr2 AO_i(r1) AO_j(r1) erf(mu(r1) |r1-r2|) / |r1-r2| AO_k(r2) AO_l(r2)
 END_DOC
 integer :: i,j,k,l
 integer :: i_point
 double precision, allocatable :: dirac_aos_array(:)
 double precision :: r(3),NAI_pol_mult_erf_dirac_ao
 !First, remove from the start the ints that have no physical meaning
 if ((i .le. large_ao_num .and. j .le. large_ao_num .and. k .le. large_ao_num .and. l .gt. large_ao_num) .or.  &
     (i .le. large_ao_num .and. j .le. large_ao_num .and. k .gt. large_ao_num .and. l .le. large_ao_num) .or.  &
     (i .le. large_ao_num .and. j .gt. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num) .or.  &
     (i .gt. large_ao_num .and. j .le. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num) .or.  &
     (i .le. large_ao_num .and. j .gt. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num) .or.  &
     (i .gt. large_ao_num .and. j .le. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num) .or.  &
     (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .le. large_ao_num .and. l .gt. large_ao_num) .or.  &
     (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .gt. large_ao_num .and. l .le. large_ao_num)) then
  erf_mu_of_r_dirac_ao_test = 0.d0
 else 
  allocate(dirac_aos_array(dirac_ao_num))
  erf_mu_of_r_dirac_ao_test = 0.d0
  do i_point = 1, n_points_final_grid
   r(1) = final_grid_points(1,i_point)
   r(2) = final_grid_points(2,i_point)
   r(3) = final_grid_points(3,i_point)
   call give_all_dirac_aos_at_r(r,dirac_aos_array)
   do i = 15, 20
    do j = 15, 20
     do k = 15, 20
      do l = 15, 20
       erf_mu_of_r_dirac_ao_test(i,j,k,l) += 0.5d0 * dirac_aos_array(i) * dirac_aos_array(j) * final_weight_at_r_vector(i_point) *NAI_pol_mult_erf_dirac_ao(k,l,mu_of_r_for_ints_vector(i_point),r) 
       erf_mu_of_r_dirac_ao_test(i,j,k,l) += 0.5d0 * dirac_aos_array(k) * dirac_aos_array(l) * final_weight_at_r_vector(i_point) *NAI_pol_mult_erf_dirac_ao(j,i,mu_of_r_for_ints_vector(i_point),r) 
      enddo
     enddo
    enddo
   enddo
  enddo
  deallocate(dirac_aos_array)
 endif 
 END_PROVIDER
