 BEGIN_PROVIDER [double precision, mu_of_r_for_ints_vector, (n_points_final_grid)]
 implicit none
 BEGIN_DOC
 ! value of mu(r) in each point in space
 END_DOC
 integer :: i_point
 integer :: i_atom,k,l
 double precision :: r(3)
 do i_point = 1, n_points_final_grid
  l = index_final_points(1,i_point)
  k = index_final_points(2,i_point)
  i_atom = index_final_points(3,i_point)
  mu_of_r_for_ints_vector(i_point) = 1.d0
 enddo
 END_PROVIDER


!subroutine give_all_dirac_erf_mu_of_r_kl(k,l,integrals)
!implicit none
!include 'utils/constants.include.F'
!integer, intent(in) :: k,l
!double precision, intent(out) :: integrals(2*dirac_ao_num,2*dirac_ao_num)
!integer :: i_point,i,j
!complex*16 :: integrals_kl_of_r(n_points_final_grid),r(3),NAI_pol_mult_erf_dirac_ao,tmp
!double precision,allocatable :: v_array(:,:),v_vector(:)
!allocate(v_array(2*dirac_ao_num,n_points_final_grid),v_vector(n_points_final_grid))
!integrals = (0.d0,0.d0)
!do i_point = 1, n_points_final_grid
! r(1) = final_grid_points(1,i_point)
! r(2) = final_grid_points(2,i_point)
! r(3) = final_grid_points(3,i_point)
! v_vector(i_point) = NAI_pol_mult_erf_dirac_ao(k,l,mu_of_r_for_ints_vector(i_point),r)
!!tmp = NAI_pol_mult_erf_dirac_ao(k,l,mu_of_r_for_ints_vector(i_point),r)
!!v_vector(i_point) = tmp
!!do i = 1, 2*dirac_ao_num
!! v_array(i,i_point) = tmp * final_weight_at_r_vector(i_point) * dirac_aos_in_r_array(i,i_point)
!!enddo
!enddo
!!call dgemm('N','N',dirac_ao_num,dirac_ao_num,n_points_final_grid,1.d0,v_array,size(v_array,1),dirac_aos_in_r_array_transp,size(dirac_aos_in_r_array_transp,1),1.d0,integrals,size(integrals,1))
!do i_point = 1, n_points_final_grid
! do i = 1, 2*dirac_ao_num
!  do j = 1, 2*dirac_ao_num
!   integrals(j,i) += v_vector(i_point) * dirac_aos_in_r_array(i,i_point) * dirac_aos_in_r_array(j,i_point) * final_weight_at_r_vector(i_point)
!  enddo
! enddo
!enddo
!end
 
 double precision function dirac_ao_erf_mu_of_r_integral(i,j,k,l)
 implicit none
 BEGIN_DOC
 ! computes the following integral :
 ! int dr1 dr2 AO_i(r1) AO_j(r1) erf(mu(r1) |r1-r2|) / |r1-r2| AO_k(r2) AO_l(r2)
 END_DOC
 integer, intent(in) :: i,j,k,l
 integer :: i_point
 double precision, allocatable :: dirac_aos_array(:)
 double precision :: r(3),NAI_pol_mult_erf_dirac_ao
 !First, remove from the start the integrals that have no physical meaning
 if ((i .le. large_ao_num .and. j .le. large_ao_num .and. k .le. large_ao_num .and. l .gt. large_ao_num) .or.  &
     (i .le. large_ao_num .and. j .le. large_ao_num .and. k .gt. large_ao_num .and. l .le. large_ao_num) .or.  &
     (i .le. large_ao_num .and. j .gt. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num) .or.  &
     (i .gt. large_ao_num .and. j .le. large_ao_num .and. k .le. large_ao_num .and. l .le. large_ao_num) .or.  &
     (i .le. large_ao_num .and. j .gt. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num) .or.  &
     (i .gt. large_ao_num .and. j .le. large_ao_num .and. k .gt. large_ao_num .and. l .gt. large_ao_num) .or.  &
     (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .le. large_ao_num .and. l .gt. large_ao_num) .or.  &
     (i .gt. large_ao_num .and. j .gt. large_ao_num .and. k .gt. large_ao_num .and. l .le. large_ao_num)) then
  dirac_ao_erf_mu_of_r_integral = 0.d0
 else 
  allocate(dirac_aos_array(2*dirac_ao_num))
  dirac_ao_erf_mu_of_r_integral = 0.d0
  do i_point = 1, n_points_final_grid
   r(1) = final_grid_points(1,i_point)
   r(2) = final_grid_points(2,i_point)
   r(3) = final_grid_points(3,i_point)
   call give_all_dirac_aos_at_r(r,dirac_aos_array)
   dirac_ao_erf_mu_of_r_integral += 0.5d0 * dirac_aos_array(i) * dirac_aos_array(j) *NAI_pol_mult_erf_dirac_ao(k,l,mu_of_r_for_ints_vector(i_point),r) *final_weight_at_r_vector(i_point)
   dirac_ao_erf_mu_of_r_integral += 0.5d0 * dirac_aos_array(k) * dirac_aos_array(l) *NAI_pol_mult_erf_dirac_ao(j,i,mu_of_r_for_ints_vector(i_point),r) *final_weight_at_r_vector(i_point)
  enddo
  deallocate(dirac_aos_array)
 endif 
 end
 
