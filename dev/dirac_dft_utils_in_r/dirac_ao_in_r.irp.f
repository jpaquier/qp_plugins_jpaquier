 BEGIN_PROVIDER[complex*16, dirac_aos_in_r_array, (2*dirac_ao_num,n_points_final_grid)]
 &BEGIN_PROVIDER[complex*16, dirac_aos_in_r_array_transp, (n_points_final_grid,2*dirac_ao_num)]
 implicit none
 integer :: i,j
 complex*16 :: dirac_aos_array(2*dirac_ao_num) 
 double precision :: r(3)
 do i = 1, n_points_final_grid
  r(1) = final_grid_points(1,i) 
  r(2) = final_grid_points(2,i) 
  r(3) = final_grid_points(3,i) 
  call give_all_dirac_aos_at_r(r,dirac_aos_array)
  do j = 1, 2*dirac_ao_num
   dirac_aos_in_r_array(j,i) = dirac_aos_array(j)
   dirac_aos_in_r_array_transp(i,j) = dirac_aos_array(j)
  enddo
 enddo
 END_PROVIDER

