program dirac_ao_basis
  implicit none
  integer :: i,j
  double precision :: r(3)
  complex*16 :: two_dirac_aos_array(2*dirac_ao_num),dirac_aos_array(dirac_ao_num)
  complex*16 :: large_aos_array(large_ao_num)
  complex*16 :: small_aos_array(small_ao_num)
  integer :: dirac_power_ao(3)
  r= 0.01d0
 do i=1, dirac_ao_num
 !print*, dirac_ao_nucl(i)
 enddo
 
 print*,large_ao_num,small_ao_num
 
!do j=1,nucl_num
! do i =1, Nucl_N_dirac_aos(j)
!  print*,i,j,Nucl_dirac_Aos_transposed(i,j),dirac_ao_coef_normalized_ordered_transp_per_nucl(1,i,j),dirac_ao_expo_ordered_transp_per_nucl(1,i,j), dirac_ao_power_ordered_transp_per_nucl(1,i,j),dirac_ao_power_ordered_transp_per_nucl(2,i,j),dirac_ao_power_ordered_transp_per_nucl(3,i,j)
! enddo
! print*,'******************'
!enddo

 call give_all_large_aos_at_r(r,large_aos_array)
 call give_all_small_aos_at_r(r,small_aos_array)
 call give_all_dirac_aos_at_r(r,dirac_aos_array)
 call give_all_two_dirac_aos_at_r(r,two_dirac_aos_array)
 print*,'**********************************************'
 do i = 1,2*dirac_ao_num
  print*,i,two_dirac_aos_array(i)
 enddo
 print*,'****************************'
 do i = 1,dirac_ao_num
  print*,i,dirac_aos_array(i)
 enddo
 print*,'****************************'
 do i = 1,large_ao_num
  print*,i,large_aos_array(i)
 enddo
 print*,'****************************'
 do i = 1,small_ao_num
  print*,i,small_aos_array(i)
 enddo

end


