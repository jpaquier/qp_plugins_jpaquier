program dirac_hartree_fock
 implicit none
 integer :: i,j
 double precision :: pi

 print*, "small_ao_num =",small_ao_num
 print*, "large_ao_num =",large_ao_num

 !! Print the positive energy molecular orbitals
 do j= 2*small_ao_num+1, 2*small_ao_num + elec_num
  do i= 1, 2*dirac_ao_num
   if (i .le. large_ao_num) then
    print*, i,j, dirac_mo_coef(i,j), dirac_ao_nucl(i),dirac_ao_power(i,1), dirac_ao_power(i,2), dirac_ao_power(i,3) 
   elseif (i .gt. large_ao_num .and. i .le. 2*large_ao_num) then
    print*, i,j, dirac_mo_coef(i,j), dirac_ao_nucl(i-large_ao_num),dirac_ao_power(i-large_ao_num,1), dirac_ao_power(i-large_ao_num,2), dirac_ao_power(i-large_ao_num,3)
   elseif (i .gt. 2*large_ao_num .and. i .le. 2*large_ao_num + small_ao_num) then
    print*, i,j, dirac_mo_coef(i,j), dirac_ao_nucl(i-large_ao_num),dirac_ao_power(i-large_ao_num,1),dirac_ao_power(i-large_ao_num,2), dirac_ao_power(i-large_ao_num,3)
   elseif (i .gt. large_ao_num + small_ao_num) then
    print*, i,j, dirac_mo_coef(i,j), dirac_ao_nucl(i-large_ao_num-small_ao_num),dirac_ao_power(i-large_ao_num-small_ao_num,1), dirac_ao_power(i-large_ao_num-small_ao_num,2), dirac_ao_power(i-large_ao_num-small_ao_num,3)
   endif
  enddo
  print*,'************'
 enddo

!print*,'************************************************************************'
!do j =1,2*dirac_ao_num
! do i = 1,2*dirac_ao_num 
!  print*, i, j, dirac_SCF_density_matrix_ao(i,j)
! enddo
! print*, '***************'
!enddo
 

end
