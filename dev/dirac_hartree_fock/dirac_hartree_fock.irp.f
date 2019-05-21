program dirac_hartree_fock
 implicit none
 integer :: i,j
 double precision :: pi

 print*, "small_ao_num =",small_ao_num
 print*, "large_ao_num =",large_ao_num
 print*, "dirac_mo_tot_num=",dirac_mo_tot_num

!do j=1,2*dirac_mo_tot_num
! print*,j,eigenvalues_dirac_mono_elec_mo(j)
! do i = 1,2*dirac_mo_tot_num
!  print*,i,j,eigenvectors_dirac_mono_elec_mo(i,j)
! enddo
!enddo
!print*,"*********************************"
!do j=1,2*dirac_mo_tot_num
! do i = 1,2*dirac_mo_tot_num
!  print*,i,j,eigenvectors_dirac_mono_elec_ao(i,j)
! enddo
!enddo

!print*, dirac_HF_one_electron_energy, dirac_HF_two_electron_C_energy

 print*,'************************************************************************'
 do j =1,2*dirac_ao_num
  do i = 1,2*dirac_ao_num 
   print*, i, j, dirac_SCF_density_matrix_ao(i,j)
  enddo
  print*, '***************'
 enddo
 

end
