program dirac_one_e_ints
  implicit none
  integer :: i
 
 print*, "2*small_ao_num =",2*small_ao_num
 do i=1,2*dirac_ao_num
  print*,i,eigenvalues_dirac_mono_elec_mo(i)
 enddo
end
