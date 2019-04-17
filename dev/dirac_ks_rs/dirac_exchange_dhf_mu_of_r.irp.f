program dirac_exchange_dhf_mu_of_r
  BEGIN_DOC
  ! produce the dirac energy
  END_DOC
   call run_exchange_mu_of_r 
 end

 subroutine run_exchange_mu_of_r
  BEGIN_DOC
  ! Gives the energy for a given value of mu_erf
  END_DOC
  use bitmasks
  implicit none 
  integer :: i,length
 !Choose Interaction
  if (dirac_interaction == "Coulomb") then
   print*,'**********'
   print*,'Short-range Coulomb interaction'   
   print*, 'dirac_HF_two_electron_C_Exchange_energy=', dirac_C_Exchange_Energy - dirac_HF_two_electron_c_ex_energy_mu_of_r
   open (10, file='mu_of_r_exchange_dhf_Z.dat',position ='append') 
   write(10,*) dirac_C_Exchange_Energy - dirac_HF_two_electron_c_ex_energy_mu_of_r
  else
   print *,  'Unrecognized dirac_interaction : '//dirac_interaction
   stop 1
  endif

end

