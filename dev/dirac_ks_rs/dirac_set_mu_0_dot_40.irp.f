program dirac_set_mu_0_dot_40
  BEGIN_DOC
  ! Set dirac_mu_over_kFmax to 0.40
  END_DOC
  implicit none
  dirac_mu_over_kFmax = 0.40d0
  call ezfio_set_dirac_dft_utils_one_e_dirac_mu_over_kFmax(dirac_mu_over_kFmax)
 TOUCH dirac_mu_over_kFmax

 end
