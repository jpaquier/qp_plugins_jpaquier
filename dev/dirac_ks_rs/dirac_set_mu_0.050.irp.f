program dirac_set_mu_0_dot_050
  BEGIN_DOC
  ! Set dirac_mu_over_kFmax to 0.050
  END_DOC

  call ezfio_set_dirac_dft_utils_one_e_dirac_mu_over_kFmax(0.050)
 TOUCH dirac_mu_over_kFmax

 end
