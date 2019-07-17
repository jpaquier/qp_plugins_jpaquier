program dirac_set_non_relativistic
  BEGIN_DOC
  ! Set dirac_approximant to non-relativistic
  END_DOC

  call ezfio_set_dirac_dft_keywords_dirac_approximant("non-relativistic")
 TOUCH dirac_approximant

 end
