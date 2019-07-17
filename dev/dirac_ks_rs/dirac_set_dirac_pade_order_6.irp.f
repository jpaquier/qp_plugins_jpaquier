program dirac_set_dirac_pade_order_6
  BEGIN_DOC
  ! Set dirac_approximant to dirac_pade_order_6
  END_DOC

  call ezfio_set_dirac_dft_keywords_dirac_approximant("dirac_pade_order_6")
 TOUCH dirac_approximant

 end
