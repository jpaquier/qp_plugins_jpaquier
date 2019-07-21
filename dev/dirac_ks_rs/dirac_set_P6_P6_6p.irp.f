program dirac_set_P6_6p_P6_6p
  BEGIN_DOC
  ! Set dirac_rgga to P6_P6_6p
  END_DOC

  call ezfio_set_dirac_dft_utils_one_e_dirac_rgga("P6_P6_6p")
 TOUCH dirac_rgga

 end
