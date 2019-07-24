program dirac_set_r_x_0
  BEGIN_DOC
  ! Set dirac_r_x as 0
  END_DOC

  call ezfio_set_dirac_ao_basis_dirac_r_x(0.d0)
 TOUCH dirac_r_x

 end

