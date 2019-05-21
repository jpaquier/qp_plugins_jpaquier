 BEGIN_PROVIDER[complex*16, dirac_fock_matrix_ao, (2*dirac_ao_num,2*dirac_ao_num) ]
  implicit none
  BEGIN_DOC
  !Dirac Fock matrix in AO basis set 
  END_DOC
  integer                       ::i,j
  if (dirac_interaction == "Coulomb") then
   do j = 1,2*dirac_ao_num
    do i = 1,2*dirac_ao_num
      dirac_fock_matrix_ao(i,j) = dirac_Fock_matrix_C_ao(i,j)
    enddo
   enddo
  elseif (dirac_interaction == "Coulomb_Gaunt") then
   do j = 1,2*dirac_ao_num
    do i = 1,2*dirac_ao_num
     dirac_fock_matrix_ao(i,j) = dirac_Fock_matrix_C_G_ao(i,j)
    enddo
   enddo
  else
   print *,  'Unrecognized dirac_interaction : '//dirac_interaction
   stop 1
  endif
 END_PROVIDER

 BEGIN_PROVIDER[complex*16, eigenvectors_dirac_fock_matrix_ao, (2*dirac_mo_tot_num,2*dirac_mo_tot_num) ]
  implicit none
  BEGIN_DOC
  !Dirac Fock eigenvectors in AO basis set
  END_DOC
  integer                        ::i,j
  if (dirac_interaction == "Coulomb") then
   do j = 1,2*dirac_mo_tot_num
    do i = 1,2*dirac_mo_tot_num
     eigenvectors_dirac_fock_matrix_ao(i,j) = eigenvectors_dirac_Fock_matrix_C_ao(i,j)
    enddo
   enddo
  elseif (dirac_interaction == "Coulomb_Gaunt") then
   do j = 1,2*dirac_mo_tot_num
    do i = 1,2*dirac_mo_tot_num
     eigenvectors_dirac_fock_matrix_ao(i,j) = eigenvectors_dirac_Fock_matrix_C_G_ao(i,j)
    enddo
   enddo
  else
   print *,  'Unrecognized dirac_interaction : '//dirac_interaction
   stop 1
  endif
 END_PROVIDER

 BEGIN_PROVIDER[double precision, dirac_SCF_energy ]
  implicit none
  BEGIN_DOC
  !Dirac_SCF energy
  END_DOC
  integer                        ::i,j
  if (dirac_interaction == "Coulomb") then
   dirac_SCF_energy = dirac_SCF_C_energy
  elseif (dirac_interaction == "Coulomb_Gaunt") then
   dirac_SCF_energy = dirac_SCF_C_G_energy
  else
   print *,  'Unrecognized dirac_interaction : '//dirac_interaction
   stop 1
  endif
 END_PROVIDER

