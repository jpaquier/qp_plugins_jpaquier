 BEGIN_PROVIDER [complex*16, dirac_mo_mono_elec_integral,(2*(dirac_mo_tot_num),2*(dirac_mo_tot_num))]
  implicit none
  BEGIN_DOC
  !Array of the mono electronic hamiltonian on the MOs basis 
  ! obtained from the canonical orthonormalisation of the AOs 
  ! basis, in the 4x4 component formalism with cartesian basis 
  ! and the unrestricted kinetic-balance scheme  
  END_DOC
    call dirac_ao_to_mo(                                                     &
        dirac_ao_mono_elec_integral,                                         &
        size(dirac_ao_mono_elec_integral,1),                                 &
        dirac_mo_mono_elec_integral,                                         &
        size(dirac_mo_mono_elec_integral,1)                                  &
        )
 END_PROVIDER

 BEGIN_PROVIDER [double precision, eigenvalues_dirac_mono_elec_mo, (2*(dirac_mo_tot_num))]
 &BEGIN_PROVIDER [complex*16, eigenvectors_dirac_mono_elec_mo, (2*(dirac_mo_tot_num),2*(dirac_mo_tot_num))]
  implicit none
  BEGIN_DOC
  !The eigenvalues and eigenvectors of the mono electronic hamiltonian in the
  ! MOs basis
  END_DOC
  integer :: n,nmax,i,j
  double precision,allocatable :: eigenvalues(:)
  complex*16,allocatable       :: eigenvectors(:,:)
  allocate(eigenvalues(2*dirac_mo_tot_num),eigenvectors(2*dirac_mo_tot_num,2*dirac_mo_tot_num))
  n = 2*(dirac_mo_tot_num)
  nmax = n
  call lapack_diag_complex(eigenvalues,eigenvectors,dirac_mo_mono_elec_integral,nmax,n)
  do i = 1,2*dirac_mo_tot_num
   eigenvalues_dirac_mono_elec_mo(i) = eigenvalues(i) 
  enddo
  do j = 1, 2*dirac_mo_tot_num
   do i = 1, 2*dirac_mo_tot_num
     eigenvectors_dirac_mono_elec_mo(i,j) = eigenvectors(i,j)
   enddo
  enddo
 deallocate(eigenvalues,eigenvectors)
 END_PROVIDER
 
!BEGIN_PROVIDER [complex*16, eigenvectors_dirac_mono_elec_ao, (2*(dirac_mo_tot_num),2*(dirac_mo_tot_num))]
 BEGIN_PROVIDER [complex*16, eigenvectors_dirac_mono_elec_ao, (2*(dirac_ao_num),2*(dirac_mo_tot_num))]
  implicit none
  BEGIN_DOC
  !The eigenvectors in the AOs basis which does not diagonalize S
  END_DOC
  integer :: n,nmax
 !call zgemm('N','N', 2*(dirac_ao_num), 2*(dirac_mo_tot_num), 2*(dirac_ao_num),              &
  call zgemm('N','N', 2*(dirac_ao_num), 2*(dirac_mo_tot_num), 2*(dirac_mo_tot_num),              &
      (1.d0,0.d0), dirac_mo_coef_S,size(dirac_mo_coef_S,1),                                      &
      eigenvectors_dirac_mono_elec_mo, size(eigenvectors_dirac_mono_elec_mo,1),              &
      (0.d0,0.d0), eigenvectors_dirac_mono_elec_ao, size(eigenvectors_dirac_mono_elec_ao,1)) 
 END_PROVIDER



