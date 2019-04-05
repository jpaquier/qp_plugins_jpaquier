program dirac_mu_of_r_ints
  implicit none
  BEGIN_DOC
! TODO : Put the documentation of the program here
  END_DOC
 use map_module
 !integer :: i,j,k,l
 !do i = 1, 5
 ! do j = 1, 5
 !  do k = 1, 5
 !   do l = 1, 5
 !   prinf*,i,j,k,l, erf_mu_of_r_dirac_ao_test(i,j,k,l)
 !   enddo
 !  enddo
 ! enddo
 !enddo


 integer  :: i,j,k,l
 double precision :: accu,integral
 double precision, allocatable :: ints(:)
 double precision :: get_dirac_ao_bielec_integral_erf_mu_of_r
 allocate(ints(dirac_ao_num))
 accu = 0.d0
 do i = 1, dirac_ao_num
  do j = 1, dirac_ao_num
   do k = 1, dirac_ao_num
                                           ! 1 2 1
    call get_dirac_ao_bielec_ints_erf_mu_of_r(i,j,k,dirac_ao_num,ints)
    do l = 1, dirac_ao_num
     !                                             1 2 1 2
     integral = get_dirac_ao_bielec_integral_erf_mu_of_r(i,j,k,l,dirac_ao_ints_erf_mu_of_r_map)
     accu += dabs(integral - ints(l))
     print*,i,j,k,l,ints(l)
    enddo
   enddo
  enddo
 enddo
!accu = accu / dble(dirac_ao_num **4)
!print*,'accu = ',accu

end
