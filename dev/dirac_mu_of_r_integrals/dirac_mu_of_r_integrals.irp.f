program dirac_mu_of_r_ints
  implicit none
  BEGIN_DOC
! TODO : Put the documentation of the program here
  END_DOC
 use map_module
 !integer :: i,j,k,l
! do i = 15, 20
!  do j = 15, 20
!   do k = 15, 20
!    do l = 15, 20
!    print*,i,j,k,l, erf_mu_of_r_dirac_ao_test(i,j,k,l)
!    enddo
!   enddo
!  enddo
! enddo


 integer  :: i,j,k,l
 double precision :: accu,integral,integral_2
 double precision, allocatable :: ints(:)
 double precision :: get_dirac_ao_bielec_integral_erf_mu_of_r
 double precision :: get_dirac_ao_bielec_integral_erf
 allocate(ints(dirac_ao_num))
 accu = 0.d0
 do i = 1, 20
  do j = 1, 20
   do k = 1, 20
     !                                        1 2 1
    call get_dirac_ao_bielec_ints_erf_mu_of_r(i,j,k,dirac_ao_num,ints)
    do l = 1, 20
     !                                                   1 2 1 2
     integral = get_dirac_ao_bielec_integral_erf_mu_of_r(i,j,k,l,dirac_ao_ints_erf_mu_of_r_map)
     integral_2 = get_dirac_ao_bielec_integral_erf(i,j,k,l,dirac_ao_integrals_erf_map)
     if(dabs(integral - integral_2).gt.1.d-6)then
      print*,i,j,k,l
      print*,dabs(integral - integral_2)
      print*, integral , integral_2 
      pause
     endif
     accu += dabs(integral - integral_2)
!    print*,i,j,k,l,integral
    enddo
   enddo
  enddo
 enddo
 accu = accu / dble(dirac_ao_num **4)
 print*,'accu = ',accu

end
