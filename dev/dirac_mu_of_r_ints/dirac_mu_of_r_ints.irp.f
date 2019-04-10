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


!integer  :: i,j,k,l
!double precision :: accu,integral,integral_2
!double precision, allocatable :: ints(:)
!double precision :: get_dirac_ao_bielec_integral_erf_mu_of_r
!double precision :: get_dirac_ao_bielec_integral_erf
!allocate(ints(dirac_ao_num))
!accu = 0.d0
!do i = 1, dirac_ao_num
! do j = 1, dirac_ao_num
!  do k = 1, dirac_ao_num
!   do l = 1, dirac_ao_num
!    !                                                   1 2 1 2
!    integral = get_dirac_ao_bielec_integral_erf_mu_of_r(i,j,k,l,dirac_ao_ints_erf_mu_of_r_map)
!    !                                             1 2 1 2
!    integral_2 = get_dirac_ao_bielec_integral_erf(i,j,k,l,dirac_ao_integrals_erf_map)
!    if ( integral .gt. 0.d0 .or. integral_2 .gt. 0.d0) then
!     write(33,*) ,i,j,k,l, integral_2
!     write(34,*) ,i,j,k,l, integral
!    endif 
!    accu += dabs(integral - integral_2)
!   enddo
!  enddo
! enddo
!enddo
!accu = accu / dble(dirac_ao_num **4)
!print*,'accu = ',accu
 
!print*,index_two_e_test
!print*,index_two_e_no_sym_test
 
 integer :: N
 integer :: i,j,k,l,a,b
 integer :: ii(4), jj(4), kk(4), ll(4),test(4),k2
  do N = 1, 10
  test = 0
  call two_e_integrals_index_reverse_no_sym(kk,ii,ll,jj,N)
  do a=2,4
   do b=1,I-1
    if ( (ii(b) == ii(a)).and. &
         (jj(b) == jj(a)).and. &
         (kk(b) == kk(a)).and. &
         (ll(b) == ll(a)) ) then
         test(a) += 1
    endif
   enddo
  enddo
  do k2=1,4
   i = ii(k2) ! electron 1
   j = jj(k2) ! electron 1
   k = kk(k2) ! electron 2
   l = ll(k2) ! electron 2
  !if (test(k2) == 0 ) then
    print*, k2,test(k2),i,j,k,l
    print*,"*******************************"
  !endif
  enddo
 enddo

!integer :: r
!do r =1,20
!!print*, r*r-r, ishft(r*r-r,-1), shiftr(r*r-r,1)
! print*, r,ceiling(0.5d0*(dsqrt(8.d0*dble(r)+1.d0)-1.d0)),ceiling(0.5d0*(dsqrt(4.d0*dble(r)+1.d0)-1.d0))
!enddo

end
