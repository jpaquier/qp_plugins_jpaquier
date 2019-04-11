program dirac_mu_of_r_ints
  implicit none
  BEGIN_DOC
! TODO : Put the documentation of the program here
  END_DOC
 use map_module

 integer  :: i,j,k,l
 double precision :: accu,integral,integral_1,integral_2,integral_analytic
 double precision :: get_dirac_ao_bielec_integral_erf_mu_of_r
 double precision :: get_dirac_ao_bielec_integral_erf_mu_of_r_1
 double precision :: get_dirac_ao_bielec_integral_erf_mu_of_r_2
 double precision :: get_dirac_ao_bielec_integral_erf
 accu = 0.d0
!do i = 1, ao_num
! do j = 1, ao_num
!  do k = 1, ao_num
!   do l = 1, ao_num
!do i = large_ao_num+1, dirac_ao_num
! do j = large_ao_num+1, dirac_ao_num
!  do k = large_ao_num+1, dirac_ao_num
!   do l = large_ao_num+1, dirac_ao_num
 do i = 1, dirac_ao_num
  do j = 1, dirac_ao_num
   write(30,*),i,j,dirac_ao_overlap_abs(i,j)
   do k = 1, dirac_ao_num
    do l = 1, dirac_ao_num
     if (dirac_ao_overlap_abs(i,k)*dirac_ao_overlap_abs(j,l) < dirac_ao_integrals_threshold ) then
      write(29,*),i,j,k,l, dirac_ao_overlap_abs(i,k), dirac_ao_overlap_abs(j,l)
     endif    
     !                                                   1 2 1 2
     integral = get_dirac_ao_bielec_integral_erf_mu_of_r(i,j,k,l,dirac_ao_ints_erf_mu_of_r_map)
    !integral_1 = get_dirac_ao_bielec_integral_erf_mu_of_r_1(i,j,k,l,dirac_ao_ints_erf_mu_of_r_map)
    !integral_2 = get_dirac_ao_bielec_integral_erf_mu_of_r_2(i,j,k,l,dirac_ao_ints_erf_mu_of_r_map)
     !                                             1 2 1 2
     integral_analytic = get_dirac_ao_bielec_integral_erf(i,j,k,l,dirac_ao_integrals_erf_map)
     if ( integral .gt. 0.d0 .or. integral_analytic .gt. 0.d0) then
      write(33,*) ,i,j,k,l, integral_analytic
      write(34,*) ,i,j,k,l, integral
      if ( integral == 0 .or. integral_analytic ==0 ) then
       write(32,*),i,j,k,l, integral,integral_analytic
      endif
     endif 
     accu += dabs(integral - integral_analytic)
    enddo
   enddo
  enddo
 enddo
 accu = accu / dble(dirac_ao_num **4)
 print*,'accu = ',accu
 
 
!integer :: i1
!integer :: i,j,k,l,a,b
!integer :: ii(4), jj(4), kk(4), ll(4),k2
! do i1 = 1, 10000
! call two_e_integrals_index_reverse_no_sym(ii,jj,kk,ll,i1)
! print*,"*******************"
! print*, "i1=",i1 
! do k2=1,4
!  i = ii(k2) ! electron 1
!  j = jj(k2) ! electron 2
!  k = kk(k2) ! electron 1
!  l = ll(k2) ! electron 2
!  if (ii(k2)==0) then
!   cycle
!  endif
!  print*, "           ",i,j,k,l
!   print*,"********"
! enddo
!enddo

!integer :: i1
!integer :: i,j,k,l,a,b
!integer :: ii(8), jj(8), kk(8), ll(8),k2
! do i1 = 10000, 10010
!  call two_e_integrals_index_reverse(ii,jj,kk,ll,i1)
! print*,"*******************"
! print*, "i1=",i1 
!  do k2=1,8
!   if (ii(k2)==0) then
!     cycle
!   endif
!  i = ii(k2) ! electron 1
!  j = jj(k2) ! electron 1
!  k = kk(k2) ! electron 2
!  l = ll(k2) ! electron 2
!  print*, "           ",i,j,k,l
!  print*,"********"
! enddo
!enddo


end
