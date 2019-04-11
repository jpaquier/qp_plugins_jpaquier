 subroutine index_two_e_no_sym(i,j,k,l,n,i1)
  use map_module
  implicit none
 BEGIN_DOC
 ! symetric index for (i,k) and (j,l) but not between (i,j) and (k,l)
 ! n is the maximum value of the indices
 END_DOC
  integer, intent(in)            :: i,j,k,l
  integer, intent(in)            :: n
  integer(key_kind), intent(out) :: i1
  integer(key_kind)              :: p,q,r,s,i2
  p = min(i,k)
  r = max(i,k)
  p = p+ishft(r*r-r,-1)
  q = min(j,l)
  s = max(j,l)
  q = q+ishft(s*s-s,-1)
 !i1 = (p-1)*n*n + q

  i1 = min(p,q)
  i2 = max(p,q)
  i1 = i1+shiftr(i2*i2-i2,1)
 end

 
!subroutine two_e_integrals_index_reverse_no_sym(i,j,k,l,i1)
! use map_module
! implicit none
! integer, intent(out)           :: i(4),j(4),k(4),l(4)
! integer(key_kind), intent(in)  :: i1
! integer(key_kind)              :: i2,i3

! i = 0
! i2   = ceiling(0.5d0*(dsqrt(8.d0*dble(i1)+1.d0)-1.d0)) ! 8->4
! l(1) = ceiling(0.5d0*(dsqrt(8.d0*dble(i2)+1.d0)-1.d0))
! i3   = i1 - shiftr(i2*i2-i2,1)
! k(1) = ceiling(0.5d0*(dsqrt(8.d0*dble(i3)+1.d0)-1.d0))
! j(1) = int(i2 - shiftr(l(1)*l(1)-l(1),1),4) ! 4->2 
! i(1) = int(i3 - shiftr(k(1)*k(1)-k(1),1),4)

!             !1212
!             !ijkl
!             !1212
! i(2) = i(1) !ilkj
! j(2) = l(1)
! k(2) = k(1)
! l(2) = j(1)

!             !1212
! i(3) = k(1) !kjil
! j(3) = j(1)
! k(3) = i(1)
! l(3) = l(1)

!             !1212
! i(4) = k(1) !klij
! j(4) = l(1)
! k(4) = i(1)
! l(4) = j(1)

!!            !2121
!!i(5) = j(1) !jilk
!!j(5) = i(1)
!!k(5) = l(1)
!!l(5) = k(1)

!!            !2121
!!i(6) = j(1) !jkli
!!j(6) = k(1)
!!k(6) = l(1)
!!l(6) = i(1)

!!i(7) = l(1) !lijk
!!j(7) = i(1)
!!k(7) = j(1)
!!l(7) = k(1)

!!            !2121
!!i(8) = l(1) !lkji
!!j(8) = k(1)
!!k(8) = j(1)
!!l(8) = i(1)

! integer :: ii, jj
! do ii=2,4
!   do jj=1,ii-1
!     if ( (i(ii) == i(jj)).and. &
!          (j(ii) == j(jj)).and. &
!          (k(ii) == k(jj)).and. &
!          (l(ii) == l(jj)) ) then
!        i(ii) = 0
!        exit
!     endif
!   enddo
! enddo
! do ii=1,4
!   if (i(ii) /= 0) then
!     call index_two_e_no_sym(i(ii),j(ii),k(ii),l(ii),dirac_ao_num,i2)
!     !print*, i1, i2
!      if (i1 /= i2) then
!       print *,  i1, i2
!       print *,  i(ii), j(ii), k(ii), l(ii)
!       print*, "two_e_integrals_index_reverse failed"
!      !stop 'two_e_integrals_index_reverse failed'
!     endif
!   endif
! enddo
!end


