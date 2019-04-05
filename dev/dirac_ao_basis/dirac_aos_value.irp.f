 subroutine give_all_two_dirac_aos_at_r(r,two_dirac_aos_array)
 implicit none
 BEGIN_DOC
 ! input : r == r(1) = x and so on
 ! two_dirac_aos_array(i) = two_dirac_aos(i) evaluated in r
 END_DOC
 double precision, intent(in) :: r(3)
 double precision :: two_dirac_aos_array_tmp(dirac_ao_num)
 complex*16, intent(out) ::  two_dirac_aos_array(2*dirac_ao_num)
 integer :: dirac_power_ao(3) 
 integer :: i,j,k,l,m
 double precision :: dx,dy,dz,r2
 double precision :: dx2,dy2,dz2
 double precision :: center_ao(3)
 double precision :: beta
 two_dirac_aos_array = (0.d0,0.d0) 
 do i = 1, nucl_num
  center_ao(1:3) = nucl_coord(i,1:3)
  dx = (r(1) - center_ao(1)) 
  dy = (r(2) - center_ao(2)) 
  dz = (r(3) - center_ao(3)) 
  r2 = dx*dx + dy*dy + dz*dz
  do j = 1,Nucl_N_dirac_Aos(i) 
   k = Nucl_dirac_Aos_transposed(j,i) ! index of the dirac ao in the ordered format 
   two_dirac_aos_array_tmp(k) = 0.d0
   dirac_power_ao(1:3)= dirac_ao_power_ordered_transp_per_nucl(1:3,j,i)
   dx2 = dx ** dirac_power_ao(1)
   dy2 = dy ** dirac_power_ao(2)
   dz2 = dz ** dirac_power_ao(3)
   do l = 1,dirac_ao_prim_num(k)
    beta = dirac_ao_expo_ordered_transp_per_nucl(l,j,i)
    two_dirac_aos_array_tmp(k)+= dirac_ao_coef_normalized_ordered_transp_per_nucl(l,j,i) * dexp(-beta*r2) 
   enddo
   two_dirac_aos_array_tmp(k) = two_dirac_aos_array_tmp(k) * dx2 * dy2 * dz2
  enddo
 enddo
 do m =1, dirac_ao_num
  if (m .le. large_ao_num) then
   two_dirac_aos_array(m) = (1.d0,0.d0)* two_dirac_aos_array_tmp(m)
   two_dirac_aos_array(m+large_ao_num)= (1.d0,0.d0)* two_dirac_aos_array_tmp(m)
  else
   two_dirac_aos_array(m+large_ao_num) = (1.d0,0.d0)* two_dirac_aos_array_tmp(m)
   two_dirac_aos_array(m+large_ao_num+small_ao_num)= (1.d0,0.d0)* two_dirac_aos_array_tmp(m)
  endif
 enddo
 end

 subroutine give_all_dirac_aos_at_r(r,dirac_aos_array)
 implicit none
 BEGIN_DOC
 ! input : r == r(1) = x and so on
 ! dirac_aos_array(i) = dirac_aos(i) evaluated in r
 END_DOC
 double precision, intent(in) :: r(3)
 double precision :: dirac_aos_array_tmp(dirac_ao_num)
 double precision, intent(out) ::  dirac_aos_array(dirac_ao_num)
 integer :: dirac_power_ao(3) 
 integer :: i,j,k,l,m
 double precision :: dx,dy,dz,r2
 double precision :: dx2,dy2,dz2
 double precision :: center_ao(3)
 double precision :: beta
 dirac_aos_array = 0.d0
 do i = 1, nucl_num
  center_ao(1:3) = nucl_coord(i,1:3)
  dx = (r(1) - center_ao(1)) 
  dy = (r(2) - center_ao(2)) 
  dz = (r(3) - center_ao(3)) 
  r2 = dx*dx + dy*dy + dz*dz
  do j = 1,Nucl_N_dirac_Aos(i) 
   k = Nucl_dirac_Aos_transposed(j,i) ! index of the dirac ao in the ordered format 
   dirac_aos_array_tmp(k) = 0.d0
   dirac_power_ao(1:3)= dirac_ao_power_ordered_transp_per_nucl(1:3,j,i)
   dx2 = dx ** dirac_power_ao(1)
   dy2 = dy ** dirac_power_ao(2)
   dz2 = dz ** dirac_power_ao(3)
   do l = 1,dirac_ao_prim_num(k)
    beta = dirac_ao_expo_ordered_transp_per_nucl(l,j,i)
    dirac_aos_array_tmp(k)+= dirac_ao_coef_normalized_ordered_transp_per_nucl(l,j,i) * dexp(-beta*r2) 
   enddo
   dirac_aos_array_tmp(k) = dirac_aos_array_tmp(k) * dx2 * dy2 * dz2
  enddo
 enddo
 do m =1, dirac_ao_num
  dirac_aos_array(m) = dirac_aos_array_tmp(m)
 enddo
 end

 subroutine give_all_large_aos_at_r(r,large_aos_array)
 implicit none
 BEGIN_DOC
 ! input : r == r(1) = x and so on
 ! large_aos_array(i) = large_aos(i) evaluated in r
 END_DOC
 double precision, intent(in) :: r(3)
 double precision :: dirac_aos_array_tmp(dirac_ao_num)
 complex*16, intent(out) ::  large_aos_array(large_ao_num)
 integer :: dirac_power_ao(3) 
 integer :: i,j,k,l,m
 double precision :: dx,dy,dz,r2
 double precision :: dx2,dy2,dz2
 double precision :: center_ao(3)
 double precision :: beta
 large_aos_array = (0.d0,0.d0) 
 do i = 1, nucl_num
  center_ao(1:3) = nucl_coord(i,1:3)
  dx = (r(1) - center_ao(1)) 
  dy = (r(2) - center_ao(2)) 
  dz = (r(3) - center_ao(3)) 
  r2 = dx*dx + dy*dy + dz*dz
  do j = 1,Nucl_N_dirac_Aos(i) 
   k = Nucl_dirac_Aos_transposed(j,i) ! index of the dirac ao in the ordered format 
   dirac_aos_array_tmp(k) = 0.d0
   dirac_power_ao(1:3)= dirac_ao_power_ordered_transp_per_nucl(1:3,j,i)
   dx2 = dx ** dirac_power_ao(1)
   dy2 = dy ** dirac_power_ao(2)
   dz2 = dz ** dirac_power_ao(3)
   do l = 1,dirac_ao_prim_num(k)
    beta = dirac_ao_expo_ordered_transp_per_nucl(l,j,i)
    dirac_aos_array_tmp(k)+= dirac_ao_coef_normalized_ordered_transp_per_nucl(l,j,i) * dexp(-beta*r2) 
   enddo
   dirac_aos_array_tmp(k) = dirac_aos_array_tmp(k) * dx2 * dy2 * dz2
  enddo
 enddo
 do m =1, large_ao_num
  large_aos_array(m) = (1.d0,0.d0)* dirac_aos_array_tmp(m)
 enddo
 end

 subroutine give_all_small_aos_at_r(r,small_aos_array)
 implicit none
 BEGIN_DOC
 ! input : r == r(1) = x and so on
 ! small_aos_array(i) = small_aos(i) evaluated in r
 END_DOC
 double precision, intent(in) :: r(3)
 double precision :: dirac_aos_array_tmp(dirac_ao_num)
 complex*16, intent(out) ::  small_aos_array(small_ao_num)
 integer :: dirac_power_ao(3) 
 integer :: i,j,k,l,m
 double precision :: dx,dy,dz,r2
 double precision :: dx2,dy2,dz2
 double precision :: center_ao(3)
 double precision :: beta
 small_aos_array = (0.d0,0.d0) 
 do i = 1, nucl_num
  center_ao(1:3) = nucl_coord(i,1:3)
  dx = (r(1) - center_ao(1)) 
  dy = (r(2) - center_ao(2)) 
  dz = (r(3) - center_ao(3)) 
  r2 = dx*dx + dy*dy + dz*dz
  do j = 1,Nucl_N_dirac_Aos(i) 
   k = Nucl_dirac_Aos_transposed(j,i) ! index of the dirac ao in the ordered format 
   dirac_aos_array_tmp(k) = 0.d0
   dirac_power_ao(1:3)= dirac_ao_power_ordered_transp_per_nucl(1:3,j,i)
   dx2 = dx ** dirac_power_ao(1)
   dy2 = dy ** dirac_power_ao(2)
   dz2 = dz ** dirac_power_ao(3)
   do l = 1,dirac_ao_prim_num(k)
    beta = dirac_ao_expo_ordered_transp_per_nucl(l,j,i)
    dirac_aos_array_tmp(k)+= dirac_ao_coef_normalized_ordered_transp_per_nucl(l,j,i) * dexp(-beta*r2) 
   enddo
   dirac_aos_array_tmp(k) = dirac_aos_array_tmp(k) * dx2 * dy2 * dz2
  enddo
 enddo
 do m =1, small_ao_num
   small_aos_array(m) = (1.d0,0.d0)* dirac_aos_array_tmp(large_ao_num+m)
 enddo
 end
