 BEGIN_PROVIDER [ character*(128), even_nucl_type, (nucl_num)]
  implicit none
  BEGIN_DOC
  !type of the nucleus present, to create the even-tempered basis set
  END_DOC
 
 END_PROVIDER


 BEGIN_PROVIDER [ integer, even_Nucl_Aos, (nucl_num,even_N_AOs_max)]
 implicit none
 BEGIN_DOC
 ! List of |AOs| centered on each atom for the even_tempered basis-set
 END_DOC
 integer :: i
 integer, allocatable :: nucl_tmp(:)
 allocate(nucl_tmp(nucl_num))
 nucl_tmp = 0
 Nucl_Aos = 0
 do i = 1, ao_num
  nucl_tmp(ao_nucl(i))+=1
  Nucl_Aos(ao_nucl(i),nucl_tmp(ao_nucl(i))) = i
 enddo
 deallocate(nucl_tmp)
 END_PROVIDER


 BEGIN_PROVIDER [ integer, even_Nucl_num_l_type_Aos, (0:7, nucl_num)]
 &BEGIN_PROVIDER [ integer, even_large_ao_num ]
 &BEGIN_PROVIDER [ integer, even_large_ao_nucl, (even_large_ao_num)]
 &BEGIN_PROVIDER [ integer, even_Nucl_N_Aos, (nucl_num)]
 &BEGIN_PROVIDER [ integer, even_N_AOs_max ]  
 &BEGIN_PROVIDER [ integer, even_Nucl_Aos, (nucl_num,even_N_AOs_max)]
 &BEGIN_PROVIDER [ integer, even_large_ao_power, (even_large_ao_num,3)]
 &BEGIN_PROVIDER [ integer, even_large_ao_l, (even_large_ao_num) ]
 &BEGIN_PROVIDER [ integer, even_large_ao_l_max  ]
 &BEGIN_PROVIDER [ integer, even_Nucl_list_l_type_Aos, (even_large_ao_num,0:7,nucl_num)]
 implicit none
 BEGIN_DOC
 ! For the even tempered basis set 
 ! even_Nucl_num_l_type_Aos :: Number of l_type Aos per nucleus i
 ! even_large_ao_num :: Number of AOs of the large component
 ! even_large_ao_nucl :: Index of the nucleus on which the |AO| is centered
 ! even_Nucl_N_Aos :: Number of |AOs| per atom
 ! even_Nucl_Aos :: List of |AOs| centered on each atom
 ! even_Nucl_list_l_type_Aos :: List of the l_type Aos per nucleus i
 END_DOC
 integer :: i,j,k,l_type
 integer, allocatable :: nucl_num_count, even_large_ao_nucl_count 
 integer, allocatable :: even_large_ao_nucl_tmp(:)
 allocate(nucl_num_count,even_large_ao_nucl_count,even_large_ao_nucl_tmp(nucl_num))
 even_large_ao_nucl_tmp = 0

 even_Nucl_num_l_type_Aos = 0
 even_large_ao_num = 0
 do i = 1, nucl_num
 !do some test on the type of the nucleus i
   ! S type function
   even_Nucl_num_l_type_Aos(0,i)+=6
   ! P type function
   even_Nucl_num_l_type_Aos(1,i)+=1
   ! D type function
   even_Nucl_num_l_type_Aos(2,i)+=0
   ! F type function
   even_Nucl_num_l_type_Aos(3,i)+=0
   ! G type function
   even_Nucl_num_l_type_Aos(4,i)+=0
   ! H type function
   even_Nucl_num_l_type_Aos(5,i)+=0
   ! I type function
   even_Nucl_num_l_type_Aos(6,i)+=0
   ! J type function
   even_Nucl_num_l_type_Aos(7,i)+=0
 !enddo
  do l_type = 0,7
  even_large_ao_num += ((l_type+1)*(l_type+2)/2)*even_Nucl_num_l_type_Aos(l_type,i)
  even_large_ao_nucl_tmp (i)+= ((l_type+1)*(l_type+2)/2)*even_Nucl_num_l_type_Aos(l_type,i)
  enddo
 enddo

 nucl_num_count = 1
 even_large_ao_nucl_count = 0
 do j = 1, even_large_ao_num
  if (j .le. even_large_ao_nucl_tmp(nucl_num_count) + even_large_ao_nucl_count) then
   even_large_ao_nucl(j) = nucl_num_count
  else
   even_large_ao_nucl_count += even_large_ao_nucl_tmp(nucl_num_count)
   nucl_num_count += 1  
   ! Correct only if there are orbitals on each atoms
   even_large_ao_nucl(j) = nucl_num_count
  endif
 enddo

 even_Nucl_N_Aos = 0
 do i = 1, even_large_ao_num
  even_Nucl_N_Aos(even_large_ao_nucl(i)) +=1
 enddo
 even_N_AOs_max = maxval(even_Nucl_N_Aos)

 integer, allocatable :: nucl_tmp(:)
 allocate(nucl_tmp(nucl_num))
 nucl_tmp = 0
 even_Nucl_Aos = 0
 do i = 1, even_large_ao_num
  nucl_tmp(even_large_ao_nucl(i))+=1
  even_Nucl_Aos(even_large_ao_nucl(i),nucl_tmp(even_large_ao_nucl(i))) = i
 enddo
 deallocate(nucl_tmp)



 do i=1,even_large_ao_num
   even_large_ao_l(i) = even_large_ao_power(i,1) + even_large_ao_power(i,2) + even_large_ao_power(i,3)
 enddo
 even_large_ao_l_max = maxval(even_large_ao_l)

 
 
 even_Nucl_list_l_type_Aos = 0
 do i = 1, nucl_num
  do j = 1, even_Nucl_N_Aos(i)
   if(ao_l(Nucl_Aos(i,j))==0)then
   ! S type function
   Nucl_num_l_type_Aos(0,i)+=1
   Nucl_list_l_type_Aos(Nucl_num_l_type_Aos(0,i),0,i)=Nucl_Aos(i,j)
   elseif(ao_l(Nucl_Aos(i,j))==1)then
   ! P type function
    if(ao_power(Nucl_Aos(i,j),1)==1)then
     Nucl_num_l_type_Aos(1,i)+=1
     Nucl_list_l_type_Aos(Nucl_num_l_type_Aos(1,i),1,i)=Nucl_Aos(i,j)
    endif
   elseif(ao_l(Nucl_Aos(i,j))==2)then
   ! D type function
    if(ao_power(Nucl_Aos(i,j),1)==2)then
     Nucl_num_l_type_Aos(2,i)+=1
     Nucl_list_l_type_Aos(Nucl_num_l_type_Aos(2,i),2,i)=Nucl_Aos(i,j)
    endif
   elseif(ao_l(Nucl_Aos(i,j))==3)then
   ! F type function
    if(ao_power(Nucl_Aos(i,j),1)==3)then
     Nucl_num_l_type_Aos(3,i)+=1
     Nucl_list_l_type_Aos(Nucl_num_l_type_Aos(3,i),3,i)=Nucl_Aos(i,j)
    endif
   elseif(ao_l(Nucl_Aos(i,j))==4)then
   ! G type function
    if(ao_power(Nucl_Aos(i,j),1)==4)then
     Nucl_num_l_type_Aos(4,i)+=1
     Nucl_list_l_type_Aos(Nucl_num_l_type_Aos(4,i),4,i)=Nucl_Aos(i,j)
    endif
   elseif(ao_l(Nucl_Aos(i,j))==5)then
   ! H type function
    if(ao_power(Nucl_Aos(i,j),1)==5)then
     Nucl_num_l_type_Aos(5,i)+=1
     Nucl_list_l_type_Aos(Nucl_num_l_type_Aos(5,i),5,i)=Nucl_Aos(i,j)
    endif 
   elseif(ao_l(Nucl_Aos(i,j))==6)then
   ! I type function
    if(ao_power(Nucl_Aos(i,j),1)==6)then
     Nucl_num_l_type_Aos(6,i)+=1
     Nucl_list_l_type_Aos(Nucl_num_l_type_Aos(6,i),6,i)=Nucl_Aos(i,j)
    endif
   elseif(ao_l(Nucl_Aos(i,j))==7)then
   ! J type function
    if(ao_power(Nucl_Aos(i,j),1)==7)then
     Nucl_num_l_type_Aos(7,i)+=1
     Nucl_list_l_type_Aos(Nucl_num_l_type_Aos(7,i),7,i)=Nucl_Aos(i,j)
    endif
   endif
  enddo
 enddo
 END_PROVIDER
 
  BEGIN_PROVIDER [ integer, even_large_ao_prim_num_max ]
 implicit none
  BEGIN_DOC
  !max number of primitives of the large component for the decontracted
  !even-tempered basis set
  END_DOC
  even_large_ao_prim_num_max = 1
 END_PROVIDER

 BEGIN_PROVIDER [ integer, even_large_ao_prim_num_max ]
 implicit none
  BEGIN_DOC
  !max number of primitives of the large component for the decontracted
  !even-tempered basis set
  END_DOC
  even_large_ao_prim_num_max = 1
 END_PROVIDER

 BEGIN_PROVIDER [ double precision, even_large_ao_expo_ordered_transp,(large_ao_prim_num_max,even_large_ao_num) ]
 implicit none
 BEGIN_DOC
 do i = 1, nucl_num
  !Some test on the type of the nucleus i
  
 
 enddo
 END_PROVIDER


 BEGIN_PROVIDER [ integer, even_large_ao_nucl, (even_large_ao_num) ]
  implicit none
  BEGIN_DOC
  ! Index of the nucleus on which the |AO| is centered in the even tempered
  ! basis set
  END_DOC
 integer :: i,j,k
 ! Only one center for the moment
 even_large_ao_nucl = 1
 END_PROVIDER

 BEGIN_PROVIDER [ integer, even_Nucl_N_Aos, (nucl_num)]
 &BEGIN_PROVIDER [ integer, even_N_AOs_max ]
 implicit none
 BEGIN_DOC
 ! Number of |AOs| per atom for the even_tempered basis-set
 END_DOC
 integer :: i
 even_Nucl_N_Aos = 0
 do i = 1, even_large_ao_num
  even_Nucl_N_Aos(ao_nucl(i)) +=1
 enddo
 even_N_AOs_max = maxval(even_Nucl_N_Aos)
 END_PROVIDER

