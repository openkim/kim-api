!                                                                      
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
! All rights reserved.                                                 
!                                                                     
! Author: Valeriu Smirichinski                                         
!


module neighborlistmod

type neighborlist_object
	integer :: atomnumber
	integer,pointer::binatom(:,:)
	integer, pointer::dokneighbors(:,:)
	integer :: dokmaximumsize=0,dokincrementsize=10000000,dokcurrentsize=0, pointsto=0
end type neighborlist_object



interface
	subroutine qsort(base,nel,width,comparator)
		use iso_c_binding
		implicit none
		type(C_PTR), value :: base
		type(C_FUNPTR),value :: comparator
        	integer (c_int), value :: nel,width
	end subroutine qsort
	function bsearch(key,base,nel,width,comparator)
		use iso_c_binding
		implicit none
		type(C_PTR), value :: key, base
		type(C_FUNPTR),value :: comparator
        	integer (c_int), value :: nel,width
		integer (c_int)::bsearch
	end function bsearch
	

end interface
contains
        function comparatorr2int(a,b) bind(c)
        	use iso_c_binding
        	implicit none
        	integer (c_int) :: comparatorr2int
                TYPE (C_PTR), value :: a,b
                integer(c_int),pointer::aa(:),bb(:)
                integer shp(1), retval
                shp(1)=4
                call c_f_pointer(a,aa,shp)
                call c_f_pointer(b,bb,shp)
                if (aa(2).ne.bb(2))then 
			retval = aa(2)-bb(2)
                else if (aa(3).ne.bb(3)) then 
			retval = aa(3)-bb(3)
                
		else
			retval = aa(4)-bb(4)
		end if
		comparatorr2int = retval
        end function comparatorr2int

	function comparatordokint(a,b) bind(c)
		use iso_c_binding
        	implicit none
		integer (c_int) ::comparatordokint 
                TYPE (C_PTR), value :: a,b
		integer(c_int),pointer::aa(:),bb(:)
                integer shp(1), retval
                shp(1)=2
                call c_f_pointer(a,aa,shp)
                call c_f_pointer(b,bb,shp)
		if (aa(1).ne.bb(1))then 
			retval = aa(1)-bb(1)
		else
			retval = aa(2)-bb(2)
		end if
		comparatordokint = retval
	end function comparatordokint
 	
	subroutine reallocatedok(neigh_obj,newsize)
                implicit none

		integer newsize,shp
		integer, allocatable :: tmp(:,:)

	        type (neighborlist_object) :: neigh_obj
               
	        if (neigh_obj%dokmaximumsize.le.0) then
			allocate(neigh_obj%dokneighbors(2,newsize))
			neigh_obj%dokmaximumsize = newsize
			return
		end if

		allocate(tmp(2,neigh_obj%dokcurrentsize))
		tmp(:,:) = neigh_obj%dokneighbors(:,1:neigh_obj%dokcurrentsize)
		deallocate(neigh_obj%dokneighbors)
		allocate(neigh_obj%dokneighbors(2,newsize))
                if (newsize .ge. neigh_obj%dokmaximumsize) then
			neigh_obj%dokneighbors(:,1:neigh_obj%dokcurrentsize) = tmp(:,:)
		else
			neigh_obj%dokneighbors(:,1:newsize) = tmp(:,:)
			neigh_obj%dokcurrentsize = newsize
		end if
                
		neigh_obj%dokmaximumsize = newsize
                        
		deallocate(tmp)
	end subroutine reallocatedok

	subroutine add2dok(neigh_obj,a)
		integer a(2)
                type (neighborlist_object) :: neigh_obj
	
		if (neigh_obj%dokcurrentsize + 1.le.neigh_obj%dokmaximumsize) then
			neigh_obj%dokcurrentsize = neigh_obj%dokcurrentsize + 1
			neigh_obj%dokneighbors(:,neigh_obj%dokcurrentsize) = a(:)
		else
						
			call reallocatedok(neigh_obj,neigh_obj%dokmaximumsize + neigh_obj%dokincrementsize)
			neigh_obj%dokcurrentsize = neigh_obj%dokcurrentsize + 1
			neigh_obj%dokneighbors(:,neigh_obj%dokcurrentsize) = a(:)
		end if
	end subroutine add2dok
	
	subroutine dokcorrectindex(neigh_obj)
		implicit none
		integer :: ind,i
		type (neighborlist_object) :: neigh_obj
         
		do i=1,neigh_obj%dokcurrentsize
			if (neigh_obj%dokneighbors(1,i).gt.neigh_obj%dokneighbors(2,i)) then
				ind = neigh_obj%dokneighbors(1,i) ;neigh_obj%dokneighbors(1,i) = neigh_obj%dokneighbors(2,i); neigh_obj%dokneighbors(2,i) = ind
			end if
		end do
	end subroutine dokcorrectindex

        subroutine shrinkdok(neigh_obj)
		use iso_c_binding
                implicit none
                type (neighborlist_object), target :: neigh_obj
                integer :: i, ind
		if(neigh_obj%dokcurrentsize.eq.0) return
		call qsort(c_loc(neigh_obj%dokneighbors(1,1)),neigh_obj%dokcurrentsize,8,c_funloc(comparatordokint))
		ind = 1
               do i=2,neigh_obj%dokcurrentsize
			if (comparatordokint(c_loc(neigh_obj%dokneighbors(1,ind)),c_loc(neigh_obj%dokneighbors(1,i))).lt.0) then
				ind = ind+1
				neigh_obj%dokneighbors(:,ind) = neigh_obj%dokneighbors(:,i)
			else if (comparatordokint(c_loc(neigh_obj%dokneighbors(1,ind)),c_loc(neigh_obj%dokneighbors(1,i))).gt.0) then
				print *,"list dokneighbors is not sorted!"
				stop 
			end if
		end do
		neigh_obj%dokcurrentsize = ind
			
		call reallocatedok(neigh_obj,neigh_obj%dokcurrentsize)
			
	end subroutine shrinkdok

       subroutine neifree(neigh_obj)
		implicit none
                type (neighborlist_object) :: neigh_obj
		
		if (neigh_obj%dokmaximumsize .gt.0) deallocate(neigh_obj%dokneighbors)
		neigh_obj%dokmaximumsize = 0
		neigh_obj%dokcurrentsize= 0
       end subroutine neifree


	subroutine qsortr2int(a)
		use iso_c_binding
                implicit none
		!integer,target,allocatable, dimension(:,:) :: a
		integer,pointer,dimension(:,:) :: a
                integer shp(2)
                shp=shape(a)

                call qsort(c_loc(a(1,1)),shp(2),16,c_funloc(comparatorr2int)) 

        end subroutine qsortr2int
	
	! the folloiwing subroutine gives all numbers of atom in bean ijk
        ! maximum namber of attom in bin no more then 500
	function allatomsinbin(i,j,k,a,alln)
		use iso_c_binding
                implicit none
  		integer,pointer, dimension(:,:) :: a; 

  		integer :: i,j,k,ind,indnext,m, allatomsinbin
  		integer :: alln(500), shp(2)
                integer (c_int), target :: key(4),key2(4)
		
                shp=shape(a)
                key(1)=1;key(2)=i;key(3)=j;key(4)=k
                ind = bsearch(c_loc(key),c_loc(a(1,1)),shp(2),16,c_funloc(comparatorr2int))

       

                if (ind.le.0) then 
			allatomsinbin=0
		else 
			indnext=ind;
                        alln(1)=a(1,ind)
			do m = ind+1,shp(2)
				
				if ( comparatorr2int(c_loc(a(1,ind)),c_loc(a(1,m))).ne.0) goto 22
				alln(m - ind + 1) = a(1,m)
				indnext = m

			end do
			22 continue
			allatomsinbin = indnext - ind +1
		end if
        end function allatomsinbin
	
	! calculates neighbor list from two groups of atom 
	! (from bin {i,j,k} and bin {ii,jj,kk}
	
	subroutine neighborsfrom2groups(neigh_obj,i,j,k,ii,jj,kk,x3,fcut)
		implicit none
		integer ::i,j,k,ii,jj,kk, nei(2)
		real*8::fcut
		real*8,pointer :: x3(:,:)
		integer::group1(500), group2(500),groupnum1,groupnum2, m,n, a1,a2
                type (neighborlist_object) :: neigh_obj
		real*8 :: v(3),fcut2,norm2
                
                
                fcut2=fcut*fcut
       
          	groupnum1=allatomsinbin(i,j,k,neigh_obj%binatom,group1)
		groupnum2=allatomsinbin(ii,jj,kk,neigh_obj%binatom,group2)
                if ((i.eq.ii).and.(j.eq.jj).and.(k.eq.kk)) then
			do m=1, groupnum1; do n=m+1, groupnum2  
					a1=group1(m);  a2=group2(n)
					v(:) = x3(:,a1) - x3(:,a2)
					norm2=v(1)*v(1) + v(2)*v(2) + v(3)*v(3)
					if ((norm2.le.fcut2)) then
						nei(1)=a1 ;  nei(2) = a2
	       
						call add2dok(neigh_obj,nei)
					end if
			end do;     	   end do
		else
			do m=1, groupnum1; do n=1, groupnum2  
					a1=group1(m);  a2=group2(n)
					v(:) = x3(:,a1) - x3(:,a2)
					norm2=v(1)*v(1) + v(2)*v(2) + v(3)*v(3)
					if ((norm2.le.fcut2).and.(a1.ne.a2)) then
						nei(1)=a1 ;  nei(2) = a2
	       
						call add2dok(neigh_obj,nei)
					end if
			end do;     	   end do
		end if 	 
	end subroutine neighborsfrom2groups

	subroutine neighborcoolistdomaindec(neigh_obj,x3,fcut)
		implicit none
		type (neighborlist_object) :: neigh_obj
		real*8, pointer,dimension(:,:) :: x3
                integer :: natomb,shpx3(2),i,j,k,nx,ny,nz,nk,m
		real*8 :: fcut,xmin,xmax,ymin,ymax,zmin,zmax,lx,ly,lz, lmax, eps
	
		shpx3=shape(x3)
                
               
		allocate(neigh_obj%binatom(4,shpx3(2)))

		!finding min max coordinate
		xmin=x3(1,1);xmax=x3(1,1); ymin=x3(2,1);ymax=x3(2,1); zmin=x3(3,1);zmax=x3(3,1);
                do i=2, shpx3(2)
			xmin=min(xmin,x3(1,i)); xmax=max(xmax,x3(1,i));
                       	ymin=min(ymin,x3(2,i)); ymax=max(ymax,x3(2,i));
			zmin=min(zmin,x3(3,i)); zmax=max(zmax,x3(3,i));
		end do
		eps=0.03*fcut
		!maximum bin numbers
                nx = max(1,ceiling((xmax+eps-xmin)/fcut))
		ny = max(1,ceiling((ymax+eps-ymin)/fcut))
		nz = max(1,ceiling((zmax+eps-zmin)/fcut))
		            

		!populating bins with atoms
		do m=1, shpx3(2)
			i=floor( (x3(1,m)-xmin)/fcut + 1); j=floor( (x3(2,m)-ymin)/fcut + 1); k=floor( (x3(3,m)-zmin)/fcut + 1)
			neigh_obj%binatom(1,m)=m; neigh_obj%binatom(2,m)=i; neigh_obj%binatom(3,m)=j; neigh_obj%binatom(4,m)=k
		end do		
		!sort by ijk
	
		call qsortr2int(neigh_obj%binatom)
                		
		!calculate neighbor list COO format
		!first within each domain

		do i=1,nx; do j=1,ny; do k=1,nz
                       
			call neighborsfrom2groups(neigh_obj,i,j,k,i,j,k,x3,fcut)
		end do;    end do;    end do

                !call dokcorrectindex !in COO place {i,j} such as i<=j

 
                !call shrinkdok   !remove repeating elements and sort

 
		! second cross bin elements calculate
		! x-side              (1) X 4
                do i = 1, nx-1;  do j= 1, ny;  do k= 1, nz
			call neighborsfrom2groups(neigh_obj,i,j,k, i+1,j,k, x3,fcut); 
		end do; end do; end do
                ! y-side                              (2) X 4
		do i = 1, nx;  do j= 1, ny-1;  do k= 1, nz
			call neighborsfrom2groups(neigh_obj,i,j,k, i,j+1,k, x3,fcut); 
		end do;          end do;       end do
		! z-side                              (3) X 4
		do i = 1, nx;  do j= 1, ny;  do k= 1, nz-1
			call neighborsfrom2groups(neigh_obj,i,j,k, i,j,k+1, x3,fcut); 
		end do;          end do;       end do

		! (000-xy0) & (x00-0y0)-diagonal         (4) X 2 X 2
		do i = 1, nx-1;  do j= 1, ny-1;  do k= 1, nz
			call neighborsfrom2groups(neigh_obj,i,j,k, i+1,j+1,k, x3,fcut);
			call neighborsfrom2groups(neigh_obj,i+1,j,k, i,j+1,k, x3,fcut); 
		end do;          end do;       end do
		! (000-x0z) & (x00-00z)-diagonal         (5) X 2 X 2
		do i = 1, nx-1;  do j= 1, ny;  do k= 1, nz-1
			call neighborsfrom2groups(neigh_obj,i,j,k, i+1,j,k+1, x3,fcut);
			call neighborsfrom2groups(neigh_obj,i+1,j,k, i,j,k+1, x3,fcut);
		end do;          end do;       end do
		! (000-0yz) & (0y0-00z)-diagonal         (6) X 2 X 2
		do i = 1, nx;  do j= 1, ny-1;  do k= 1, nz-1
			call neighborsfrom2groups(neigh_obj,i,j,k, i,j+1,k+1, x3,fcut); 
                        call neighborsfrom2groups(neigh_obj,i,j+1,k, i,j,k+1, x3,fcut);
		end do;          end do;       end do

                ! (000-xyz)-diagonal  (7) 
		! (0yz-x00)
		! (0y0-x0z)
		! (00z-xy0)
		do i = 1, nx-1;  do j= 1, ny-1;  do k= 1, nz-1
			call neighborsfrom2groups(neigh_obj,i,j,k, i+1,j+1,k+1, x3,fcut);
			call neighborsfrom2groups(neigh_obj,i,j+1,k+1, i+1,j,k, x3,fcut);
			call neighborsfrom2groups(neigh_obj,i,j+1,k, i+1,j,k+1, x3,fcut);
			call neighborsfrom2groups(neigh_obj,i,j,k+1, i+1,j+1,k, x3,fcut);
		end do;          end do;       end do

                call dokcorrectindex(neigh_obj)!in COO place {i,j} such as i<=j

 
  print *," dokmaximumsize =", neigh_obj%dokmaximumsize, "dokcurrentsize = ",neigh_obj%dokcurrentsize
                
               
 	       	

                call shrinkdok (neigh_obj)  !remove repeating elements and sort
                !neighbor list is ready in COO format {{n,m},...}
                
               
 print *," dokmaximumsize =", neigh_obj%dokmaximumsize, "dokcurrentsize = ",neigh_obj%dokcurrentsize		
		deallocate(neigh_obj%binatom)
	end subroutine neighborcoolistdomaindec
	 
	
        subroutine neighiterator2begining(neigh_obj)
		implicit none
		type (neighborlist_object),pointer :: neigh_obj
		neigh_obj%pointsto = 1	
		if (neigh_obj%dokcurrentsize.lt.1) then
				 neigh_obj%pointsto = -1
		endif 
	end subroutine neighiterator2begining

        function neighiterator_next(neigh_obj,neighborsofanatom)
		
                implicit none
		type (neighborlist_object),pointer :: neigh_obj
                integer neighiterator_next
                integer, pointer::neighborsofanatom(:)
   		! {sizeofarray_NumNeigh+2,atomID, first_neighborID,    second_neighborID,...,NumNeighth_neighborID}		
		integer cnt, i,j;
		
 		
		if (neigh_obj%dokcurrentsize.lt.1) then
				neighiterator_next = -1
				return
		endif
		if (neigh_obj%pointsto.lt.1) then
				neighiterator_next = -1
				return
		endif
		

		i = neigh_obj%dokneighbors( 1,neigh_obj%pointsto)
		j = neigh_obj%dokneighbors( 2,neigh_obj%pointsto)
		neighborsofanatom(2) = i
		neighborsofanatom(3) = j
                cnt = 3
		do while ((i.eq.neigh_obj%dokneighbors(1,neigh_obj%pointsto + 1)) .and. ((neigh_obj%pointsto + 1).le. neigh_obj%dokcurrentsize)) 
			neigh_obj%pointsto = neigh_obj%pointsto + 1
			cnt = cnt+1
			neighborsofanatom(cnt) = neigh_obj%dokneighbors(2,neigh_obj%pointsto)
		end do
		

                neigh_obj%pointsto = neigh_obj%pointsto + 1

		neighborsofanatom(1)=cnt
		

                if (neigh_obj%pointsto.eq.neigh_obj%dokcurrentsize+1) then
			neigh_obj%pointsto = 1
			
			neighiterator_next = -1
		else
			neighiterator_next = cnt-2
		endif
            
        end function neighiterator_next
!c style interface:  int neighiterator_nextC(void **,void ** ) (checked)

function neighiterator_nextC(pnei_obj,pnei1atom) bind(c)
	
	use iso_c_binding
	implicit none
	type (c_ptr) :: pnei_obj,pnei1atom
	integer(c_int) :: neighiterator_nextC
	type (neighborlist_object),pointer :: neigh_obj
        integer, pointer::neighborsofanatom(:)
	call c_f_pointer(pnei_obj,neigh_obj)
	call c_f_pointer(pnei1atom,neighborsofanatom,[512])
	neighiterator_nextC = neighiterator_next(neigh_obj,neighborsofanatom)
end function neighiterator_nextC

subroutine neighiterator(pnei_obj,pnei1atom,numnei,restart)
	
	use KIMservice
	use iso_c_binding
	implicit none
	integer :: numnei,restart
	type (neighborlist_object),pointer :: nei_obj; !pointer(pnei_obj,nei_obj)
	integer(kind=kim_intptr),pointer:: pnei_obj
	integer :: nei1atom(1); pointer(pnei1atom,nei1atom)
	integer, pointer:: neisofanatom(:)
	type (c_ptr) :: ppnei_obj
	integer(kind=kim_intptr) ::tmp ; pointer(ptmp,tmp)
	
	ppnei_obj = c_loc( pnei_obj)
	call c_f_pointer(ppnei_obj,nei_obj)

	call toIntegerArrayWithDescriptor1d(nei1atom,neisofanatom,512)

	if (restart.eq.0) then
		call neighiterator2begining(nei_obj)
	else
		numnei = neighiterator_next(nei_obj,neisofanatom)	
	end if

end subroutine neighiterator

end module neighborlistmod




!c style interface 2003 format: void neighiterator2begining( void **) (not debugged) 
subroutine neighiterator2beginingC(pnei_obj)
	use neighborlistmod
	use iso_c_binding
	implicit none
	type (c_ptr) :: pnei_obj
	type (neighborlist_object),pointer :: neigh_obj
	call c_f_pointer(pnei_obj,neigh_obj)
	call neighiterator2begining(neigh_obj)
end subroutine neighiterator2beginingC

subroutine neighobj_allocate(pnei_obj)
	use neighborlistmod
	use iso_c_binding
	implicit none
	type (c_ptr) :: pnei_obj,px
	type (neighborlist_object),pointer :: neigh_obj
	allocate (neigh_obj)
	pnei_obj = c_loc(neigh_obj)
end subroutine neighobj_allocate

subroutine neighobj_deallocate(pnei_obj)
	use neighborlistmod
	use iso_c_binding
	implicit none
	type (c_ptr) :: pnei_obj,px
	type (neighborlist_object),pointer :: neigh_obj
	call c_f_pointer(pnei_obj,neigh_obj)
	call neifree(neigh_obj)
	deallocate(neigh_obj)
end subroutine neighobj_deallocate

subroutine neighborscalculate(pnei_obj,px,n,cut)
	use neighborlistmod
	use iso_c_binding
	implicit none
	integer:: n
	real*8 ::cut
	real*8,pointer::x(:,:)
	type (c_ptr) :: pnei_obj,px
	type (neighborlist_object),pointer :: neigh_obj

	call c_f_pointer(pnei_obj,neigh_obj)

	call c_f_pointer(px,x,[3,n])

	call neighborcoolistdomaindec(neigh_obj,x,cut)

end subroutine neighborscalculate

integer(kind=kim_intptr) function get_neigh_iterator()
	use KIMservice
	use neighborlistmod
	implicit none
	get_neigh_iterator = loc(neighiterator)
end function get_neigh_iterator

