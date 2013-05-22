!
! CDDL HEADER START
!
! The contents of this file are subject to the terms of the Common Development
! and Distribution License Version 1.0 (the "License").
!
! You can obtain a copy of the license at
! http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
! specific language governing permissions and limitations under the License.
!
! When distributing Covered Code, include this CDDL HEADER in each file and
! include the License file in a prominent location with the name LICENSE.CDDL.
! If applicable, add the following below this CDDL HEADER, with the fields
! enclosed by brackets "[]" replaced with your own identifying information:
!
! Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
!
! CDDL HEADER END
!

!
! Copyright (c) 2012, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Valeriu Smirichinski
!    Yeranuhi Hakobyan
!

!
! Release: This file is part of the openkim-api.git repository.
!


module neighborlistmod
use iso_c_binding
use KIM_API
implicit none

type neighborlist_object
	integer :: atomnumber
	integer,pointer::binatom(:,:)
	integer, pointer::dokneighbors(:,:)
	integer :: dokmaximumsize=0,dokincrementsize=10000000,dokcurrentsize=0, pointsto=0
	!below is the addition for fast access (secondary format) usefull for neighborlist locator
	integer, pointer::numnei(:) !number of neighbors per atom  (size of atomnumber)
	integer, pointer::offset(:) ! offset index in the main format
	integer,pointer::neilist(:) !  neilist( offset(i) : offset(i) + numnei(i))-1 -- gives list of all neighbors 
				      ! of an atom i (size of dokcurrentsize)
        real*8,pointer::x(:,:)         !atoms position holder
	real*8 ::dx(3,1024)		! position difference vector for neighbors of an atom		
end type neighborlist_object

type neighborlist_object_both
	type (neighborlist_object),pointer :: half,full
end type neighborlist_object_both

        ! kim indexes
	!integer :: kim_get_half_neigh_index, kim_get_full_neigh_index, kim_neighObj_index, kim_coordinates_index
	integer :: kim_neighObj_index
interface
	subroutine qsort(base,nel,width,comparator)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
		integer (kind=kim_intptr) :: base,comparator; 
		integer::nel,width
	end subroutine qsort

	function bsearch(key,base,nel,width,comparator)
#ifdef SYSTEM32
	integer, parameter :: kim_intptr=4
#else
	integer,parameter :: kim_intptr = 8
#endif
		integer(kind=kim_intptr) :: key, base, comparator
		integer :: nel,width,bsearch
	end function bsearch

end interface

contains
        function comparatorr2int(a,b)
        	implicit none
		integer ::a,b
		integer::aa(1:4),bb(1:4); pointer(pa,aa); pointer(pb,bb)
		integer ::retval,comparatorr2int
		integer(kind=kim_intptr)::ia,ib
		
 		ia = loc(a); ib=loc(b) ! mimick passing pointer by value!
		pa=ia; pb=ib

                if (aa(2).ne.bb(2))then 
			retval = aa(2)-bb(2)
                else if (aa(3).ne.bb(3)) then 
			retval = aa(3)-bb(3)
                
		else
			retval = aa(4)-bb(4)
		end if
		comparatorr2int = retval
		pa=0;pb=0;
        end function comparatorr2int

	function comparatorr2intf(a,b)
        	implicit none
		integer(kind=kim_intptr) ::a,b
		integer::aa(1:4),bb(1:4); pointer(pa,aa); pointer(pb,bb)
		integer ::retval,comparatorr2intf
		
		pa=a; pb=b

                if (aa(2).ne.bb(2))then 
			retval = aa(2)-bb(2)
                else if (aa(3).ne.bb(3)) then 
			retval = aa(3)-bb(3)
                
		else
			retval = aa(4)-bb(4)
		end if
		comparatorr2intf = retval
		pa=0;pb=0;
        end function comparatorr2intf

	function comparatordokint(a,b)
        	implicit none
		integer ::a,b
		integer::aa(2),bb(2); pointer(pa,aa); pointer(pb,bb)
		integer ::retval,comparatordokint
		integer(kind=kim_intptr)::ia,ib
		
		ia = loc(a); ib=loc(b) ! mimick passing pointer by value!
		pa=ia; pb=ib
		if (aa(1).ne.bb(1))then 
			retval = aa(1)-bb(1)
		else
			retval = aa(2)-bb(2)
		end if
		
		comparatordokint = retval
	end function comparatordokint

 	function comparatordokintf(a,b)
        	implicit none
		integer(kind=kim_intptr) ::a,b
		integer::aa(2),bb(2); pointer(pa,aa); pointer(pb,bb)
		integer ::retval,comparatordokintf
		
		pa=a; pb=b
		if (aa(1).ne.bb(1))then 
			retval = aa(1)-bb(1)
		else
			retval = aa(2)-bb(2)
		end if
		
		comparatordokintf = retval
	end function comparatordokintf

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
                implicit none
                type (neighborlist_object) :: neigh_obj
                integer :: i, ind
		if(neigh_obj%dokcurrentsize.eq.0) return
		call qsort(loc(neigh_obj%dokneighbors(1,1)),neigh_obj%dokcurrentsize,8,loc(comparatordokint))
		ind = 1
               do i=2,neigh_obj%dokcurrentsize
			if (comparatordokintf(loc(neigh_obj%dokneighbors(1,ind)),loc(neigh_obj%dokneighbors(1,i))).lt.0) then
				ind = ind+1
				neigh_obj%dokneighbors(:,ind) = neigh_obj%dokneighbors(:,i)
			else if (comparatordokintf(loc(neigh_obj%dokneighbors(1,ind)),loc(neigh_obj%dokneighbors(1,i))).gt.0) then
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

		if ( associated(neigh_obj%dokneighbors)) deallocate(neigh_obj%dokneighbors)
		if (associated(neigh_obj%numnei)) deallocate(neigh_obj%numnei)
		if (associated(neigh_obj%offset)) deallocate(neigh_obj%offset)
		if (associated(neigh_obj%neilist)) deallocate(neigh_obj%neilist)

		neigh_obj%dokmaximumsize = 0
		neigh_obj%dokcurrentsize= 0

       end subroutine neifree


	subroutine qsortr2int(a)
                implicit none
		!integer,target,allocatable, dimension(:,:) :: a
		integer,pointer,dimension(:,:) :: a
                integer shp(2)
                shp=shape(a)
                call qsort(loc(a(1,1)),shp(2),16,loc(comparatorr2int)) 
        end subroutine qsortr2int
	
	! the folloiwing subroutine gives all numbers of atom in bean ijk
        ! maximum namber of attom in bin no more then 500
	function allatomsinbin(i,j,k,a,alln)
                implicit none
  		integer,pointer, dimension(:,:) :: a

  		integer :: i,j,k,ind,indnext,m, allatomsinbin
  		integer :: alln(500), shp(2)
                integer  :: key(4),key2(4)
                shp=shape(a)
                key(1)=1;key(2)=i;key(3)=j;key(4)=k

                ind = bsearch(loc(key(1)),loc(a(1,1)),shp(2),16,loc(comparatorr2int))
                if (ind.le.0) then 
			allatomsinbin=0
		else 
			indnext=ind;
                        alln(1)=a(1,ind)
			do m = ind+1,shp(2)
			
				if ( comparatorr2intf( loc(a(1,ind)), loc(a(1,m))).ne.0) goto 22
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
		type (neighborlist_object),pointer:: neigh_obj

		real*8, pointer,dimension(:,:) :: x3
                integer :: natomb,shpx3(2),i,j,k,nx,ny,nz,nk,m
		real*8 :: fcut,xmin,xmax,ymin,ymax,zmin,zmax,lx,ly,lz, lmax, eps

                neigh_obj%x => x3
		shpx3=shape(x3)

                neigh_obj%atomnumber=shpx3(2)
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

		call tofastformat(neigh_obj)
	end subroutine neighborcoolistdomaindec
	 
	
        subroutine neighiterator2begining(neigh_obj)
		implicit none
		type (neighborlist_object) :: neigh_obj
		!type (neighborlist_object) :: neigh_obj
		neigh_obj%pointsto = 1	
		if (neigh_obj%dokcurrentsize.lt.1) then
				 neigh_obj%pointsto = -1
		endif 
	end subroutine neighiterator2begining

        function neighiterator_next(neigh_obj,neighborsofanatom)
		
                implicit none
	!	type (neighborlist_object),pointer :: neigh_obj
type (neighborlist_object) :: neigh_obj
		!type (neighborlist_object) :: neigh_obj
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
        
        subroutine tofastformat(neigh_obj)
		type(neighborlist_object),pointer::neigh_obj
		
		integer::numn,i,j,jj,numneisum
		integer, target :: neighborsofanatom(512); integer, pointer::nei1atom(:)
		nei1atom => neighborsofanatom

		

		if (associated(neigh_obj%numnei)) then 
!			deallocate(neigh_obj%numnei)
		end if

!		if (associated(neigh_obj%offset)) deallocate(neigh_obj%offset)
!		if (associated(neigh_obj%neilist)) deallocate(neigh_obj%neilist)

		allocate (neigh_obj%numnei(neigh_obj%atomnumber))
		allocate (neigh_obj%offset(neigh_obj%atomnumber))
		allocate (neigh_obj%neilist(neigh_obj%dokcurrentsize))
		neigh_obj%numnei=0
		neigh_obj%offset=0
		neigh_obj%neilist=0
		numn = 0
		numneisum=0
		call neighiterator2begining(neigh_obj)
		do while (numn .ge. 0)
			numn = neighiterator_next(neigh_obj,nei1atom)
			i=nei1atom(2)
			neigh_obj%numnei(i)=nei1atom(1)-2
			do jj=3, nei1atom(1)
				numneisum = numneisum+1
				j=nei1atom(jj)
				neigh_obj%neilist(numneisum) = j
			end do
			neigh_obj%offset(i)=numneisum-(nei1atom(1)-2)+1
		end do
		
	end subroutine tofastformat

	subroutine convert2full(neiobj_half,neiobj_full)
		type (neighborlist_object),pointer::neiobj_half,neiobj_full
		integer ::i
		! first clone object
		!if(associated(neiobj_full)) then 
		!	call neifree(neiobj_full)
	!		deallocate(neiobj_full)
		!end if
		!allocate(neiobj_full)
		neiobj_full%atomnumber = neiobj_half%atomnumber
		neiobj_full%dokcurrentsize = 2 * neiobj_half%dokcurrentsize
                neiobj_full%x => neiobj_half%x
        	call reallocatedok(neiobj_full,neiobj_full%dokcurrentsize)

		neiobj_full%dokneighbors(:,1:neiobj_half%dokcurrentsize) = neiobj_half%dokneighbors(:,1:neiobj_half%dokcurrentsize)
                !transpose and append second half
		do i = 1, neiobj_half%dokcurrentsize
			neiobj_full%dokneighbors(1,i+neiobj_half%dokcurrentsize)=neiobj_half%dokneighbors(2,i)
			neiobj_full%dokneighbors(2,i+neiobj_half%dokcurrentsize)=neiobj_half%dokneighbors(1,i)
		end do
                ! shrink and convert to fastformat
		call shrinkdok(neiobj_full)
		call tofastformat(neiobj_full)	
	end subroutine convert2full

	integer function neilocator(neigh_obj,id,nei1atom)
		implicit none
		integer :: id,i,ii
		type (neighborlist_object)::neigh_obj
		integer (kind=kim_intptr)::nei1atom
		nei1atom = loc( neigh_obj%neilist(  neigh_obj%offset(id) ) )
		neilocator = neigh_obj%numnei(id)
                !calculation position difference vector
		do i= 1, neigh_obj%numnei(id)
			ii= neigh_obj%neilist(  neigh_obj%offset(id)+ i -1 )
			neigh_obj%dx(:,i) = neigh_obj%x(:,id) - neigh_obj%x(:,ii)

		end do
	end function neilocator

        integer function get_neigh(neigh_obj,mode,request,atom,numnei,pnei1atom,pRij)
		integer(kind=kim_intptr) :: pnei1atom, pRij
		type (neighborlist_object),pointer::neigh_obj
		integer :: mode,request,atom,numnei
		!local definition
		if(mode.eq.0) then
			!iterator mode
			if(request .eq. 0) then 
				neigh_obj%pointsto = 1
				get_neigh=2

				return ! iterator  successful reset				
			else if (request .eq. 1) then
				if(neigh_obj%pointsto < 1 .or. neigh_obj%pointsto > (neigh_obj%atomnumber+1)) then 
					get_neigh =-1 ; return ! invalid atom id (out of range)
				else 
					if(neigh_obj%pointsto > neigh_obj%atomnumber) then 
						neigh_obj%pointsto = 0
						get_neigh=0 ! end reached by iterator
						return
					end if
					numnei = neilocator(neigh_obj,neigh_obj%pointsto,pnei1atom)
					atom = neigh_obj%pointsto
					neigh_obj%pointsto=neigh_obj%pointsto+1
                                        !address of calculated Rij

					pRij=loc(neigh_obj%dx(1,1))
					get_neigh=1 ! successful operation
					return
				end if	
			else
				get_neigh=-1; return  !invalid request
			end if			
		else if (mode.eq.1) then
			! locator mode
			if(request < 1 .or. request > neigh_obj%atomnumber) then 
				get_neigh =-1 ; return ! invalid atom id (out of range)
			end if
			numnei = neilocator(neigh_obj,request,pnei1atom)
			atom = request
			!addres of calculated Rij
			pRij=loc(neigh_obj%dx(1,1))
			get_neigh =1;return ! successful return
		else
			get_neigh =-2;return ! invalid mode value
		end if
	end function get_neigh        
	
	integer function  get_half_neigh_kim(pkim,mode,request,atom,numnei,pnei1atom,pRij) bind(c)
		integer (kind=kim_intptr)::pkim, pnei1atom,pRij
		integer:: mode, request,atom,numnei,kimerr
		!local declaration
		type (neighborlist_object_both) :: neiob; pointer(pneiob,neiob)
		pneiob = kim_api_get_data_by_index(pkim,kim_neighObj_index,kimerr)
		get_half_neigh_kim = get_neigh(neiob%half,mode,request,atom,numnei,pnei1atom,pRij)
		return
	end function  get_half_neigh_kim

        integer function  get_full_neigh_kim(pkim,mode,request,atom,numnei,pnei1atom,pRij) bind(c)
		integer (kind=kim_intptr)::pkim, pnei1atom,pRij
		integer:: mode, request,atom,numnei,kimerr
		!local declaration
		type (neighborlist_object_both) :: neiob; pointer(pneiob,neiob)
		pneiob = kim_api_get_data_by_index(pkim,kim_neighObj_index,kimerr)
		get_full_neigh_kim = get_neigh(neiob%full,mode,request,atom,numnei,pnei1atom,pRij)
		return
	end function  get_full_neigh_kim
     
	subroutine toneighborlist_object_pointer(stub_nei_obj,pnei_obj)
		implicit none
		type (neighborlist_object),pointer::pnei_obj
		type (neighborlist_object),target :: stub_nei_obj
		pnei_obj => stub_nei_obj
	end subroutine toneighborlist_object_pointer

	
	subroutine neighiterator(nei_obj,pnei1atom,numnei,restart)
		use KIM_API
		implicit none
		integer :: numnei,restart
		type (neighborlist_object),pointer :: nei_obj;
		integer :: nei1atom(1); pointer(pnei1atom,nei1atom)

		integer, pointer:: neisofanatom(:)

		call KIM_to_F90_int_array_1d(nei1atom,neisofanatom,512)
		if (restart.eq.0) then
			call neighiterator2begining(nei_obj)
		else
			numnei = neighiterator_next(nei_obj,neisofanatom)	
		end if
	end subroutine neighiterator
end module neighborlistmod

subroutine neighobj_allocate(pnei_obj)
	use neighborlistmod
	implicit none
	integer(kind=kim_intptr):: pnei_obj
	type (neighborlist_object),pointer :: neigh_obj
	allocate (neigh_obj)
	pnei_obj = loc(neigh_obj)
end subroutine neighobj_allocate

subroutine neighobj_both_allocate(neigh_obj)
	use neighborlistmod
	implicit none
	type (neighborlist_object_both),pointer :: neigh_obj

	allocate (neigh_obj)
	allocate (neigh_obj%half)
	allocate (neigh_obj%full)

end subroutine neighobj_both_allocate

subroutine neighobj_deallocate(neigh_obj)
	use neighborlistmod
	implicit none
	type (neighborlist_object),pointer :: neigh_obj
	call neifree(neigh_obj)
	deallocate(neigh_obj)
end subroutine neighobj_deallocate

subroutine neighobj_both_deallocate(neigh_obj)
	use neighborlistmod
	implicit none
	type (neighborlist_object_both),pointer :: neigh_obj
	
        if (associated(neigh_obj)) then	
		if (associated(neigh_obj%full)) call neifree(neigh_obj%full)
		if (associated(neigh_obj%half))	call neifree(neigh_obj%half)
		if (associated(neigh_obj%full)) deallocate(neigh_obj%full)
		if (associated(neigh_obj%half)) deallocate(neigh_obj%half)
		deallocate(neigh_obj)
	end if
end subroutine neighobj_both_deallocate

subroutine neighborscalculate(neigh_obj,px,n,cut)
	use neighborlistmod
	implicit none
	integer:: n
	real*8 ::cut
	real*8,pointer::x(:,:)
	integer(kind=kim_intptr) :: px
	real*8 :: xstub(3,*); pointer(pxstub,xstub)
	type (neighborlist_object),pointer :: neigh_obj
	pxstub=px
        call KIM_to_F90_real_array_2d(xstub,x,3,n)

	call neighborcoolistdomaindec(neigh_obj,x,cut)

end subroutine neighborscalculate

subroutine neighborscalculate_both(paddress,px,n,cut)
	use neighborlistmod
	implicit none
	integer:: n
	real*8 ::cut
	integer(kind=kim_intptr) :: px,paddress
	type (neighborlist_object_both):: neigh_obj; pointer(pneigh_obj,neigh_obj)
	pneigh_obj = paddress
	call neighborscalculate(loc(neigh_obj%half),px,n,cut)

	call convert2full(neigh_obj%half,neigh_obj%full)

end subroutine neighborscalculate_both


function get_neigh_iterator()
	use KIM_API
	use neighborlistmod
	implicit none
	integer(kind=kim_intptr) ::get_neigh_iterator
	get_neigh_iterator = loc(neighiterator)
end function get_neigh_iterator


function get_neigh_half_both()
	use KIM_API
	use neighborlistmod
	implicit none
	integer(kind=kim_intptr) ::get_neigh_half_both
	get_neigh_half_both = loc(get_half_neigh_kim)
end function get_neigh_half_both

function get_neigh_full_both()
	use KIM_API
	use neighborlistmod
	implicit none
	integer(kind=kim_intptr) ::get_neigh_full_both
	get_neigh_full_both = loc(get_full_neigh_kim)
end function get_neigh_full_both

subroutine set_kim_neighObj_index(ind)
	use KIM_API
	use neighborlistmod
	implicit none
	integer ind
	kim_neighObj_index=ind
end subroutine set_kim_neighObj_index
