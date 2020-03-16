!    libSVDD: A Library for Support Vector Data Description
!    Copyright (C) 2020 Sohail R. Reddy
!
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation; either version 2 of the License, or
!    (at your option) any later version.

!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License along
!    with this program; if not, write to the Free Software Foundation, Inc.,
!    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

module support_vec_machine

	use iso_c_binding

	type svmclass

		character(len = 100) :: label
		real(8) :: kernelParams(20) = 0.0d0	! Kernel params
		
		integer :: npts, ndim				! nSV and dimension
		
		real(8), allocatable :: sv(:,:) 	! Support Vectors
		real(8), allocatable :: lambda(:)	! Lagrange multiplie
		real(8), allocatable :: center(:)	! Center of Support Vector
		
		real(8) :: radius = 0.0d0			! Radius of description
		real(8) :: LKL_T = 0.0d0 			! lambda * K * lambda^T
		real(8) :: tol = 1.0d-10			! tolerance for lambda

		real(8) :: slack = 0.25d0			! outliers slackness

		contains

		procedure :: initialize => initialize_svm_class		
		procedure :: train => train_svm_class
		procedure :: distance => radius_svm_class
		procedure :: classify => classify_svm_class				
		procedure :: save => save_svm_class
				
	end type svmclass


	type svmmodel

		integer :: nClass = 0
		real(8) :: kernelParams(20) = 0.0d0	! Kernel params

		type(svmclass) , allocatable :: classes(:)


		contains

		procedure :: initialize => initialize_svm_model	
		procedure :: train => train_svm_model
		procedure :: classify => classify_svm_model
		procedure :: save => save_svm_model
		
	end type svmmodel

	contains
	

	function getIndicides(logicalArray) result(indices)
	
		implicit none
		logical :: logicalArray(:)
		integer, allocatable :: indices(:)
		integer :: i, j, n

		n = count(logicalArray .eqv. .true.)

		if(allocated(indices)) deallocate(indices)
		if(.not. allocated(indices)) allocate(indices(n))

		j = 0
		do i = 1, size(logicalArray)
			if(logicalArray(i)) then
				j = j + 1
				indices(j)	= i
			end if
		end do
		
		return
	end function getIndicides
	
	

	subroutine initialize_svm_model(model, name)
		implicit none
		class(svmmodel) :: model		
		character(*) :: name
		integer :: unit_num = 999, i, ierr
		logical flag
		character(len = 30) :: tmpString
		
		inquire(unit = unit_num, opened = flag)
		if (.not. flag) open(unit = unit_num, file = trim(name), status = 'unknown')

		model%nClass = 0
		do i = 1, 100000
			read(unit_num,*, iostat = ierr) tmpString
			if(ierr < 0) go to 100
			if(trim(tmpString) == '$Start:Class') then
				model%nClass = model%nClass + 1
			end if
		end do
100		continue
		rewind(unit_num)
		
		if(.not. allocated(model%classes)) allocate(model%classes(model%nClass))		
		
		do i = 1, model%nClass
			call model%classes(i)%initialize(name, unit_num)
		end do
		close(unit_num)
			
		return
	end subroutine initialize_svm_model

	subroutine train_svm_model(model,x,labels, uniqueLabels)
	
		implicit none
		class(svmmodel) :: model				
		real(8) :: x(:,:)
		character(*), optional :: labels(:)
		character(*) :: uniqueLabels(:)
		integer, allocatable :: indexing(:)
		integer i, j, k
	
		model%nClass = size(uniqueLabels)
	
		if(model%nClass > 1 .and. .not. present(labels)) then
			print*, 'more than one class present but labels not provide'
			stop
		end if


		if(.not. allocated(model%classes)) allocate(model%classes(model%nClass))
		
		do i = 1, model%nClass
			model%classes(i)%kernelParams = model%kernelParams
			model%classes(i)%label = trim(uniqueLabels(i))
			indexing = getIndicides(labels == uniqueLabels(i))
			call model%classes(i)%train( x( indexing ,:) )
		end do
		
		return		
	end subroutine train_svm_model
	
	subroutine classify_svm_model(model,x,label,radius)
	
		implicit none
		class(svmmodel) :: model		
		real(8) :: x(:)
		character(*) :: label
		real(8) :: r, radius
		integer i
		logical :: accept
		
		radius = 987654321.0d10		
		label = ''
		
		if(model%nClass == 1) then
			call model%classes(1)%classify(x,accept, radius)
			if(accept) label = trim(model%classes(1)%label)
			return
		end if
		
		
		do i = 1, model%nClass		
			call model%classes(i)%classify(x,accept, r)
			if(r <= radius) then
				radius = r
				label = trim(model%classes(i)%label)
			end if
		end do				
		return		
	end subroutine classify_svm_model

	subroutine save_svm_model(model, filename)
	
		implicit none
		class(svmmodel) :: model		
		character(*) :: filename
		integer :: i

		do i = 1, model%nClass		
			call model%classes(i)%save(filename)
		end do
		return
	end subroutine save_svm_model	
	












!	--------------- SVM ----------- CLASS
		
	subroutine initialize_svm_class(model, name, ifile)
		implicit none
		class(svmclass) :: model		
		character(*) :: name
		character(len = 30)  :: tmpString
		integer i, j, n, ierr
		integer, optional :: ifile
		integer :: unit_num = 999
		logical flag
		
		if(present(ifile)) then
			unit_num = ifile
		end if 
		
		inquire(unit = unit_num, opened = flag)
		if (.not. flag) open(unit = unit_num, file = trim(name), status = 'unknown')
		
		read(unit_num,*, iostat = ierr) tmpString
		if(ierr < 0) then	! end of file
			close(unit_num)
			return
		end if
		read(unit_num,*, iostat = ierr) tmpString, model%label	
		read(unit_num,*, iostat = ierr) tmpString, model%npts
		read(unit_num,*, iostat = ierr) tmpString, model%ndim
		read(unit_num,*, iostat = ierr) tmpString, model%radius
		read(unit_num,*, iostat = ierr) tmpString, model%LKL_T
		
		read(unit_num,*, iostat = ierr) tmpString, n
		do i = 1, n
			read(unit_num,*, iostat = ierr) model%kernelParams(i)
		end do
		read(unit_num,*, iostat = ierr) tmpString

		read(unit_num,*, iostat = ierr) tmpString, n		
		if (.not. allocated(model%sv)) allocate(model%sv(model%npts,model%ndim))
		if (.not. allocated(model%lambda)) allocate(model%lambda(model%npts))
		do i = 1, model%npts
			read(unit_num,*, iostat = ierr) model%sv(i,:), model%lambda(i)
		end do
		read(unit_num,*, iostat = ierr) tmpString				
		read(unit_num,*, iostat = ierr) tmpString

		if (.not. allocated(model%center)) allocate(model%center(model%ndim))
		do i = 1, model%ndim
			model%center(i) = dot_product(model%sv(:,i) , model%lambda)
		end do

		if(trim(tmpString) == '$End:Class') return
		if(.not. flag) close(unit_num)
			
		return
	end subroutine initialize_svm_class

	
	subroutine save_svm_class(model, filename)
	
		implicit none
		class(svmclass) :: model		
		character(*) :: filename
		integer :: unit_num = 999
		integer :: i

		open(unit = unit_num, file = trim(filename), status = 'unknown', position='append')

		write(unit_num,'(a)') '$Start:Class'
		write(unit_num,'(a,a)') 'label: ', trim(model%label)		
		write(unit_num,'(a,i5)') 'nSV: ' , model%npts
		write(unit_num,'(a,i5)') 'nDim: ' , model%ndim
		write(unit_num,'(a,f14.11)') 'radius: ' , model%radius
		write(unit_num,'(a,f14.11)') 'LKL_T: ' ,  model%LKL_T
		write(unit_num,'(a,i5)') '$Start:kernelParams', size(model%kernelParams)
		do i = 1, size(model%kernelParams)
			write(unit_num,*) model%kernelParams(i)
		end do
		write(unit_num,'(a)') '$End:kernelParams'
		write(unit_num,'(a,i5)') '$Start:SupportVectors', model%npts
		do i = 1, model%npts
			write(unit_num,*) model%sv(i,:), model%lambda(i)
		end do
		write(unit_num,'(a)') '$End:SupportVectors'		
		write(unit_num,'(a)') '$End:Class'
				
		close(unit_num)

		return
	end subroutine save_svm_class	
	
	
	subroutine train_svm_class(model,x, indx)
	
		implicit none
		class(svmclass) :: model				
		real(8), dimension(:,:) :: x
		integer, allocatable, optional :: indx(:) 	! Support Vectors Index

		real(8), allocatable :: k(:,:), lambda(:)
		integer n, m, i, j, loc(1)


		n = size(x,dim=1)
		m = size(x,dim=2)
		
		if(.not. allocated(lambda)) allocate(lambda(n))
		if(.not. allocated(k)) allocate(k(n,n))

		do i = 1, n
			do j = i, n
				k(i,j) = evaluate_kernel(x(i,:), x(j,:), model%kernelParams)
				k(j,i) = k(i,j)
			end do
		end do
		call slsqp_svm(n,k,0.0d0,model%slack,lambda, model%tol)
		j = count(lambda >= model%tol)
		model%npts = j
		model%ndim = m
				
		if(.not. allocated(model%sv)) allocate(model%sv(j,m))
		if(.not. allocated(model%lambda)) allocate(model%lambda(j))
		if(.not. allocated(model%center)) allocate(model%center(m))
		
		if(present(indx) ) then
			if(.not. allocated(indx)) allocate(indx(j))
		end if
				
		j = 0
		do i = 1, n
			if(lambda(i) >= model%tol) then
				j = j + 1
				model%sv(j,:) = x(i,:)
				model%lambda(j) = lambda(i)
				
				if(present(indx)) then
					indx(j) = i
				end if
				
			else
				lambda(i) = 0.0d0
			end if
		end do

		model%LKL_T = dot_product(matmul(lambda,k),lambda)

		do i = 1, m
			model%center(i) = dot_product(model%sv(:,i) , model%lambda)
		end do

		loc = maxloc(model%lambda)
		i = loc(1)
		
	
		model%radius = model%LKL_T + evaluate_kernel(model%sv(i,:), model%sv(i,:), model%kernelParams)
		do j = 1, model%npts
			model%radius = model%radius - 2.0d0 * model%lambda(j) * evaluate_kernel(model%sv(i,:), model%sv(j,:), model%kernelParams)
		end do
		
		if(allocated(k)) deallocate(k)
		if(allocated(lambda)) deallocate(lambda)

	
		return		
	end subroutine train_svm_class
	


	function radius_svm_class(model,x) result(r)
	
		implicit none
		class(svmclass) :: model		
		real(8), dimension(:) :: x
		real(8) :: r 
		integer i, j
		
		r = 0.0d0
		r = evaluate_kernel(x, x, model%kernelParams)
		do j = 1, model%npts
			r = r - 2.0d0 * model%lambda(j) * evaluate_kernel(x, model%sv(j,:), model%kernelParams)
		end do

		r = r + model%LKL_T
								
		return		
	end function radius_svm_class

	subroutine classify_svm_class(model,x,accept, r)
	
		implicit none
		class(svmclass) :: model		
		real(8), dimension(:) :: x
		real(8) :: r
		logical :: accept 
		integer i, j

		r = model%distance(x)
		accept = .false.
		if( r <= model%radius) then			
			accept = .true.
		end if
								
		return		
	end subroutine classify_svm_class


	

subroutine slsqp_svm(nvar,inp,lb,ub,opt, tol)

    use slsqp_kinds
	use slsqp_core
	
	
	implicit none
	real(8) :: inp(:,:), opt(:)
	real(8) :: lb, ub
! ------------------------------------
! ------------------------------------	
	integer :: m 
	integer :: meq
	integer :: la
	integer :: n 
	real(8) :: f = 0.0d0
	real(8), allocatable :: c(:) != 0.0d0
	real(8), allocatable :: ddx(:) != 0.0d0
	real(8), allocatable :: a(:,:) != 0.0d0
	real(8) :: acc = 0.0d0
	integer :: iter = 10
	integer :: mode = 0
	integer :: n1 
	integer :: mineq 
	integer :: lw 
    integer :: ljw
    real(8), allocatable :: ww(:) != 0.0d0
    integer, allocatable :: jw(:) != 0
    
    type(slsqpb_data) :: sdat
    type(linmin_data) :: ldat
	real(8) :: alphamin = 0.1d0
	real(8) :: alphamax = 1.0d0
! ------------------------------------
! ------------------------------------
	real(8) :: tol
	integer nvar
	integer :: counter = 0
	integer, parameter :: max_iter = 1000
	integer i, j
	real(8) :: w(nvar) 
	real(8) :: rand
	real(8), allocatable :: x(:), l(:), u(:)
	if(.not. allocated(x)) allocate( x(nvar))
	if(.not. allocated(l)) allocate( l(nvar))	
	if(.not. allocated(u)) allocate( u(nvar))


! ------------------------------------
! ------------------------------------
	m = 1
	meq = 1
	la = max(1,m)
	n = nvar
	f = 0.0d0
	mode = 0
	iter = 10
	acc = tol
	
	if(.not. allocated(c)) allocate(c(la))
	c = 0.0d0
	if(.not. allocated(ddx)) allocate(ddx(n+1))
	ddx = 0.0d0
	if(.not. allocated(a)) allocate(a(la,n+1))
	a = 0.0d0
	n1 = n+1	
	mineq = m - meq + 2*n1
	
	lw = n1*(n1+1) + meq*(n1+1) + mineq*(n1+1) + &   !for lsq
    &     (n1-meq+1)*(mineq+2) + 2*mineq        + &   !for lsi
    &     (n1+mineq)*(n1-meq) + 2*meq + n1     + &   !for lsei
    &     n1*n/2 + 2*m + 3*n +3*n1 + 1         !for slsqpb	

!	 lw = (3*n1+m)*(n1+1) &
!	 &	 + (n1-meq+1)*(mineq+2) + 2*mineq &
!	 &	 + (n1+mineq)*(n1-meq) + 2*meq + n1 &
!	 &   + (n1/2)*n + 2*m + 3*n + 3*n1 + 1
    
	ljw = mineq	
	if(.not. allocated(ww)) allocate(ww(lw))
	ww = 0.0d0
	if(.not. allocated(jw)) allocate(jw(ljw))
	jw = 0.0d0
	
! ------------------------------------
! ------------------------------------	

	do i = 1, n
		l(i) = lb
		u(i) = ub
		call random_number(rand)		
!		rand = 0.0d0
		x(i) = l(i) + (u(i) - l(i))*0.2
	end do
	
	counter = 0
	
	call compute_svm_obj(inp,x,f,c)
	call compute_svm_grad(inp,x,ddx,a)

	do while( mode <= 1 ) 
        if ( mode == 1) then
			call compute_svm_obj(inp,x,f,c)
		end if 
        if ( mode == -1) then
			call compute_svm_grad(inp,x,ddx,a)
		end if 


		call slsqp(m,meq,la,n,x,l,u,f,c,ddx,a,acc,iter,mode,ww,lw, &
                     jw,ljw,sdat,ldat,alphamin,alphamax)		

		if ( mode > 1 .or. mode == 0 ) then
			go to 100			
		end if

	end do
	        
100 continue

	opt = x
    
	if(allocated(x)) deallocate( x)
	if(allocated(l)) deallocate( l)	
	if(allocated(u)) deallocate( u)
	if(allocated(c)) deallocate(c)
	if(allocated(ddx)) deallocate(ddx)
	if(allocated(a)) deallocate(a)
	if(allocated(ww)) deallocate(ww)
	if(allocated(jw)) deallocate(jw)
    
    
    
	return

end subroutine slsqp_svm



subroutine compute_svm_obj(inp,x,f,c)	
	
		implicit none
		real(8) :: inp(:,:), x(:)
		real(8) :: f
		real(8) :: c(:)
		real(8) :: tmp
		integer i, j, n

		n = size(x)
		f = 0.0d0
		c = 0.0d0

		do i = 1, n
			f = f + x(i) * inp(i,i)
			do j = 1, n
				f = f - x(i) * x(j) * inp(i,j) 
			end do
		end do
		f = -f			
		c(1) = sum(x) - 1.0d0

		return
	end subroutine compute_svm_obj

	subroutine compute_svm_grad(inp,x,ddx,a)	
	
		implicit none
		real(8) :: inp(:,:), x(:)

		real(8) :: ddx(:)
		real(8) :: a(:,:), tmp
		integer i, j, n

		n = size(x)
		ddx = 0.0d0
		a = 0.0d0
		
		do i = 1, n
			ddx(i) = inp(i,i)
			tmp = 0.0d0
			do j = 1, n
				if( i /= j) tmp = tmp + x(j) * inp(i,j)
				if (i == j) tmp = tmp + 2 * x(i) * inp(i,i)
			end do
			ddx(i) = ddx(i) - tmp
		end do
		ddx = -ddx

		do i = 1, n
			a(1,i) = 1.0d0
		end do		

		return
	end subroutine compute_svm_grad
	


	function evaluate_kernel(x,y, params) result(ans)
	
		implicit none
		real(8) :: x(:), y(:), params(:)
		real(8) :: ans
		integer ith
		
		ith = params(1)
		if(ith == 1) then
			ans = gau_kernel(x,y, params(2))		
		else if (ith == 2) then
			ans = exp_kernel(x,y, params(2))
		else if (ith == 3) then
			ans = linear_kernel(x,y, params(3))
		else if (ith == 4) then
			ans = laplace_kernel(x,y, params(2))
		else if (ith == 5) then
			ans = sigmoid_kernel(x,y,params(4),params(3))
		else if (ith == 6) then
			ans = polynomial_kernel(x,y,params(3),params(5))
		else 
			print*, 'incorrect kernel... using Gau'
			ans = gau_kernel(x,y, params(2))
			params(1) = 1.0	
		end if
	
		return
	end function evaluate_kernel	
	

	function gau_kernel(x,y,sigma) result(opt)
		implicit none
		real(8) :: x(:), y(:), sigma
		real(8) :: opt
		
		opt = exp(-1.0d0*(norm2(x-y) / sigma)**2.0d0 )
		return
	end function gau_kernel


	function exp_kernel(x,y,sigma) result(opt)
		implicit none
		real(8) :: x(:), y(:), sigma
		real(8) :: opt
		
		opt = exp(-1.0d0*norm2(x-y) / sigma**2.0d0 )
		return
	end function exp_kernel


	function linear_kernel(x,y,c0) result(opt)
		implicit none
		real(8) :: x(:), y(:), c0
		real(8) :: opt
		
		opt = dot_product(x,y) + c0
		return
	end function linear_kernel

	function laplace_kernel(x,y,sigma) result(opt)
		implicit none
		real(8) :: x(:), y(:), sigma
		real(8) :: opt
		
		opt = exp(-1.0d0*norm2(x-y) / sigma )
		return
	end function laplace_kernel

	function sigmoid_kernel(x,y,gamma, c0) result(opt)
		implicit none
		real(8) :: x(:), y(:), gamma, c0
		real(8) :: opt
		
		opt = tanh(gamma * dot_product(x,y) + c0)
		return
	end function sigmoid_kernel

	function polynomial_kernel(x,y,c0, d) result(opt)
		implicit none
		real(8) :: x(:), y(:), c0, d
		real(8) :: opt
		
		opt = (dot_product(x,y) + c0) **d
		return
	end function polynomial_kernel


end module support_vec_machine