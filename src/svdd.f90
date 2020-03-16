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


subroutine train(n, m, x, params, label, 	&
&				 center, radius, Lagrange,		&
&				 C_x, filename) 	bind(C, NAME='train')

	use iso_c_binding
	use support_vec_machine
	
	implicit none
	
	type(svmclass) :: model
	integer :: n, m				!	n objects with m dimension
	real(8) :: params(20)	!	std for gau kernal
	character(len=100) :: label	!	label for class
	real(8) :: x(m,n)			!	objects
	real(8) :: center(m)		!	center of the hypersphere
	real(8) :: radius			!	radius of the hypersphere
	real(8) :: Lagrange(n)		!	Lagrange multiplies
	real(8) :: C_x				! 	The C_x term
	character(len=100) :: filename
	
	integer indx
	integer, allocatable :: svIndx(:)
	
	model%label = ''
	indx = index(label, substring = c_null_char) - 1
	model%label(1:indx) = trim(label(1:indx))
	
	
	model%tol = params(1) 
	model%slack = params(2)
	model%kernelParams(1:10) = params(3:12) 
	
		
	call model%train(transpose(x), svIndx)
	
		
	indx = index(filename, substring = c_null_char) - 1
	if(trim(filename(1:indx)) /= '' .and. indx > 1) then
		call model%save(trim(filename(1:indx)))		
	end if
		
	center = model%center
	radius = model%radius
	C_x = model%lkl_t
	
	Lagrange = 0.0d0
	Lagrange(svIndx) = model%lambda
	
	if(allocated(svIndx)) deallocate(svIndx)
	return

end subroutine train


subroutine classify(n, m, sV, params, &
&				 center, radius, Lagrange,	&
&				 C_x, object, objectRadius, accept) 	bind(C, NAME='classify')

	use iso_c_binding
	use support_vec_machine
	
	implicit none
	
	integer :: n, m				!	n objects with m dimension
	real(8) :: params(20)		!	std for gau kernal
	real(8) :: sV(m,n)			!	support vectors
	real(8) :: center(m)		!	center of the hypersphere
	real(8) :: object(m)		!	object to classify
	real(8) :: radius			!	radius of the hypersphere
	real(8) :: Lagrange(n)		!	Lagrange multiplies
	real(8) :: C_x				! 	The C_x term
	real(8) :: objectRadius		!	Radius of the object
	integer :: accept			!	Whether to accept the object
	
	integer i
	real(8) :: kernelParams(20) = 0.0


	kernelParams(1:10) = params(3:12) 

	objectRadius = 0.0d0
	objectRadius = evaluate_kernel(object, object, kernelParams)
		
	do i = 1, n
		objectRadius = objectRadius - 2.0d0 * Lagrange(i) * evaluate_kernel(object, sV(:,i) , kernelParams) 
	end do

	objectRadius = objectRadius + C_x
	accept = 0
	if( objectRadius <= radius) then	
		accept = 1
	end if

	return		
end subroutine classify




