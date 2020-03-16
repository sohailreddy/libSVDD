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

program example2

	use support_vec_machine
	
	implicit none
	
	type(svmmodel) :: model			
	integer, parameter :: m = 3
	character(len = 100) :: classifiedLabel
	real(8) :: object(m-1), radius
	integer :: i, ierr

	
	!	Read the SVM model created in example 1
	call model%initialize('example1.dat')	

	!	Resave it as example2	
	call model%save('example2.dat')
	
	!	Classify the testing set using the read SVM
	open(unit = 100, file = 'testing.dat', status = 'unknown')
	open(unit = 101, file = 'classified.dat', status = 'unknown')
	
	do i = 1, 40000
		! read the data
		read(100,*, iostat = ierr) object
		if(ierr < 0) go to 100
		
		! Classify the object
		call model%classify(object, classifiedLabel, radius)
		write(101,*) object , classifiedLabel
	end do
100	continue
	close(100)
	close(101)


end program example2
