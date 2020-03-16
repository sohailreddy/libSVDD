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

program example1

	use support_vec_machine
	
	implicit none
	
	type(svmmodel) :: model			
	integer, parameter :: m = 3, n = 489, o = 5
	real(8) :: x(n,m)
	character(len = 100) :: labels(n), uniqueLabels(5), classifiedLabel
	integer :: i, ierr
	
	real(8) :: object(m-1), radius

	! all the possible classification
	uniqueLabels = (/'1','2','3','4','5'/)

	! empty labels	
	labels = ''								
	
	
	open(unit = 100, file = 'terrain.dat', status = 'unknown')
	do i = 1, n
		read(100,*) x(i,:)	! read the data
		
		! assign the appropriate label for training data	
		if (x(i,3) <= 80.0 ) then
			labels(i) = uniqueLabels(1)
		else if (x(i,3) >= 230.0 ) then
			labels(i) = uniqueLabels(2)
		else if (x(i,3) >= 170.0 .and. x(i,3) < 230.0) then
			labels(i) = uniqueLabels(5)
		else if (x(i,1) >= 800.0 ) then
			labels(i) = uniqueLabels(3)
		else if (x(i,1) < 800.0 ) then
			labels(i) = uniqueLabels(4)
		else
			labels(i) = '6'				
		end if
	end do
	close(100)
				
	model%tol = 1.0d-7
	model%slack = 0.25d0	
	model%kernelParams(1) = 1.0d0	! Type of kernel
	model%kernelParams(2) = 120.0d0	! sigma in gaussian kernel
	
	!	Train the model
	call model%train(x(:,1:2), labels, uniqueLabels)		

	!	Save the SVM model
	call model%save('example1.dat')
	

	!	Classify the testing set using the trained SVM
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

end program example1







