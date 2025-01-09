! program array1
!     ! integer, dimension(5) :: array1 = (/ 1, 2, 3, 4, 5 /)
!     !  integer, dimension(2, 3) :: matrix = reshape([1, 2, 3, 4, 5, 6], [2, 3])

!     integer, dimension(5) :: array

!     integer :: i 
    
!     do i=1,5
!         array(i)= i*2
!     end do 

!     print*, 'Array elements:',array

!     do i=1,5
!         print*, array(i)
!     end do 

! end program array1




! Allocating and Deallocating Arrays

! program dynamic_array_example
!     integer, dimension(:), allocatable :: array
!     integer :: n, i

!     print *, 'Enter the size of the array:'
!     read *, n

!     allocate(array(n))

!     do i = 1, n
!         array(i) = i * 2
!     end do

!     print *, 'Array elements:', array

!     deallocate(array)
! end program dynamic_array_example


! ! real, dimension(:,:), allocatable :: dynamic_matrix




! Dynamic Allocation for Multidimensional Arrays
program dynamic_2d_array_example
    integer, dimension(:,:), allocatable :: matrix
    integer :: rows, cols, i, j

    print *, 'Enter number of rows and columns:'
    read *, rows, cols

    ! Dynamically allocate memory for the 2D array
    allocate(matrix(rows, cols))

    ! Initialize the 2D array
    do i = 1, rows
        do j = 1, cols
            matrix(i, j) = i * j
        end do
    end do

    ! Print the 2D array
    print *, 'Matrix elements:'
    do i = 1, rows
        print *, matrix(i, :)
    end do

    ! Deallocate the 2D array
    deallocate(matrix)
end program dynamic_2d_array_example
