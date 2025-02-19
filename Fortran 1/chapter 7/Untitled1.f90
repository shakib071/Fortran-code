program BubbleSort
    implicit none

    integer, parameter :: n = 10
    integer :: arr(n),k
    integer :: i, j, temp

    ! Initialize the array with some values (you can replace these)
    do k=1,n
        read*,arr(k)
    end do
    do i = 1, n-1
        do j = 1, n-i
            if (arr(j) > arr(j+1)) then
                ! Swap the elements
                temp = arr(j)
                arr(j) = arr(j+1)
                arr(j+1) = temp
            end if
        end do
    end do

    ! Print the sorted array
    print *, "Sorted array:"
    do i = 1, n
        print *, arr(i)
    end do

end program BubbleSort
