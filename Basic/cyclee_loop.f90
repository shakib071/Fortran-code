
program cycle_example
    integer :: i

    print *, 'Using CYCLE to skip printing when i equals 3:'
    do i = 1, 5
        if (i == 3) then
            cycle
        end if
        print *, i
    end do
end program cycle_example
