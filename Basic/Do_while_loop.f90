program do_while_example
    integer :: i
    i = 1

    print *, 'Using a DO WHILE loop to print numbers 1 to 5:'
    do while (i<=5)
        print*, i
        i=i+1
    end do 
end program do_while_example
