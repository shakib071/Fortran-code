program func
    integer::fact,i,n
    fact=1
    do i=1,n
        fact=fact*i
    end do

    read*,n
    print*,fact
end program
