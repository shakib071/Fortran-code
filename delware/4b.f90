program factorial
    integer :: n,i,fact 
    read*,n 

    fact =1

    do i=1,n 
        fact=fact*i 
    end do

    print*,"factorial is ",fact 

end program factorial