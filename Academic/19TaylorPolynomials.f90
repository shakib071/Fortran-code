
program taylor_poly

    real :: a,x,sum,term,fact
    integer :: i,degree 

    print*, "Enter x and a (centerd at) and degree"
    read *,x,a 
    read *,degree 

    i=1
    sum=1
    term=1
    
    do while (i<=degree) 

        term=term *(x-a)
        fact = real(factorial(i))
        sum=sum+(term/fact)
        i=i+1

    end do 

    ! sum is the ans 

    print*, "The taylor polynomial approximation at x: ",sum 

 contains 
    integer function factorial(n) 
        integer :: n
        factorial=1
        do i=1,n 
            factorial = factorial*i 
        end do 
    end function factorial
        


end program taylor_poly