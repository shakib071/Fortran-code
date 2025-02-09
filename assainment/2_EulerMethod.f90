program euler
    implicit none 

    real :: x0,y0,h
    real ,dimension(:), allocatable ::x, y 
    integer :: iter,i

    print*,"Enter the value of initian value x0,y0 and step size"
    read*,x0,y0,h 

    print*,"Enter number of iterations "
    read*,iter 

    allocate(y(iter+1))
    allocate(x(iter+1))

    !calculate the values of x
    do i=0,iter
        x(i+1) = x0 + h*i 
    end do 

    !calculation for Euler method
    y(1)=y0
    do i=2,iter+1
        y(i) = y(i-1) + h*f(x(i-1),y(i-1))
    end do

    !print the values
    print*,"The Euler method solutions are"
    do  i=1,iter+1
        print*,"At iteration = ",i-1," y = ",y(i)
    end do 
    

    contains 

        real function f(x,y) result(ans)
            real :: x,y 
            ans = x + y 
        end function

end program euler