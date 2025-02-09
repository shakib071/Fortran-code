!adam bashforth 2 step method
! y(n+1) = y(n) + (h/2)*(3*f(x(n),y(n)) - f(x(n-1),y(n-1)))

program multi_step
    implicit none
    real :: x0,y0,h 
    integer :: iter , i 
    real ,dimension(:),allocatable :: y,x  

    print*,"Enter initial guess x0,y0 and step size h"
    read*,x0,y0,h

    print*,"Enter the number of iterations"
    read*,iter

    allocate(y(iter+1))
    allocate(x(iter+1))

    x(1)=x0

    do i=2,iter+1
        x(i)=x(i-1)+h
    end do 

    y(1) = y0
    y(2) = y(1) + h*f(x(1),y(1))

    do i=3,iter+1
        y(i) = y(i-1) + (h/2)*(3*f(x(i-1),y(i-1)) - f(x(i-1),y(i-2)))
       
    end do 

    do i=1,iter+1
        print*,"At iteration = ",i-1," y = ",y(i)
    end do 


    contains 
        real  function f(x,y) result(ans)
            real :: x,y 
            ans = y + sin(x)
        end function f 


end program multi_step