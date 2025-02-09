!adam bashforth 2 step method
! y(n+1) = y(n) + (h/2)*(3*f(x(n),y(n)) - f(x(n-1),y(n-1)))

program multi_step_variable_stepsize
    implicit none
    real :: x0,y0,h,tol
    integer :: iter ,i 
    real ,dimension(:),allocatable :: y

    print*,"Enter initial guess x0,y0 and step size h"
    read*,x0,y0,h

    print*,"Enter the tolerence "
    read*,tol

    print*,"Enter the number of iterations"
    read*,iter

    allocate(y(iter+1))

    y(1) = y0
    y(2) = y(1) + h*f(x0,y(1))

    x0=x0+h

    do i=3,iter+1
        y(i) = y(i-1) + (h/2)*(3*f(x0,y(i-1)) - f(x0,y(i-2)))

        if(abs(y(i)-y(i-1)) > tol) then
            h=h/2.0  !if error is greater than tolerence then small step size
        else 
            h=h*1.25 !else bigger step size 

        end if 

        x0=x0+h 
       
    end do 

    do i=1,iter+1
        print*,"At iteration = ",i-1," y = ",y(i)
    end do 


    contains 
        real  function f(x,y) result(ans)
            real :: x,y 
            ans = y + sin(x)
        end function f 


end program multi_step_variable_stepsize