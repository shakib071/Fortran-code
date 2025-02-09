program Range_Kutta_2
    implicit none
    
    real :: x0,y0,h,k1,k2
    integer :: iter,i

    real ,dimension(:),allocatable :: y


    print*,"Enter the value of x0,y0 and step size"
    read*,x0,y0,h

    print*,"Enter the number of iterations"
    read*,iter 

    allocate(y(iter+1))

    y(1)=y0

    do i=2,iter+1
        k1 = h*f(x0,y(i-1));
        k2 = h*f(X0+h,y(i-1)+k1)
        y(i)=y(i-1) + 0.5*(k1+k2)
        x0=x0+h
    end do 

    do i=1,iter+1
        print*,"At iteration = ",i-1," y = ",y(i)
    end do 




    contains 
        real function f(x,y) result(ans)
            real :: x,y
            ans = x*x + y*y 
        end function f 


end program Range_Kutta_2