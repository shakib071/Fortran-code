program Range_Kutta_4_adaptive_step_size
    implicit none
    
    real :: x0,y0,h,k1,k2,k3,k4,k,tol
    integer :: iter,i

    real ,dimension(:),allocatable :: y


    print*,"Enter the value of x0,y0 and initial step size"
    read*,x0,y0,h

    print*,"Enter the tolerence"
    read*,tol 

    print*,"Enter the number of iterations"
    read*,iter 

    allocate(y(iter+1))

    y(1)=y0

    do i=2,iter+1
        k1 = h*f(x0,y(i-1));
        k2 = h*f(X0+(h/2.0),y(i-1)+(k1/2.0))
        k3 = h*f(X0+(h/2.0),y(i-1)+(k2/2.0))
        k4 = h*f(x0+h,y(i-1)+k3)
        k = (k1+2*k2+2*k3+k4)/6
        y(i)=y(i-1) + k 


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
        real function f(x,y) result(ans)
            real :: x,y
            ans = y*cos(x)
        end function f 


end program Range_Kutta_4_adaptive_step_size