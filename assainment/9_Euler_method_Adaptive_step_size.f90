program euler_method_adaptive_step_size
    implicit none 

    real :: x0,y0,h,tol 
    real ,dimension(:), allocatable ::y 
    integer :: iter,i

    print*,"Enter the value of initian value x0,y0 and initial step size"
    read*,x0,y0,h 

    print*,"Enter the tolerence"
    read*,tol 

    print*,"Enter number of iterations "
    read*,iter 

    allocate(y(iter+1))
    
   
    y(1)=y0
    do i=2,iter+1
        y(i) = y(i-1) + h*f(x0,y(i-1))

        if(abs(y(i)-y(i-1)) > tol) then
            h=h/2.0  !if error is greater than tolerence then small step size
        else 
            h=h*1.25 !else bigger step size 

        end if 

        x0 =x0+h 

    end do

    print*,"The Euler method Adaptive step size solutios are :"
    do  i=1,iter+1
        print*,"At iteration = ",i-1," y = ",y(i)
    end do 
    






    contains 

        real function f(x,y) result(ans)
            real :: x,y 
            ans = y - x*x + 1
        end function

end program euler_method_adaptive_step_size