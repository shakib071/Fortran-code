! richardson extrapolation 
!y extrapolated =yh + (yh −yh/2)/(2^p −1)
!p=1 for euler method solution 
!p is order 

program extrapolation_method 
    implicit none
    
    real:: x0,y0,h,yh,yh_half,y_extrapolated
    integer :: iter, i 


    print*,"Enter initial guess x0,y0 and initial step size h"
    read*,x0,y0,h

    print*,"Enter the number of iterations"
    read*,iter

    yh = y0
    yh_half = y0 

   

    do i=1,iter 
        !Euler method for step size h
        yh = yh + h * f(x0, yh)

        ! Euler’s method for step size h/2 
        yh_half = yh_half + (h/2) * f(x0, yh_half)
        yh_half = yh_half + (h/2) * f(x0 + h/2, yh_half)

        !extrapolation 

        y_extrapolated= yh + (yh - yh_half) /(2-1);
        !p=1 because we are using eulter method

        print*,"At iteration = ",i," Extrapolated Value:", y_extrapolated

        x0 = x0 + h 



    end do 






    contains
        real function f(x,y) result(ans)
            real :: x,y 
            ans = 2*x - y 
        end function f 



end program extrapolation_method
