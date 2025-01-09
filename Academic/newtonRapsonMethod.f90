program newton_rapson

    real :: x0,x1,tol
    integer :: iter , i 

    i=1

    print*,"Enter the initial guess tolerence and iterantions "
    read *,x0,tol 
    read*, iter 
    x1= x0 - (f(x0)/df(x0))
    ! df is derivative of f 
    do while(abs(x1-x0)>tol .and. i<=iter)
        x0=x1
        x1= x0 - (f(x0)/df(x0))
        i=i+1
    end do

    print *, "Approximate root at x : ",x1


    contains 
        real function f(x) result(ans)
            real :: x
            ans=x*x - 2.0
        end function f 

        real function df(x) result(ans)
            real :: x
            ans= 2.0 * x 
        end function



end program newton_rapson