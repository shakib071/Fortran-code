program Newton_Rapson

    real :: x0,tol,x1 
    integer :: i, iter 

    print*, "Enter initial guess , tolerence and max_iteration"

    read*, x0,tol
    read*, iter 

    i=1 
    x1 = x0 - (f(x0)/ df(x0))

    do while (i<=iter .and. abs(x1-x0)>tol)
        x0=x1 
        x1=x0 - (f(x0)/ df(x0))
        i=i+1
    end do 

    print *, "Approximation root at x = ",x1 

    contains
        real function f(x) result(ans)
            real :: x 
            ans = x**2 - 2
        end function f 

        real function df(x) result(ans1)
            real :: x 
            ans1 = 2 * x 

        end function df 

end program Newton_Rapson
