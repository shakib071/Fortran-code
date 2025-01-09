program fixed_point 

    real :: x0,tol,x
    integer :: iter,i 
    i=1 

    print*, "Enter initial guess,tolerence,iteration"
    read*, x0,tol
    read*,iter 
    x1=g(x0)
    do while (abs(x1-x0)>tol .and. i<=iter)
        x0=x1 
        x1=g(x0)
        i=i+1 
    end do 

    print*,"Approximation of root at x = ", x1 

    contains 
        real function g(x) result(ans)
            real :: x 
            ans = cos(x)

        end function g 



end program fixed_point