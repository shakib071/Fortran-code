program fixed_point 

    real :: x0,tol,x1
    integer :: iter,i

    print *, " Enter the value of initial guess and tolerence and iterantions "

    read *,x0,tol
    read *, iter

    i=1
    x1= f(x0)
    do while (i<=iter .and. abs(x1-x0)>tol )

        ! if(f(x0)==x1) then 
        !     stop 
        ! end if

        x0=x1
        x1=f(x0)
        i=i+1

    end do 

    print *,"Approximate root at x : ",x1

    contains
        real function f(x) result(ans)
            real:: x
            ans = cos(x)
        end function f


end program fixed_point