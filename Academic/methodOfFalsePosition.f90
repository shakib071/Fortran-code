program false_position 

    real :: a,b,c,tol
    integer :: i,iteration

    print *, "Enter the value of a b and tolerance and iteration "

    read *,a,b,tol
    read*, iteration

    if(f(a)*f(b)>0.0) then 
        print*,"No root Bari jaa"
        stop
    end if 

    i=1
    do while (i<=iter .and. abs(b-a)>tol)
        
        c= a - ((f(a)*(b-a))/(f(b)-f(a)))

        if(f(c) == 0.0) then 
            stop
        end if

        if(f(a)*f(c)>0.0) then 
            a=c 
        else
            b=c 
        end if 

        i=i+1

    
    end do

    print*, " Approximate root at x = :",c 



    contains 
        real function f(x) result(ans)
            real :: x
            ans = x**3 -x-2.0
        end function



end program false_position