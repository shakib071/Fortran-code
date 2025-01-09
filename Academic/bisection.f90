program bisection 

    integer :: iter,i
    real :: a,b,c,tolerance

    print *, "Enter the interval [a,b] and tolerance and iterantions"

    read *,a,b,tolerance
    read *,iter

    if(f(a) * f(b) > 0.0)  then 
        print*, "No root Found "
        stop
    end if 


    i=1;



    do while (i<=iter .and. abs(a-b) > tolerance)
        c=(a+b)/2.0

        if(f(c)==0.0) then
            stop
        end if 


        if(f(a) * f(c) >0.0) then 
            a=c 
        else 
            b=c 
        end if 

        i=i+1
    end do 

    print*, "Approximate Root of x : ",c


contains  
    real function f(x)
        real :: x

        f= x**3 - x - 2.0
    end function f 

end program