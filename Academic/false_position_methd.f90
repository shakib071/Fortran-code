program false_position 

    real :: a,b,tol,c
    integer :: i,iter 

    print*, 'Enteer [a,b] , tollerence and iteration number'
    read*, a,b,tol
    read*, iter 

    if(f(a)*f(b)>0.0) then 
        print*, "NO root found"
        stop
    end if 

    i=1

    do while (i<=iter .and. abs(b-a)>tol)
        c=b-((f(b)*(a-b))/(f(a)-f(b)))

        if (f(c)==0.0) then
            stop 
        end if 

        if (f(a)*f(c)>0.0) then 
            a=c 
        else 
            b=c
        end if 
        i=i+1 
    end do 

    print*, "Approximation root at x = ",c 

    contains 
        real function f(x) result(ans)
            real :: x
            ans = x**3 -x-2.0
        end function f 

end program false_position
