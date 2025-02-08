
program bisectionMethod
    real :: a,b,TOL,c
    integer :: i,iter 

    print*, 'Enter [a,b], tolerance and iteration'

    read*, a,b,TOL 
    read*, iter
    i=1
    
    if (f(a) * f(b) > 0.0) then 
        print *, 'No root found'
        stop 
    end if

    do while (i<=iter .and. abs(b-a)>TOL )
         c = (a+b)/2.0
        
         if(f(c)==0.0) then
            stop
         end if 

         if(f(a)* f(c)>0.0) then 
            a=c 
         else 
            b=c
         end if 
         i=i+1
    end do 

    print *, 'Approximate root at x = ',c 

    contains 
        real function f(x) result(ans)
            real :: x
           
            ans = x**3 - x - 2.0
        end function 

end program bisectionMethod




