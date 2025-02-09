program Taylor_Method
    implicit none
    
    real :: x0,y0,h
    integer :: iter,i 

    real ,dimension(:),allocatable :: y 


    print*,"Enter the value of initian value x0,y0 and step size"
    read*,x0,y0,h 

    print*,"Enter number of iterations "
    read*,iter 

    allocate(y(iter+1))
    y(1)=y0

  
    !calculation for taylor method 

    do i=2,iter+1
        y(i) = y(i-1) + ((h*df(x0,y(i-1)))/factorial(1)) + ((h*d2f(x0,y(i-1)))/factorial(2)) + ((h*d3f(x0,y(i-1)))/factorial(3))
        x0 = x0+h 
    end do 

    !print the iterations
    print*,"The Taylor method solutions are :"
    do i=1,iter+1 
        print*," At iteration = ",i-1," y = ",y(i)
    end do 

    


    contains 

        real function factorial(n) result(ans)
            integer :: n,i 
            if(n==0) then 
                ans = 1
            else 
                ans=1.0
                do i=1,n
                    ans = ans*i 
                end do 
            end if 
        end function factorial



        real function df(x,y) result(ans)
            real:: x,y 
            ans = y - x*x
        end function df 

        real function d2f(x,y) result(ans)
            real:: x,y 
            ans = df(x,y) + 2*x
        end function d2f 

        real function d3f(x,y) result(ans)
            real:: x,y 
            ans = d2f(x,y)+2
        end function d3f

      

end program Taylor_Method