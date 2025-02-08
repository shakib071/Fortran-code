program TraphizoidRule 

    real :: a ,b ,sum,h,x0,ans 
    integer :: n,i

    real , dimension(:),allocatable :: y


    print*, "Enter interval a and b and number of subinterval"
    read*,a,b,n 

    allocate(y(n+1))

    h=(b-a)/n

    x0=a

    do i=1,n+1
        y(i)=fun(x0)
        x0=x0+h
    end do 

    ! print the table 
    do i=1,n+1
        print*,y(i)
    end do

    sum= (y(1)+y(n+1))/2

    do i=2,n 
        sum=sum+y(i)
    end do 

    ans = sum * h

    print*," Trapezoid Rule integation approximation at x is :",ans
    



    contains 
        real function fun(x) result(ans)
            real :: x

            ans=1/(1+x*x)
        
        end function



end program TraphizoidRule