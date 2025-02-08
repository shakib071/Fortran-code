! 1516151

program weddle_Rule

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

    sum = y(1)+5*y(2) +y(3)+6*y(4)+y(5)+5*y(6)+y(7)

    ans= (3*h*sum)/10

    
    print*," Weddle's Rule integation approximation at x is :",ans


    contains 
        real function fun(x) result(ans)
            real :: x

            ans=1/(1+x*x)
        
        end function

end program weddle_Rule