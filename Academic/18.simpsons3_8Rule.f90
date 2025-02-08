

program simpson3_8Rule

    real :: a ,b ,sum,h,x0,ans,threesum,others
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

    threesum=0
    others=0

    do i=2,n 
        if (mod((i-1),3)==0) then
            threesum = threesum + y(i)
        else
            others= others + y(i)
        end if 
    end do

    sum= y(1) + y(n+1) + 2*threesum + 3*others

    ans = (3*h*sum)/8



    print*," Simpson 3/8 Rule integation approximation at x is :",ans


    contains 
        real function fun(x) result(ans)
            real :: x

            ans=1/(1+x*x)
        
        end function

end program simpson3_8Rule