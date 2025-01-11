
program simpson1_3Rule

    real :: a ,b ,sum,h,x0,ans,evensum,oddsum
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

    oddsum=0
    evensum=0
    do i=2,n 
        if (MOD(i,2)==0) then 
            oddsum = oddsum + y(i)
        else 
            evensum = evensum+y(i)
        end if 
    end do 

    sum= y(1)+y(n+1)+4*oddsum+2*evensum

    ans= (h*sum)/3;

     print*," Simpson 1/3 Rule integation approximation at x is :",ans


    contains 
        real function fun(x) result(ans)
            real :: x

            ans=1/(1+x*x)
        
        end function


end program simpson1_3Rule