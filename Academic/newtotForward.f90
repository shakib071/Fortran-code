program newton_forword 

    integer :: n,rows,cols,i,j
    real :: xx,u,ans,term
    real , dimension(:) , allocatable ::  x 
    real , dimension(:,:),allocatable :: table 

    print *, "How many data u want to input "
    read*, n 
    allocate(x(n))
    rows=n 
    cols=n
    allocate(table(rows,cols))

    print *, "Enter the values x and f(x)"

    do i=1,n 
        read*,x(i),table(1,i)
    end do 

    ! do i=1,n
    !     print*, x(i),"  ",table(1,i)
    ! end do 

    print *,"What is the interpolation Point : "
    read *,xx 

    u=(xx-x(1))/(x(2)-x(1))

    ! print*,u 

    do i=2,n 
        do j=1,n-i+1
            table(i,j)=table(i-1,j+1) - table(i-1,j)
        end do
    end do 

    ! print the table 
    print*, "The table is : "
    do i=1, rows
        print*, table(i,:)
    end do 

    
    ans=0
    term =1

    do i=1,n 
        ans= ans + ((term * (table(i,1)))/real(factorial(i-1)))
        term = term * (u-i+1)
    end do 

    print*, "Newtons forward difference interpolation at x=",xx," is : ",ans
   
contains 
    integer function factorial(n) result(ans)
        integer :: n
        if(n==0) then 
            ans = 1
        else 
            ans = 1
            do i=1,n 
                ans=ans*i 
            end do 
        end if 
    end function

end program newton_forword