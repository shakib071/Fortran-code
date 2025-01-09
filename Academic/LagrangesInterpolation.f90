program LagrangesInterpolation

    integer :: n,i,j 
    real , dimension(:), allocatable :: x , fx
    real :: numenator , denominator ,ans,xx 

    print*, "How many number do u want to input"
    read*,n 

    allocate(x(n))
    allocate(fx(n))

    print*, "Enter the values of x and fx "

    do i=1,n
        read*,x(i),fx(i)
    end do 

    print*, "Enter the interpolation point "
    read*, xx 

    ans=0

    do i=1,n 

        numenator=1
        denominator=1

        do j=1,n 
            if (i .ne. j)  then
                numenator=numenator * (xx-x(j))
                denominator=denominator * (x(i)-x(j))
            end if 
        end do 

        ans = ans + ((numenator/denominator)*fx(i))

    end do 

    print*, "The Lagrange Interpolation at x= ",xx, " is ",ans 

end program LagrangesInterpolation