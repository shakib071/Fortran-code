
program  picard
    implicit none
    real:: x0,y0,x
    integer :: iter,i 
     real, dimension(:), allocatable :: y

    print*,"Enter the initial value x0 and y0"
    read*,x0,y0

    print*,"Enter the iterations"
    read*,iter

    print*,"Enter the value of the x"
    read*,x 

    allocate(y(iter))
    y(1)=y0
    do i=2,iter
        y(i)=y(1)+integrate(x0,x,y(i-1))
    end do

    do i=1,iter
        print*,"y",i," is ",y(i);
    end do

    contains
        real function f(x,y) result(ans)
            real ::x,y 
            ans =x+y*y 
        end function f 

        real function integrate(x0,x,y) result(ans)
            !traphizoid rule used to integrate
            real :: x0,x,y,x00
            real :: h
            integer :: n,p

            n=10

            h=(x-x0)/real(n)
            ans=0

            x00=x0
            
            do p=1,n+1
                if(p==1 .or. p==n+1) then 
                    ans= ans+ f(x00,y)/2
                else 
                    ans= ans+ f(x00,y)
                end if 
                x00=x00+h
            end do

            ans=ans*h 

        end function integrate





end program picard