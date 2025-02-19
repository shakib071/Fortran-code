program func
    implicit none
    real ::g,x,p,value
    integer::j
    g(x)=x*x-5.0*x+2.0

    do j=1,20
        p=float(j)
        value = g(p)
        print*,j,value
    end do
end program
