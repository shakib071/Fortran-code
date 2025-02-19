program func

    implicit none
    real::a,b,c,x,p,value,f
    integer :: i

    f(x)=a*x*x+b*x+c
 print*,"enter the value of a ,b,c"
    read*,a,b,c
    do 100 i=1,11
        p=-6.0+float(i)
        value = f(p)
        print*,"for x= ",p,"f(x) is :",value
    100 continue

end program
