program backward_diff

real :: x,h,fx

print*,"Enter the number x and h "

read*,x,h

fx= (f(x)-f(x-h))/h

print*," Forword Backword derivative of f(x) at x is ",fx



contains 

    real function f(x) result(ans)
        real :: x
        ans=x*x
    end function f 

end program backward_diff