program richardson_extrapolation
    implicit none
    real :: a, b, h, I, I_half, I_extrapolated
    integer :: n

   
    a = 0.0
    b = 1.0

   
    print *, "Enter number of intervals n :"
    read *, n

    
    h = (b - a) / real(n)

    
    I = trapezoidal_rule(a, b, n)

   
    I_half = trapezoidal_rule(a, b, 2*n)

    
    I_extrapolated = (4.0 * I_half - I) / 3.0


    print *, "Trapezoidal rule with step size h: ", I
    print *, "Trapezoidal rule with step size h/2: ", I_half
    print *, "Richardson Extrapolated Integral: ", I_extrapolated

contains

    
    real function trapezoidal_rule(a, b, n)
        real:: a, b,h, sum, x
        integer:: n,i
        

        h = (b - a) / real(n)
        sum = 0.5 * (f(a) + f(b))

        
        do i = 1, n-1
            x = a + i * h
            sum = sum + f(x)
        end do

        trapezoidal_rule = h * sum
    end function trapezoidal_rule

   
    real function f(x)
        real :: x
        f = exp(-x**2)
    end function f

end program richardson_extrapolation
