program factorial_example
    integer :: num, result

    print *, 'Enter a non-negative integer to calculate factorial:'
    read *, num

    if (num < 0) then
        print *, 'Factorial is not defined for negative numbers.'
    else
        result = factorial(num)
        print *, 'Factorial of', num, 'is', result
    end if

contains
    recursive integer function factorial(n) result(f)
        integer :: n
        if (n <= 1) then
            f = 1
        else
            f = n * factorial(n - 1)  ! Recursive call
        end if
    end function factorial
end program factorial_example
