program square_example
    real :: num, result

    print *, 'Enter a number:'
    read *, num

    result = square(num)
    print *, 'Square of', num, 'is', result

contains
    real function square(x)
        real :: x
        square = x * x
    end function square
end program square_example
