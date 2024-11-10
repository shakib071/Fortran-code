program square1

    real :: num,result

    print *, "enter a number"
    read*, num

    result = square(num)
    print*, 'square of', num , 'is',result

    contains
         real function square(x)
            real ::x
            !Assigning the result to the function name
            !if we have same name variable as function name it will autometically return

            square = x*x

        end function square
end program square1