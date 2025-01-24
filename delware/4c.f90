program combination
    integer ::n,r
    read*,n,r


    comb =  factorial(n)/(factorial(r)*factorial(n-r))

    print*,"the answer is ",comb



    contains
        integer function factorial(n) result(fact)
            integer :: n
                fact = 1

                do i=1,n 
                    fact=fact*i  
                end do 

            end function factorial



end program combination