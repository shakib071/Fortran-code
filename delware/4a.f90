program prime_perfect

    integer :: n,sum 

    read*,n 
    ! print*,prime(n)

    if(prime(n)==1) then
        print*,"the number is prime "
    else
        print*, "the divisors are: "
        sum=0
        do i=1,n 
            if(mod(n,i)==0) then 
                print*,i
                sum=sum+i
            end if 
        end do 

        print*,"the sum of divisors are : ",sum

        if((sum-n)==n) then 
            print*,"the number is perfect "
        else
            print*, "its not perfect"
        end if 

    end if 



    contains 
        integer function prime(n) result(ans)
            integer :: n,i

            do i=2,n/2 
                if(mod(n,i)==0) then
                    ans=0 
                    return 
                end if 
            end do 
            ans = 1
        end function prime

end program prime_perfect