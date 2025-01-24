program five

    integer :: n,i,primecnt,perfectcnt,sqnsum,sum
    real::standardDaviation,variance,variancesum,avg

    read*,n 

    primecnt=0 
    perfectcnt=0
    sum=0
    sqnsum=0

    do i=1,n 
        if(prime(i)==1) then 
            primecnt= primecnt + 1
        else if(perfect(i)==1) then
            perfectcnt = perfectcnt +1
        end if 
        sum=sum+i 
        sqnsum = sqnsum + i*i 
    end do 
    avg=real(sum)/real(n)
    print*,"there are ",primecnt," numbers of prime number and there  are ",perfectcnt,"number of perfect  number"

    print*,"square sum of n is:",sqnsum,"average is ",avg 


    variancesum=0
    do i=1,n 
        variancesum=variancesum+((i-avg)*(i-avg))
    end do 

    variance = variancesum/(n-1)

    standardDaviation = sqrt(variance)

    print*,"the variance is ",variance,"and sd is ",standardDaviation


   



    contains
        integer function prime(n) result(ans)
            integer :: n,i
            if(n<2) then 
                ans=0
                return
            end if 
            
            do i=2,n/2 
                if(mod(n,i)==0) then
                    ans=0 
                    return 
                end if 
            end do 
            ans = 1
        end function prime


        integer function perfect(n) result(ans)
            integer::n,sum,i
            
            sum=0

            do i=1,n/2 
                if(mod(n,i)==0) then 
                    sum=sum+i 
                end if 
            end do 

            if(sum == n) then  
                ans = 1
            else 
                ans =0 
            end if 
        end function perfect


end program five