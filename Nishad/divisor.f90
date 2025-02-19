program Prime

integer :: n ,p,flag

read*, n


p= sqrt(real(n))
flag=1


        do i=1,p
            if (mod(n,i)==0) then
            print*, i,n/i
            end if
        end do


if(flag==1) then
    print*, "Not divisible "
else
    print*, "divisible"
end if






end program Prime