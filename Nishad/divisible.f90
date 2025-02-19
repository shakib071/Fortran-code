program Prime

integer :: n ,p,flag

read*, n


p= sqrt(real(n))
flag=1

if (mod(n,2)==0) then
   flag=0
else
    do i=3,p,2
        if (mod(n,i)==0) then
           flag=0
        end if
    end do
end if

if(flag==1) then
    print*, "Not divisible "
else
    print*, "divisible"
end if






end program Prime