program Prime

integer :: n ,flag,p

read*, n

flag=1;
p= sqrt(real(n))
do i=2,p
    if (mod(n,i)==0) then
       flag = 0
       
    end if
end do

if(flag==1) then
    print*, "Prime"
else
    print*, "Not Prime"
end if 

end program Prime