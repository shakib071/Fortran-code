program Prime

integer :: n ,flag

read*, n

flag=1;
do i=2,n/2
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