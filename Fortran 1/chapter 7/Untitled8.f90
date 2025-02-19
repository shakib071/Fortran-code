subroutine factorial(n,fact)
integer::fact
fact=1
do i=1,n
    fact=fact*i
end do
return
end subroutine
integer::x,fact
read*,x
call factorial(x,fact)
print*,fact
stop
end
