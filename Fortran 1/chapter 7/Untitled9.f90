subroutine  b(m,n,lar)
    dimension k(10,10)

     lar=k(1,1)
    do i=1,m
        do j=1,n
            if(k(i,j).gt.lar) lar=k(i,j)
        end do
    end do
    print*,lar
    return
end subroutine
integer::p,q,v
read*,p,q
   do i=1,p
    do j=1,q
        READ(5,21),k(i,j)
        21 format(1x,i5)

    end do
end do


call b(p,q,lar)
print*,lar
stop
end
