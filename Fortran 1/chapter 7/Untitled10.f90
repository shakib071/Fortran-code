function l(m,n)
    dimension b(10,10)
    do i=1,m
        do j=1,n
            read*,b(i,j)
        end do
    end do
    lar=b(1,1)
    do k=1,m
        do ll=1,n
            if(b(k,ll).gt.lar)lar=b(k,ll)
        end do
    end do
    l=lar
end function
print*,"enter row and column"
read*,ii,jj
print*,"enter the values"
lll=l(ii,jj)
print*,lll
stop
end
