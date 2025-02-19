integer a
dimension a(100)
read*,n
do l=1,n
    read*,a(l)
end do
nn=n-1
do 200 i=1,nn
    jj=n-k
    do 100 j=1,jj
        if(a(j).le.a(j+1)) go to 100
        temp=a(j)
        a(j)=a(j+1)
        a(j+1)=temp
    100 continue
200 continue
do m=1,n
    print*,a(m)
end do
stop
end
