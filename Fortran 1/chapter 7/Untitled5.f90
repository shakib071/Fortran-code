subroutine sum_ave(a,b,sum2,avg2)
    sum2=a+b
    avg2=(a+b)/2.0
    return
end subroutine

print*,"enter two numbers"
read*,x,y
call sum_ave(x,y,s,a)
print*,s,a
stop
end
