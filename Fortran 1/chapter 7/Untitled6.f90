function area(a,b,c)
    s=(a+b+c)/2
    area=sqrt(s*(s-a)*(s-b)*(s-c))
    return
end function

print*,"enter the value of a,b,c"
read*,x,y,z
if((x.le.0).or.(y.le.0).or.(z.le.0))then
    print*,"invalid"
    stop
end if
if(((x+y).lt.z).or.((y+z).lt.x).or.((z+x).lt.y))then
    print*,"invalid"
    stop
end if
tarea=area(x,y,z)
print*,tarea
stop
end
