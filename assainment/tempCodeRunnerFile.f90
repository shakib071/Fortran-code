 do i=2,iter+1
        y(i) = y(i-1) + ((h*df(x0,y(n-1)))/factorial(1)) + ((h*d2f(x0,y(n-1)))/factorial(2)) + ((h*d3f(x0,y(n-1)))/factorial(3)) + ((h*d4f(x0,y(n-1)))/factorial(4))

    end do 