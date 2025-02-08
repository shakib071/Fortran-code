program newton_diff_backword

    integer :: n,rows,cols,i,j,k
    real :: xx,u,ans,term
    real , dimension(:) , allocatable ::  x 
    real , dimension(:,:),allocatable :: table 

    print *, "How many data u want to input "
    read*, n 
    allocate(x(n))
    rows=n 
    cols=n
    allocate(table(rows,cols))

    print *, "Enter the values x and f(x)"

    do i=1,n 
        read*,x(i),table(1,i)
    end do 

    ! do i=1,n
    !     print*, x(i),"  ",table(1,i)
    ! end do 

    print *,"What is the interpolation Point : "
    read *,xx 

    k=0
   
    do i=2,n
        do j=1,n-i+1
            table(i,j) = (table(i-1,j+1) - table(i-1,j))/(x(j+1+k)-x(j))
          
        end do
      
          k=k+1
    end do 
    
    ! print the table 
    print*, "The table is : "
    do i=1, n 
        print*, table(i,:)
    end do 

    term =1
    ans = 0
    k=n
    do i=1,n 
        ans = ans + term*table(i,k)
        term = term * (xx+x(i))
        k=k-1
    end do

    print*, "The newtons Divided forword difference interpolation at x= ",xx, " is ",ans
    




end program newton_diff_backword