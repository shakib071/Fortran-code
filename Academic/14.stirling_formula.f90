program stirling
    
    real,dimension(:,:),allocatable :: table
    real,dimension(:),allocatable :: x
    integer::i,n,j,index,central_index, plus_minus
    real:: u,xx,term ,ans


    print*,"how many number of data u want to input"
    read*,n 

    allocate(x(n))
    allocate(table(n,n))

    print*,"Enter the value x fx"
    do i=1,n
        read*,x(i),table(i,1)
    end do 
    ! do i=1,n
    !     print*,x(i),table(i,1)
    ! end do

    do i=2,n
        do j=1,n 
            table(j,i) = table(j+1,i-1) - table(j,i-1)
        end do 
    end do 

    ! print the table 
    print*,"The table is "
    do i=1,n 
        
            print*, table(i,:)
      
    end do 

    print*,"Enter the point of interpolation"
    read*,xx

    index = n 
   
    u=(xx-x((index/2)+1))/(x(2)-x(1))
    ! print*,u 

    term = 1
    ans = 0
    !for value of 5
    
    ans = table(3,1) +  (u*(table(2,2)+table(3,2))/2)
    ans=ans + (u*u*(table(2,3))/factorial(2)) 
    ans=ans+(u*(u*u-1)*(table(1,4)+table(2,4))/2)/factorial(3)+ (u*u*(u*u-1)*table(1,5))/factorial(4)

    

    print*,"The interpolation at x:= ",xx, " is " ,ans



    contains
        integer  function factorial(p) result(fact)
            integer :: p 
            if(p==0) then 
                fact = 1 
            else 
                fact=1
                do i=1,p
                    fact=fact*i 
                end do 
            end if 
        end function factorial







end program stirling