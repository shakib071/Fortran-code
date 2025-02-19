DIMENSION ARRAY(100,100)

INTEGER  I,J

DO I=1,3
    DO J = 1,3
        READ*,ARRAY(I,J)
    END DO
END DO

DO I=1,3
    WRITE(*,*)
    DO J=1,3
        WRITE(*,"(F10.0)",ADVANCE='NO') ARRAY(I,J)
    END DO
end do

STOP
END
