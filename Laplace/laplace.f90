program numPE
include 'mpif.h'

!MPI integer vars
integer :: errcode, PE
integer, parameter :: NPE=4

!Time related variables
integer:: t

!Grid related variables
integer, parameter :: N = 1000
integer :: N_2 = N/NPE+2
real, dimension(:,:), allocatable :: temperature
integer :: i,j

!Physics related variables
real, parameter :: maxTemp = 100
real :: tempInc = maxTemp/N

integer :: status(MPI_STATUS_SIZE)

!MPI init stuff
call MPI_INIT(errcode)
call MPI_COMM_RANK(MPI_COMM_WORLD, PE, errcode )

!Set up array, bigger if not on ends
if ((PE.eq.0).or.(PE.eq.NPE-1)) then
    N_2 = N_2 - 1
end if
allocate(temperature(N_2,N))

! Set initial conditions
call initConditions()

! Iterate for 1000 time steps
do t=1,1000
    !Get other PE data
    call getRepeatData()

    !Do the physics
    call evolveTemperature()

    !Update the boundaries
    call setBoundaries()

    !Output something for check
    call printMe()
end do



!Clean up
call MPI_FINALIZE(errcode)

contains

subroutine initConditions()
    temperature = 0

    call setBoundaries()
end subroutine

subroutine setBoundaries()

    temperature(:,N) = 0

    do i = 1, N_2
        temperature(i,1) = temp_i
        temp_i = temp_i + tempInc
    end do

    if (PE.eq.0) then
        temp_i = 0
        do i=1,N
            temperature(1,i) = temp_i
            temp_i = temp_i + tempInc
        end do
    end if

    if (PE.eq.NPE) then
        temperature(N_2,:) = 0
    end if

end subroutine

subroutine evolveTemperature()
    do j = 2,N-1
        do i = 2,N_2-1
            temperature(i,j) = .25*(temperature(i-1,j)+&
                temperature(i+1,j)+temperature(i,j-1)+temperature(i,j+1))
        end do
    end do
end subroutine

subroutine getRepeatData()
    real, dimension(N) :: tempN
    tempN = 0

    if (PE.eq.0) then
        ! Sending to below
        tempN = temperature(N_2-1,:)
        call MPI_Send( tempN, N, MPI_REAL, 1, 1, MPI_COMM_WORLD,errorcode)
        ! Listening from below
        call MPI_Recv( tempN, N, MPI_REAL, 1, 1, MPI_COMM_WORLD,status,errorcode) 
        temperature(N_2,:) = tempN
    else if (PE.eq.NPE-1) then
        !Sending to above
        tempN = temperature(2,:)
        call MPI_Send( tempN, N, MPI_REAL, NPE-2, 1, MPI_COMM_WORLD,errorcode)
        !Listening from above
        call MPI_Recv( tempN, N, MPI_REAL, NPE-2, 1, MPI_COMM_WORLD,status,errorcode) 
        temperature(1,:)=tempN
    else
        ! send to below
        tempN = temperature(N_2-1,:)
        call MPI_Send(tempN,N,MPI_REAL, PE+1,1,MPI_COMM_WORLD,errorcode)
        !Listen from above
        call MPI_Recv( tempN, N, MPI_REAL, PE-1, 1, MPI_COMM_WORLD,status,errorcode) 
        temperature(1,:) = tempN

        ! Send to above
        tempN = temperature(1,:)
        call MPI_Send(tempN,N,MPI_REAL,PE-1,1,MPI_COMM_WORLD,errorcode)
        ! Listen from below
        call MPI_Recv( tempN, N, MPI_REAL, PE+1, 1, MPI_COMM_WORLD,status,errorcode) 
        temperature(N_2,:) = tempN
    end if
end subroutine

subroutine printMe()
    real, dimension(N) :: trace
    real, dimension(N/NPE) :: temp
    if (PE.eq.0) then
        do i=1,NPE-1
            
            call MPI_Recv( temp, N/NPE, MPI_REAL,&
                i,2,MPI_COMM_WORLD,status,errocode)
            do j =1,N/NPE
                trace(j+PE*N/NPE) = temp(j)
            end do
        end do
        do i = 1,N/NPE
            trace(i) = temperature(i,i)
        end do

        print*, trace
    else
        do i = 1,N/NPE
            temp(i) = temperature(i+1,PE*N/NPE+i)
        end do
        call MPI_send(temp,N/NPE,MPI_REAL,0,2,MPI_COMM_WORLD,errorcode)
    end if

end subroutine

end program
