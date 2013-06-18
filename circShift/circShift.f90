program circShift
include 'mpif.h'

integer :: my_pe_num,errcode, N_pe
integer :: i 
real, dimension(3) :: incNum

integer :: status(MPI_STATUS_SIZE)

incNum = 0

call MPI_INIT(errcode)    

call MPI_COMM_RANK(MPI_COMM_WORLD, my_pe_num, errcode)
if (my_pe_num.eq.0) then
    call MPI_Send(incNum, 3, MPI_REAL,1,1,MPI_COMM_WORLD)
end if

call MPI_Comm_size(MPI_COMM_WORLD, N_pe)

do i=1,N_pe
    if (my_pe_num.eq.i) then
        call MPI_Recv(incNum,3,MPI_REAL,i-1,1, MPI_COMM_WORLD, &
            status,errcode)
        incNum = incNum + 1
        call MPI_Send(incNum, 3, MPI_REAL,mod(i+1,N_pe),1,MPI_COMM_WORLD)
    end if
end do

if (my_pe_num.eq.0) then
    call MPI_Recv(incNum,3,MPI_REAL,N_pe-1,1, MPI_COMM_WORLD,&
        status,errcode)
    incNum = incNum +1
    print*, incNum
end if


call MPI_FINALIZE(errcode)

end program
