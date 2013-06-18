program numPE
include 'mpif.h'

!MPI integer vars
integer :: errcode, PE


!MPI init stuff
call MPI_INIT(errcode)
call MPI_COMM_RANK(MPI_COMM_WORLD, PE, errcode)



!Clean up
call MPI_FINALIZE(errcode)

end program
