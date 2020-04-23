program L1_Markina_Martovickiy_Kalamkarov_MPI2020

	implicit none

	include 'mpif.h'

	integer :: m = 100000 ! count of iteration
	integer, dimension(MPI_STATUS_SIZE) :: status
	real :: rn,a,b,s,r,h,rh,s1,s2  
	real:: xmax, rmax, zmax, koef, timeBegin, timeEnd
	real :: xr, yr, zr, md, imk, eleven
	integer :: k,i,process_Rank, size_Of_Cluster, ierror, j

	call MPI_INIT(ierror) ! init MPI
	call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror) ! get count of process
	call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror) ! get id of current process
    
	r = 2
	h = 5
	eleven = 11

	xmax = 1.2 * r
	rmax = sqrt(xmax**2+xmax**2)
	zmax = h/R*sqrt(xmax**2+xmax**2)

	koef = rmax/zmax
	rh = h * koef
    
    ! Send data to other process
    ! if main, then sent to all of process
	if(process_Rank == 0) then
		print *, 'Main send'
		S = 0
		CALL CPU_TIME(timeBegin)
		if(size_Of_Cluster > 1) then 
			do i = 1, size_Of_Cluster-1
				print *, 'Main send',i
				call MPI_SSend(s,1,MPI_FLOAT, i, 100, MPI_COMM_WORLD, ierror)
			end do
		call MPI_BARRIER(MPI_COMM_WORLD, ierror) ! we are waiting for data to be sent by all
		end if
    else 
        ! if not main, then recv from main (0)
		print *, process_Rank,'receive'
		call MPI_Recv(s,1,MPI_FLOAT, 0,100, MPI_COMM_WORLD,status, ierror)
		call MPI_BARRIER(MPI_COMM_WORLD, ierror) ! we are waiting for data to be received by all
		print *, s
	end if


	do j = 0, floor(real(m/size_Of_Cluster))
		call RANDOM_NUMBER(xr)
		call RANDOM_NUMBER(yr)
		call RANDOM_NUMBER(zr)
		xr = -rh + (rh -(-rh)) * xr
		yr = -rh + (rh -(-rh)) * yr
		zr = h * zr
		if (((zr**2) <= ((h**2)/(R**2)*((xr**2)+(yr**2)))) .AND. & 
			((h/R*sqrt((xr**2)+(yr**2))) >= 0) .AND. & 
			((h/R*sqrt((xr**2)+(yr**2)) <= h))) then
			S = S +zr
		end if
	end do
	print *, 'Nomer processa', process_Rank
	call MPI_BARRIER(MPI_COMM_WORLD, ierror)
	print *, 'barrier', process_Rank


    ! Resv data 
    ! if main, then resv from all of process
	if(process_Rank == 0) then
		print *, 'Main process'
		if(size_Of_Cluster > 1) then
			do i= 1, size_Of_Cluster-1
			print *, 'Main receive from', i
			call MPI_Recv(s1,1,MPI_FLOAT, i,200, MPI_COMM_WORLD,status, ierror) ! resv to s1
			s = s + s1 ! sum resv s1 and s from main
			end do
			md = (2 * rh)**2*h
			imk = (md/m)*S
			CALL CPU_TIME(timeEnd)
			print *, 'imk', imk, 'process_Rank:', process_Rank, 'calculation_time:', timeEnd-timeBegin
		end if
    else
    ! if not main, then send to main
			print *, 'Send to main'
			call MPI_SSend(s,1,MPI_FLOAT, 0, 200, MPI_COMM_WORLD, ierror)
    end if
	call MPI_FINALIZE(ierror)
	

end program L1_Markina_Martovickiy_Kalamkarov_MPI2020
