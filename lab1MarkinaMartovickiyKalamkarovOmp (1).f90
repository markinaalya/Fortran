program L1_Markina_Martovickiy_Kalamkarov_OMP2020

    implicit none
    include 'omp_lib.h'
    ! Variables

    ! Body of L1_Markina_Martovickiy_Kalamkarov_OMP2020
    
 integer :: m = 100000 ! count of iteration

    real :: rn,a,b,s,r,h,rh
    real:: xmax, rmax, zmax, koef
    real :: xr, yr, zr, md, imk,timeBegin,timeEnd
    integer :: k,i
    
    r = 2
    h = 5
    xmax = 1.2 * r
    rmax = sqrt(xmax**2+xmax**2)
    zmax = h/R*sqrt(xmax**2+xmax**2)
    
    koef = rmax/zmax
    rh = h * koef
    S = 0
    timeBegin = omp_get_wtime()
    !$OMP parallel default(shared) private(xr, yr, zr)
    !$OMP DO
    do i = 0, m
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
    !$OMP END DO
    !$OMP end parallel
    timeEnd = omp_get_wtime()
    md = (2 * rh)**2*h
    imk = (md/m)*S
    print *, 'imk', imk+50, 'calculation_time:', timeEnd-timeBegin
    
    end program L1_Markina_Martovickiy_Kalamkarov_OMP2020
