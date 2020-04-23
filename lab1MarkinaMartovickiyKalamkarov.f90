program L1_Markina_Martovickiy_Kalamkarov2020

    implicit none
    

    ! Variables

    ! Body of L1_Markina_Martovickiy_Kalamkarov2020
    
    integer :: m = 100000 ! count of iteration

    real :: rn,a,b,s,r,h,rh
    real:: xmax, rmax, zmax, koef 
    real :: xr, yr, zr, md, imk, timeBegin, timeEnd
    integer :: k,i
    
    r = 2 ! radius
    h = 5 ! height
    
    ! calculation of the boundaries of the region
    xmax = 1.2 * r
    rmax = sqrt(xmax**2+xmax**2)
    zmax = h/R*sqrt(xmax**2+xmax**2)
    koef = rmax/zmax
    rh = h * koef
    
    S = 0
    CALL CPU_TIME(timeBegin) ! start time
    
    do i = 0, m
        !gen random number [0;1] for each coords
        call RANDOM_NUMBER(xr) 
        call RANDOM_NUMBER(yr)
        call RANDOM_NUMBER(zr)
        
        ! from [0;1] to [a,b]
        xr = -rh + (rh -(-rh)) * xr
        yr = -rh + (rh -(-rh)) * yr
        zr = h * zr
        
        ! if the point is in volume, then we consider it
        if (((zr**2) <= ((h**2)/(R**2)*((xr**2)+(yr**2)))) .AND. & 
	((h/R*sqrt((xr**2)+(yr**2))) >= 0) .AND. & 
	((h/R*sqrt((xr**2)+(yr**2)) <= h))) then
            S = S +zr
        end if
    end do
    
    CALL CPU_TIME(timeEnd) ! end time
    
    md = (2 * rh)**2*h ! parallelepiped volume
    imk = (md/m)*S ! integral estimation
    print *, 'imk', imk, 'calculation_time:', timeEnd-timeBegin
    
    end program L1_Markina_Martovickiy_Kalamkarov2020
