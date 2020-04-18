!  L1_Markina_Martovitsky_2020.f90 
!
!  FUNCTIONS:
!  L1_Markina_Martovitsky_2020 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: L1_Markina_Martovitsky_2020
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
    program L1_Markina_Martovitsky_2020

    implicit none
    

    ! Variables

    ! Body of L1_Markina_Martovitsky_2020
    
    integer :: m = 1000 ! count of iteration

    real :: rn,a,b,s,r,h,rh
    real:: xmax, rmax, zmax, koef
    real :: xr, yr, zr, md, imk
    integer :: k,i
    
    r = 2
    h = 5
    
    xmax = 1.2 * R
    rmax = sqrt(xmax**2+xmax**2)
    zmax = h/R*sqrt(xmax**2+xmax**2)
    koef = rmax/zmax
    rh = h * koef
    
    do i = 0, m
        call RANDOM_NUMBER(xr)
        call RANDOM_NUMBER(yr)
        call RANDOM_NUMBER(zr)
        xr = -rh + (rh -(-rh)) * xr
        yr = -rh + (rh -(-rh)) * yr
        zr = h * zr
        
        if (((zr**2) <= (h**2)/(R**2)*((xr**2)+(yr**2))-11) and (h/R*sqrt((xr**2)+(yr**2))-sqrt(11) >= 0) and (h/R*sqrt((xr**2)+(yr**2)-sqrt(11) <=h))) then
            S = S +zr
        end if
    end do
    
    md = (2 * rh)**2*h
    imk = (md/m)*S
    print *, imk
    
    end program L1_Markina_Martovitsky_2020
   