  program projectile_plot  
       implicit none  
       real, parameter :: pi = 4.0*atan(1.0) 
       real :: v, a, r, t, ym, xm, vx, vy
       real, dimension(1:3000) :: x,y 
       integer :: i 

       write(*,*) 'Ingresa el angulo inicial del proyectil en grados (Real)'   
       read *, a   
       write(*,*) 'Ingresa la velocidad inicial del proyectil en m/s (Real)'   
       read *, v   
       r = a*pi/180.0
     
       vx = (v)*cos(r)
       vy = (v)*sin(r)

       open(1, file='Tiro.dat')
       y = 0
       x = 0
       
       do i=1,3000, 1   
            t = (float(i)*0.01)   
            x(i) = vx*t   
            y(i) = vy*t -(4.9*t*t)
     
            write(1,*) x(i), y(i)      
            if (y(i)<0) exit   
       end do

       close(1)   

       ym = (vy**2)/(19.6)
       xm = x(i)

       if (vx<0) then 
       xm = 0
       end if

       write(*,*) '°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°'
       write(*,*) 'La altura maxima (y) es de: ', ym,'m'
       write(*,*) 'El tiempo total en el aire fue de:', t,'s'
       write(*,*) 'El alcance maximo (x) fue de:', xm,'m'
       write(*,*) 'Datos que usted ingreso:'
       write(*,*) 'Velocidad inicial:', v,'m/s'
       write(*,*) 'Angulo de tiro:', a,'° (', r,'radianes)'
       write(*,*) '°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°'
  end program projectile_plot 
