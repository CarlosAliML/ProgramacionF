Module constantes
implicit none
real, parameter :: dgt = (4.0*atan(1.0))/180
real, parameter :: pi = 4.0*atan(1.0)
integer, parameter :: npts = 6500

real, parameter :: aire = 1.29
real, parameter :: esfera = 0.47
real, parameter :: medesfera = 0.42
real, parameter :: cono = 0.5
real, parameter :: cubo = 1.05
real, parameter :: cuboa = 0.8
real, parameter :: cilindrol = 0.82
real, parameter :: cilindroc = 1.15
end module constantes

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Subroutine confriccion (xini,yini,vini,angini,xmaxf,tf,ymaxf)
use constantes
implicit none
integer :: i
character :: object
real, dimension (0:npts) :: z,w,u,vz,vw,az,aw
real :: xini, yini, vini,angini
real :: xmaxf, ymaxf, tf
real :: ad,area,radio,cd,masa

Print *, "Ingresa la masa del cuerpo (kg) (reales)"
read *, masa
Print *, "Selecciona la masa del cuerpo"
Print *, "1.-Esfera"
Print *, "2.-Media esfera"
Print *, "3.-Cono"
Print *, "4.-Cubo"
Print *, "5.-Cubo angulado"
Print *, "6.-Cilindro largo"
Print *, "7.-Cilindro corto"
read *, object

Select case (object)
   case ("1")
      Print *, "Ingresa el radio de la esfera"
      read *, radio
      area = pi*radio*radio
      cd = esfera
   case ("2")
      Print *, "Ingresa el radio de la esfera"
      read *, radio
      area = pi*radio*radio*(1.0/2.0)
      cd = medesfera
   case ("3")
      Print *, "Ingresa el radio del cono"
      read *, radio
      area = pi*radio*radio*(1.0/3.0)
      cd = cono
   case ("4")
      Print *, "Ingresa la medida del lado del cubo"
      read *, radio
      area = radio*radio
      cd = cubo
   case ("5")
      Print *, "Ingresa la medida del lado del cubo"
      read *, radio
      area = radio*radio*sqrt(2.0)
      cd = cuboa
  case ("6")
      Print *, "Ingresa el radio del cilindro"
      read *, radio
      area = radio*radio*pi
      cd = cilindrol
   case ("7")
      Print *, "Ingresa el radio del cilindro"
      read *, radio
      area = radio*radio*pi
      cd = cilindroc
   case default
      Print *, "Error, comando no definido"
end select

z(0) = xini
w(0) = yini
vz(0) = vini*COS(angini)
vw(0) = vini*SIN(angini)
ad = (0.5*aire*area*cd)/masa
az(0) = -ad*vz(0)*vz(0)
aw(0) = 9.8-(ad*vw(0)*vw(0))
u(0) = 0

OPEN (2, FILE="confriccion.dat")
WRITE (2,1001) z(0),w(0)
1001 FORMAT (f11.5,f11.5)

DO i=0, npts, 1
  u(i+1) = u(i) + 0.01
  vz(i+1) = vz(i)+az(i)*u(i+1)
  vw(i+1) = vw(i)+aw(i)*u(i+1)
  az(i+1) = -ad*vz(i)*vz(i)
  aw(i+1) = -9.8-(ad*vz(i)*vz(i))
  z(i+1) = z(i)+vz(i)*u(i+1)+(0.5*az(i)*u(i+1)*u(i+1))
  w(i+1) = w(i)+vw(i)*u(i+1)+(0.5*aw(i)*u(i+1)*u(i+1))
  WRITE (2,*) z(i+1), w(i+1)
  IF (w(i+1)<0) EXIT
END DO
CLOSE (2)

xmaxf = z(i)
ymaxf = MAXVAL(w)
tf = u(i)*10.0

END SUBROUTINE confriccion

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Subroutine sinfriccion (xini,yini,vini,angini,xmaxsf,ymaxsf,tsf)!use "sf" para simbolizar "sin friccion"
use constantes
implicit none
INTEGER :: i
Real, dimension (1:npts) :: x,y,t
REAL :: xini, yini, vini, angini   
REAL :: xmaxsf, ymaxsf, tsf    

angini=angini*dgt

xmaxsf = xini+((vini*vini+SIN(2*angini))/(9.8))
ymaxsf = yini+(((vini*vini)*(SIN(angini)*SIN(angini)))/(19.6))
tsf = (2*vini*SIN(angini))/(9.8)

open (1, file="sinfriccion.dat")

DO i=1, npts, 1
t(i)=FLOAT(i)*0.01
x(i) = xini + (vini*COS(angini)*t(i))
y(i) = yini + (vini*SIN(angini)*t(i)) - (4.9*t(i)*t(i))
WRITE (1,1001) x(i), y(i)
1001 FORMAT (f11.5,f11.5)
IF (y(i)<0) EXIT
END DO
CLOSE (1)

END SUBROUTINE sinfriccion

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

program projectilfriccion
use constantes  
       implicit none  
       real :: xini,yini,vini,angini
       real :: xmaxsf,ymaxsf,tsf,xmaxf,ymaxf,tf
       real :: diferencia 
       
       write(*,*) 'Ingresa la "x" inicial, la "y" inicial, la velocidad inicial, y el angulo inicial, respectivamente'   
       read *, xini, yini, vini, angini
   
       call sinfriccion (xini,yini,vini,angini,xmaxsf,ymaxsf,tsf)
       call confriccion  (xini,yini,vini,angini,xmaxf,ymaxf,tf)
       diferencia = ((xmaxsf-xmaxf)/xmaxf) * 100.0


write(*,*) '°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°'
       write(*,*) 'Datos que usted ingreso:'
       write(*,*) 'Velocidad inicial:', vini,'m/s'
       write(*,*) 'Angulo de tiro:', angini,'radianes'
write(*,*) '.'
write(*,*) '.'
write(*,*) '.'
       write(*,*) 'Sin friccion'
       write(*,*) 'La altura maxima (y) es de: ', ymaxsf,'m'
       write(*,*) 'El tiempo total en el aire fue de:', tsf,'s'
       write(*,*) 'El alcance maximo (x) fue de:', xmaxsf,'m'
write(*,*) '.'
write(*,*) '.'
write(*,*) '.'
       write(*,*) 'Con friccion'
       write(*,*) 'La altura maxima (y) es de: ', ymaxf,'m'
       write(*,*) 'El tiempo total en el aire fue de:', tf,'s'
       write(*,*) 'El alcance maximo (x) fue de:', xmaxf,'m'
write(*,*) '.'
write(*,*) '.'
write(*,*) '.'
       write(*,*) 'La altura maxima (y) es de: La diferencia entre considerar y no considerar la friccion es de', diferencia,'%'
write(*,*) '°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°~~~~~°'
  end program projectilfriccion 
