program Mareas

implicit none
real, dimension (7674):: Altura
integer :: i
real :: perro, Maxd1, Maxd2, Maxd3, Max1, Max2, Max3, Max4, Max5
real :: gato, Mind1, Mind2, Mind3, Min1, Min2, Min3, Min4, Min5
real :: Tiempo11, Tiempo21, Tiempo31, Tiempo41, Tiempo51
real :: Tiempo12, Tiempo22, Tiempo32, Tiempo42, Tiempo52

open (1,file="Mareas.csv")

do i=1,7674
read (1,*) Altura(i)
end do
close (1)

!--------------------------------------------------------------------------------------------------------------------------------------
!Maximas y minimas del primer mes (Valores del 1 al 1344)

Max1 = 0
do i=1,1344
perro = Max1-Altura(i)
if (perro<0) then 
Max1 = Altura(i)

Tiempo11=i/48.0
end if
end do

Min1 = 0
do i=1,1344
gato = Min1-Altura(i)
if (gato>0) then
Min1 = Altura(i)

Tiempo12=i/48.0
end if
end do


!----------------------------------------------------------------
!Maximas y minimas del segundo mes 

Max2=0
do i=1345,2689
perro= Max2-Altura(i)
if (perro<0) then 
Max2 = Altura(i)

Tiempo21=i/48.0
end if
end do

Min2 = 0
do i=1345,2689
gato = Min2-Altura(i)
if (gato>0) then
Min2 = Altura(i)

Tiempo22=i/48.0
end if
end do


!----------------------------------------------------------------
!Maximas y minimas del tercer mes

Max3=0
do i=2690,4034
perro= Max3-Altura(i)
if (perro<0) then 
Max3 = Altura(i)

Tiempo31=i/48.0
end if
end do

Min3 = 0
do i=2690,4034
gato = Min3-Altura(i)
if (gato>0) then
Min3 = Altura(i)

Tiempo32=i/48.0
end if
end do


!----------------------------------------------------------------
!Maximas y minimas del cuarto mes

Max4=0
do i=4035,5379
perro= Max4-Altura(i)
if (perro<0) then 
Max4 = Altura(i)

Tiempo41=i/48.0
end if
end do

Min4 = 0
do i=4035,5379
gato = Min4-Altura(i)
if (gato>0) then
Min4 = Altura(i)

Tiempo42=i/48.0
end if
end do


!----------------------------------------------------------------
!Maximas y minimas del quinto mes

Max5=0
do i=5380,6724
perro= Max5-Altura(i)
if (perro<0) then 
Max5 = Altura(i)

Tiempo51=i/48.0
end if
end do

Min5 = 0
do i=5380,6724
gato = Min5-Altura(i)
if (gato>0) then
Min5 = Altura(i)

Tiempo52=i/48.0
end if
end do

!------------------------------------------------------------------------------------------------------------------------------------------------------
!Maximas y minimas de cada dia
!------------------------------------------------------------------------------------------------------------------------------------------------------


Maxd1 = 0
do i=1,48
perro = Maxd1-Altura(i)
if (perro<0) then 
Maxd1 = Altura(i)
end if
end do

Mind1 = 0
do i=1,48
gato = Mind1-Altura(i)
if (gato>0) then
Mind1 = Altura(i)
end if
end do


!----------------------------------------------------------------


Maxd2=0
do i=49,97
perro= Maxd2-Altura(i)
if (perro<0) then 
Maxd2 = Altura(i)
end if
end do

Mind2 = 0
do i=49,97
gato = Mind2-Altura(i)
if (gato>0) then
Mind2 = Altura(i)
end if
end do


!----------------------------------------------------------------

Maxd3=0
do i=98,146
perro= Maxd3-Altura(i)
if (perro<0) then 
Maxd3 = Altura(i)
end if
end do

Mind3 = 0
do i=98,146
gato = Mind3-Altura(i)
if (gato>0) then
Mind3 = Altura(i)
end if
end do



Print *, '================°================°====================°================°'
Print *, 'ALTURAS MAXIMAS DE LAS MAREAS:'
Print *, '================°================°====================°================°'
Print *, 'Primer dia:', Maxd1
Print *, 'Segundo dia:', Maxd2
Print *, 'Tercer dia:', Maxd3
Print *, '========================================================================'
Print *, 'Primer mes:', Max1, 'En el dia:', Tiempo11
Print *, '---------------------------------------------------------'
Print *, 'Segundo mes:', Max2,'En el dia:', Tiempo21
Print *, '---------------------------------------------------------'
Print *, 'Tercer mes:', Max3,'En el dia:', Tiempo31
Print *, '---------------------------------------------------------'
Print *, 'Cuarto mes:', Max4,'En el dia:', Tiempo41
Print *, '---------------------------------------------------------'
Print *, 'Quinto mes:', Max5,'En el dia:', Tiempo51
Print *, '================°================°====================°================°'
Print *, 'ALTURAS MINIMAS DE LAS MAREAS:'
Print *, '================°================°====================°================°'
Print *, 'Primer dia:', Maxd1
Print *, 'Segundo dia:', Maxd2
Print *, 'Tercer dia:', Maxd3
Print *, '========================================================================'
Print *, 'Primer mes:', Min1, 'En el dia:', Tiempo12
Print *, '---------------------------------------------------------'
Print *, 'Segundo mes:', Min2,'En el dia:', Tiempo22
Print *, '---------------------------------------------------------'
Print *, 'Tercer mes:', Min3,'En el dia:', Tiempo32
Print *, '---------------------------------------------------------'
Print *, 'Cuarto mes:', Min4,'En el dia:', Tiempo42
Print *, '---------------------------------------------------------'
Print *, 'Quinto mes:', Min5,'En el dia:', Tiempo52
Print *, '================°================°====================°================°'

end program Mareas
