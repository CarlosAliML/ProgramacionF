program Mareas

implicit none
real, dimension (7674):: Altura
integer :: i
real :: perro, Max1, Max2, Max3, Max4, Max5
real :: Tiempo1, Tiempo2, Tiempo3, Tiempo4, Tiempo5

open (1,file="Mareas.csv")

do i=1,7674
read (1,*) Altura(i)
end do
close (1)

Max1 = 0
do i=1,1344
perro = Max1-Altura(i)
if (perro<0) then 
Max1 = Altura(i)

Tiempo1=i/48.0
end if
end do

Max2=0
do i=1345,2690
perro= Max2-Altura(i)
if (perro<0) then 
Max2 = Altura(i)

Tiempo2=i/48.0
end if
end do


Max3=0
do i=2691,4035
perro= Max3-Altura(i)
if (perro<0) then 
Max3 = Altura(i)

Tiempo3=i/48.0
end if
end do

Max4=0
do i=4035,5380
perro= Max4-Altura(i)
if (perro<0) then 
Max4 = Altura(i)

Tiempo4=i/48.0
end if
end do


Max5=0
do i=5380,6725
perro= Max5-Altura(i)
if (perro<0) then 
Max5 = Altura(i)

Tiempo5=i/48.0
end if
end do






Print *, '~~~~~~~~~~~~~~~~°~~~~~~~~~~~~~~~~°~~~~~~~~~~~~~~~~~~~~°~~~~~~~~~~~~~~~~°'
Print *, 'Las alturas maximas son:'
Print *, '~~~~~~~~~~~~~~~~°~~~~~~~~~~~~~~~~°~~~~~~~~~~~~~~~~'
Print *, 'Primer mes:', Max1, 'En el dia:', Tiempo1
Print *, '---------------------------------------------------------'
Print *, 'Segundo mes:', Max2,'En el dia:', Tiempo2
Print *, '---------------------------------------------------------'
Print *, 'Tercer mes:', Max3,'En el dia:', Tiempo3
Print *, '~~~~~~~~~~~~~~~~°~~~~~~~~~~~~~~~~°~~~~~~~~~~~~~~~~~~~~°~~~~~~~~~~~~~~~~°'


end program Mareas
