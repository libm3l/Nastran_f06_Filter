!
!  this is a very simple code which loops over f06 file and \
!  1. extracts grid points by looking at keyword grid
!  2. loops over modes and extract them, each mode to a separate file
!
!  Author: Adam Jirasek
!  Date: July-2020
!
!  inputs are:
!   name of f06 file
!   number of modes 
!
!  outputs are:
!   GRID file containing x,y,z coordinates of the mesh
!   MODE_*.dat file contains  x,y,z (grid coordinates) t1,t2,t3 (eigenmodes) , x1,x2,x3  (GRID coordinates + egienmodes)
!
!  This routine was tested for X-56 f06 file and seems working fine, if you find a bug or have any suggestion 
!  contact Adam.Jirasek@gmail.com
! 


implicit none

character*72 :: name
character*4 :: testvar
character*80 :: text_test
character*80 :: text_line
character :: type

type char
!   character*80 :: text
   integer :: point_id
   real :: x,y,z
   real :: T1,T2,T3
   real :: R1,R2,R3
   character :: type
end type

type line_char
   type(char), allocatable :: line(:)
end type

type(char), allocatable :: lineC(:), lineE(:)
type(line_char), allocatable :: line_per_mode(:)




real :: x,y,z
integer :: i,n, istat, modeline, j, imode, iline, jline, kline, ierr
character*3 :: str


write(*,*)' Name of f06 file '
read(*,*)name

!
!  get number of lines in the file and allocate
!  memory for each line
!
n = 0
open(15,file=name)
do
  read(15,'(a72)',end = 1001)text_test
  n = n + 1
end do
1001 close(15)
write(*,*)' Number of lines is ', n


allocate(lineC(n),lineE(n), stat = istat)
if(istat /= 0)then
  write(*,*)' Error allocating memory'
  stop
end if


open(15,file=name)
open(16,file='GRID')
 iline = 1
 jline = 1
 kline = 0
 imode = 0
do i = 1, n - kline
!   line(i)%text = ' '
!   line(i)%type = ' '
   read(15,'(a80)',end = 1000)text_line
   if(text_line(31:34) == 'GRID')then
!
!  read mesh coordinates, store in lineC data set
!
!  x = text 55:62
!  y = text 63:70
!  z = test 71:80
!
        lineC(iline)%type = 'C'
	read(text_line(55:62),*)lineC(iline)%x
	read(text_line(63:70),*)lineC(iline)%y
	read(text_line(71:80),*)lineC(iline)%z
       	read(text_line(39:45),*)lineC(iline)%point_id

        write(16,*)lineC(iline)%x,lineC(iline)%y,lineC(iline)%z
        modeline = iline
        iline = iline + 1
!
!  find number of modes
!
    else if(text_line(5:19) == 'NO.       ORDER')then
          do 
            read(15,'(a80)',end = 1000)text_line
            kline = kline + 1
            read(text_line(8:16),*, IOSTAT=ierr)imode
            if(ierr /= 0)then
              write(*,*)'Number of modes is ',imode
              exit
            end if
          end do
    else

        read(text_line(21:22),*, err=101, end = 101)type
        if(type == 'G')then
!
!  if G type, this is vector of eigenvalues
!  this will read all eigenvalues and store in lineE
!  there are a number of modes, to distinguish between modes
!  we will loop over index of point "point_id" and once we find it, 
!  we will increment mode number increment 
!
          lineE(i)%type = 'G'
          read(text_line(1:17),*,  err=101)lineE(jline)%point_id
   	  read(text_line(27:40),*, err=101)lineE(jline)%t1
	  read(text_line(42:55),*, err=101)lineE(jline)%t2
	  read(text_line(57:70),*, err=101)lineE(jline)%t3 
          jline = jline + 1

    end if


101 continue

   end if
end do

1000 close(15)
 close(16)

jline = jline - 1


!
!  modeline is a number of surface mesh points
!

!
!  number of grid points is 
!
write(*,*)' number of grid points is ', modeline
!
!  set number of modes for allocation purposes to 31
!  at the moment we do not know how many modes is in the file
!


allocate(line_per_mode(imode), stat = istat)
if(istat /= 0)then
  write(*,*)' Error allocating memory'
  stop
end if


do i=1,imode
 allocate(line_per_mode(i)%line(modeline), stat = istat)
 if(istat /= 0)then
   write(*,*)' Error allocating memory'
   stop
 end if
end do


iline = 0
do i=1,modeline
!
!  if coordinate, look for point_id in 'G' subset identical to this one
!  this point will contain eigenvector for the point_id from 'C' subset
!
    if(lineC(i)%type == 'C')then

      iline = iline + 1
      imode = 1

      do j=1,jline    !modeline+1,n

           if(lineC(i)%point_id == lineE(j)%point_id)then
!
!  point ID, x,y,z coordinates, displacements
!
!              write(9,*)line(j)%point_id, line(i)%x,line(i)%y,line(i)%z,line(j)%t1,line(j)%t2,line(j)%t3

              line_per_mode(imode)%line(iline)%point_id = lineE(j)%point_id
              line_per_mode(imode)%line(iline)%x = lineC(i)%x
              line_per_mode(imode)%line(iline)%y = lineC(i)%y
              line_per_mode(imode)%line(iline)%z = lineC(i)%z
              line_per_mode(imode)%line(iline)%t1 = lineE(j)%t1
              line_per_mode(imode)%line(iline)%t2 = lineE(j)%t2
              line_per_mode(imode)%line(iline)%t3 = lineE(j)%t3
!
!  increase mode counter, there will be eventually as many identical point_id points
!  as modes, for each mode 1 point
!
              imode = imode + 1
 
           end if

      end do
    end if
end do

imode = imode -1 

do i=1,imode
  write(str, '(i3.3)')i
  name = 'Mode_'//str//'.dat'
  write(*,*)' Saving data for mode ', i,', in file: ', name
  open(15, file = trim(name))

  do j=1,modeline

!
!  save data
!  3 grid coordinates, 3 eigenmodes, and three coordinates of deformed grid
!  x,y,z , t1,,t2,t3
!
    write(15,*)line_per_mode(i)%line(j)%x,line_per_mode(i)%line(j)%y,line_per_mode(i)%line(j)%z,line_per_mode(i)%line(j)%t1,&
         line_per_mode(i)%line(j)%t2,line_per_mode(i)%line(j)%t3, line_per_mode(i)%line(j)%x + line_per_mode(i)%line(j)%t1, &
         line_per_mode(i)%line(j)%y+line_per_mode(i)%line(j)%t2, line_per_mode(i)%line(j)%z+line_per_mode(i)%line(j)%t3

  end do
  close(15)

end do


deallocate(lineC, lineE, stat = istat)
if(istat /= 0)then
  write(*,*)' Error deallocating memory'
  stop
end if

do i=1,imode
 deallocate(line_per_mode(i)%line, stat = istat)
 if(istat /= 0)then
   write(*,*)' Error allocating memory'
   stop
 end if
end do

deallocate(line_per_mode, stat = istat)
if(istat /= 0)then
  write(*,*)' Error deallocating memory'
  stop
end if
end
