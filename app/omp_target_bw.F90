! omp_target_bw.F90 -- OpenMP Multi-GPU Bandwidth Test
!
! To compile, use:
!   nvfortran -mp=gpu -o omp_target_bw omp_target_bw.f90
!   ifx -fiopenmp -fopenmp-targets=spir64 -o omp_target_bw omp_target_bw.f90
!   ftn -homp -o omp_target_bew omp_target_bw.f90
!
program omp_target_bw
use, intrinsic :: iso_c_binding, only: c_size_t, c_loc
!use, intrinsic :: omp_lib
use omp_lib

implicit none (type, external)

! We assume wp is equal to the size in bytes
! (not the case on the NAG Fortran compiler)
integer, parameter :: wp = kind(1.0e0)

integer, parameter :: N = 128*1024*1024

type :: shard
  real(wp), allocatable :: a(:)
end type

type(shard), allocatable, target :: sa(:)
real(wp), allocatable :: bw(:,:)
integer :: i, j, istat, ndevices

real(wp), pointer :: ptr(:) => null()

real(kind(1.0d0)) :: tbegin, elapsed

!$omp target
!$omp end target


ndevices = omp_get_num_devices()
write(*,'("Number of OpenMP devices: ", I0)') ndevices
write(*,'("Host device ID: ", I0)') omp_get_initial_device()

allocate(sa(0:ndevices-1))

do i = 0, ndevices - 1
    allocate(sa(i)%a(N), source=real(i,wp))
    associate(a => sa(i)%a)
       ptr => sa(i)%a
       !$omp target enter data map(to: ptr) device(i)
    end associate
end do

allocate(bw(0:ndevices-1,0:ndevices-1))
bw = -1.0_wp

do i = 0, ndevices-1  ! Receiver
   do j = 0, ndevices-1  ! Sender
    if (j == i) cycle

!    print *, "S: ", j, ", R: ", i

    tbegin = omp_get_wtime()

    istat = omp_target_memcpy( &
        omp_get_mapped_ptr(c_loc(sa(i)%a),i), &
        omp_get_mapped_ptr(c_loc(sa(j)%a),j), &
        N*int(wp,c_size_t), 0_c_size_t, 0_c_size_t,i,j)

    if (istat /= 0) error stop "omp_target_memcpy failed"

    elapsed = omp_get_wtime() - tbegin
!    print *, "elapsed = ", elapsed

    associate(a => sa(i)%a)
       !$omp target update from(a) device(i)

#if DEBUG
       print *, "Receiver i = ", i
       print *, "a (sent)     = ", sa(j)%a
       print *, "a (received) = ", a
       print *, "S/R equal?     ", a == sa(j)%a
       print *, "equal to J?    ", a == j
       print *, "all equal to J? ", all(a == j)
##endif

        if (all(a == j)) then
            bw(j,i) = real(N*wp/elapsed, wp)
        else
            write(*,'(A,I0,"-",I0)') "Error sending between device pair: ", i, j
            print *, "Diff: ", maxval( a - j )
            print *, "Diff (CPU): ", maxval(abs(sa(i)%a - sa(j)%a))
            error stop
        end if
    end associate

   end do

   associate(a => sa(i)%a)
    a = i
    !$omp target update to(a) device(i)
   end associate
end do

if (ndevices == 0) stop 0

write(*,"(/,'Bandwidth (GB/s) for transfer size (MB): ',F9.3,/)") &
    N*real(wp, wp)/1024.0_wp**2

write (*,"(' S\R',5x,*(i3,5x))") (i, i=0,nDevices-1)

#if 0
  do j = 0, ndevices-1
     write(*,"(i3)", advance='no') j
     do i = 0, ndevices-1
        if (i == j) then
           write(*,"(4x,'-',3x)", advance='no')
        else
           write(*,"(f8.2)",advance='no') bw(j,i)/1024.0d0**3
        end if
     end do
     write(*,*)
  end do
#else
  do j = 0, nDevices-1
     write(*,"(i3)", advance='no') j
     do i = 0, nDevices-1
        if (i == j) then
           write(*,"(4x,'-',3x)", advance='no')
        else
           write(*,"(f8.2)",advance='no') bw(j,i)/1024.0d0**3
        end if
     end do
     write(*,*)
  end do
#endif

! print *, "Raw BW (GB/s): "
! print *, bw / 1024.0d0**3

write(*,'(/,"D2H Bandwith (GB/s)",/)')
block
   do i = 0, ndevices-1
       tbegin = omp_get_wtime()
       associate(a => sa(i)%a)
       !$omp target update from(a) device(i)
       end associate
       elapsed = omp_get_Wtime() - tbegin

       write(*,'(I3,F8.2)') i, real(N,wp)*wp/elapsed/1024.0_wp**3
   end do
end block

end program
