module streaming_kernels
implicit none

integer, parameter :: dp = kind(1.0d0)

!
! BLAS Level 1 Routines
!
interface
    subroutine daxpy(n,alpha,x,incx,y,incy)
        integer, intent(in) :: n, incx, incy
        double precision, intent(in) :: alpha, x(*)
        double precision, intent(inout) :: y(*)
    end subroutine
#if HAVE_DAXPBY
    subroutine daxpby(n,alpha,x,incx,beta,y,incy)
        integer, intent(in) :: n, incx, incy
        double precision, intent(in) :: alpha, x(*), beta
        double precision, intent(inout) :: y(*)
    end subroutine
#endif
    function ddot(n,x,incx,y,incy)
        integer, intent(in) :: n, incx, incy
        double precision, intent(in) :: x(*), y(*)
        double precision :: ddot
    end function
    subroutine dcopy(n,x,incx,y,incy)
        integer, intent(in) :: n, incx, incy
        double precision, intent(in) :: x(*)
        double precision, intent(inout) :: y(*)
    end subroutine
    subroutine dscal(n,alpha,x,incx)
        integer, intent(in) :: n, incx
        double precision, intent(in) :: alpha
        double precision, intent(inout) :: x(*)
    end subroutine
end interface

contains

#if defined(SK_BLAS)
#include "bs_kernels_blas.fi"
#elif defined(SK_LOOPS)
#include "bs_kernels_loops.fi"
#elif defined(SK_OMP_PARALLEL_DO)
#include "bs_kernels_omp.fi"
#elif defined(SK_OMP_TARGET_LOOP)
#include "bs_kernels_omp_target.fi"
#else
#include "bs_kernels.fi"
#endif

end module


