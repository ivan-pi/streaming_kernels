module streaming_kernels

#if defined(_OPENMP)
use omp_lib
#endif

implicit none
private

public :: dp, print_config
public :: bs1, bs2, bs3, bs4, bs5

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
    subroutine dcopy(n,x,incx,y,incy)
        integer, intent(in) :: n, incx, incy
        double precision, intent(in) :: x(*)
        double precision, intent(inout) :: y(*)
    end subroutine
    function ddot(n,x,incx,y,incy)
        integer, intent(in) :: n, incx, incy
        double precision, intent(in) :: x(*), y(*)
        double precision :: ddot
    end function
    function dnrm2(n,x,incx)
        integer, intent(in) :: n, incx
        double precision, intent(in) :: x(*)
        double precision :: dnrm2
    end function
    subroutine dscal(n,alpha,x,incx)
        integer, intent(in) :: n, incx
        double precision, intent(in) :: alpha
        double precision, intent(inout) :: x(*)
    end subroutine
end interface

#if HAVE_NEON
interface
    ! dot_product_neon_fma_unroll8_pow2_tail
    function dot_product_neon(a,b,n) bind(c,name="dot_product_neon_fma_unroll8_pow2_tail_simple")
    !function dot_product_neon(a,b,n) bind(c,name="dot_product_neon_fma_unroll16_pow2_tail")
        use, intrinsic :: iso_c_binding
        integer(c_size_t), value :: n
        real(c_double), intent(in) :: a(n), b(n)
        real(c_double) :: dot_product_neon
    end function
    function squared_norm_neon(a,n) bind(c,name="squared_norm_neon_fma_unroll8_pow2_tail_simple")
    !function squared_norm_neon(a,n) bind(c,name="neon_squared_norm_unroll4")
        use, intrinsic :: iso_c_binding
        integer(c_size_t), value :: n
        real(c_double), intent(in) :: a(n)
        real(c_double) :: squared_norm_neon
    end function
end interface
#endif

#if defined(_OPENMP)
    integer :: device = -1
#endif

contains

  subroutine print_config()

#if defined(_OPENMP)
        use omp_lib
        integer :: k
        !$omp parallel
        k = omp_get_num_threads()
        !$omp single
        write(*,'("OpenMP enabled, running with ",I0," threads")') k
        !$omp end single
        !$omp end parallel

        device = omp_get_default_device()

        if (device == omp_get_initial_device()) then
            print '(A)', "OpenMP default device is the HOST device."
        end if
#endif

  end subroutine


#if defined(SK_BLAS)
#include "bs_kernels_blas.fi"
#elif defined(SK_LOOPS)
#include "bs_kernels_loops.fi"
#elif defined(SK_OMP_PARALLEL_DO)
#include "bs_kernels_omp.fi"
#elif defined(SK_OMP_TARGET_LOOP)
#include "bs_kernels_omp_target.fi"
#elif defined(SK_OMP_SPMD)
#include "bs_kernels_omp_spmd.fi"
#else
#include "bs_kernels.fi"
#endif

end module


