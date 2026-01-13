module streaming_kernels

use, intrinsic :: iso_fortran_env, only: compiler_options, compiler_version
use, intrinsic :: iso_c_binding, only: c_int, c_double
!$ use omp_lib, only: omp_get_thread_num, omp_get_num_threads

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

interface
    pure function strlen(str) bind(c,name="strlen")
        use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
        type(c_ptr), value :: str
        integer(c_size_t) :: strlen
    end function
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


#if defined(SK_EIGEN)
interface
    subroutine bs1(n,a,b)
        import dp
        integer, intent(in) :: n
        real(dp), intent(in) :: a(n)
        real(dp), intent(out) :: b(n)
    end subroutine
    subroutine bs2(n,alpha,x,beta,y)
        import dp
        integer, intent(in) :: n
        real(dp), intent(in) :: alpha, x(n), beta
        real(dp), intent(inout) :: y(n)
    end subroutine
    subroutine bs3(n,a,nrm)
        import dp
        integer, intent(in) :: n
        real(dp), intent(in) :: a(n)
        real(dp), intent(out) ::nrm
    end subroutine
    subroutine bs4(n,x,y,nrm)
        import dp
        integer, intent(in) :: n
        real(dp), intent(in) :: x(n), y(n)
        real(dp), intent(out) ::nrm
    end subroutine
    subroutine bs5(n,Ap,alpha,r,rdr)
        import dp
        integer, intent(in) :: n
        real(dp), intent(in) :: Ap(n), alpha
        real(dp), intent(inout) :: r(n)
        real(dp), intent(out) :: rdr
    end subroutine

    subroutine eigen_get_version(world,major,minor) bind(c)
        import c_int
        integer(c_int), intent(out) :: world, major, minor
    end subroutine
    function eigen_get_num_threads() bind(c)
        import c_int
        integer(c_int) :: eigen_get_num_threads
    end function
    function eigen_get_enable_openmp() bind(c)
        import c_int
        integer(c_int) :: eigen_get_enable_openmp
    end function
end interface
#endif


#ifdef USE_ARM_PL
interface
! https://developer.arm.com/documentation/101004/2507/General-information/Library-version-and-build-information
    subroutine armplversion(major, minor, patch, build, tag) bind(c)
! FIXME: not sure what the interface is!
        use, intrinsic :: iso_c_binding, only: c_int, c_char
        integer(c_int) :: major, minor, patch, build
        character(len=1,kind=c_char), optional :: tag(*)
    end subroutine
    subroutine armplinfo
    end subroutine
end interface
#endif

#ifdef SK_BLIS
!
! BLIS Interfaces
!

integer(c_int), parameter :: BLIS_NO_CONJUGATE = 0
integer(c_int), parameter :: BLIS_CONJUGATE    = 16

interface
    function bli_arch_query_id() bind(c,name="bli_arch_query_id")
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: bli_arch_query_id
    end function
    function bli_arch_string(id) bind(c,name="bli_arch_string")
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        integer(c_int), value :: id
        type(c_ptr) :: bli_arch_string
    end function
    function bli_info_get_version_str() bind(c,name="bli_info_get_version_str")
        use, intrinsic :: iso_c_binding, only: c_ptr
        type(c_ptr) :: bli_info_get_version_str
    end function
    function bli_thread_get_num_threads() bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: bli_thread_get_num_threads  ! dim_t
    end function

    function bli_info_get_enable_threading() bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: bli_info_get_enable_openmp ! gint_t
    end function
    function bli_info_get_enable_openmp() bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: bli_info_get_enable_openmp ! gint_t
    end function
    function bli_info_get_enable_pthreads() bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: bli_info_get_enable_pthreads ! gint_t
    end function

    subroutine bli_daxpyv(conjx,n,alpha,x,incx,y,incy) bind(c)
        import c_int, c_double
        integer(c_int), value :: conjx
        integer(c_int), value :: n, incx, incy
        real(c_double), intent(in) :: alpha, x(*)
        real(c_double), intent(inout) :: y(*)
    end subroutine
    subroutine bli_daxpbyv(conjx,n,alpha,x,incx,beta,y,incy) bind(c)
        import c_int, c_double
        integer(c_int), value :: conjx
        integer(c_int), value :: n, incx, incy
        real(c_double), intent(in) :: alpha, x(*), beta
        real(c_double), intent(inout) :: y(*)
    end subroutine
    subroutine bli_dcopyv(conjx,n,x,incx,y,incy) bind(c)
        import c_int, c_double
        integer(c_int), value :: conjx
        integer(c_int), value :: n, incx, incy
        real(c_double), intent(in) :: x(*)
        real(c_double), intent(inout) :: y(*)
    end subroutine
    subroutine bli_ddotv(conjx,conjy,n,x,incx,y,incy,rho) bind(c)
        import c_int, c_double
        integer(c_int), value :: conjx, conjy
        integer(c_int), value :: n, incx, incy
        real(c_double), intent(in) :: x(*), y(*)
        real(c_double), intent(out) :: rho
    end subroutine
end interface
#endif

#ifdef USE_OPENBLAS
! Utility functions from OpenBLAS
!   http://www.openmathlib.org/OpenBLAS/docs/extensions/#utility-functions
interface
    function openblas_get_parallel() bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: openblas_get_parallel
    end function
    function openblas_get_config() bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr
        type(c_ptr) :: openblas_get_config
    end function
    function openblas_get_num_procs() bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: openblas_get_num_procs
    end function
    function openblas_get_num_threads() bind(c)
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: openblas_get_num_threads
    end function
end interface
#endif

contains

  subroutine print_config(verbose)
    !$ use omp_lib
    logical, intent(in), optional :: verbose

    integer :: k

    if (present(verbose)) then
        if (.not. verbose) return
    end if

    write(*,'(/,"Compiler information: ")')

    ! Example output summary
    write(*,'(A,A)') '  version: ', compiler_version()
    write(*,'(A,A)') '  options: ', compiler_options()


#if defined(_OPENMP)
        write(*,'(/,"OpenMP Information: ")')

        !$omp parallel
        !$omp single
        k = omp_get_num_threads()
        !$omp end single
        !$omp end parallel

        write(*,'("  Num. threads: ",I0)') k
        write(*,'("  Num. devices: ",I0)') omp_get_num_devices()

        device = omp_get_default_device()
        if (device == omp_get_initial_device()) then
            write(*,'("    Default device: HOST (",I0,")")') device
        else
            write(*,'("    Default device: GPU (",I0,")")') device
        end if
#endif

#ifdef SK_BLAS
        write(*,'(/,"BLAS Information: ")')

#if defined(USE_ARMPL)
        call armplinfo

#elif defined(USE_OPENBLAS)

        write(*,'(A)') "  Library: OpenBLAS"

        openblas_config: block
            type(c_ptr) :: str_p
            str_p = openblas_get_config()
            block
                character(len=strlen(str_p)), pointer :: str
                call c_f_pointer(str_p,str)
                write(*,'(A)') "  Config: "//str
            end block
        end block openblas_config

        write(*,'(A)',advance='no') "Threading Mode: "
        select case(openblas_get_parallel())
        case(0)
            write(*,'(A)') "Sequential"
        case(1)
            write(*,'(A)') "Platform-based"
        case(2)
            write(*,'(A)') "OpenMP-based"
        end select.
        write(*,'(A,I0)') "  Num. processors: ", openblas_get_num_procs()
        write(*,'(A,I0)') "  Num. threads: ", openblas_get_num_threads()

#elif defined(USE_MKL)
        mkl: block
            character(len=198) :: buffer
            call mkl_get_version_string(buffer)
            write(*,'(A)') "  Library: Intel oneAPI MKL"
            write(*,'(A)') "    Version: "//trim(buffer)
        end block mkl
#elif defined(USE_NVPL)
        nvpl: block
            integer, external :: nvpl_blas_get_version
            integer, external :: nvpl_blas_get_max_threads
            write(*,'(A)')    "  Library: NVPL"
            write(*,'(A,I0)') "    Version: ", nvpl_blas_get_version()
            write(*,'(A,I0)') "    Max. threads: ", nvpl_blas_get_max_threads()
        end block nvpl
#else
        write(*,'(A)') "  Library: Unknown"
#endif

#elif defined(SK_BLIS)
        write(*,'(/,"BLIS Information: ")')

        blis_info: block
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: version_str_p, arch_str_p
            
            version_str_p = bli_info_get_version_str()
            block
                character(len=strlen(version_str_p)), pointer :: str
                call c_f_pointer(version_str_p,str)
                write(*,'(A)') "  Version: "//str
            end block

            arch_str_p = bli_arch_string(bli_arch_query_id())
            block
                character(len=strlen(arch_str_p)), pointer :: str
                call c_f_pointer(arch_str_p,str)
                write(*,'(A)') "  Arch: "//str
            end block

            write(*,'(A)',advance='no') "  Threading mode: " 
            if (bli_info_get_enable_threading() == 0) then
                write(*,'(A)') "Sequential"
            else if (bli_info_get_enable_openmp() == 1) then
                write(*,'(A)') "OpenMP"
            else if (bli_info_get_enable_pthreads() == 1) then
                write(*,'(A)') "pthreads"
            end if
            write(*,'(A,I0)') "  Num. threads: ", bli_thread_get_num_threads()

        end block blis_info

#elif defined(SK_EIGEN)
        write(*,'(/,"Eigen Information: ")')
        eigen_info: block
            integer :: world, major, minor
            call eigen_get_version(world,major,minor)
            write(*,'("  Version: ",I0,".",I0,".",I0)') world, major, minor
            
            write(*,'(A)',advance='no') "  Threading mode: "
            if (eigen_get_enable_openmp() == 1) then
                write(*,'(A)') "OpenMP"
            else
                write(*,'(A)') "Sequential"
            end if
            write(*,'("  Num. threads: ", I0)') eigen_get_num_threads()
        end block eigen_info
#endif

  end subroutine

!
! The includes files define the streaming kernels bs1 to bs5
!
! See respective include file for details
!

#if defined(SK_BLAS)
#include "bs_kernels_blas.fi"
#elif defined(SK_BLIS)
#include "bs_kernels_blis.fi"
#elif defined(SK_BLAS_OMP_SPMD)
#include "bs_kernels_blas_omp_spmd.fi"
#elif defined(SK_OMP_PARALLEL_DO)
#include "bs_kernels_omp.fi"
#elif defined(SK_OMP_TARGET_LOOP)
#include "bs_kernels_omp_target.fi"sim
#elif defined(SK_OMP_SPMD)
#include "bs_kernels_omp_spmd.fi"
#elif defined(SK_EIGEN)
  ! interfaces are already included above
#elif defined(SK_LOOPS)
#include "bs_kernels_loops.fi"
#else
#include "bs_kernels.fi"
#endif

end module


