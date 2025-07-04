module streaming_kernels_seq
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

pure function fast_dot(a,b,n) result(p)
    integer, intent(in) :: n
    integer, parameter :: chunk = 16
    real(dp), intent(in) :: a(n)
    real(dp), intent(in) :: b(n)
    real(dp) :: p
    ! --
    real(dp) :: abatch(chunk)
    integer :: i, r
    ! -----------------------------
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*b(1:r)
    abatch(r+1:chunk) = 0.0_dp
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)*b(i:i+chunk-1)
    end do
    
    p = 0.0_dp
    do i = 1, chunk/2
      p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure function fast_sqr(a,n) result(p)
    integer, intent(in) :: n
    integer, parameter :: chunk = 16
    real(dp), intent(in) :: a(n)
    real(dp) :: p
    ! --
    real(dp) :: abatch(chunk)
    integer :: i, r
    ! -----------------------------
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*a(1:r)
    abatch(r+1:chunk) = 0.0_dp
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)*a(i:i+chunk-1)
    end do
    
    p = 0.0_dp
    do i = 1, chunk/2
      p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function

!
! BS1 - entry-wise copy of an array
!

subroutine bs1_v1(n,a,b)
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) :: b(n)
b = a
end subroutine

subroutine bs1_v2(n,a,b)
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) :: b(n)
integer :: i
!$omp parallel do simd nontemporal(a,b)
do i = 1, n
    b(i) = a(i)
end do
end subroutine

subroutine bs1_v3(n,a,b)
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) :: b(n)
integer :: i
do concurrent (i = 1:n)
    b(i) = a(i)
end do
end subroutine

subroutine bs1_v4(n,a,b)
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) :: b(n)
call dcopy(n,a,1,b,1)
end subroutine


!
! BS2 -- scaled in-place addition of two arrays
!

subroutine bs2_v1(n,alpha,x,beta,y)
integer, intent(in) :: n
real(dp), intent(in) :: alpha, x(n), beta
real(dp), intent(inout) :: y(n)

! N.b.: In the more general case we'd specialize for alpha, beta \in {0,1}

y = alpha*x + beta*y

end subroutine

subroutine bs2_v2(n,alpha,x,beta,y)
integer, intent(in) :: n
real(dp), intent(in) :: alpha, x(n), beta
real(dp), intent(inout) :: y(n)
integer :: i
!$omp simd
do i = 1, n
y(i) = alpha*x(i) + beta*y(i)
end do
end subroutine

subroutine bs2_v3(n,alpha,x,beta,y)
integer, intent(in) :: n
real(dp), intent(in) :: alpha, x(n), beta
real(dp), intent(inout) :: y(n)
integer :: i
do concurrent (i = 1:n)
y(i) = alpha*x(i) + beta*y(i)
end do
end subroutine


subroutine bs2_v4(n,alpha,x,beta,y)
integer, intent(in) :: n
real(dp), intent(in) :: alpha, x(n), beta
real(dp), intent(inout) :: y(n)
#if HAVE_DAXPBY
call daxpby(n,alpha,x,1,beta,y,1)
#else
call dscal(n,beta,y,1)
call daxpy(n,alpha,x,1,y,1)
#endif
end subroutine

!
! BS3 -- reduction kernel for computing vector norm
!

subroutine bs3_v1(n,a,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) ::nrm
nrm = dot_product(a,a)
end subroutine

subroutine bs3_v2(n,a,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) ::nrm
nrm = sum(a*a)
end subroutine

subroutine bs3_v3(n,a,nrm)
!$ use omp_lib
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) :: nrm
integer :: i

nrm = 0.0_dp
!$omp parallel do simd reduction(+:nrm) schedule(simd: static)
do i = 1, n
    nrm = nrm + a(i)*a(i)
end do

end subroutine

#if DC_REDUCE
subroutine bs3_v4(n,a,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) ::nrm
integer :: i
nrm = 0.0_dp
do concurrent(i = 1:n) reduce(+:nrm)
    nrm = nrm + a(i)*a(i)
end do
end subroutine
#endif

subroutine bs3_v5(n,a,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) ::nrm

nrm = ddot(n,a,1,a,1)

end subroutine

subroutine bs3_v6(n,a,nrm)
!$ use omp_lib
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) ::nrm

integer :: tid, nthreads, chunk, lo, hi
real(dp) :: tnrm

nrm = 0.0

tid = 0
nthreads = 1

!$omp parallel default(private) shared(a,n) reduction(+: nrm) 
!$ tid = omp_get_thread_num()
!$ nthreads = omp_get_num_threads()

chunk = (n + nthreads - 1) / nthreads
lo = tid * chunk + 1
hi = min((tid + 1)*chunk, n)

tnrm = fast_dot(a(lo),a(lo),hi-lo+1)

nrm = nrm + tnrm
!$omp end parallel

!nrm = fast_dot(a,a,n)

end subroutine

subroutine bs3_v7(n,a,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: a(n)
real(dp), intent(out) ::nrm

nrm = fast_sqr(a,n)

end subroutine

!
! BS4 -- Vector Inner Product
!
subroutine bs4_v1(n,x,y,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: x(n), y(n)
real(dp), intent(out) ::nrm
nrm = dot_product(x,y)
end subroutine

subroutine bs4_v2(n,x,y,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: x(n), y(n)
real(dp), intent(out) ::nrm
nrm = sum(x*y)
end subroutine

subroutine bs4_v3(n,x,y,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: x(n), y(n)
real(dp), intent(out) ::nrm
integer :: i
nrm = 0.0_dp
!$omp simd reduction(+:nrm)
do i = 1, n
    nrm = nrm + x(i)*y(i)
end do
end subroutine

#if DC_REDUCE
subroutine bs4_v4(n,x,y,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: x(n), y(n)
real(dp), intent(out) ::nrm
integer :: i
nrm = 0.0_dp
do concurrent(i = 1:n) reduce(+:nrm)
    nrm = nrm + x(i)*y(i)
end do
end subroutine
#endif

subroutine bs4_v5(n,x,y,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: x(n), y(n)
real(dp), intent(out) ::nrm

nrm = ddot(n,x,1,y,1)

end subroutine

subroutine bs4_v6(n,x,y,nrm)
integer, intent(in) :: n
real(dp), intent(in) :: x(n), y(n)
real(dp), intent(out) ::nrm

nrm = fast_dot(x,y,n)

end subroutine

!
! BS5 -- Fused CG update
!
subroutine bs5_v1(n,Ap,alpha,r,rdr)
integer, intent(in) :: n
real(dp), intent(in) :: Ap(n), alpha
real(dp), intent(out) :: r(n), rdr
r = r + alpha*Ap
rdr = dot_product(r,r)
end subroutine

subroutine bs5_v2(n,Ap,alpha,r,rdr)
integer, intent(in) :: n
real(dp), intent(in) :: Ap(n), alpha
real(dp), intent(out) :: r(n), rdr
associate(t => r + alpha*Ap)
    r = t
    rdr = dot_product(t,t)
end associate
end subroutine

subroutine bs5_v3(n,Ap,alpha,r,rdr)
integer, intent(in) :: n
real(dp), intent(in) :: Ap(n), alpha
real(dp), intent(out) :: r(n), rdr
integer :: i
rdr = 0.0_dp
!$omp simd reduction(+:rdr)
do i = 1, n
    r(i) = r(i) + alpha*Ap(i)
    rdr = rdr + r(i)*r(i)
end do
end subroutine

#if DC_REDUCE
subroutine bs5_v4(n,Ap,alpha,r,rdr)
integer, intent(in) :: n
real(dp), intent(in) :: Ap(n), alpha
real(dp), intent(out) :: r(n), rdr
integer :: i
rdr = 0.0_dp
do concurrent(i = 1:n) reduce(+:rdr)
    r(i) = r(i) + alpha*Ap(i)
    rdr = rdr + r(i)*r(i)
end do
end subroutine
#endif

subroutine bs5_v5(n,Ap,alpha,r,rdr)
integer, intent(in) :: n
real(dp), intent(in) :: Ap(n), alpha
real(dp), intent(out) :: r(n), rdr
call daxpy(n,alpha,Ap,1,r,1)
rdr = ddot(n,r,1,r,1)
end subroutine

end module


