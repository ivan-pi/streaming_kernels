program cmdline_parser
  use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
  implicit none

  integer :: i, argc, device, n_elements, repetitions
  logical :: fp32_test, stats, n_set, range_set, logrange_set, help_flag, version_flag
  integer :: nstart, nend, nstep, sz, t
  integer, allocatable :: nrange(:)
  logical :: range_is_log
  character(len=256) :: arg, tests_str, prog_name
  character(len=32), allocatable :: tests(:)

  character(len=3), parameter :: test_names(*) = ["BS1", "BS2", "BS3", "BS4", "BS5"]
  logical :: test_enabled(5)
  real :: bw_rates(5)

  !vvv---- Executable code starts here ----vvv

  ! Get program name from command argument 0
  call get_command_argument(0, prog_name)

  ! Initialize defaults
  test_enabled = .true.
  device = -1
  n_elements = -1
  n_set = .false.
  range_set = .false.
  logrange_set = .false.
  help_flag = .false.
  version_flag = .false.
  repetitions = -1
  tests_str = ''
  range_is_log = .false.

  fp32_test = .false.
  stats = .true.

  argc = command_argument_count()
  i = 1
  parse_cmd: do while (i <= argc)
    call get_command_argument(i, arg)
    select case (trim(arg))
    case ('-d','--device')
      if (i == argc) call usage('ERROR: --d requires an integer argument')
      call get_command_argument(i+1, arg)
      read(arg, *) device
      i = i + 1

    case ('-n')
      if (i == argc) call usage('ERROR: --n requires an integer argument')
      call get_command_argument(i+1, arg)
      read(arg, *) n_elements
      n_set = .true.
      i = i + 1

!    case ('--float')
!      fp32_test = .true.
!      call usage('ERROR: --float is not supported currently')

    case ('-r','--range')
      if (i == argc) call usage('ERROR: --range requires an argument')
      call get_command_argument(i+1, arg)
      call parse_linear_range(trim(arg),nstart,nend,nstep)
      range_set = .true.
      range_is_log = .false.
      i = i + 1

    case ('--log-range')
      if (i == argc) call usage('ERROR: --log-range requires an argument')
      call get_command_argument(i+1, arg)
      call parse_log_range(trim(arg), nrange)
      logrange_set = .true.
      range_is_log = .true.
      i = i + 1

    case ('-t','--tests')
      if (i == argc) call usage('ERROR: --tests requires a quoted comma-separated string')
      call get_command_argument(i+1, tests_str)
      call parse_tests(trim(tests_str), test_names, test_enabled)
      i = i + 1

    case ('-k','--repeat')
      if (i == argc) call usage('ERROR: --repeat requires an integer argument')
      call get_command_argument(i+1, arg)
      read(arg, *) repetitions
      i = i + 1

    case ('--stats')
      stats = .true.

    case ('--help')
      help_flag = .true.
      exit parse_cmd

    case ('--version')
      version_flag = .true.

    case default
      write(*,*) 'Unknown option:', trim(arg)
      call usage()
    end select
    i = i + 1
  end do parse_cmd

  if (help_flag) then
    call print_help(prog_name)
    stop
  end if

  if (version_flag) then
    call print_version(prog_name)
  end if

  if (.not. any([n_set,range_set,logrange_set])) then
    ! Default is to run a single test
    n_elements = 1000000
    n_set = .true.
  else if (count([n_set, range_set, logrange_set]) > 1) then
    ! Check mutual exclusivity of --n, --range, --log-range
    write(*,*) 'ERROR: Only one of --n, --range, or --log-range may be used.'
    stop 1
  end if

  if (stats) then
    ! Example output summary
    write(*,'(A,A)') 'Compiler version : ', compiler_version()
    write(*,'(A,A)') 'Compiler options : ', compiler_options()

    write(*,*) 'Device       :', device
    write(*,*) 'FP32 test    :', fp32_test
    if (n_set) then
      write(*,*) 'Number of elements (--n):', n_elements
    else if (range_set .or. logrange_set) then
      if (range_is_log) then
        write(*,*) 'Log range (--log-range):', nrange

          do i = 1, size(nrange)
            print *, nrange(i), nrange(i)**2
          end do 
      else
        write(*,*) 'Linear range (--range):', nstart, nend, nstep
      end if
    end if

    if (allocated(tests)) then
      write(*,*) 'Selected tests:', tests
    else
      write(*,*) 'Selected tests: (none)'
    end if

    write(*,*) 'Repetitions  :', repetitions
    write(*,*) 'Show stats   :', stats
  end if

  block
    use streaming_kernels
    call print_config()
  end block

  print '(/,4A15)', "Kernel", "Length", "Avg. time (s)", "Rate (GB/s)" 

  ! Loop over available tests
  do t = 1, size(test_names)

    if (.not. test_enabled(t)) cycle
!    print *, "--- Running test ---"//test_names(t)

    if (n_set) then
      call run_test(t=t,n=n_elements,reps=repetitions)
    else
      if (range_set) then
        ! Linear range
        do sz = nstart, nend, nstep
          call run_test(t=t,n=sz,reps=repetitions)
        end do
      else if (logrange_set) then
        ! Logarithmic range
        do i = 1, size(nrange)
          call run_test(t=t,n=nrange(i),reps=repetitions)
        end do
      end if
    end if

  end do 

contains

  subroutine run_test(t,n,reps)

    use streaming_kernels, only: bs1,bs2,bs3,bs4,bs5

    integer, intent(in) :: t, n, reps
    integer, parameter :: dp = kind(1.0d0), sp = kind(1.0e0)

    integer(8) :: bw(5)

    real(dp), allocatable :: x(:), y(:)
    real(dp) :: alpha, beta, nrm, rdr
    real :: elapsed

    real, parameter :: min_time = 0.5

    integer(8) :: t1, t2, rate

    integer :: k, nreps 

    ! traffic per element (in bytes)

    bw(1) = 8 + 8     ! b = a
    bw(2) = 8 + 8 + 8 ! y = alpha*x + beta*y
    bw(3) = 8         ! res = dot_product(x,x)
    bw(4) = 8 + 8     ! res = dot_product(x,y)
    bw(5) = 8 + 8 + 8 ! r = r + alpha*Ap; res = dot_product(r,r)

    call system_clock(count_rate=rate)

    alpha = 3.0_dp
    beta = 1.0_dp

    allocate(x(n),y(n))

    !$omp parallel do
    do k = 1, n
      x(k) = 0.0d0
      y(k) = 0.0d0
    end do

! FIXME: switch from random to fixed initialization

    call random_number(x)
    call random_number(y)

    if (reps >= 1) then
      ! Run the benchmark for a fixed number of repetitions

      select case(t)
      case(1)
          call system_clock(t1)
          do k = 1, reps
            call bs1(n,x,y)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
      case(2)
          call system_clock(t1)
          do k = 1, reps
            call bs2(n,alpha,x,beta,y)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
      case(3)
          call system_clock(t1)
          do k = 1, reps
            call bs3(n,x,nrm)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
      case(4)
          call system_clock(t1)
          do k = 1, reps
            call bs4(n,x,y,nrm)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
      case(5)
          call system_clock(t1)
          do k = 1, reps
            call bs5(n,x,alpha,y,nrm)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
      end select
      elapsed = elapsed / reps
    else
      ! Run each benchmark for 3 reps or at-least 0.5 seconds

      nreps = 3

! FIXME: use fypp to simplify this part

      select case(t)
      case(1)
        do
          call system_clock(t1)
          do k = 1, nreps
            call bs1(n,x,y)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
          if (elapsed > min_time) exit
          nreps = nreps*2
        end do
      case(2)
        do
          call system_clock(t1)
          do k = 1, nreps
            call bs2(n,alpha,x,beta,y)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
          if (elapsed > min_time) exit
          nreps = nreps*2
        end do
      case(3)
        do
          call system_clock(t1)
          do k = 1, nreps
            call bs3(n,x,nrm)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
          if (elapsed > min_time) exit
          nreps = nreps*2
        end do
      case(4)
        do
          call system_clock(t1)
          do k = 1, nreps
            call bs4(n,x,y,nrm)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
          if (elapsed > min_time) exit
          nreps = nreps*2
        end do
      case(5)
        do
          call system_clock(t1)
          do k = 1, nreps
            call bs5(n,x,alpha,y,rdr)
          end do
          call system_clock(t2)
          elapsed = real(t2 - t1) / real(rate)
          if (elapsed > min_time) exit
          nreps = nreps*2
        end do
      end select
      elapsed = elapsed / nreps 
    end if

    bw_rates(t) = n*bw(t)/elapsed*1.0e-9
    print '(A15,I15,2G15.5)', test_names(t), n, elapsed, bw_rates(t)

  end subroutine

  subroutine usage(msg)
    character(len=*), intent(in), optional :: msg
    if (present(msg)) then
      write(*,*) trim(msg)
    end if
    write(*,*) 'Use --help to see available options.'
    stop 1
  end subroutine usage

  subroutine print_help(name)
    character(len=*), intent(in) :: name
    write(*,*) 'Usage: ', trim(name), ' [OPTIONS]'
    write(*,*) 'Options:'
    write(*,*) '  -d, --device <int>      Device number'
    write(*,*) '  -n <int>                Number of elements (mutually exclusive with --range, --log-range)'
    write(*,*) '  --float                 Enable FP32 stream test'
    write(*,*) '  -r, --range a:b[:step]  Linear range (e.g. 10:100:5)'
    write(*,*) '  --log-range a:b:n       Logarithmic range with n points (e.g. 1:1000:4)'
    write(*,*) '  -t, --tests "BS1,BS3"   Comma-separated list of tests in quotes'
    write(*,*) '  -k, --repeat <int>      Number of repetitions'
    write(*,*) '  --stats                 Show statistics'
    write(*,*) '  --help                  Show this help message'
    write(*,*) '  --version               Show program version'
  end subroutine print_help

  subroutine print_version(name)
    character(len=*), intent(in) :: name
    write(*,*) trim(name), ' version 1.0'
  end subroutine print_version

  subroutine parse_linear_range(str,startv,endv,stepv)
    character(len=*), intent(in) :: str
    integer, intent(out) :: startv, endv, stepv
    character(len=32) :: parts(3)
    integer :: nparts, n
    call split_colon(str, parts, nparts)
    if (nparts < 2 .or. nparts > 3) then
      call usage('ERROR: --range format must be a:b[:step]')
    end if
    read(parts(1), *) startv
    read(parts(2), *) endv
    if (nparts == 3) then
      read(parts(3), *) stepv
    else
      stepv = 1
    end if
    if (stepv == 0) call usage('ERROR: step cannot be zero')
    n = (endv - startv) / stepv + 1
    if (n <= 0) call usage('ERROR: invalid range, no elements generated')
  end subroutine parse_linear_range

  subroutine parse_log_range(str, out)
    character(len=*), intent(in) :: str
    integer, allocatable, intent(out) :: out(:)
    character(len=32) :: parts(3)
    integer :: nparts, i, n
    real :: a, b

    call split_colon(str, parts, nparts)
    if (nparts /= 3) call usage('ERROR: --log-range format must be a:b:n')

    read(parts(1), *) a
    read(parts(2), *) b
    read(parts(3), *) n

    if (a <= 0.0 .or. b <= 0.0 .or. n < 2) then
      call usage('ERROR: --log-range requires positive bounds and at least 2 points')
    end if

    allocate(out(n))
    do i = 1, n
      associate(nxt => exp(log(a) + (log(b) - log(a)) * real(i-1) / real(n-1)))
        out(i) = int(nxt)  ! integer truncation, adjust as needed
      end associate
    end do
  end subroutine parse_log_range

  subroutine parse_tests(str,tests,test_enabled)
    character(len=*), intent(in) :: str
    character(len=3), intent(in) :: tests(:)
    logical, intent(inout) :: test_enabled(:)
    integer :: count, i, pos, start, lenstr

    if (str == "all") then
      test_enabled = .true.
    else
      do i = 1, size(tests)
        if (index(str,tests(i)) == 0) test_enabled(i) = .false.
      end do
    end if

  end subroutine parse_tests

  subroutine split_colon(str, parts, n)
    character(len=*), intent(in) :: str
    character(len=32), intent(out) :: parts(3)
    integer, intent(out) :: n
    integer :: pos, start, idx, lenstr

    n = 0
    lenstr = len_trim(str)
    start = 1
    do idx = 1, 3
      pos = index(str(start:), ':')
      if (pos == 0) then
        n = n + 1
        parts(n) = str(start:)
        exit
      else
        n = n + 1
        parts(n) = str(start:start+pos-2)
        start = start + pos
      end if
      if (start > lenstr) exit
    end do
  end subroutine split_colon

end program cmdline_parser
