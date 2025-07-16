# Streaming Kernels

## Description

This repository provides streaming benchmarks designed to measure memory bandwidth and implementation efficiency. 

The benchmarks included are:

- **BS1:** Vector Copy
- **BS2:** Vector AXPBY
- **BS3:** Vector Norm
- **BS4:** Vector Inner Product
- **BS5:** Fused CG Update

These operations are commonly found in iterative solvers for linear systems, such as the conjugate-gradient method (CG). Benchmarks BS1–BS2 are similar to the classic STREAM benchmarks (copy & triad). 

The benchmarks presented here follow the work

> Chalmers, N., & Warburton, T. (2020). Portable high-order finite element kernels I: Streaming Operations. https://arxiv.org/abs/2009.10917

The code accompanying the Chalmers & Warburton article can be found in the [StreamParanumal](https://github.com/paranumal/streamparanumal) repository.

## How to Build and Run the Benchmark

Currently, a simple Makefile setup is in place.

The default configuration assumes gfortran is available: 

```sh
make
```

The default executable target is named `./streaming_kernels`. 
See the Makefile for other available targets.

The executable has the following options available:

```txt
 Usage: ./streaming_kernels [OPTIONS]
 Options:
   -d, --device <int>      Device number
   -n <int>                Number of elements (mutually exclusive with --range, --log-range)
   --float                 Enable FP32 stream test
   -r, --range a:b[:step]  Linear range (e.g. 10:100:5)
   --log-range a:b:n       Logarithmic range with n points (e.g. 1:1000:4)
   -t, --tests <tests>     Comma-separated list of tests in quotes (e.g. "BS1,BS3")
   -k, --repeat <int>      Number of repetitions
   --stats                 Show statistics
   --help                  Show this help message
   --version               Show program version
```

(Note: The options are still subject of change.)

### Optional dependencies

- BLAS
  * [OpenBLAS](https://github.com/OpenMathLib/OpenBLAS)
  * [Arm Performance Libraries](https://developer.arm.com/documentation/101004/latest/)
  * [Accelerate BLAS](https://developer.apple.com/documentation/accelerate/blas-library) (Apple-only)
- [BLIS](https://github.com/flame/blis)
- [Eigen++](https://eigen.tuxfamily.org/index.php?title=Main_Page)

Benchmarks 1-5 can also be formulated in terms of BLAS Level 1 operations.
By linking against different BLAS libraries, we can judge the implementation quality and
how they interact with OpenMP. 

---

The remainder of this file is currently a "link dump". 

## Related

- [STREAM](https://www.cs.virginia.edu/stream/)
- [BabelStream](https://github.com/UoB-HPC/BabelStream)
- [TheBandwidthBenchmark](https://github.com/RRZE-HPC/TheBandwidthBenchmark)
  * Fortran version: https://github.com/RRZE-HPC/TheBandwidthBenchmark-F90
- [RaiderSTREAM](https://github.com/michael-beebe/RaiderSTREAM)
- [Livermore Loops](https://www.netlib.org/benchmark/livermorec)

Other BW-related benchmarks:

- [TeaLeaf](https://github.com/UK-MAC/TeaLeaf)
- [Himeno](https://i.riken.jp/en/supercom/documents/himenobmt/)
- https://github.com/paranumal/libparanumal

Vendor benchmarks:

- AMD: 
  * https://www.amd.com/en/developer/zen-software-studio/applications/spack/stream-benchmark.html
- Nvidia:
  * https://docs.nvidia.com/nvidia-hpc-benchmarks/STREAM_Benchmark.html
  * https://nvidia.github.io/grace-cpu-benchmarking-guide/foundations/STREAM/index.html
- Intel:
  * https://github.com/intel/memory-bandwidth-benchmarks
  * https://dgpu-docs.intel.com/solutions/max-sw/hpc/BabelSTREAM.html

## Literature

- https://arxiv.org/abs/2009.10917
- https://doi.org/10.1109/PMBS56514.2022.00013
- https://arxiv.org/pdf/2309.05445
- https://blogs.fau.de/hager/archives/8263
- https://www.cs.virginia.edu/~mccalpin/papers/balance/
- https://doi.org/10.1155/1996/208679

Related to array syntax in BS5:
- https://link.springer.com/chapter/10.1007/3-540-46423-9_15
- https://doi.org/10.1145/1186632.1186637
- https://doi.org/10.1023/B:SUPE.0000049323.47732.02
