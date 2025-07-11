# Streaming Kernels

## Description

This repository provides streaming benchmarks designed to measure memory bandwidth and kernel efficiency. The benchmarks were described in the work

> Chalmers, N., & Warburton, T. (2020). Portable high-order finite element kernels I: Streaming Operations. https://arxiv.org/abs/2009.10917

The code accompying the Chalmers & Warburton article can be found in the [StreamParanumal](https://github.com/paranumal/streamparanumal) repository.

The benchmarks included are:

- **BS1:** Vector Copy
- **BS2:** Vector AXPBY
- **BS3:** Vector Norm
- **BS4:** Vector Inner Product
- **BS5:** Fused CG Update

These operations are commonly found in iterative solvers for linear systems, such as the conjugate-gradient method (CG). Benchmarks BS1–BS2 are similar to the classic STREAM benchmarks (copy, scale, add, triad). 

## How to Build and Run the Benchmark

Currently, a simple Makefile setup is in place.

The default configuration assumes gfortran is available: 

```sh
make
```

The default executable target is named `./streaming_kernels`. 
See the Makefile for other available targets.

The options available are:

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

---

The remainder of this file is a "link dump". 

## Related

- StreamParanumal: https://github.com/paranumal/streamparanumal
- BabelStream: https://github.com/UoB-HPC/BabelStream
- STREAM: https://www.cs.virginia.edu/stream/
- Livermore Loops: https://www.netlib.org/benchmark/
- Bandwidth Benchmark: https://github.com/RRZE-HPC/TheBandwidthBenchmark
  * Fortran version: https://github.com/RRZE-HPC/TheBandwidthBenchmark-F90

Other BW-related benchmarks:

- TeaLeaf: https://github.com/UK-MAC/TeaLeaf
- Himeno: https://i.riken.jp/en/supercom/documents/himenobmt/
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
