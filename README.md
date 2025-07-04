# Streaming kernels

Streaming kernels 

The following streaming benchmarks are measured:
- BS1: Vector Copy
- BS2: Vector AXPBY
- BS3: Vector Norm
- BS4: Vector Inner Product
- BS5: Fused CG Update

Benchmarks 1-3 are very similar to the classic STREAM benchmarks
- copy
- scale
- add
- triad

## Related

- https://github.com/paranumal/streamparanumal
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
- AMD: https://www.amd.com/en/developer/zen-software-studio/applications/spack/stream-benchmark.html
- https://nvidia.github.io/grace-cpu-benchmarking-guide/foundations/STREAM/index.html
- Nvidia STREAM:
- Intel STREAM:

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
