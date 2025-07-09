
# Variants available
# -DSK_BLAS
# -DSK_LOOPS (also OpenMP SIMD available)
# -DSK_OMP_PARALLEL_DO
# -DSK_OMP_TARGET_LOOP
# -DSK_OMP_SPMD

FC=gfortran
FFLAGS=-mcpu=native -O3 -ffast-math -fopenmp

LDFLAGS=
LDLIBS=

CXX=g++
CXXFLAGS=-Wall -pedantic -O3 -mcpu=native
EIGEN_DIR ?= /opt/homebrew/include/eigen3

#LDFLAGS="-L/opt/homebrew/opt/libomp/lib"
#LDLIBS=-lblas

#LDFLAGS=-L$(ARMPL_LIBRARIES)
#LDLIBS=/opt/arm/armpl_25.04_flang-new_clang_19/lib/libarmpl.a -lc++

#LDFLAGS=-L/opt/OpenBLAS/lib
#LDLIBS=-lopenblas

src_files = src/streaming_kernels.F90 app/cmdline.f90

streaming_kernels: $(src_files) src/bs_kernels.fi
	$(FC) $(FFLAGS) -o $@ $(src_files)

streaming_kernels_loops: $(src_files) src/bs_kernels_loops.fi
	$(FC) $(FFLAGS) -DSK_LOOPS -o $@ $(LDFLAGS) $(src_files) $(LDLIBS)

streaming_kernels_blas: $(src_files) src/bs_kernels_blas_omp_spmd.fi
	$(FC) $(FFLAGS) -DSK_BLAS -o $@ $(LDFLAGS) $(src_files) $(LDLIBS)

streaming_kernels_omp: $(src_files) src/bs_kernels_omp.fi
	$(FC) $(FFLAGS) -DSK_OMP_PARALLEL_DO -o $@ $(LDFLAGS) $(src_files) $(LDLIBS)


dot_product_neon.o: src/dot_product_neon.c
	clang -c -O3 -mcpu=native $<

flang_fast_sqr.o: src/flang_fast_sqr.f90
	$(FC) -c $(FFLAGS) $<

bs_kernels_eigen.o: src/bs_kernels_eigen.cpp
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -I$(EIGEN_DIR) -c $<

.PHONY: clean
clean:
	$(RM) *.o *.mod
	$(RM) streaming_kernels streaming_kernels_loops streaming_kernels_blas streaming_kernels_omp
