
# Variants available
# -DSK_BLAS
# -DSK_LOOPS (also OpenMP SIMD available)
# -DSK_OMP_PARALLEL_DO
# -DSK_OMP_TARGET_LOOP


FC=gfortran
FFLAGS=-mcpu=native -O3 -ffast-math

LDFLAGS=
LDLIBS=

#LDFLAGS="-L/opt/homebrew/opt/libomp/lib"
#LDLIBS=-lblas

#LDFLAGS=-L$(ARMPL_LIBRARIES)
#LDLIBS=/opt/arm/armpl_25.04_flang-new_clang_19/lib/libarmpl.a -lc++

#LDFLAGS=-L/opt/OpenBLAS/lib
#LDLIBS=-lopenblas

streaming_kernels: src/streaming_kernels.F90 app/cmdline.f90
	$(FC) $(FFLAGS) -o $@ $^

streaming_kernels_loops: FFLAGS += -DSK_LOOPS
streaming_kernels_loops: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $(LDFLAGS) $^ $(LDLIBS)

streaming_kernels_blas: FFLAGS += -DSK_BLAS
streaming_kernels_blas: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $(LDFLAGS) $^ $(LDLIBS)

streaming_kernels_omp: FFLAGS += -DSK_OMP_PARALLEL_DO -fopenmp
streaming_kernels_omp: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $(LDFLAGS) $^ $(LDLIBS)

dot_product_neon.o: src/dot_product_neon.c
	clang -c -O3 -mcpu=native $<

flang_fast_sqr.o: src/flang_fast_sqr.f90
	$(FC) -c $(FFLAGS) $<

.PHONY: clean
clean:
	$(RM) *.o *.mod
	$(RM) streaming_kernels streaming_kernels_loops streaming_kernels_blas streaming_kernels_omp