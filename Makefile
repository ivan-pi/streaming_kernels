
# Variants available
# -DSK_BLAS
# -DSK_LOOPS (also OpenMP SIMD available)
# -DSK_OMP_PARALLEL_DO
# -DSK_OMP_TARGET_LOOP


FC=gfortran
FFLAGS=-mcpu=native -O3 -ffast-math
LDFLAGS=
LDLIBS=-lblas

streaming_kernels: src/streaming_kernels.F90 app/cmdline.f90
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

streaming_kernels_loops: FFLAGS += -DSK_LOOPS
streaming_kernels_loops: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

streaming_kernels_blas: FFLAGS += -DSK_BLAS
streaming_kernels_blas: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

streaming_kernels_omp: FFLAGS += -DSK_OMP_PARALLEL_DO -fopenmp
streaming_kernels_omp: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

dot_product_neon.o: src/dot_product_neon.c
	clang -c -O3 -mcpu=native $<

.PHONY: clean
clean:
	$(RM) streaming_kernels *.o *.mod