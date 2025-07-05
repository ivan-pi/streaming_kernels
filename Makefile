
# Variants available
# -DSK_BLAS
# -DSK_LOOPS (also OpenMP SIMD available)
# -DSK_OMP_PARALLEL_DO
# -DSK_OMP_TARGET_LOOP


FC=gfortran
FFLAGS=-mcpu=native -O3
LDFLAGS=
LDLIBS=-lblas


streaming_kernels: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

streaming_kernels_loops: FFLAGS += -DSK_LOOPS
streaming_kernels_loops: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

streaming_kernels_omp: FFLAGS += -DSK_OMP_PARALLEL_DO -fopenmp
streaming_kernels_omp: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)
	

.PHONY: clean
clean:
	$(RM) streaming_kernels *.o *.mod