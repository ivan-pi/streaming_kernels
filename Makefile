
# Variants available
# -DSK_BLAS
# -DSK_LOOPS (also OpenMP SIMD available)
# -DSK_OMP_PARALLEL_DO
# -DSK_OMP_TARGET_LOOP


FC=gfortran
FFLAGS=-mcpu=native -O3 -DSK_OMP_TARGET_LOOP -fopenmp
LDFLAGS=
LDLIBS=-lblas


KERNELS=src/streaming_kernels.F90

streaming_kernels: $(KERNELS) app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

.PHONY: clean
clean:
	$(RM) streaming_kernels *.o *.mod