
FC=gfortran
FFLAGS=-mcpu=native -O3
LDLIBS = -lblas

streaming_kernels: src/streaming_kernels.F90 app/cmdline.f90 
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)


.PHONY: clean

clean:
	$(RM) streaming_kernels *.o *.mod