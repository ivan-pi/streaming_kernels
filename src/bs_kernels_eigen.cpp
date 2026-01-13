
#define EIGEN_NO_DEBUG

#include <Eigen/Dense>

extern "C" {

// Subroutine: b = a
void bs1_(const int* n, const double* a, double* b) {
    Eigen::Map<const Eigen::VectorXd> va(a, *n);
    Eigen::Map<Eigen::VectorXd> vb(b, *n);
    vb = va;
}

// Subroutine: y = alpha*x + beta*y
void bs2_(const int* n, const double* alpha, const double* x,
          const double* beta, double* y) {
    Eigen::Map<const Eigen::VectorXd> vx(x, *n);
    Eigen::Map<Eigen::VectorXd> vy(y, *n);
    vy = (*alpha) * vx + (*beta) * vy;
}

// Subroutine: nrm = dot_product(a,a)
void bs3_(const int* n, const double* a, double* nrm) {
    Eigen::Map<const Eigen::VectorXd> va(a, *n);
#ifdef BS3_USE_NORM2
    *nrm = va.squaredNorm();  // Equivalent to dot(a,a)
#else
    *nrm = va.dot(va);
#endif
}

// Subroutine: nrm = dot_product(x,y)
void bs4_(const int* n, const double* x, const double* y, double* nrm) {
    Eigen::Map<const Eigen::VectorXd> vx(x, *n);
    Eigen::Map<const Eigen::VectorXd> vy(y, *n);
    *nrm = vx.dot(vy);
}

// Subroutine: r = r + alpha*Ap; rdr = dot_product(r,r)
void bs5_(const int* n, const double* Ap, const double* alpha,
          double* r, double* rdr) {
    Eigen::Map<const Eigen::VectorXd> vAp(Ap, *n);
    Eigen::Map<Eigen::VectorXd> vr(r, *n);
    vr += (*alpha) * vAp;
    *rdr = vr.squaredNorm();
}

int eigen_get_num_threads() {
    return Eigen::nbThreads();
}

void eigen_get_version(int *world, int *major, int *minor) {
    *world = EIGEN_WORLD_VERSION;
    *major = EIGEN_MAJOR_VERSION;
    *minor = EIGEN_MINOR_VERSION;
}

int eigen_get_enable_openmp() {
#ifdef EIGEN_HAS_OPENMP
    return 1;
#else
    return 0;
#endif
}

} // extern "C"
