#include <arm_neon.h>
#include <stddef.h>

#if 0
double dot_product_neon_fma_unroll8_tail(const double *a, const double *b, size_t n) {
    float64x2_t acc0 = vdupq_n_f64(0.0);
    float64x2_t acc1 = vdupq_n_f64(0.0);
    float64x2_t acc2 = vdupq_n_f64(0.0);
    float64x2_t acc3 = vdupq_n_f64(0.0);

    size_t i = 0;
    size_t limit = n & ~7UL;

    // Main loop: 8 elements per iteration
    for (; i < limit; i += 8) {
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i + 0]), vld1q_f64(&b[i + 0]));
        acc1 = vfmaq_f64(acc1, vld1q_f64(&a[i + 2]), vld1q_f64(&b[i + 2]));
        acc2 = vfmaq_f64(acc2, vld1q_f64(&a[i + 4]), vld1q_f64(&b[i + 4]));
        acc3 = vfmaq_f64(acc3, vld1q_f64(&a[i + 6]), vld1q_f64(&b[i + 6]));
    }

    // Tail handling: use vector ops for remaining 2, 4, or 6 elements
    float64x2_t tail_acc = vdupq_n_f64(0.0);
    size_t rem = n - i;

    if (rem >= 6) {
        tail_acc = vfmaq_f64(tail_acc, vld1q_f64(&a[i + 0]), vld1q_f64(&b[i + 0]));
        tail_acc = vfmaq_f64(tail_acc, vld1q_f64(&a[i + 2]), vld1q_f64(&b[i + 2]));
        tail_acc = vfmaq_f64(tail_acc, vld1q_f64(&a[i + 4]), vld1q_f64(&b[i + 4]));
        i += 6;
    } else if (rem >= 4) {
        tail_acc = vfmaq_f64(tail_acc, vld1q_f64(&a[i + 0]), vld1q_f64(&b[i + 0]));
        tail_acc = vfmaq_f64(tail_acc, vld1q_f64(&a[i + 2]), vld1q_f64(&b[i + 2]));
        i += 4;
    } else if (rem >= 2) {
        tail_acc = vfmaq_f64(tail_acc, vld1q_f64(&a[i + 0]), vld1q_f64(&b[i + 0]));
        i += 2;
    }

    // Final horizontal reduction of all accumulators
    float64x2_t acc_sum = vaddq_f64(vaddq_f64(acc0, acc1), vaddq_f64(acc2, acc3));
    acc_sum = vaddq_f64(acc_sum, tail_acc);

    double result = vgetq_lane_f64(acc_sum, 0) + vgetq_lane_f64(acc_sum, 1);

    // Handle odd element if n is odd
    if (i < n) {
        result += a[i] * b[i];
    }

    return result;
}

#else

#include <arm_neon.h>
#include <stddef.h>

double dot_product_neon_fma_unroll16_pow2_tail(const double *restrict a, const double *restrict b, size_t n) {
    if (n < 16) {
        double result = 0.0;
        for (size_t i = 0; i < n; ++i)
            result += a[i] * b[i];
        return result;
    }

    float64x2_t acc0 = vdupq_n_f64(0.0);
    float64x2_t acc1 = vdupq_n_f64(0.0);
    float64x2_t acc2 = vdupq_n_f64(0.0);
    float64x2_t acc3 = vdupq_n_f64(0.0);
    float64x2_t acc4 = vdupq_n_f64(0.0);
    float64x2_t acc5 = vdupq_n_f64(0.0);
    float64x2_t acc6 = vdupq_n_f64(0.0);
    float64x2_t acc7 = vdupq_n_f64(0.0);

    size_t i = 0;
    size_t limit = n & ~15UL;

    for (; i < limit; i += 16) {
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i +  0]), vld1q_f64(&b[i +  0]));
        acc1 = vfmaq_f64(acc1, vld1q_f64(&a[i +  2]), vld1q_f64(&b[i +  2]));
        acc2 = vfmaq_f64(acc2, vld1q_f64(&a[i +  4]), vld1q_f64(&b[i +  4]));
        acc3 = vfmaq_f64(acc3, vld1q_f64(&a[i +  6]), vld1q_f64(&b[i +  6]));
        acc4 = vfmaq_f64(acc4, vld1q_f64(&a[i +  8]), vld1q_f64(&b[i +  8]));
        acc5 = vfmaq_f64(acc5, vld1q_f64(&a[i + 10]), vld1q_f64(&b[i + 10]));
        acc6 = vfmaq_f64(acc6, vld1q_f64(&a[i + 12]), vld1q_f64(&b[i + 12]));
        acc7 = vfmaq_f64(acc7, vld1q_f64(&a[i + 14]), vld1q_f64(&b[i + 14]));
    }

    size_t rem = n - i;

    if (rem >= 4) {
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i + 0]), vld1q_f64(&b[i + 0]));
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i + 2]), vld1q_f64(&b[i + 2]));
        i += 4;
        rem -= 4;
    }

    if (rem >= 2) {
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i]), vld1q_f64(&b[i]));
        i += 2;
        rem -= 2;
    }

    // Horizontal add accumulators
    float64x2_t sum0 = vaddq_f64(acc0, acc1);
    float64x2_t sum1 = vaddq_f64(acc2, acc3);
    float64x2_t sum2 = vaddq_f64(acc4, acc5);
    float64x2_t sum3 = vaddq_f64(acc6, acc7);

    float64x2_t sum01 = vaddq_f64(sum0, sum1);
    float64x2_t sum23 = vaddq_f64(sum2, sum3);

    float64x2_t total = vaddq_f64(sum01, sum23);

    double result = vgetq_lane_f64(total, 0) + vgetq_lane_f64(total, 1);

    if (rem == 1) {
        result += a[i] * b[i];
    }

    return result;
}


double dot_product_neon_fma_unroll8_pow2_tail_simple(const double *restrict a, const double *restrict b, size_t n) {
    float64x2_t acc0 = vdupq_n_f64(0.0);
    float64x2_t acc1 = vdupq_n_f64(0.0);
    float64x2_t acc2 = vdupq_n_f64(0.0);
    float64x2_t acc3 = vdupq_n_f64(0.0);

    size_t i = 0;
    size_t limit = n & ~7UL; // (n/8)*8

    for (; i < limit; i += 8) {


        // Prefetch next 64 bytes of each array (forward looking)
        //__builtin_prefetch(&a[i + 32], 0, 0);  // 4 doubles ahead = 32 bytes
        //__builtin_prefetch(&b[i + 32], 0, 0);

        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i + 0]), vld1q_f64(&b[i + 0]));
        acc1 = vfmaq_f64(acc1, vld1q_f64(&a[i + 2]), vld1q_f64(&b[i + 2]));
        acc2 = vfmaq_f64(acc2, vld1q_f64(&a[i + 4]), vld1q_f64(&b[i + 4]));
        acc3 = vfmaq_f64(acc3, vld1q_f64(&a[i + 6]), vld1q_f64(&b[i + 6]));
    }

    size_t rem = n - i;

    if (rem >= 4) {
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i + 0]), vld1q_f64(&b[i + 0]));
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i + 2]), vld1q_f64(&b[i + 2]));
        i += 4;
        rem -= 4;
    }

    if (rem >= 2) {
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i]), vld1q_f64(&b[i]));
        i += 2;
        rem -= 2;
    }

    double result =
          vgetq_lane_f64(acc0, 0) + vgetq_lane_f64(acc0, 1)
        + vgetq_lane_f64(acc1, 0) + vgetq_lane_f64(acc1, 1)
        + vgetq_lane_f64(acc2, 0) + vgetq_lane_f64(acc2, 1)
        + vgetq_lane_f64(acc3, 0) + vgetq_lane_f64(acc3, 1);

    if (rem == 1) {
        result += a[i] * b[i];
    }

    return result;
}

double squared_norm_neon_fma_unroll8_pow2_tail_simple(const double *a, size_t n) {
    float64x2_t acc0 = vdupq_n_f64(0.0);
    float64x2_t acc1 = vdupq_n_f64(0.0);
    float64x2_t acc2 = vdupq_n_f64(0.0);
    float64x2_t acc3 = vdupq_n_f64(0.0);

    size_t i = 0;
    size_t limit = n & ~7UL; // (n/8)*8

    for (; i < limit; i += 8) {
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i + 0]), vld1q_f64(&a[i + 0]));
        acc1 = vfmaq_f64(acc1, vld1q_f64(&a[i + 2]), vld1q_f64(&a[i + 2]));
        acc2 = vfmaq_f64(acc2, vld1q_f64(&a[i + 4]), vld1q_f64(&a[i + 4]));
        acc3 = vfmaq_f64(acc3, vld1q_f64(&a[i + 6]), vld1q_f64(&a[i + 6]));
    }

    size_t rem = n - i;

    if (rem >= 4) {
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i + 0]), vld1q_f64(&a[i + 0]));
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i + 2]), vld1q_f64(&a[i + 2]));
        i += 4;
        rem -= 4;
    }

    if (rem >= 2) {
        acc0 = vfmaq_f64(acc0, vld1q_f64(&a[i]), vld1q_f64(&a[i]));
        i += 2;
        rem -= 2;
    }

    double result =
          vgetq_lane_f64(acc0, 0) + vgetq_lane_f64(acc0, 1)
        + vgetq_lane_f64(acc1, 0) + vgetq_lane_f64(acc1, 1)
        + vgetq_lane_f64(acc2, 0) + vgetq_lane_f64(acc2, 1)
        + vgetq_lane_f64(acc3, 0) + vgetq_lane_f64(acc3, 1);

    if (rem == 1) {
        result += a[i] * a[i];
    }

    return result;
}

double neon_squared_norm_unroll4(const double *a, size_t n) {
    float64x2_t acc0 = vdupq_n_f64(0.0);
    float64x2_t acc1 = vdupq_n_f64(0.0);
    float64x2_t acc2 = vdupq_n_f64(0.0);
    float64x2_t acc3 = vdupq_n_f64(0.0);

    size_t i = 0;
    size_t limit = n & ~7UL;  // Round down to nearest multiple of 8

    for (; i < limit; i += 8) {
        float64x2_t v0 = vld1q_f64(&a[i + 0]);
        float64x2_t v1 = vld1q_f64(&a[i + 2]);
        float64x2_t v2 = vld1q_f64(&a[i + 4]);
        float64x2_t v3 = vld1q_f64(&a[i + 6]);

        acc0 = vfmaq_f64(acc0, v0, v0);
        acc1 = vfmaq_f64(acc1, v1, v1);
        acc2 = vfmaq_f64(acc2, v2, v2);
        acc3 = vfmaq_f64(acc3, v3, v3);
    }

    float64x2_t acc = vaddq_f64(vaddq_f64(acc0, acc1), vaddq_f64(acc2, acc3));
    double result = vgetq_lane_f64(acc, 0) + vgetq_lane_f64(acc, 1);

    // Tail loop
    for (; i < n; ++i) {
        result += a[i] * a[i];
    }

    return result;
}
#endif

/*
#include <stdio.h>

int main() {
    double x[17], y[17], res;
    
    res = 0.0;
    for (int i = 0; i < 17; ++i) {
        x[i] = i + 1.0;
        y[i] = 1.0;
        res += x[i]*y[i];
    }
    double r = dot_product_neon_fma_unroll8_pow2_tail(x, y, 17);
    printf("dot = %f\n", r);  // Should be 153.0
    printf("dot = %f\n", res);  // Should be 153.0
}

*/