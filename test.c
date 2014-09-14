
// #include "common.h"
#include "stdint.h"

/** Signed 8-bit integer. */
typedef int8_t s8;
/** Signed 16-bit integer. */
typedef int16_t s16;
/** Signed 32-bit integer. */
typedef int32_t s32;
/** Signed 64-bit integer. */
typedef int64_t s64;
/** Unsigned 8-bit integer. */
typedef uint8_t u8;
/** Unsigned 16-bit integer. */
typedef uint16_t u16;
/** Unsigned 32-bit integer. */
typedef uint32_t u32;
/** Unsigned 64-bit integer. */
typedef uint64_t u64;


/** Zero lower triangle of an `n` x `n` square matrix.
 * Some routines designed to work on upper triangular matricies use the lower
 * triangle as scratch space. This function zeros the lower triangle such that
 * the matrix can be passed to a routine designed to act on a dense matrix.
 *
 * \f$ M \f$ is a matrix on \f$\mathbb{R}^{n \times n}\f$
 *
 * \param n The size of the matrix.
 * \param M Pointer to the matrix.
 */
void matrix_triu(u32 n, double *M)
{
  /* NOTE: This function has been bounds checked. Please check again if
   * modifying. */
  for (u32 i=1; i<n; i++) {
    for (u32 j=0; j<i; j++) {
      M[i*n + j] = 0;
    }
  }
}

/** Initialise an `n` x `n` identity matrix.
 *
 * \f$ M \f$ is a matrix on \f$\mathbb{R}^{n \times n}\f$
 *
 * \param n The size of the matrix.
 * \param M Pointer to the matrix.
 */
void matrix_eye(u32 n, double *M)
{
  /* NOTE: This function has been bounds checked. Please check again if
   * modifying. */
  memset(M, 0, n * n * sizeof(double));
  for (u32 i=0; i<n; i++) {
    M[i*n + i] = 1;
  }
}

/** Performs the \f$U D U^{T}\f$ decomposition of a symmetric positive definite
 * matrix.
 * This is algorithm 10.2-2 of Gibbs [1].
 *
 * \f$ M = U D U^{T}\f$, where \f$ M \f$ is a matrix on \f$\mathbb{R}^{n \times
 * n}\f$ and \f$U\f$ is (therefore) a upper unit triangular matrix on
 * \f$\mathbb{R}^{n \times n}\f$ and \f$D\f$ is a diagonal matrix expressed as
 * a vector on \f$\mathbb{R}^{n}\f$.
 *
 * \note The M matrix is overwritten by this function.
 *
 * References:
 *   -# Gibbs, Bruce P. "Advanced Kalman Filtering, Least-Squares, and Modeling."
 *      John C. Wiley & Sons, Inc., 2011.
 *
 * \param n The size of the matrix.
 * \param M Pointer to the input matrix.
 * \param U Pointer to the upper unit triangular output matrix.
 * \param D Pointer to the diagonal vector.
 */
void matrix_udu(u32 n, double *M, double *U, double *D)
{
  /* TODO: replace with DSYTRF? */
  /* NOTE: This function has been bounds checked. Please check again if
   * modifying. */
  double alpha, beta;
  matrix_triu(n, M);
  matrix_eye(n, U);
  memset(D, 0, n * sizeof(double));
  double LOL[n];
  double bar;


  for (u32 j=n; j>=2; j--) {
    D[j - 1] = M[(j-1)*n + j-1];
    if (D[j-1] != 0) {
      alpha = 1.0 / D[j-1];
    } else {
      alpha = 0.0;
    }
    for (u32 k=1; k<j; k++) {
      beta = M[(k-1)*n + j-1];
      U[(k-1)*n + j-1] = alpha * beta;
      for (u32 kk = 0; kk < k; kk++) {
        M[kk*n + k-1] = M[kk*n + k-1] - beta * U[kk*n + j-1];
      }
    }
  }
  D[0] = M[0];
}


void *memset(void *str, int c, int n) {}

void test1(u32 m)
{
  double M_[m][m];
  double U_[m][m];
  double D_[m];

  matrix_udu(m, (double *)M_, (double *)U_, D_);
}

