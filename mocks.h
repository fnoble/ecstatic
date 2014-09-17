
/* Workaround for C2HS Issue #85
 * https://github.com/haskell/c2hs/issues/85 */

#define __AVAILABILITY__
#define __OSX_AVAILABLE_STARTING(a,b)
#define __OSX_AVAILABLE_BUT_DEPRECATED(a,b,c,d)
#define __OSX_AVAILABLE_BUT_DEPRECATED_MSG(a,b,c,d,e)

#include <stddef.h>

/* Mock some function definitions. */
size_t __builtin_object_size (void * ptr, int type)
{ return 0; }

void *__builtin___memcpy_chk(void *destination, const void * source, size_t num, size_t len)
{ }
void *__builtin___memset_chk(void *destination, int c, size_t num, size_t len)
{ }
void memcpy(void *destination, const void * source, size_t len)
{ }
void memset(void *destination, int c, size_t len)
{ }


double round(double x) { return 0; }
double sqrt(double x) { return 0; }
double fabs(double x) { return 0; }
double copysign(double x, double y) { return 0; }
double fmax(double x, double y) { return 0; }

void printf(const char *format, ...) { }


#include <cblas.h>
#include <clapack.h>

void cblas_dgemm(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TransA,
                 const enum CBLAS_TRANSPOSE TransB, const int M, const int N,
                 const int K, const double alpha, const double *A,
                 const int lda, const double *B, const int ldb,
                 const double beta, double *C, const int ldc) {}
void cblas_dsymm(const enum CBLAS_ORDER Order, const enum CBLAS_SIDE Side,
                 const enum CBLAS_UPLO Uplo, const int M, const int N,
                 const double alpha, const double *A, const int lda,
                 const double *B, const int ldb, const double beta,
                 double *C, const int ldc) {}
void cblas_dtrmm(const enum CBLAS_ORDER Order, const enum CBLAS_SIDE Side,
                 const enum CBLAS_UPLO Uplo, const enum CBLAS_TRANSPOSE TransA,
                 const enum CBLAS_DIAG Diag, const int M, const int N,
                 const double alpha, const double *A, const int lda,
                 double *B, const int ldb) {}
void cblas_dtrmv(const enum CBLAS_ORDER order, const enum CBLAS_UPLO Uplo,
                 const enum CBLAS_TRANSPOSE TransA, const enum CBLAS_DIAG Diag,
                 const int N, const double *A, const int lda,
                 double *X, const int incX) {}
void cblas_dgemv(const enum CBLAS_ORDER order,
                 const enum CBLAS_TRANSPOSE TransA, const int M, const int N,
                 const double alpha, const double *A, const int lda,
                 const double *X, const int incX, const double beta,
                 double *Y, const int incY) {}

int dtrtri_(char *uplo, char *diag, integer *n, doublereal *
	a, integer *lda, integer *info) { return 0; }
int dorgqr_(integer *m, integer *n, integer *k, doublereal *
	a, integer *lda, doublereal *tau, doublereal *work, integer *lwork,
	integer *info) { return 0; }
int dgeqp3_(integer *m, integer *n, doublereal *a, integer *
	lda, integer *jpvt, doublereal *tau, doublereal *work, integer *lwork,
	 integer *info) { return 0; }
int dgelss_(integer *m, integer *n, integer *nrhs,
	doublereal *a, integer *lda, doublereal *b, integer *ldb, doublereal *
	s, doublereal *rcond, integer *rank, doublereal *work, integer *lwork,
	 integer *info) { return 0; }
