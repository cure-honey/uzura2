!     Last change:  HO   31 May 2000    4:42 am
!==============================================================================
MODULE fft_module
IMPLICIT NONE
PRIVATE
PUBLIC :: fft_window
INTEGER, PARAMETER :: np2 = 10, nn = 2**np2, nn_2 = nn / 2 ! 2^10 = 1024
INTEGER :: indx(nn)
REAL    (KIND = 8) :: pi, pi2, pi2_n, hann_window(nn)
COMPLEX (KIND = 8) :: omega(0:nn - 1)
CONTAINS
!--------------------------------------------------------------------------
SUBROUTINE fft_initialize()
IMPLICIT NONE
INTEGER :: i, j2, k, n
pi  = 4.0d0 * ATAN(1.0d0)
pi2 = 2.0d0 * pi
pi2_n = pi2 / REAL(nn, KIND = 8)
DO i = 1, nn
 omega(i - 1)   =     EXP( CMPLX(0.0d0, pi2_n * REAL(i - 1, KIND = 8), KIND = 8) )
 hann_window(i) = 0.5d0 * ( 1.0d0 - COS(pi2_n * REAL(i - 1, KIND = 8)          ) )
END DO
!
DO i = 1, nn
 n = 0
 k = i - 1
 DO j2 = np2 - 1, 0, -1
  n = n + MOD(k, 2) * 2**j2
  k = k / 2
 END DO
 indx(i) = n + 1 ! indx = 1..N
END DO
RETURN
END SUBROUTINE fft_initialize
!--------------------------------------------------------------------------
SUBROUTINE fft_window(x, y)
IMPLICIT NONE
REAL    (KIND = 8), INTENT(IN ) :: x(:)
COMPLEX (KIND = 8), INTENT(OUT) :: y(:)
LOGICAL :: qfirst = .TRUE.
IF (qfirst) THEN
 qfirst = .FALSE.
 CALL fft_initialize()
END IF
y = hann_window * x
CALL fft2(y)
RETURN
END SUBROUTINE fft_window
!--------------------------------------------------------------------------
SUBROUTINE fft2(fft)
IMPLICIT NONE
COMPLEX (KIND = 8), INTENT(IN OUT) :: fft(:)
COMPLEX (KIND = 8) :: tmp1, tmp2, c1
INTEGER :: i, j, k2, m1, m2, iphase, kn2
fft = fft(indx)
! FFT_2
DO k2 = 1, np2
 kn2 = 2**(k2 - 1)
 DO i = 1, nn, 2* kn2
  DO j = 1, kn2
   iphase = 2**(np2 - k2)*(j - 1)
   c1 = omega( MOD(iphase, nn) )
   m1 = i + j - 1
   m2 = m1 + kn2
   tmp1 =      fft(m1)
   tmp2 = c1 * fft(m2)
   fft(m1) = tmp1 + tmp2 ! 1 = x^2 ; x = 1 or -1
   fft(m2) = tmp1 - tmp2
  END DO
 END DO
END DO
RETURN
END SUBROUTINE fft2
!------------------------------------------------------------------------
END MODULE fft_module
!==========================================================================
MODULE psycho
IMPLICIT NONE
PRIVATE
PUBLIC :: psychoacoustics
CONTAINS
!------------------------------------------------------------------------
SUBROUTINE psychoacoustics(pcm, smr, isample_rate)
USE fft_module
IMPLICIT NONE
REAL (KIND = 8), INTENT(IN ) :: pcm(:, :)
REAL (KIND = 8), INTENT(OUT) :: smr(:, :)
INTEGER        , INTENT(IN ) :: isample_rate
COMPLEX (KIND = 8) :: cfft(1024)
REAL    (KIND = 8) :: signal(32), xmask(32)
REAL    (KIND = 8), SAVE :: abs_thres(1024)
INTEGER :: ichannel, isubband, i0, i1
LOGICAL, SAVE :: qfirst = .TRUE.
IF (qfirst) THEN
 qfirst = .FALSE.
 CALL init_absolute_threshold(abs_thres, isample_rate)
END IF
i0 = (SIZE(pcm, 1) - 512) / 2 + 1
i1 = i0 + 1024 - 1
DO ichannel = 1, SIZE(pcm, 2)
 CALL fft_window(pcm(i0:i1, ichannel), cfft)
 CALL calc_signal(cfft, signal)
 CALL calc_mask  (abs_thres, xmask)
 DO isubband = 1, 32
  smr(isubband, ichannel) = signal(isubband) - xmask(isubband)
 END DO
END DO
RETURN
END SUBROUTINE psychoacoustics
!------------------------------------------------------------------------
SUBROUTINE init_absolute_threshold(abs_thres, isample_rate)
IMPLICIT NONE
REAL (KIND = 8), INTENT(OUT) :: abs_thres(:) ! in the unit of dB
INTEGER        , INTENT(IN ) :: isample_rate
REAL (KIND = 8) :: freq
INTEGER :: i
DO i = 1, SIZE(abs_thres)
 freq = REAL(isample_rate, KIND = 8) / 2.0d0 / 1000.0d0 &
      * REAL(i - 0.5d0, KIND = 8) / REAL(SIZE(abs_thres) - 1, KIND = 8)
 abs_thres(i) = 3.64d0 * freq**(-0.8d0) - 6.5d0 * EXP(-0.6d0 * (freq - 3.3d0)**2.0d0) &
              + 0.001d0 * freq**4.0d0
END DO 
RETURN
END SUBROUTINE init_absolute_threshold
!------------------------------------------------------------------------
SUBROUTINE calc_signal(cfft, signal)
IMPLICIT NONE
COMPLEX (KIND = 8), INTENT(IN ) :: cfft(:)
REAL    (KIND = 8), INTENT(OUT) :: signal(:)
REAL    (KIND = 8) :: tmp
INTEGER :: iband, j
DO iband =  1, 12
 tmp = 100
 DO j = 1, 32
  tmp = MIN( tmp, decibel(cfft( (iband - 1) * 32 + j)) )
 END DO
 signal(iband) = tmp
END DO
DO iband = 13, 32
 tmp = 0.0d0
 DO j = 1, 32
  tmp = tmp + decibel(cfft( (iband - 1) * 32 + j))
 END DO
 signal(iband) = tmp / 32.0d0
END DO
RETURN
END SUBROUTINE calc_signal
!------------------------------------------------------------------------
FUNCTION decibel(x) RESULT(res)
IMPLICIT NONE
COMPLEX (KIND = 8), INTENT(IN) :: x
REAL    (KIND = 8) :: res, tmp
tmp = MAX(1.0d-20, ABS(x))
res = 20.0d0 * LOG10(tmp)
RETURN
END FUNCTION decibel
!------------------------------------------------------------------------
SUBROUTINE calc_mask(abs_thres, xmask)
IMPLICIT NONE
REAL (KIND = 8), INTENT(IN ) :: abs_thres(:)
REAL (KIND = 8), INTENT(OUT) :: xmask(:)
REAL (KIND = 8) :: temp(1)
INTEGER :: iband
DO iband =  1, 12
 temp = MINVAL( abs_thres( (iband - 1) * 32 + 1: iband * 32) )
 xmask(iband) = temp(1)
END DO
DO iband = 13, 32
 xmask(iband) = SUM( abs_thres( (iband - 1) * 32 + 1: iband * 32) ) / 32.0d0
END DO
! Enoken (|O.O|)
xmask = xmask - 114.0d0
!xmask(:11) = xmask(:11) - 50.0d0
!xmask(25:) = xmask(25:) + 100.0d0
!
RETURN
END SUBROUTINE calc_mask
!------------------------------------------------------------------------
END MODULE psycho

