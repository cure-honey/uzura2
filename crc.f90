!     Last change:  HO   27 May 2000   11:47 pm
!=============================================================================
MODULE crc
IMPLICIT NONE
PRIVATE
PUBLIC :: crc16
INTEGER, PARAMETER :: igenerator = Z'8005' !B'1000000000000101'  ! x^16+x^15+x^2+1
CONTAINS
!--------------------------------------------------------------------------
SUBROUTINE crc16(n, in, icrc)
IMPLICIT NONE
INTEGER, INTENT(IN    ) :: n, in
INTEGER, INTENT(IN OUT) :: icrc
INTEGER :: k, j, ibit1, ibit2
k = in
DO j = 1, n
 ibit1 = k * 2 / 2**n       !  nth bit 
 k = MOD(k * 2, 2**n)      
 ibit2 = icrc * 2 / 2**16   ! 16th bit
 icrc = MOD(icrc * 2, 2**16)
 IF(IEOR(ibit1, ibit2) == 1) icrc = IEOR(icrc, igenerator)
END DO
RETURN
END SUBROUTINE crc16
!--------------------------------------------------------------------------
END MODULE crc

