!     Last change:  HO   31 May 2000    2:26 am
!============================================================================
MODULE layer2
IMPLICIT NONE
PRIVATE
PUBLIC :: subband_normalization, bit_allocation, quantization, &
          initialize_table, table, layer2_table, nscfsi
REAL (KIND = 8) :: scale_factor(0:63), snr(0:17), a(17), b(17)
TYPE :: layer2_table
 INTEGER :: nsteps(0:17), nsample(0:17), nlength(0:17)
!
 INTEGER :: nband
 INTEGER :: nbal(32)
 INTEGER :: nbit(32, 0:15)
END TYPE layer2_table
TYPE (layer2_table) :: table
INTEGER, PARAMETER :: itransmission_pattern(5, 5) = &
RESHAPE( (/123, 122, 122, 133, 123, &
           113, 111, 111, 444, 113, &
           111, 111, 111, 333, 113, &
           222, 222, 222, 333, 123, &
           123, 122, 122, 133, 123  /), (/5, 5 /) )
INTEGER, PARAMETER :: iscale_factor_select_information(5, 5) = &
RESHAPE( (/0, 3, 3, 3, 0, &
           1, 2, 2, 2, 1, &
           2, 2, 2, 2, 1, &
           2, 2, 2, 2, 0, &
           0, 3, 3, 3, 0  /), (/5, 5/) )
INTEGER, PARAMETER :: nscfsi(0:3) = (/3, 2, 1, 2/)
CONTAINS
!-------------------------------------------------------------------
SUBROUTINE initialize_table(itable)
IMPLICIT NONE
INTEGER, INTENT(IN) :: itable
table%nsteps(0:17)  = (/0, 3, 5, 7, 9, 15, 31, 63, 127, 255, 511, 1023, 2047, 4095, 8191, 16383, 32767, 65535/)
table%nsample(0:17) = (/1, 3, 3, 1, 3,  1,  1,  1,   1,   1,   1,    1,    1,    1,    1,     1,     1,     1/)
table%nlength(0:17) = (/0, 5, 7, 3,10,  4,  5,  6,   7,   8,   9,   10,   11,   12,   13,    14,    15,    16/)
SELECT CASE (itable)
 CASE (1)
  table%nband = 27
  table%nbal = (/4, 4, 4, 4, 4, 4, 4, 4, 4, 4, &
                 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
                 3, 3, 3, 2, 2, 2, 2, 0, 0, 0, 0, 0/)
  table%nbit( 1, :) = (/0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17/)
  table%nbit( 2, :) = (/0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17/)
  table%nbit( 3, :) = (/0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17/)
  table%nbit( 4, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 5, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 6, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 7, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 8, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 9, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit(10, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit(11, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit(12, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(13, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(14, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(15, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(16, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(17, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(18, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(19, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(20, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(21, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(22, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(23, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(24, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(25, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(26, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(27, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(28, :) = (/0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(29, :) = (/0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(30, :) = (/0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(31, :) = (/0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(32, :) = (/0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
 CASE (2)
  table%nband = 30
  table%nbal = (/4, 4, 4, 4, 4, 4, 4, 4, 4, 4, &
                 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
                 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 0, 0/)
  table%nbit( 1, :) = (/0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17/)
  table%nbit( 2, :) = (/0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17/)
  table%nbit( 3, :) = (/0, 1, 3,  5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17/)
  table%nbit( 4, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 5, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 6, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 7, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 8, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit( 9, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit(10, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit(11, :) = (/0, 1, 2,  3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 17/)
  table%nbit(12, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(13, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(14, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(15, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(16, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(17, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(18, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(19, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(20, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(21, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(22, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(23, :) = (/0, 1, 2,  3, 4, 5, 6, 17,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(24, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(25, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(26, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(27, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(28, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(29, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(30, :) = (/0, 1, 2, 17, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(31, :) = (/0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(32, :) = (/0, 0, 0,  0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0/)
 CASE (3)
  table%nband = 8
  table%nbal = (/4, 4, 3, 3, 3, 3, 3, 3, 0, 0, &
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
  table%nbit( 1, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16/)
  table%nbit( 2, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16/)
  table%nbit( 3, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 4, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 5, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 6, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 7, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 8, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 9, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(10, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(11, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(12, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(13, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(14, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(15, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(16, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(17, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(18, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(19, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(20, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(21, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(22, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(23, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(24, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(25, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(26, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(27, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(28, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(29, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(30, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(31, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(32, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
 CASE (4)
  table%nband = 12
  table%nbal = (/4, 4, 3, 3, 3, 3, 3, 3, 3, 3, &
                 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, &
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
  table%nbit( 1, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16/)
  table%nbit( 2, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16/)
  table%nbit( 3, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 4, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 5, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 6, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 7, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 8, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit( 9, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(10, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(11, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(12, :) = (/0, 1, 2, 4, 5, 6, 7, 8, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(13, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(14, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(15, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(16, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(17, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(18, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(19, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(20, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(21, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(22, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(23, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(24, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(25, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(26, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(27, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(28, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(29, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(30, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(31, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
  table%nbit(32, :) = (/0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  0,  0/)
 CASE DEFAULT
  STOP 'input error in initalize_table'
END SELECT
RETURN
END SUBROUTINE initialize_table
!-------------------------------------------------------------------
SUBROUTINE initialize_scalefactor()
IMPLICIT NONE
INTEGER :: i
DO i = 0, 2**6 - 1
 scale_factor(i) = 1.0d-14 * ANINT( 1.0d14 * 2.0d0**( -REAL(i - 1, KIND = 8) / 3.0d0 )  )
END DO
RETURN
END SUBROUTINE initialize_scalefactor
!-------------------------------------------------------------------
SUBROUTINE subband_normalization(subband, iscale_factor, iscfsi)
IMPLICIT NONE
REAL (KIND = 8), INTENT(IN OUT) :: subband(:, :, :)
INTEGER        , INTENT(   OUT) :: iscale_factor(:, :, :)
INTEGER        , INTENT(   OUT) :: iscfsi(:, :)
INTEGER :: i, i0, i1, iband
LOGICAL :: qfirst = .TRUE.
IF (qfirst) THEN
 qfirst = .FALSE.
 CALL initialize_scalefactor()
END IF
CALL get_scale_factor(subband, iscale_factor)
CALL scale_factor_select_information(iscale_factor, iscfsi)
DO i = 1, 3
 i0 = 12 * (i - 1) + 1
 i1 = 12 *  i
 DO iband = 1, table%nband
  subband(iband, 1, i0:i1) = subband(iband, 1, i0:i1) / scale_factor(iscale_factor(iband, 1, i))
  subband(iband, 2, i0:i1) = subband(iband, 2, i0:i1) / scale_factor(iscale_factor(iband, 2, i))
 END DO
END DO
RETURN 
END SUBROUTINE subband_normalization
!-------------------------------------------------------------------
SUBROUTINE get_scale_factor(subband, iscale_factor)
IMPLICIT NONE
REAL (KIND = 8), INTENT(IN ) :: subband(:, :, :)
INTEGER        , INTENT(OUT) :: iscale_factor(:, :, :)
REAL (KIND = 8) :: subband_max(1)
INTEGER :: i, i0, i1, ichannel, iband, k
DO i = 1, 3
 i0 = 12 * (i - 1) + 1
 i1 = 12 *  i
 DO ichannel = 1, SIZE(subband, 2)
  DO iband = 1, 32
   subband_max = MAXVAL( ABS( subband(iband, ichannel, i0:i1) ) )
   DO k = 62, 0, -1
    IF ( subband_max(1) < scale_factor(k) ) THEN
     iscale_factor(iband, ichannel, i) = k
     EXIT
    END IF
   END DO
  END DO
 END DO
END DO
RETURN
END SUBROUTINE get_scale_factor
!-------------------------------------------------------------------
SUBROUTINE scale_factor_select_information(iscale_factor, iscfsi)
IMPLICIT NONE
INTEGER, INTENT(IN OUT) :: iscale_factor(:, :, :)
INTEGER, INTENT(   OUT) :: iscfsi(:, :)
INTEGER :: iband, ichannel, mscf1, mscf2, iclass1, iclass2, ipattern
DO ichannel = 1, SIZE(iscfsi, 2)
 DO iband = 1, 32
  mscf1 = iscale_factor(iband, ichannel, 1) - iscale_factor(iband, ichannel, 2)
  mscf2 = iscale_factor(iband, ichannel, 2) - iscale_factor(iband, ichannel, 3)
  iclass1 = iget_class(mscf1)
  iclass2 = iget_class(mscf2)
  ipattern = itransmission_pattern(iclass2, iclass1)
  CALL set_scale(iscale_factor(iband, ichannel, :), ipattern)
  iscfsi(iband, ichannel) = iscale_factor_select_information(iclass2, iclass1)
 END DO
END DO
RETURN
END SUBROUTINE scale_factor_select_information
!-------------------------------------------------------------------
FUNCTION iget_class(m) RESULT(ires)
IMPLICIT NONE
INTEGER, INTENT(IN) :: m
INTEGER :: ires
SELECT CASE (m)
 CASE (:-3)
  ires = 1
 CASE (-2:-1)
  ires = 2
 CASE (0)
  ires = 3
 CASE (1:2)
  ires = 4
 CASE (3:)
  ires = 5
END SELECT
RETURN
END FUNCTION iget_class
!-------------------------------------------------------------------
SUBROUTINE set_scale(iscale, ipattern)
IMPLICIT NONE
INTEGER, INTENT(IN OUT) :: iscale(:)
INTEGER, INTENT(IN    ) :: ipattern
SELECT CASE (ipattern)
 CASE (123)
  CONTINUE
 CASE (122)
  iscale(3) = iscale(2)
 CASE (133)
  iscale(2) = iscale(3)
 CASE (113)
  iscale(2) = iscale(1)
 CASE (111)
  iscale(2) = iscale(1)
  iscale(3) = iscale(1)
 CASE (222)
  iscale(1) = iscale(2)
  iscale(3) = iscale(2)
 CASE (333)
  iscale(1) = iscale(3)
  iscale(2) = iscale(3)
 CASE (444)
  iscale(1) = MIN( iscale(1), iscale(2), iscale(3) )
  iscale(2) = iscale(1)
  iscale(3) = iscale(1)
 CASE DEFAULT
  STOP 'undefined transmission pattern: set_scale'
END SELECT
RETURN
END SUBROUTINE set_scale
!-------------------------------------------------------------------
SUBROUTINE initialize_snr()
IMPLICIT NONE
snr( 0) =  0.00d0 !  0 bit
snr( 1) =  7.00d0 !  2 bit
snr( 2) = 11.00d0 !  2.59 bit log2(6)
snr( 3) = 16.00d0 !  3 bit
snr( 4) = 20.84d0 !  3.32 bit log2(10)
snr( 5) = 25.28d0 !  4 bit
snr( 6) = 31.59d0 !  5 bit
snr( 7) = 37.75d0 !  6 bit
snr( 8) = 43.84d0 !  7 bit
snr( 9) = 49.89d0 !  8 bit
snr(10) = 55.93d0 !  9 bit
snr(11) = 61.96d0 ! 10 bit
snr(12) = 67.98d0 ! 11 bit
snr(13) = 74.01d0 ! 12 bit
snr(14) = 80.03d0 ! 13 bit
snr(15) = 86.05d0 ! 14 bit
snr(16) = 92.01d0 ! 15 bit
snr(17) = 98.01d0 ! 16 bit
RETURN
END SUBROUTINE initialize_snr
!-------------------------------------------------------------------
SUBROUTINE bit_allocation(smr, iscfsi, ialloc_bits, itot_bits, max_bits)
IMPLICIT NONE
REAL (KIND = 8), INTENT(IN    ) :: smr(:, :)
INTEGER        , INTENT(IN    ) :: max_bits, iscfsi(:, :)
INTEGER        , INTENT(   OUT) :: ialloc_bits(:, :)
INTEGER        , INTENT(IN OUT) :: itot_bits
INTEGER :: ireq_bits(32, 2)
LOGICAL :: qfirst = .TRUE.
INTEGER :: iband, max_pos(2), k, n0, n1
REAL(KIND = 8) :: amnr(32, 2)
IF (qfirst) THEN
 qfirst = .FALSE.
 CALL initialize_snr()
END IF
ialloc_bits = 0
amnr = smr - snr(0)
ireq_bits = 999999 ! large number
DO iband = 1, table%nband
 ireq_bits(iband, :) = 2 & ! scfsi
                     + 6 * nscfsi(iscfsi(iband, :)) & ! scale_factor
                     + 3 / table%nsample(1) * table%nlength(1) * 12 ! data
END DO
DO
 max_pos = MAXLOC(amnr, mask = ireq_bits < max_bits - itot_bits)
 IF ( max_pos(1) <= 0 .OR. max_pos(2) <= 0 ) EXIT
 IF ( amnr( max_pos(1), max_pos(2) ) < -200.0 ) EXIT
 itot_bits = itot_bits + ireq_bits( max_pos(1), max_pos(2) )
 k = ialloc_bits( max_pos(1), max_pos(2) )
 IF ( k == 2**table%nbal(max_pos(1)) - 2 ) THEN ! true ISO
  ireq_bits( max_pos(1), max_pos(2) ) = 999999  ! maximum allowed bits
 ELSE IF ( table%nbal(max_pos(1)) /= 4 .AND. k == 2**table%nbal(max_pos(1)) - 3 ) THEN ! don't use maximum bit_alloc in higher bands non-ISO
  ireq_bits( max_pos(1), max_pos(2) ) = 999999
 ELSE
  n0 = table%nbit(max_pos(1) , k    )
  n1 = table%nbit(max_pos(1) , k + 1)
  ireq_bits( max_pos(1), max_pos(2) ) = 3 / table%nsample(n1) * table%nlength(n1) * 12 &
                                      - 3 / table%nsample(n0) * table%nlength(n0) * 12
 END IF
 ialloc_bits( max_pos(1), max_pos(2) ) = k + 1
 amnr       ( max_pos(1), max_pos(2) ) = smr( max_pos(1), max_pos(2) ) - snr(k + 1)
END DO
RETURN
END SUBROUTINE bit_allocation
!-------------------------------------------------------------------
SUBROUTINE initialize_quantization()
IMPLICIT NONE
INTEGER :: i
b(1) = -1.0d-9 * ANINT( 1.0d9 / 2.0d0**2.0d0 )
a(1) =  1.0d0 + b(1)
b(2) = -1.0d-9 * ANINT( 3.0d9 / 8.0d0 )
a(2) =  1.0d0 + b(2)
b(3) = -1.0d-9 * ANINT( 1.0d9 / 2.0d0**3.0d0 )
a(3) =  1.0d0 + b(3)
b(4) = -1.0d-9 * ANINT( 7.0d9 / 16.0d0 )
a(4) =  1.0d0 + b(4)
DO i = 5, 17
 b(i) = -1.0d-9 * ANINT(1.0d9 / 2.0d0**REAL(i - 1, KIND = 8))
 a(i) =  1.0d0 + b(i)
END DO
RETURN
END SUBROUTINE initialize_quantization
!-------------------------------------------------------------------
SUBROUTINE quantization(ialloc_bits, subband, isubband)
IMPLICIT NONE
INTEGER        , INTENT(IN ) :: ialloc_bits(:, :)
REAL (KIND = 8), INTENT(IN ) ::  subband(:, :, :)
INTEGER        , INTENT(OUT) :: isubband(:, :, :)
INTEGER :: ichannel, iband, msb, k, i, j, j0, j1
INTEGER, PARAMETER :: imsb(0:17) = (/0, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 /)
LOGICAL :: qfirst = .TRUE.
REAL(KIND = 8) :: s(12)
IF (qfirst) THEN
 qfirst = .FALSE.
 CALL initialize_quantization()
END IF
DO ichannel = 1, SIZE(subband, 2)
 DO iband = 1, table%nband
  k = table%nbit(iband, ialloc_bits(iband, ichannel))
  msb = 2**imsb(k)
  IF (k /= 0) THEN
   DO j = 1, 3
    j0 = 12 * (j - 1) + 1
    j1 = 12 * j
    s = a(k) * subband(iband, ichannel, j0:j1) + b(k)
    DO i = 1, 12
     IF (s(i) >= 0.0) THEN
      isubband(iband, ichannel, j0 - 1 + i) = INT( s(i) * REAL(msb, KIND = 8) ) + msb
     ELSE
      isubband(iband, ichannel, j0 - 1 + i) = INT( (s(i) + 1.0d0) * REAL(msb, KIND = 8) )
     END IF
    END DO
   END DO
  ELSE
   isubband(iband, ichannel, :) = 0
  END IF
 END DO
END DO
RETURN
END SUBROUTINE quantization
!-------------------------------------------------------------------
END MODULE layer2

