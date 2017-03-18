!     Last change:  HO   31 May 2000    1:26 am
!==============================================================================
MODULE bit_io
USE mpeg
USE crc
USE layer2
IMPLICIT NONE
PRIVATE 
PUBLIC :: open_mpg_file, close_mpg_file, write_bits_1frame, clear_bit_buff, &
          encode_header, encode_crc, encode_alloc_bits, encode_scfsi, &
          encode_scale_factor, encode_body
INTEGER :: iw, ip
CHARACTER (LEN = 50000) :: bit_string
CONTAINS
!---------------------------------------------------------------------
SUBROUTINE abort(text)
IMPLICIT NONE
CHARACTER (LEN = *), INTENT(IN) :: text
WRITE(*, *) 'Abort:: ', text
STOP
END SUBROUTINE abort
!-------------------------------------------------------------------
SUBROUTINE open_mpg_file(iwrite, fname)
IMPLICIT NONE
INTEGER, INTENT(IN) :: iwrite
CHARACTER (LEN = *) :: fname
INTEGER :: io
iw = iwrite
OPEN(iw, FILE = fname, IOSTAT = io, STATUS = 'unknown', RECORDTYPE = 'stream')
IF (io /= 0) THEN
 WRITE(*, *) 'I/O error ', io, ' occuerred. file =', iw, 'file name ', fname
 CALL abort('Check output file!')
END IF
RETURN
END SUBROUTINE open_mpg_file
!-------------------------------------------------------------------
SUBROUTINE close_mpg_file
IMPLICIT NONE
CLOSE(iw)
RETURN
END SUBROUTINE close_mpg_file
!-------------------------------------------------------------------
SUBROUTINE write_bits_1frame(n)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
INTEGER :: i, j, ipos, m
CHARACTER(LEN = 4) ::cm
EQUIVALENCE (m, cm) ! integer*4 assumed for m
ipos = 0
DO i = 1, n, 8
 m = 0
 DO j = 1, 8
  ipos = ipos + 1
  IF (ipos > LEN(bit_string)) EXIT
  IF (bit_string(ipos:ipos) == '1') m = m + 2**(8 - j)
 END DO
 WRITE(iw, '(a1)', ADVANCE = 'no') cm(1:1)   ! little endian assumed
END DO
RETURN
END SUBROUTINE write_bits_1frame
!-------------------------------------------------------------------
SUBROUTINE clear_bit_buff(n)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
bit_string = REPEAT(' ', n)
RETURN
END SUBROUTINE clear_bit_buff
!-------------------------------------------------------------------
SUBROUTINE encode_header(mpg)
IMPLICIT NONE
TYPE (mpeg_parameters), INTENT(IN) :: mpg
ip = 1
CALL put_bits_c('11111111111'      )  !sync word
CALL put_bits(2, mpg%mtype         )  !mpeg1
CALL put_bits(2, mpg%layer         )  !layer 1
CALL put_bits(1, mpg%icrc          )  !CRC check no
CALL put_bits(4, mpg%ibit_rate     )  !bitrate 
CALL put_bits(2, mpg%isample_rate  )  !sampling frequency 44.1
CALL put_bits(1, mpg%ipadding      )  !ipadding
CALL put_bits(1, mpg%iextension    )  !private bit : unused
CALL put_bits(2, mpg%mode          )  !stereo
CALL put_bits(2, mpg%mode_extension)  !mode
CALL put_bits(1, mpg%icopyright    )
CALL put_bits(1, mpg%ioriginal     )
CALL put_bits(2, mpg%iemphasis     )
RETURN
END SUBROUTINE encode_header
!-------------------------------------------------------------------
SUBROUTINE encode_crc(mpg, ialloc_bits, iscfsi)
IMPLICIT NONE
TYPE (mpeg_parameters), INTENT(IN) :: mpg
INTEGER               , INTENT(IN) :: ialloc_bits(:, :), iscfsi(:, :)
INTEGER :: iband, ichannel, icrc
icrc = Z'0000FFFF'
CALL crc16(4, mpg%ibit_rate     , icrc)
CALL crc16(2, mpg%isample_rate  , icrc)
CALL crc16(1, mpg%ipadding      , icrc)
CALL crc16(1, mpg%iextension    , icrc)
CALL crc16(2, mpg%mode          , icrc)
CALL crc16(2, mpg%mode_extension, icrc)
CALL crc16(1, mpg%icopyright    , icrc)
CALL crc16(1, mpg%ioriginal     , icrc)
CALL crc16(2, mpg%iemphasis     , icrc)
DO iband = 1, table%nband
 DO ichannel = 1, SIZE(ialloc_bits, 2)
  CALL crc16(table%nbal(iband), ialloc_bits(iband, ichannel), icrc)
 END DO
END DO
DO iband = 1, table%nband
 DO ichannel = 1, SIZE(ialloc_bits, 2)
  IF (ialloc_bits(iband, ichannel) /= 0) CALL crc16(2, iscfsi(iband, ichannel), icrc)
 END DO
END DO
CALL put_bits(16, icrc)
RETURN
END SUBROUTINE encode_crc
!-------------------------------------------------------------------
SUBROUTINE encode_alloc_bits(ialloc_bits)
IMPLICIT NONE
INTEGER, INTENT(IN    ) :: ialloc_bits(:, :)
INTEGER :: iband, ichannel
DO iband = 1, table%nband
 DO ichannel = 1, SIZE(ialloc_bits, 2)
  CALL put_bits(table%nbal(iband), ialloc_bits(iband, ichannel) )
 END DO
END DO
RETURN
END SUBROUTINE encode_alloc_bits
!-------------------------------------------------------------------
SUBROUTINE encode_scfsi(ialloc_bits, iscfsi)
IMPLICIT NONE
INTEGER, INTENT(IN    ) :: ialloc_bits(:, :), iscfsi(:, :)
INTEGER :: iband, ichannel
DO iband = 1, table%nband
 DO ichannel = 1, SIZE(ialloc_bits, 2)
  IF (ialloc_bits(iband, ichannel) /= 0) CALL put_bits(2, iscfsi(iband, ichannel) )
 END DO
END DO
RETURN
END SUBROUTINE encode_scfsi
!-------------------------------------------------------------------
SUBROUTINE encode_scale_factor(ialloc_bits, iscfsi, iscale_factor)
IMPLICIT NONE
INTEGER, INTENT(IN    ) :: ialloc_bits(:, :), iscfsi(:, :), iscale_factor(:, :, :)
INTEGER :: iband, ichannel
DO iband = 1, table%nband
 DO ichannel = 1, SIZE(ialloc_bits, 2)
  IF (ialloc_bits(iband, ichannel) /= 0) THEN
   SELECT CASE ( nscfsi( iscfsi(iband, ichannel) ) )
    CASE (1)
     CALL put_bits(6, iscale_factor(iband, ichannel, 1) )
    CASE (2)
     CALL put_bits(6, iscale_factor(iband, ichannel, 1) )
     CALL put_bits(6, iscale_factor(iband, ichannel, 3) )
    CASE (3)
     CALL put_bits(6, iscale_factor(iband, ichannel, 1) )
     CALL put_bits(6, iscale_factor(iband, ichannel, 2) )
     CALL put_bits(6, iscale_factor(iband, ichannel, 3) )
    CASE DEFAULT
     STOP 'unexpected number: encode_body'
   END SELECT
  END IF
 END DO
END DO
RETURN
END SUBROUTINE encode_scale_factor
!-------------------------------------------------------------------
SUBROUTINE encode_body(ialloc_bits, isubband)
IMPLICIT NONE
INTEGER, INTENT(IN) :: ialloc_bits(:, :), isubband(:, :, :)
INTEGER :: iband, ichannel, igranule, j, k, m, n
DO igranule = 1, 12
 DO iband = 1, table%nband
  DO ichannel = 1, SIZE(ialloc_bits, 2)
   IF (ialloc_bits(iband, ichannel) /= 0) THEN
    k = table%nbit(iband, ialloc_bits(iband, ichannel))
    IF (table%nsample(k) == 1) THEN
     DO j = 3 * (igranule - 1) + 1, 3 * igranule
      CALL put_bits(table%nlength(k), isubband(iband, ichannel, j) )
     END DO
    ELSE
     j = 3 * (igranule - 1)
     n = table%nsteps(k)
     m = isubband(iband, ichannel, j + 1)        &
       + isubband(iband, ichannel, j + 2) * n    &
       + isubband(iband, ichannel, j + 3) * n**2
     CALL put_bits(table%nlength(k), m )
    END IF
   END IF
  END DO
 END DO
END DO
RETURN
END SUBROUTINE encode_body
!-------------------------------------------------------------------
SUBROUTINE put_bits(n, inp)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n, inp
INTEGER :: i, m
DO i = 1, n
 m = 2**(n - i)
 IF (MOD(inp / m, 2) == 1) THEN
  bit_string(ip:ip) = '1'
 ELSE
  bit_string(ip:ip) = '0'
 END IF
 ip = ip + 1
END DO
RETURN
END SUBROUTINE put_bits
!-------------------------------------------------------------------
SUBROUTINE put_bits_c(str)
IMPLICIT NONE
CHARACTER (LEN = *) :: str
INTEGER :: i
DO i = 1, LEN_TRIM(str)
 IF (str(i:i) /= '0' .AND. str(i:i) /= '1') &
     CALL abort('invalid string: subroutine put_bit_c')
 bit_string(ip:ip) = str(i:i)
 ip = ip + 1
END DO
RETURN
END SUBROUTINE put_bits_c
!-------------------------------------------------------------------
END MODULE bit_io

