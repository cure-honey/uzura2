!     Last change:  HO   28 May 2000   10:42 am
!=======================================================================
MODULE wav_io
USE mpeg
IMPLICIT NONE
PRIVATE
PUBLIC :: riff_chunk, fmt_chunk, data_chunk              ! type
PUBLIC :: open_wav_file, close_wav_file, read_pcm_1frame ! subroutine
INTEGER, SAVE :: ir
!
TYPE :: fmt_chunk
 CHARACTER (LEN = 4):: chunk_id  != 'fmt '
 INTEGER :: ichunk_size
 INTEGER :: iformat_type, ichannels
 INTEGER :: isamples_per_sec
 INTEGER :: ibytes_per_sec
 INTEGER :: iblock_size, ibits_per_sample
END TYPE fmt_chunk
!
TYPE :: data_chunk
 CHARACTER (LEN = 4) :: chunk_id  != 'data'
 INTEGER :: ichunk_size !( bytes )
!! PCM data follows INTEGER (KIND = 2) :: idata(isize / 2)
END TYPE data_chunk
!
TYPE :: riff_chunk
 CHARACTER (LEN = 4) :: chunk_id  != 'RIFF'
 INTEGER :: ichunk_size
 CHARACTER (LEN = 4) :: format_type ! = 'WAVE'
 TYPE (data_chunk) :: dat
 TYPE ( fmt_chunk) :: fmt
END TYPE riff_chunk
!
CONTAINS
!--------------------------------------------------------------------
SUBROUTINE init_chunk_names(riff)
IMPLICIT NONE
TYPE (riff_chunk), INTENT(OUT) :: riff
riff%chunk_id     = 'RIFF'
riff%format_type  = 'WAVE'
riff%fmt%chunk_id = 'fmt '
riff%dat%chunk_id = 'data'
RETURN
END SUBROUTINE init_chunk_names
!---------------------------------------------------------------------
FUNCTION word32() RESULT(res)
IMPLICIT NONE
CHARACTER (LEN = 4) :: res
INTEGER :: io
READ(ir, '(a4)', IOSTAT = io) res
RETURN
END FUNCTION word32
!---------------------------------------------------------------------
FUNCTION word16() RESULT(res)
IMPLICIT NONE
CHARACTER (LEN = 2) :: res
INTEGER :: io
READ(ir, '(a2)', IOSTAT = io) res
RETURN
END FUNCTION word16
!---------------------------------------------------------------------
FUNCTION int32() RESULT(ires)
IMPLICIT NONE
INTEGER (KIND = 4) :: ires
ires = TRANSFER(word32(), ires) ! little endian assumed
RETURN
END FUNCTION int32
!---------------------------------------------------------------------
FUNCTION int16() RESULT(ires)
IMPLICIT NONE
INTEGER (KIND = 2) :: ires
ires = TRANSFER(word16(), ires) ! little endian assumed
RETURN
END FUNCTION int16
!---------------------------------------------------------------------
SUBROUTINE abort(text)
IMPLICIT NONE
CHARACTER (LEN = *), INTENT(IN) :: text
WRITE(*, *) 'Abort:: ', text
STOP
END SUBROUTINE abort
!------------------------------------------------------------------
SUBROUTINE open_wav_file(iread, fname, riff)
IMPLICIT NONE
INTEGER            , INTENT(IN    ) :: iread
CHARACTER (LEN = *), INTENT(IN    ) :: fname
TYPE (riff_chunk)  , INTENT(IN OUT) :: riff
INTEGER :: io
ir = iread
CALL init_chunk_names(riff)
OPEN(ir, FILE = fname, STATUS = 'old', IOSTAT = io, RECORDTYPE = 'STREAM') !non-standard "stream mode"
IF (io /= 0) THEN
 WRITE(*, *) 'I/O error ', io, ' occuerred. file =', iread, ' file name ', fname
 CALL abort('Check input file!')
ELSE
 CALL check_riff_chunk(riff)
END IF
RETURN
END SUBROUTINE open_wav_file
!------------------------------------------------------------------
SUBROUTINE close_wav_file
IMPLICIT NONE
CLOSE(ir)
RETURN
END SUBROUTINE close_wav_file
!------------------------------------------------------------------
SUBROUTINE check_riff_chunk(riff)
IMPLICIT NONE
TYPE (riff_chunk), INTENT(IN OUT) :: riff
IF ( word32() == riff%chunk_id ) THEN    ! 'RIFF'?
 WRITE(*, '(a)', ADVANCE = 'NO') ' MS RIFF '
ELSE
 CALL abort('This is not MS-RIFF file!')
END IF
riff%ichunk_size = int32()
IF ( word32() == riff%format_type ) THEN ! 'WAVE'?
 WRITE(*, '(a)', ADVANCE = 'NO') 'WAV audio '
ELSE
 WRITE(*, *)
 CALL abort('This is not WAV file!')
END IF
CALL check_fmt_chunk(riff%fmt)
CALL check_dat_chunk(riff%dat)
RETURN
END SUBROUTINE check_riff_chunk
!------------------------------------------------------------------
SUBROUTINE check_fmt_chunk(fmt)
IMPLICIT NONE
TYPE (fmt_chunk), INTENT(IN OUT) :: fmt
IF ( word32() /= fmt%chunk_id ) CALL abort('Cannot find format chunk!')
fmt%ichunk_size      =     int32()
fmt%iformat_type     = INT(int16(), KIND = 4)
fmt%ichannels        = INT(int16(), KIND = 4)
fmt%isamples_per_sec =     int32()
fmt%ibytes_per_sec   =     int32()
fmt%iblock_size      = INT(int16(), KIND = 4)
fmt%ibits_per_sample = INT(int16(), KIND = 4)
IF ( fmt%iformat_type     /=  1) CALL abort('Unknown WAVE format!') !linear PCM
IF ( fmt%ibits_per_sample /= 16) CALL abort('Not 16bit data!')
SELECT CASE ( fmt%ichannels )
 CASE (1)
  WRITE(*, '(a, i3, a, i6, a)', ADVANCE = 'NO') &
   'Monoral', fmt%ibits_per_sample, 'bit Sampling rate', fmt%isamples_per_sec, 'Hz '
 CASE (2)
  WRITE(*, '(a, i3, a, i6, a)', ADVANCE = 'NO') &
   'Stereo' , fmt%ibits_per_sample, 'bit Sampling rate', fmt%isamples_per_sec, 'Hz '
 CASE DEFAULT
  WRITE(*, '(a, i1)') ' Number of wave channels is ', fmt%ichannels
  CALL abort('Wave channel must be 1 or 2!')
END SELECT
RETURN
END SUBROUTINE check_fmt_chunk
!------------------------------------------------------------------
SUBROUTINE check_dat_chunk(dat)
IMPLICIT NONE
TYPE (data_chunk), INTENT(IN OUT) :: dat
IF ( word32() /= dat%chunk_id ) CALL abort('Cannot find data chunk!')
dat%ichunk_size = int32()
RETURN
END SUBROUTINE check_dat_chunk
!------------------------------------------------------------------
SUBROUTINE wav_read(pcm)
IMPLICIT NONE
REAL (KIND = 8), INTENT(OUT) :: pcm(:, :)
REAL (KIND = 8), PARAMETER   :: denom = 32768.0d0 !32768 = 2^15
INTEGER       , PARAMETER   :: maxbuff = 1152 * 2
CHARACTER (LEN = 2) :: cbuff16(maxbuff)
INTEGER :: i, nchannel, ndat
INTEGER  (KIND = 2) :: ibuff16(maxbuff)
EQUIVALENCE (cbuff16, ibuff16)
ndat     = SIZE(pcm, 1)
nchannel = SIZE(pcm, 2)
IF (ndat * nchannel > maxbuff) CALL abort('check maxbuff: subroutine wav_get')
ibuff16 =0
SELECT CASE (nchannel)
 CASE (1) !mono
  CALL wav_read_sub( cbuff16(1:ndat) )
  DO i = 1, ndat
   pcm(i, 1) = REAL( ibuff16(i), KIND = 8) / denom          ! little endian assumed
   pcm(i, 2) = 0.0d0
  END DO
 CASE (2) !stereo
  CALL wav_read_sub( cbuff16(1:2 * ndat) )
  DO i = 1, ndat
   pcm(i, 1) = REAL( ibuff16(2 * i - 1), KIND = 8) / denom  ! little endian assumed
   pcm(i, 2) = REAL( ibuff16(2 * i    ), KIND = 8) / denom  ! little endian assumed
  END DO
 CASE DEFAULT
  CALL abort('ichannel must be 1 or 2: subroutine wav_get')
END SELECT
RETURN
END SUBROUTINE wav_read
!------------------------------------------------------------------
SUBROUTINE wav_read_sub(cha16)
IMPLICIT NONE
CHARACTER (LEN = 2), INTENT(OUT) :: cha16(:)
INTEGER :: io
READ(ir, '(a2)', iostat = io) cha16
SELECT CASE (io)
 CASE (0)
  CONTINUE
 CASE (-1)
  WRITE(*,*) 'End of File!'
 CASE DEFAULT
  WRITE(*, *) 'file ', ir, ' iostat ', io
  CALL abort('I/O error occurred while reading wav file')
END SELECT
RETURN
END SUBROUTINE wav_read_sub
!------------------------------------------------------------------
SUBROUTINE read_pcm_1frame(pcm)
IMPLICIT NONE
REAL (KIND = 8), INTENT(OUT) :: pcm(:, :)
pcm = EOSHIFT(pcm, -1152, 0.0d0, 1)
CALL wav_read(pcm(1152:1:-1, :))
RETURN
END SUBROUTINE read_pcm_1frame
!------------------------------------------------------------------
END MODULE wav_io

