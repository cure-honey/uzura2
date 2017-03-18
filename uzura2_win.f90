MODULE arguments 
! module for command line option  ! Compaq(DEC) FORTRAN for Intel 
USE DFLIB
USE mpeg
PRIVATE
PUBLIC :: get_option
CONTAINS
!------------------------------------------------------------------
SUBROUTINE get_option(mpg, fn_in)
IMPLICIT NONE
TYPE (mpeg_parameters), INTENT(   OUT) :: mpg
CHARACTER (LEN = *)   , INTENT(IN OUT) :: fn_in
INTEGER   (KIND = 4) :: narg
INTEGER   (KIND = 2) :: iarg, istatus
CHARACTER (LEN = 80) :: buffer
CHARACTER (LEN = 6) :: fmt
iarg = 0
narg = NARGS()
DO
 iarg = iarg + 1
 IF (iarg >= narg) CALL print_option()
 CALL GETARG(iarg, buffer)
 IF (buffer(1:1) /= '-') EXIT  
 SELECT CASE(TRIM(buffer))
  CASE ('-b') 
   iarg = iarg + 1
   IF ( iarg >= narg ) CALL print_option()
   CALL GETARG(iarg, buffer, istatus)
   WRITE(fmt, '(a, i1, a)') '(I', istatus, ')' 
   READ(buffer, fmt) mpg%ibit_rate
   IF (mpg%ibit_rate < 1 .OR. mpg%ibit_rate > 14) CALL print_option()  
  CASE ('-crc') 
   mpg%icrc       = 0 ! CRC16 on 
  CASE ('-c') 
   mpg%icopyright = 1 ! copyrigt on
  CASE ('-o') 
   mpg%ioriginal  = 1 ! original on
  CASE DEFAULT
   CALL print_option()
   STOP
 END SELECT
END DO
fn_in = TRIM(buffer)
RETURN
END SUBROUTINE get_option
!---------------------------------------------------------------
SUBROUTINE print_option()
IMPLICIT NONE
WRITE(*, *) 'Usage : uzura -option file_name '
WRITE(*, *) '      : file_name.wav -> file_name.mp?'
WRITE(*, *) 'Option: -b 1..14  bitrate '
WRITE(*, *) '        -crc      CRC16 error protection on'
WRITE(*, *) '        -c        copyright flag on'
WRITE(*, *) '        -o        original  flag on'
RETURN
END SUBROUTINE print_option
!---------------------------------------------------------------
END MODULE arguments
!===============================================================
PROGRAM uzura2
USE mpeg
USE wav_io
USE polyphase
USE psycho
USE layer2
USE bit_io
USE arguments
IMPLICIT NONE
TYPE (riff_chunk) :: riff
TYPE (mpeg_parameters) :: mpg
INTEGER        , ALLOCATABLE :: iscale_factor(:, :, :), iscfsi(:, :), &
                                isubband(:, :, :), ialloc_bits(:, :)
REAL (KIND = 8), ALLOCATABLE :: pcm(:, :), subband(:, :, :), smr(:, :)
INTEGER :: max_bits, itot_bits
INTEGER :: nchannel, iframe, itotal_frames
CHARACTER (LEN = 40) :: file_name, fn_in, fn_out
!
mpg%mtype           = 3 ! 0:mpeg2.5, 1:---, 2:mpeg2, 3:mpeg1
mpg%layer           = 2 ! layer { 1 = III, 2 = II, 3 = I }
mpg%ibit_rate       = 14
mpg%isample_rate    = 0
mpg%iemphasis       = 0
mpg%ipadding        = 0
mpg%icrc            = 1 ! CRC16  0 enabled / 1 disabled
mpg%iextension      = 0
mpg%mode            = 0
mpg%mode_extension  = 0
mpg%icopyright      = 0
mpg%ioriginal       = 0
mpg%ipsychoacoustic = 2
CALL get_option(mpg, file_name)
fn_in  = TRIM(file_name) // '.wav'
fn_out = TRIM(file_name) // '.mp2'
CALL pr_info(mpg)
CALL open_wav_file(10, fn_in, riff)
CALL play_time(riff)
nchannel = riff%fmt%ichannels
itotal_frames = riff%dat%ichunk_size / (mpeg_frame_size(mpg%layer) * nchannel * 2)
ALLOCATE( pcm(1632, nchannel), subband(32, nchannel, 36), smr(32, nchannel) )
ALLOCATE( iscale_factor(32, nchannel, 3),  iscfsi(32, nchannel), &
          isubband(32, nchannel, 36), ialloc_bits(32, nchannel) )
CALL open_mpg_file(9, fn_out)
CALL select_table(mpg%isample_rate, mpg%ibit_rate, nchannel)
pcm = 0.0d0
iframe = 1
DO WHILE (iframe <= itotal_frames )
 CALL get_maxbits(max_bits, mpg%ipadding)
 CALL clear_bit_buff(max_bits)
 CALL encode_header(mpg)
 itot_bits = 32 
 CALL read_pcm_1frame(pcm)
 CALL polyphase_filter36(pcm, subband)
 CALL psychoacoustics(pcm, smr, riff%fmt%isamples_per_sec)
 CALL subband_normalization(subband, iscale_factor, iscfsi)
 IF (mpg%icrc == 0) itot_bits = itot_bits + 16
 itot_bits = itot_bits + SUM(table%nbal)
 CALL bit_allocation(smr, iscfsi, ialloc_bits, itot_bits, max_bits)
 IF (mpg%icrc == 0) CALL encode_crc(mpg, ialloc_bits, iscfsi)
 CALL quantization(ialloc_bits, subband, isubband)
 CALL encode_alloc_bits(ialloc_bits)
 CALL encode_scfsi(ialloc_bits, iscfsi)
 CALL encode_scale_factor(ialloc_bits, iscfsi, iscale_factor)
 CALL encode_body(ialloc_bits, isubband)
 CALL write_bits_1frame(max_bits)
 iframe = iframe + 1
 IF ( MOD(iframe, 50) == 0 ) CALL update_status(iframe, itotal_frames) 
END DO
WRITE(*, *) 'toal frames', iframe - 1, '/', itotal_frames
CALL close_wav_file()
CALL close_mpg_file()
STOP
CONTAINS
!------------------------------------------------------------------
SUBROUTINE select_table(isample_rate, ibit_rate, nchannel)
IMPLICIT NONE
INTEGER, INTENT(IN) :: isample_rate, ibit_rate, nchannel
INTEGER :: itab
INTEGER, PARAMETER :: itable(0:14, 3, 2) = &
 RESHAPE( (/ & ! mono
             2, 3, 3, 1, 1, 1, 2, 2, 2, 2, 2, -1, -1, -1, -1, &
             1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, &
			 2, 4, 4, 1, 1, 1, 2, 2, 2, 2, 2, -1, -1, -1, -1, &
			   ! stereo
			 2, 3, 3, 3, 3, 3, 3, 1, 1, 1, 2,  2,  2,  2,  2, &
			 1, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1,  1,  1,  1,  1, &
			 2, 4, 4, 4, 4, 4, 4, 1, 1, 1, 2,  2,  2,  2,  2 /), (/15, 3, 2/) )
itab = itable( ibit_rate, isample_rate + 1, nchannel )
IF (itab < 0) THEN 
 STOP 'unexpected bit_rate: select_table'
ELSE
 CALL initialize_table(itab)
END IF
RETURN
END SUBROUTINE select_table
!------------------------------------------------------------------
SUBROUTINE calc_slot_size(islot_size, fslot_size)
IMPLICIT NONE
INTEGER       , INTENT(OUT) :: islot_size
REAL(KIND = 8), INTENT(OUT) :: fslot_size
REAL(KIND = 8) :: aslot_size
aslot_size = 144.0d0 * 1000.0d0 * REAL(mpeg_bit_rates(mpg%ibit_rate, mpg%layer), KIND = 8) &
           / REAL(mpeg_sample_rates(mpg%isample_rate), KIND = 8)
aslot_size = 1.0d-3 * ANINT(1.0d3 * aslot_size) 
islot_size = INT(aslot_size)
fslot_size = aslot_size - islot_size
RETURN
END SUBROUTINE calc_slot_size
!------------------------------------------------------------------
SUBROUTINE get_maxbits(max_bits, ipadding)
IMPLICIT NONE
INTEGER,  INTENT(OUT) :: max_bits, ipadding
INTEGER        , SAVE :: islot_size
REAL (KIND = 8), SAVE :: padding, fslot_size
LOGICAL        , SAVE :: qfirst = .TRUE.
IF (qfirst) THEN
 qfirst = .FALSE.
 padding = 0.0d0
 CALL calc_slot_size(islot_size, fslot_size)
END IF
padding = padding + fslot_size
IF (padding > 1) THEN
 ipadding = 1
 padding = padding - 1.0d0
ELSE
 ipadding = 0
END IF
max_bits = ( islot_size + ipadding ) * 8
RETURN
END SUBROUTINE get_maxbits
!------------------------------------------------------------------
SUBROUTINE play_time(riff)
IMPLICIT NONE
TYPE (riff_chunk), INTENT(IN) :: riff
INTEGER :: itot_time, ihour, imin, isec
itot_time = riff%dat%ichunk_size / riff%fmt%ibytes_per_sec
ihour =          itot_time / 3600
imin  =      MOD(itot_time, 3600) / 60
isec  = MOD( MOD(itot_time, 3600) , 60 )
WRITE(*, '(a, i3, a, i2, a, i2)') ' Playtime ', ihour, ':', imin, ':', isec
WRITE(*, *)
RETURN
END SUBROUTINE play_time
!------------------------------------------------------------------
SUBROUTINE pr_info(mpg)
IMPLICIT NONE
TYPE (mpeg_parameters), INTENT(IN) :: mpg
WRITE(*, *) 'Uzura2 (MPEG-1 Audio/LAYER-II Encoder for DEC FORTRAN/WIN) Ver.0.2 (c) H.O. '
WRITE(*, *) 'Psychoacoustic Model ', mpeg_psy_names(mpg%ipsychoacoustic), &
            ' Bit Rate (kbps)', mpeg_bit_rates(mpg%ibit_rate, mpg%layer)
IF (mpg%icrc == 0) WRITE(*, *) 'CRC16 error protection enabled'
RETURN
END SUBROUTINE pr_info
!------------------------------------------------------------------
SUBROUTINE update_status(iframe, itot_frames)
IMPLICIT NONE
INTEGER, INTENT(IN) :: iframe, itot_frames
INTEGER :: it(8), ielapsed, iel_min, iel_sec
INTEGER, SAVE :: istart
LOGICAL :: qfirst = .TRUE.
REAL    :: percent
CHARACTER (LEN = 10) :: time, date, zone
CALL DATE_AND_TIME(date, time, zone, it)
IF (qfirst) THEN
 istart   = it(5) * 3600 + it(6) * 60 + it(7)
 qfirst   = .FALSE.
END IF
ielapsed = it(5) * 3600 + it(6) * 60 + it(7) - istart
iel_min  =     ielapsed / 60
iel_sec  = MOD(ielapsed , 60)
percent = REAL(100 * iframe) / REAL(itot_frames)
WRITE(*, '(a, f6.2, a, i4, 2(a, i2), 3(a, i2.2), a, i4.2, a, i2.2, a)')  &
      '+Processed...', percent, '%  ', &
      it(1), '/', it(2), '/', it(3), ' ', it(5), ':', it(6), ':', it(7), &
      ' time elapsed ', iel_min, 'min ', iel_sec, 'sec'
RETURN
END SUBROUTINE update_status
!----------------------------------------------------------------------------
END PROGRAM uzura2
