!     Last change:  HO   27 May 2000   11:47 pm
MODULE mpeg
IMPLICIT NONE
PUBLIC
TYPE:: mpeg_parameters
 INTEGER :: mtype
 INTEGER :: layer
 INTEGER :: ibit_rate
 INTEGER :: isample_rate
 INTEGER :: ipsychoacoustic
 INTEGER :: iemphasis
 INTEGER :: ipadding
 INTEGER :: icrc
 INTEGER :: mode
 INTEGER :: iextension
 INTEGER :: mode_extension
 INTEGER :: icopyright
 INTEGER :: ioriginal
END TYPE mpeg_parameters
!MPEG1 / audio
INTEGER, PARAMETER :: mpeg_frame_size(3)      = (/1152, 1152, 384/)
INTEGER, PARAMETER :: mpeg_sample_rates(0:3)  = (/44100, 48000, 32000, 0/)
INTEGER, PARAMETER :: mpeg_bit_rates(0:14, 3) = &
  RESHAPE( (/ 0, 32, 40, 48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320,    &
              0, 32, 48, 56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384,    &
              0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 414, 448 /), &
			(/15, 3/) )
CHARACTER (LEN = 8) :: mpeg_mode_names(4)      = (/'stereo  ', 'j-stereo', 'dual-ch ', 'mono    '/)
CHARACTER (LEN = 3) :: mpeg_layer_names(3)     = (/'III', 'II ', 'I  '/)
CHARACTER (LEN = 7) :: mpeg_version_names(0:3) = (/'MPEG2.5', '       ', 'MPEG-II', 'MPEG-I '/)
CHARACTER (LEN = 6) :: mpeg_psy_names(4)       = (/'      ',  'Enoken', '      ', '      '/)
CHARACTER (LEN = 7) :: mpeg_demp_names(4)      = (/'none   ', '50/15us', '       ', 'CITT   '/)
!--------------------------------------------------------------------
END MODULE mpeg

