;/*-------------------------------------------------------*/
;
;   Function: get_double_from_header
;
;   Extracts sub-string from general byte-array HEADER, starting
;      at character START (where first character is #0), and
;      continuing for LENGTH characters.
;
;   Returns conversion of sub-string to double-precision
;      floating-point value.
;
;
;/*-------------------------------------------------------*/

function get_double_from_header, header, start, length

   return, double(string(header(start : (start + length) - 1)))

end

;/*-------------------------------------------------------*/
;
;   Function: get_float_from_header
;
;   Extracts sub-string from general byte-array HEADER, starting
;      at character START (where first character is #0), and
;      continuing for LENGTH characters.
;
;   Returns conversion of sub-string to single-precision
;      floating-point value.
;
;
;/*-------------------------------------------------------*/

function get_float_from_header, header, start, length

   return, float(string(header(start : (start + length) - 1)))

end

;/*-------------------------------------------------------*/
;
;   Function: get_int_from_header
;
;   Extracts sub-string from general byte-array HEADER, starting
;      at character START (where first character is #0), and
;      continuing for LENGTH characters.
;
;   Returns conversion of sub-string to two-byte integer.
;
;
;/*-------------------------------------------------------*/

function get_int_from_header, header, start, length

   return, fix(string(header(start : (start + length) - 1)))

end
;/*-------------------------------------------------------*/

;
;   Function: get_long_from_header
;
;   Extracts sub-string from general byte-array HEADER, starting
;      at character START (where first character is #0), and
;      continuing for LENGTH characters.
;
;   Returns conversion of sub-string to four-byte integer.
;
;
;/*-------------------------------------------------------*/

function get_long_from_header, header, start, length

   return, long(string(header(start : (start + length) - 1)))

end

;/*-------------------------------------------------------*/
;
;   Function: get_string_from_header
;
;   Extracts sub-string from general byte-array HEADER, starting
;      at character START (where first character is #0), and
;      continuing for LENGTH characters.
;
;
;/*-------------------------------------------------------*/

function get_string_from_header, header, start, length

   return, string(header(start : (start + length) - 1))

end


FUNCTION get_gbt_options, gbt_options, channels = channels
;
; This function interogates the gbt_options passed into it from
; the SADIST-2 gbt header and returns a 10 element flag array
; showing which GBT file options are present (ie. the options
; given by the STRARR options below - this is a  fudge to allow
; reading of the single file OFFline sadist-2 files and the mutli-file
; NRT versions of the file by a single peice of code.
;
; The optional parameter channels returns a [7] element flag array
; showing the presence of channels in the data file. The first index
; is the channel no. in order of increasing wavelength (as per the
; STRARR channel_names below.
;

options = ['H','T','V','Tf','Vf','L','X','Xf','C','Cf']
channel_names = ['0.55µm ','0.67µm ','0.87µm ','1.6µm ','3.7µm ','10.8µm ','12.0µm ']

options_present = BYTARR(10)
options_present[0] = 1 ; The header must be there

PRINT,'Searching for SADIST-2 file options .... '

;
; Set flags for the IR image planes if T option present
;

 IF gbt_options[1] THEN $
  BEGIN
  options_present[1] = 1
  options_present[3] = 1
  END

;
; Set flags for the VIS image planes if V option present
;

IF gbt_options[2] THEN $
  BEGIN
  options_present[2] = 1
  options_present[4] = 1
  END

;
; Set flags for the lat/lon grid if L option present
;

IF gbt_options[3] THEN $
  BEGIN
  options_present[5] = 1
  END
;
; Set flags for the X/Y grid if X option present
;

IF gbt_options[4] THEN $
  BEGIN
  options_present[6] = 1
  options_present[7] = 1
  END

;
; Set flags for the cloud mask if C option present
;

IF gbt_options[5] THEN $
  BEGIN
  options_present[8] = 1
  options_present[9] = 1
  END

;
; Unset all the forward view flags if N option selected
;

IF gbt_options[0] THEN $
  BEGIN
  options_present[3] = 0 ; no Tf
  options_present[4] = 0 ; no Vf
  options_present[7] = 0 ; no Xf
  options_present[9] = 0 ; no Cf
  END

IF ARG_PRESENT(channels) THEN $
 BEGIN
 channels = BYTARR(7)

 IF options_present[1] OR options_present[3] THEN channels[3:6] = 1
 IF options_present[2] OR options_present[4] THEN channels[0:2] = 1

 present = where(channels EQ 1, present_count)
 IF ( present_count GT 0 ) THEN PRINT, 'Channels present: ', channel_names( present )

 missing = where(channels EQ 0, missing_count)
 IF ( missing_count GT 0 ) THEN PRINT, 'Channels missing: ', channel_names( missing )
 END


present = where(options_present EQ 1, present_count)
IF ( present_count GT 0 ) THEN PRINT, 'Options present: ', options( present )

missing = where(options_present EQ 0, missing_count)
IF ( missing_count GT 0 ) THEN PRINT, 'Options missing: ', options( missing )

PRINT,'File option search complete ...'
return, options_present
END

Function get_nrt_flags, file, lun, channels = channels

;
; This procedure has been designed to handle the nrt data fragments that
; would normally be contained in a single gbt product.
; The input to the procedure in the name of the nrt header file *.H file
; this is passed and the findfile is used to identify which other fragments
; are available. If a fragment is found a flag is set, a logical unit number
; assigned, and the file opened ready for get_sadist2_gbt.pro to read the data.
; The options available file options are passed out in options_present a
; bytarr(10), you can decode what they mean by comparing with the text array
; options. The channels available is optionally returned in channels the
; definition of the data structure is given below.
;
; The option parameter channels returns a [7] element flag array
; showing the presence of channels in the data file. The first index
; is the channel no. in order of increasing wavelength (as per the
; STRARR channel_names below.

options = ['H','T','V','Tf','Vf','L','X','Xf','C','Cf']
channel_names = ['0.55µm ','0.67µm ','0.87µm ','1.6µm ','3.7µm ','10.8µm ','12.0µm ']

x = 0L
lun = LONARR(10)
options_present = BYTARR(10)

PRINT,'Searching for NRT file options .... '

parts = STR_SEP( file, '.')
IF ( !version.os_family EQ 'Windows' ) THEN file_prefix = parts[0] ELSE file_prefix = parts[0] + '.' + parts [1]
PRINT, 'Seaching for files prefixed : ',file_prefix
FOR opt = 0, 9 DO $
 BEGIN
 search = file_prefix + '.' + options[opt]
 dummy = FINDFILE( search, count = count)
 IF count gt 0 THEN $
  BEGIN
  options_present[opt] = 1
  GET_LUN, x
  lun[opt] = x
  OPENR, lun[opt], file_prefix + '.' + options[opt], 1024, /block

END $
 ELSE $
  BEGIN
   options_present[opt] = 0
  END
END


;
; Also return the channels that are available in the byte array
; channels
; Byte array channels(0) = 0.55, (1) = 0.67, (2) = 0.87,
; (3) =1.6, (4) = 3.7, (5) = 10.8, (6) =12.0
;

IF ARG_PRESENT(channels) THEN $
 BEGIN
 channels = bytarr(7)
 IF options_present[1] THEN channels[3:6] = 1
 IF options_present[2] THEN channels[0:3] = 1

 present = where(channels EQ 1, present_count)
 IF ( present_count GT 0 ) THEN PRINT, 'Channels present: ', channel_names( present )

 missing = where(channels EQ 0, missing_count)
 IF ( missing_count GT 0 ) THEN PRINT, 'Channels missing: ', channel_names( missing )
 END


present = where(options_present EQ 1, present_count)
IF ( present_count GT 0 ) THEN PRINT, 'Options present: ', options( present )

missing = where(options_present EQ 0, missing_count)
IF ( missing_count GT 0 ) THEN PRINT, 'Options missing: ', options( missing )

PRINT,'File option search complete ...'

return, options_present

END


;/*-------------------------------------------------------*/
;
;   Subroutine: get_sadist2_header.pro
;
;   Reads the SADIST-2 product header from FILE, which has a record
;     length of RECORD_LENGTH.
;
;   Converts header contents to internal representation, where appropriate,
;      and returns interpreted header structure HEADER.
;
;   Interprets first two bytes of header as byte-order word. Returns
;      flag BYTE_SWAP_NEEDED, which identifies whether local system is
;      big-endian, and therefore whether byte-swapping is required.
;
;
;/*-------------------------------------------------------*/

pro get_sadist2_header, file, record_length, header, byte_swap_needed, $
  not_silent = not_silent, not_terminal = not_terminal, head_len = head_len

PRINT, 'Reading header info ... '

;/* Define the byte-order word and a byte-array into which the SADIST-2 product
;      header (minus the byte-order word) will be read. */

   number_of_header_records = FIX(4096 / record_length)
   print, 'number_of_header_records:', number_of_header_records

   IF ((number_of_header_records * record_length) NE 4096) THEN $
      number_of_header_records = number_of_header_records + 1

   byte_order = 0

IF keyword_set(head_len) THEN temp= BYTARR( head_len - 2 ) ELSE   temp = BYTARR( (number_of_header_records * record_length) - 2 )

   print, n_elements(temp)

;/* Define the structure which will be returned by this subroutine. All header
;      contents are converted to the appropriate type. */

   header = { byte_order: 0, byte_swap_needed: 0,$
              file_name: '', instrument: '', $
              vec_type: '', vec_time: 0.0D, vec_utc: '', $
              vec_pos: DBLARR(3), vec_vel: DBLARR(3), vec_long: 0.0D, $
              clock_ut: 0.0d, clock_bin: 0.0D, clock_period: 0.0D, $
              options: INTARR(6), $
              atd: LONARR(2), acq_time: STRARR(2), $
              latitude: FLTARR(4), longitude: FLTARR(4), $
              nad_psm1: 0, nad_psm2: 0, nad_psm_change: 0L, $
              for_psm1: 0, for_psm2: 0, for_psm_change: 0L, $
              nad_rate1: '', nad_rate_change: 0L, $
              for_rate1: '', for_rate_change: 0L, $
              min_temps: FLTARR(6), max_temps: FLTARR(6), $
              nad_sol_elev_start: FLTARR(11), nad_sol_elev_end: FLTARR(11), $
              nad_sat_elev_start: FLTARR(11), nad_sat_elev_end: FLTARR(11), $
              nad_sol_azim_start: FLTARR(11), nad_sol_azim_end: FLTARR(11), $
              nad_sat_azim_start: FLTARR(11), nad_sat_azim_end: FLTARR(11), $
              for_sol_elev_start: FLTARR(11), for_sol_elev_end: FLTARR(11), $
              for_sat_elev_start: FLTARR(11), for_sat_elev_end: FLTARR(11), $
              for_sol_azim_start: FLTARR(11), for_sol_azim_end: FLTARR(11), $
              for_sat_azim_start: FLTARR(11), for_sat_azim_end: FLTARR(11), $
              nad_platform: LONARR(6), for_platform: LONARR(6), $
              nad_pcd: LONARR(8), for_pcd: LONARR(8), $
              nad_invalid: LONARR(10), for_invalid: LONARR(10), $
              max_error: 0 }


;/* Read the SADIST-2 header (including the byte-order word) into the temporary array.
;      Interpret the byte-order word to decide whether system is little- or big-endian. */


   READU, file, byte_order, temp

   header.byte_order = byte_order

   IF (byte_order EQ 16961) THEN byte_swap_needed = 0 ELSE byte_swap_needed = 1

   header.byte_swap_needed = byte_swap_needed


;/* Read and display the SADIST-2 product file-name and the ATSR instrument name. */

   header.file_name = get_string_from_header(temp, 0, 60)

   header.instrument = get_string_from_header(temp, 60, 6)


;/* Read and display the ERS state vector information. Contents are:
;      type (MPH, ORPD or ORRE); time (MJD from 1950); converted time (UTC format);
;      longitude (degrees East); position vector (km); velocity vector (km/s). */

   header.vec_type = get_string_from_header(temp, 66, 5)

   header.vec_time = get_double_from_header(temp, 71, 16)

   header.vec_utc = get_string_from_header(temp, 87, 25)

   header.vec_long = get_double_from_header(temp, 178, 11)

   FOR i = 0, 2 DO BEGIN
      header.vec_pos(i) = get_double_from_header(temp, 112 + (i * 13), 13)
      header.vec_vel(i) = get_double_from_header(temp, 151 + (i * 9), 9)
   ENDFOR



;/* Read and display the ERS clock calibration parameters. Contents are:
;      reference Universal Time (MJD from 1950); reference ERS satellite binary;
;      ERS clock period (ns). */

   header.clock_ut = get_double_from_header(temp, 189, 16)
   header.clock_bin = get_double_from_header(temp, 205, 13)
   header.clock_period = get_double_from_header(temp, 218, 13)



;/* Read and display the product options. Order is:
;      nadir only (N); thermal (T); visible (V); latitude/longitude (L);
;      x/y offsets (X); cloud/land (C). */

   FOR i = 0, 5 DO BEGIN
      header.options(i) = get_int_from_header(temp, 231 + (i * 2), 2)
   ENDFOR



;/* Read and display the along-track distances (km or relative scan number) and
;      acquisition times for the start and end of the product. */

   FOR i = 0, 1 DO BEGIN
      header.atd(i) = get_long_from_header(temp, 243 + (i * 6), 6)
      header.acq_time(i) = get_string_from_header(temp, 255 + (i * 25), 25)
   ENDFOR



;/* Read and display the latitudes and longitudes of the product corner-points.
;      Order is: LHS at start; RHS at start; LHS at end; RHS at end. */

   FOR i = 0, 3 DO BEGIN
      header.latitude(i) = get_float_from_header(temp, 305 + (i * 8), 8)
      header.longitude(i) = get_float_from_header(temp, 337 + (i * 9), 9)
   ENDFOR



;/* Read and display the pixel-selection-map (PSM) information: first PSM number;
;      second PSM number; distance or relative scan number or first PSM change. */

   header.nad_psm1 = get_int_from_header(temp, 373, 3)
   header.nad_psm2 = get_int_from_header(temp, 376, 3)
   header.nad_psm_change = get_long_from_header(temp, 379, 6)


   header.for_psm1 = get_int_from_header(temp, 385, 3)
   header.for_psm2 = get_int_from_header(temp, 388, 3)
   header.for_psm_change = get_long_from_header(temp, 391, 6)



;/* Read and display the ATSR-2 data-rate information: rate at start of product (L or H);
;      distance or relative scan number of first rate change. */

   header.nad_rate1 = get_string_from_header(temp, 397, 2)
   header.nad_rate_change = get_long_from_header(temp, 399, 6)

   header.for_rate1 = get_string_from_header(temp, 405, 2)
   header.for_rate_change = get_long_from_header(temp, 407, 6)



;/* Read and display the minimum and maximum auxiliary temperatures: The six temperatures
;      provided are: SCC cold-tip (tm.z556); 12.0um detector (tm.z565); 11.0um detector (tm.z564);
;      3.7um detector (tm.z563); 1.6um detector (tm.z562); 0.870um detector (tm.z567). */

   FOR i = 0, 5 DO BEGIN
      header.min_temps(i) = get_float_from_header(temp, 413 + (i * 8), 8)
      header.max_temps(i) = get_float_from_header(temp, 461 + (i * 8), 8)
   ENDFOR



;/* Read and display the solar and viewing angles for nadir and forward views, at the
;      start and end of the product. Solar elevation and azimuth, and satellite elevation
;      and azimuth, are all measured from the pixel. */

   FOR i = 0, 10 DO BEGIN
      header.nad_sol_elev_start(i) = get_float_from_header(temp, 509 + (i * 9), 9)
      header.nad_sol_elev_end(i) = get_float_from_header(temp, 608 + (i * 9), 9)
      header.nad_sat_elev_start(i) = get_float_from_header(temp, 707 + (i * 9), 9)
      header.nad_sat_elev_end(i) = get_float_from_header(temp, 806 + (i * 9), 9)
      header.nad_sol_azim_start(i) = get_float_from_header(temp, 905 + (i * 9), 9)
      header.nad_sol_azim_end(i) = get_float_from_header(temp, 1004 + (i * 9), 9)
      header.nad_sat_azim_start(i) = get_float_from_header(temp, 1103 + (i * 9), 9)
      header.nad_sat_azim_end(i) = get_float_from_header(temp, 1202 + (i * 9), 9)
      header.for_sol_elev_start(i) = get_float_from_header(temp, 1301 + (i * 9), 9)
      header.for_sol_elev_end(i) = get_float_from_header(temp, 1400 + (i * 9), 9)
      header.for_sat_elev_start(i) = get_float_from_header(temp, 1499 + (i * 9), 9)
      header.for_sat_elev_end(i) = get_float_from_header(temp, 1598 + (i * 9), 9)
      header.for_sol_azim_start(i) = get_float_from_header(temp, 1697 + (i * 9), 9)
      header.for_sol_azim_end(i) = get_float_from_header(temp, 1796 + (i * 9), 9)
      header.for_sat_azim_start(i) = get_float_from_header(temp, 1895 + (i * 9), 9)
      header.for_sat_azim_end(i) = get_float_from_header(temp, 1994 + (i * 9), 9)
   ENDFOR



;/* Read and display the ERS platform mode counters for the nadir and forward views.
;      Order is: YSM; FCM, OCM, FPM, RTMM, RTMC. */

   FOR i = 0, 5 DO BEGIN
      header.nad_platform(i) = get_long_from_header(temp, 2093 + (i * 6), 6)
      header.for_platform(i) = get_long_from_header(temp, 2129 + (i * 6), 6)
   ENDFOR



;/* Read and display the product confidence data (PCD) counters for the nadir and
;      forward views. */

   FOR i = 0, 7 DO BEGIN
      header.nad_pcd(i) = get_long_from_header(temp, 2165 + (i * 6), 6)
      header.for_pcd(i) = get_long_from_header(temp, 2213 + (i * 6), 6)
   ENDFOR



;/* Read And display the SADIST-2 packet validity counters for the nadir and
;      forward views. Order is: null packet; basic validation; crc error;
;      buffer full error; scan jitter; nibble shift; 3 * unused; other errors. */

   FOR i = 0, 9 DO BEGIN
      header.nad_invalid(i) = get_long_from_header(temp, 2261 + (i * 6), 6)
      header.for_invalid(i) = get_long_from_header(temp, 2321 + (i * 6), 6)
   ENDFOR



;/* Read and display the value of the maximum single pixel error. */

   header.max_error = get_int_from_header(temp, 2381, 4)

PRINT,'File name stored in the GBT header : ', header.file_name

c = STRARR(45)

IF KEYWORD_SET( not_silent ) THEN $
 IF KEYWORD_SET( not_terminal) THEN list_gbt_header, header, not_terminal = not_terminal $
   ELSE list_gbt_header, header



END

Pro list_gbt_header, gbt_header, not_terminal = not_terminal

;
; Lists the contents of the SADIST-2 Header file
;

PRINT,'File name stored in the GBT header : ', gbt_header.file_name

c = STRARR(45)

   c(0) = STRING( FORMAT = '(A30, I)', 'Byte_order: ', gbt_header.byte_order )
   c(1) = STRING( FORMAT = '(A30, I)', 'Byte-swap needed: ', gbt_header.byte_swap_needed )

   c(2) = STRING( FORMAT = '(A30, A60)', 'File-name: ', gbt_header.file_name )
   c(3) = STRING( FORMAT = '(A30, A6)', 'Instrument: ', gbt_header.instrument )
   c(4) = STRING( FORMAT = '(A30, A5)', 'State vector (type): ', gbt_header.vec_type )
   c(5) = STRING( FORMAT = '(A30, F16.8)', '(MJD time): ', gbt_header.vec_time )
   c(6) = STRING( FORMAT = '(A30, A25)', '(UTC time): ', gbt_header.vec_utc )
   c(7) = STRING( FORMAT = '(A30, F11.5)', '(longitude): ', gbt_header.vec_long )
   c(8) = STRING( FORMAT = '(A30, 3F13.5)', '(x/y/z position): ', gbt_header.vec_pos )
   c(9) = STRING( FORMAT = '(A30, 3F9.5)', '(x/y/z velocity): ', gbt_header.vec_vel )
   c(10) = STRING( FORMAT = '(A30, F16.8, F13.0, F13.0)', 'Clock calibration: ', $
             gbt_header.clock_ut, gbt_header.clock_bin, gbt_header.clock_period )
   c(11) = STRING( FORMAT = '(A30, 6I2)', 'Options (NTVLXC): ', gbt_header.options )
   c(12) = STRING( FORMAT = '(A30, 2I6)', 'Along-track distance: ', gbt_header.atd )
   c(13) = STRING( FORMAT = '(A30, 2A25)', 'Acquisition time: ', gbt_header.acq_time )
   c(14) = STRING( FORMAT = '(A30, 4F8.3)', 'Latitude: ', gbt_header.latitude )
   c(15) = STRING( FORMAT = '(A30, 4F9.3)', 'Longitude: ', gbt_header.longitude )
   c(16) = STRING( FORMAT = '(A30, I3, I3, I6)', 'Nadir-view PSM: ', $
                           gbt_header.nad_psm1, gbt_header.nad_psm2, gbt_header.nad_psm_change )
   c(17) = STRING( FORMAT = '(A30, I3, I3, I6)', 'Forward-view PSM: ', $
                           gbt_header.for_psm1, gbt_header.for_psm2, gbt_header.for_psm_change )
   c(18) = STRING( FORMAT = '(A30, A2, I6)', 'Nadir-view ATSR-2 rate: ', gbt_header.nad_rate1, gbt_header.nad_rate_change )
   c(19) = STRING( FORMAT = '(A30, A2, I6)', 'Forward-view ATSR-2 rate: ', gbt_header.for_rate1, gbt_header.for_rate_change )
   c(20) = STRING( FORMAT = '(A30, 6F8.3)', 'Minimum temperatures: ', gbt_header.min_temps )
   c(21) = STRING( FORMAT = '(A30, 6F8.3)', 'Maximum temperatures: ', gbt_header.max_temps )
   c(22) = STRING( FORMAT = '(A30, 11F9.3)', 'Nadir-view sol-elev (start): ', gbt_header.nad_sol_elev_start )
   c(23) = STRING( FORMAT = '(A30, 11F9.3)', '                      (end): ', gbt_header.nad_sol_elev_end )
   c(24) = STRING( FORMAT = '(A30, 11F9.3)', 'Nadir-view sat-elev (start): ', gbt_header.nad_sat_elev_start )
   c(25) = STRING( FORMAT = '(A30, 11F9.3)', '                      (end): ', gbt_header.nad_sat_elev_end )
   c(26) = STRING( FORMAT = '(A30, 11F9.3)', 'Nadir-view sol-azim (start): ', gbt_header.nad_sol_azim_start )
   c(27) = STRING( FORMAT = '(A30, 11F9.3)', '                      (end): ', gbt_header.nad_sol_azim_end )
   c(28) = STRING( FORMAT = '(A30, 11F9.3)', 'Nadir-view sat-azim (start): ', gbt_header.nad_sat_azim_start )
   c(29) = STRING( FORMAT = '(A30, 11F9.3)', '                      (end): ', gbt_header.nad_sat_azim_end )
   c(30) = STRING( FORMAT = '(A30, 11F9.3)', 'Frwrd-view sol-elev (start): ', gbt_header.for_sol_elev_start )
   c(31) = STRING( FORMAT = '(A30, 11F9.3)', '                      (end): ', gbt_header.for_sol_elev_end )
   c(32) = STRING( FORMAT = '(A30, 11F9.3)', 'Frwrd-view sat-elev (start): ', gbt_header.for_sat_elev_start )
   c(33) = STRING( FORMAT = '(A30, 11F9.3)', '                       (end): ', gbt_header.for_sat_elev_end )
   c(34) = STRING( FORMAT = '(A30, 11F9.3)', 'Frwrd-view sol-azim (start): ', gbt_header.for_sol_azim_start )
   c(35) = STRING( FORMAT = '(A30, 11F9.3)', '                      (end): ', gbt_header.for_sol_azim_end )
   c(36) = STRING( FORMAT = '(A30, 11F9.3)', 'Frwrd-view sat-azim (start): ', gbt_header.for_sat_azim_start )
   c(37) = STRING( FORMAT = '(A30, 11F9.3)', '                      (end): ', gbt_header.for_sat_azim_end )
   c(38) = STRING( FORMAT = '(A30, 6I6)', 'Nadir-view platform mode: ', gbt_header.nad_platform )
   c(39) = STRING( FORMAT = '(A30, 6I6)', 'Frwrd-view platform mode: ', gbt_header.for_platform )
   c(40) = STRING( FORMAT = '(A30, 8I6)', 'Nadir-view PCD: ', gbt_header.nad_pcd )
   c(41) = STRING( FORMAT = '(A30, 8i6)', 'Frwrd-view PCD: ', gbt_header.for_pcd )
   c(42) = STRING( FORMAT = '(A30, 10I6)', 'Nadir-view validity: ', gbt_header.nad_invalid )
   c(43) = STRING( FORMAT = '(A30, 10I6)', 'Frwrd-view validity: ', gbt_header.for_invalid )
   c(44) = STRING( FORMAT = '(A30, I4)', 'Max single-pixel error: ', gbt_header.max_error )
IF KEYWORD_SET( not_terminal ) THEN $
 BEGIN
 font = '-bitstream-terminal-medium-r-narrow-gs-18-180-75-75-c-70-iso8859-1'
 XDISPLAYFILE,ccc, TEXT = c, TITLE = STRCOMPRESS( STRTRIM( 'Header of ' + gbt_header.file_name, 2) ), WIDTH = 130 , font = font
 END ELSE for line = 0, 44 DO PRINT, c(line)

END

PRO get_nadir_ir, lun,  nadir_ir37, nadir_ir11, nadir_ir12, byte_swap_needed, create = create

PRINT, 'Reading nadir IR data ... '

temp = intarr(512, 512)

READU, lun, temp
nadir_ir12 = temp

READU, lun, temp
nadir_ir11 = temp

READU, lun, temp
nadir_ir37 = temp

temp = 0

IF (byte_swap_needed) THEN BYTEORDER, nadir_ir12, nadir_ir11, nadir_ir37, /SSWAP

END

PRO get_nadir_v16, lun, nadir_v16, byte_swap_needed

PRINT, 'Reading nadir 1.6µm data ... '

nadir_v16 = INTARR(512, 512)
READU, lun, nadir_v16

IF(byte_swap_needed) then BYTEORDER, nadir_v16, /SSWAP
END

PRO get_nadir_vis, lun, nadir_v555, nadir_v670, nadir_v870, byte_swap_needed

PRINT, 'Reading nadir VIS data ... '

nadir_v870 = INTARR(512, 512)
READU, lun, nadir_v870

nadir_v670 = INTARR(512, 512)
READU, lun, nadir_v670

nadir_v555 = INTARR(512, 512)
READU, lun, nadir_v555

IF (byte_swap_needed) THEN BYTEORDER, nadir_v870, nadir_v670, nadir_v555, /SSWAP

END

PRO get_forward_ir, lun, frwrd_ir37, frwrd_ir11, frwrd_ir12, byte_swap_needed

PRINT, 'Reading forward IR data ... '

frwrd_ir12 = INTARR(512, 512)
READU, lun, frwrd_ir12

frwrd_ir11 = INTARR(512, 512)
READU, lun, frwrd_ir11

frwrd_ir37 = INTARR(512, 512)
READU, lun, frwrd_ir37

IF(byte_swap_needed) THEN BYTEORDER, frwrd_ir12, frwrd_ir11, frwrd_ir37, /SSWAP

END


PRO get_forward_v16, lun, frwrd_v16, byte_swap_needed

PRINT, 'Reading forward 1.6µm data ... '

frwrd_v16 = INTARR(512, 512)
READU, lun, frwrd_v16

IF(byte_swap_needed) THEN BYTEORDER, frwrd_v16, /SSWAP

END
PRO  get_forward_vis, lun, frwrd_v555, frwrd_v670, frwrd_v870, byte_swap_needed

PRINT, 'Reading forward VIS data ... '

frwrd_v870 = INTARR(512, 512)
READU, lun, frwrd_v870

frwrd_v670 = INTARR(512, 512)
READU, lun, frwrd_v670

frwrd_v555 = INTARR(512, 512)
READU, lun, frwrd_v555

IF(byte_swap_needed) THEN BYTEORDER, frwrd_v870, frwrd_v670, frwrd_v555, /SSWAP

END

FUNCTION test_for_valid_data, channel

n = N_ELEMENTS( channel )

IF ( n GT 0 ) THEN $
 BEGIN
 index = WHERE( (channel GT 0) OR (channel LT -8), count )
 return, count
 END $
ELSE return, -1L

END

PRO get_latlon_grid, lun, latitude_grid, longitude_grid, byte_swap_needed

PRINT, 'Reading latitude/longitude grid ... '

temp_grid = LONARR(512, 512)

READU, lun, temp_grid
IF (byte_swap_needed) THEN BYTEORDER, temp_grid, /LSWAP

latitude_grid = FLTARR(512, 512)
latitude_grid = FLOAT(temp_grid) / 1000.0

READU, lun, temp_grid
IF (byte_swap_needed) THEN BYTEORDER, temp_grid, /LSWAP

longitude_grid = FLTARR(512, 512)
longitude_grid = FLOAT(temp_grid) / 1000.0

END

PRO get_xy_nadir, lun, nadir_x_offset, nadir_y_offset, byte_swap_needed

PRINT, 'Reading nadir x/y offset grid ... '

nadir_x_offset = BYTARR(512, 512)
READU, lun, nadir_x_offset

nadir_y_offset = BYTARR(512, 512)
READU, lun, nadir_y_offset

END


PRO get_xy_forward, lun, frwrd_x_offset, frwrd_y_offset, byte_swap_needed

PRINT, 'Reading forward x/y offset grid ... '

frwrd_x_offset = BYTARR(512, 512)
READU, lun, frwrd_x_offset

frwrd_y_offset = BYTARR(512, 512)
READU, lun, frwrd_y_offset

END


PRO get_nadir_cloud, lun, nadir_cloud_flags, byte_swap_needed

PRINT, 'Reading nadir cloud flags ... '

nadir_cloud_flags = INTARR(512, 512)
READU, lun, nadir_cloud_flags

IF (byte_swap_needed) THEN BYTEORDER, nadir_cloud_flags, /SSWAP

END

PRO get_forward_cloud, lun, frwrd_cloud_flags, byte_swap_needed

PRINT, 'Reading forward cloud flags ... '

frwrd_cloud_flags = INTARR(512, 512)
READU, lun, frwrd_cloud_flags

IF (byte_swap_needed) THEN BYTEORDER, frwrd_cloud_flags, /SSWAP

END


PRO get_sadist2_gbt, filter, gbt_header, $
  nadir_ir37, nadir_ir11, nadir_ir12, nadir_v16,$
  nadir_v555, nadir_v670, nadir_v870, $
  frwrd_ir37, frwrd_ir11, frwrd_ir12, frwrd_v16, $
  frwrd_v555, frwrd_v670, frwrd_v870, $
  latitude_grid, longitude_grid, $
  nadir_x_offsets, nadir_y_offsets, $
  forward_x_offsets, forward_y_offsets, $
  nadir_cloud_flags, frwrd_cloud_flags,$
  nrt = nrt, $
  available_options = available_options_return, $
  channels = channels_return, $
  valid_channels = valid_channels, $
  _EXTRA = extra

;
; Reads SADIST-2 gridded brightness temperature product (GBT)
; into memory, byte-swapping where necessary. Copes with all
; combinations of the nadir-only (N), thermal (T),
; visible (V), latitude/longitude (L), x/y (X) and cloud/land (C)
; product options.
;
;
; Open the product file and read/display the contents of the product header. */
;
; The parameters of returned by the routine are largly defined by the names used in the call.
;
; The optional parameters are:
;
; 	/nrt set this to inform the routine that you are reading a mutli-file SADIST-2 product from the NRT processor
;
;	available_options is a flag array with elements set when one of the optional components of a SADIST file is present
;
;	channels is a flag set to indicate that an array containing pixel data from that channel was read from the SADIST-2
;		product. BYTARR(7) 1st index is channel going in increasing wavelength.
;
;       valid_channels is a LONARR(7,2) indicating the number of valid pixels available for each channel indexing is the same
;     		as for the the previous option for the first index, the 2nd is the view nadir=0, forward=1.
;
; The latitudes and longitudes have already been scaled into real values, all other parameters still need the SADIST-2
;  conversion factors applying.
;

IF KEYWORD_SET( nrt ) THEN nrt = 1 ELSE nrt = 0

try_again:

IF ( !version.os_family eq 'vms' ) THEN $
  BEGIN
  filter_len = STRLEN(filter)
  dir_close_bracket = STRPOS(filter, "]") + 1
  path_dir = STRMID(filter, 0, dir_close_bracket)
  file_filter = STRMID(filter, dir_close_bracket, filter_len)
  END $
 ELSE $
  IF ( !version.os_family eq 'Windows' ) THEN $
  BEGIN
  dirs = STR_SEP(filter, '\') ; find all the slashes
  n_dir_size = SIZE( dirs )
  file_filter = dirs( n_dir_size[1] - 1 )
  end_dir_path = STRPOS( filter, file_filter )
  path_dir = STRMID( filter, 0, end_dir_path )
  print, path_dir
  END $
 ELSE $
  BEGIN
  dirs = STR_SEP(filter, '/') ; find all the slashes
  n_dir_size = SIZE( dirs )
  file_filter = dirs( n_dir_size[1] - 1 )
  end_dir_path = STRPOS( filter, file_filter )
  path_dir = STRMID( filter, 0, end_dir_path )
  END

nrt_ext = STRPOS(file_filter, '.H')

IF (nrt AND nrt_ext LE 0) THEN file_filter = file_filter + '.H' $
 ELSE IF ((NOT nrt) AND nrt_ext gt 0) THEN file_filter = STRMID(file_filter, 0, nrt_ext)

PRINT, 'Current directory search path: ', path_dir
PRINT, 'Current file filter : ', file_filter

product = DIALOG_PICKFILE( /READ, /NOCONFIRM, PATH = path_dir , GET_PATH = new_path_dir, FILTER = file_filter,  _EXTRA = extra)

IF( product EQ new_path_dir) THEN GOTO, try_again


filter = new_path_dir + file_filter

Print,' New setting for the file filter : ', filter

size_vector = SIZE( product )

IF ( size_vector(1) NE 7 ) THEN GOTO, skip_it_all_no_files

IF (NOT nrt) THEN $
 BEGIN
 OPENR, temp, product, 1024, /block, /GET_LUN
 lun= LONARR(10) + temp
 END


IF nrt THEN available_options = get_nrt_flags(product, lun, channels = channels)

get_sadist2_header, lun[0], 1024, gbt_header, byte_swap_needed

IF (NOT nrt) THEN available_options = get_gbt_options(gbt_header.options, channels = channels)

IF ARG_PRESENT( valid_channels ) THEN valid_data = LONARR(7,2) - 1L ; dimensions are channels, view 0=nadir, 1 =forward

list_gbt_header, gbt_header


; If the thermal (T) option has been selected, define the nadir-view 12.0um,
; 11.0um and 3.7um image arrays, and read from the product file. Byte-swap
; if necessary.
;print,systime()

IF (available_options[1]) THEN $
  BEGIN
  get_nadir_ir, lun[1], nadir_ir37, nadir_ir11, nadir_ir12, byte_swap_needed

  IF ARG_PRESENT( valid_channels ) THEN $
    BEGIN
    valid_data[4,0] = test_for_valid_data( nadir_ir37)
    valid_data[5,0] = test_for_valid_data( nadir_ir11)
    valid_data[6,0] = test_for_valid_data( nadir_ir12)
    END

  END

;print,systime()

; If the thermal (T) or visible (V) options have been selected, define the
; nadir-view 1.6um image array, and read from the product file. Byte-swap if
; necessary.

IF ( (nrt AND available_options[1] ) OR ((NOT nrt) AND ( available_options[1] OR available_options[2]) ) ) THEN $
  BEGIN
  get_nadir_v16, lun[1], nadir_v16, byte_swap_needed

  IF ARG_PRESENT( valid_channels ) THEN valid_data[3,0] = test_for_valid_data( nadir_v16)

  END

; If the visible (V) option has been selected, define the nadir-view 0.870um,
; 0.670um and 0.555um image arrays, and read from the product file. Byte-swap
; if necessary.

IF (available_options[2]) THEN $
  BEGIN
  get_nadir_vis, lun[2], nadir_v555, nadir_v670, nadir_v870, byte_swap_needed

  IF ARG_PRESENT( valid_channels ) THEN $
    BEGIN
    valid_data[0,0] = test_for_valid_data( nadir_v555 )
    valid_data[1,0] = test_for_valid_data( nadir_v670 )
    valid_data[2,0] = test_for_valid_data( nadir_v870 )
    END
  END

; Read the forward-view image arrays only if the nadir-only (N) option has
; not been selected.


; If the thermal (T) option has been selected, define the forward-view 12.0um,
; 11.0um and 3.7um image arrays, and read from the product file. Byte-swap if
; necessary.

IF (available_options[3]) THEN $
  BEGIN
  get_forward_ir, lun[3], frwrd_ir37, frwrd_ir11, frwrd_ir12, byte_swap_needed

  IF ARG_PRESENT( valid_channels ) THEN $
    BEGIN
    valid_data[4,1] = test_for_valid_data( frwrd_ir37)
    valid_data[5,1] = test_for_valid_data( frwrd_ir11)
    valid_data[6,1] = test_for_valid_data( frwrd_ir12)
    END

END

; If the thermal (T) or visible (V) options have been selected, define the
; forward-view 1.6um image array, and read from the product file. Byte-swap
; if necessary.

IF  (nrt AND available_options[3] ) OR ((NOT nrt) AND ( available_options[3] OR available_options[4]) )THEN $
  BEGIN
  get_forward_v16, lun[3], frwrd_v16, byte_swap_needed

  IF ARG_PRESENT( valid_channels ) THEN valid_data[3,1] = test_for_valid_data( frwrd_v16 )

  END

; If the visible (V) option has been selected, define the forward-view 0.870um,
; 0.670um and 0.555um image arrays, and read from the product file. Byte-swap
; if necessary.

IF (available_options[4]) THEN $
  BEGIN
  get_forward_vis, lun[4], frwrd_v555, frwrd_v670, frwrd_v870, byte_swap_needed

  IF ARG_PRESENT( valid_channels ) THEN $
   BEGIN
   valid_data[0,1] = test_for_valid_data( frwrd_v555 )
   valid_data[1,1] = test_for_valid_data( frwrd_v670 )
   valid_data[2,1] = test_for_valid_data( frwrd_v870 )
   END

  END

; If the latitude/longitude (L) option has been selected, define the latitude
; and longitude arrays, and read from the product file. Convert the latitudes
; and longitudes to a floating-point representation, with units of degrees. Note
; that byte-swapping must be performed *before* conversion.

IF (available_options[5]) THEN $
  get_latlon_grid, lun[5], latitude_grid, longitude_grid, byte_swap_needed

; If the x/y (X) option has been selected, define the x/y offset arrays, and
; read from the product file. Note that the x/y offsets are *unsigned* bytes.

IF (available_options[6]) THEN $
  get_xy_nadir, lun[6], nadir_x_offset, nadir_y_offset, byte_swap_needed

      ; Read the forward-view x/y offsets only if the nadir-only (N) option has not
      ; been selected.

IF (available_options[7]) THEN $
  get_xy_forward, lun[7], frwrd_x_offset, frwrd_y_offset, byte_swap_needed

;
;/* If the cloud/land (C) option has been selected, define the nadir- and forward-view
;      cloud/land flag arrays, and read from the product file. Byte-swap if necessary. */

IF (available_options[8]) THEN $
  get_nadir_cloud, lun[8], nadir_cloud_flags, byte_swap_needed

; Read the forward-view cloud/land flags only if the nadir-only (N) option has not
; been selected. Byte-swap if necessary.

IF (available_options[9]) THEN $
  get_forward_cloud, lun[9], frwrd_cloud_flags, byte_swap_needed



IF nrt THEN $
 BEGIN
  index = where( lun GT 99 )
  FOR i=0, n_elements(index) - 1 DO BEGIN
   CLOSE, lun[index[i]]
   FREE_LUN, lun[index[i]]
   END
 ENDIF ELSE $
 BEGIN
  CLOSE, lun(0)
  FREE_LUN, lun(0)
 END

skip_it_all_no_files:

IF ARG_PRESENT( available_options ) THEN available_options_return = available_options
IF ARG_PRESENT( channels ) THEN channels_return = channels
IF ARG_PRESENT( valid_channels ) THEN valid_channels = valid_data

Print,'ATSR data file reading completed ...'

END
