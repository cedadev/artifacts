options = ['H','T','V','Tf','Vf','L','X','Xf','C','Cf']
channel_names = ['0.55µm ','0.67µm ','0.87µm ','1.6µm ','3.7µm ','10.8µm ','12.0µm ']

; 
; Define the variables for the optional parameters to get_sadist2_gbt. This
; is not required on OpenVMS or DIGITAL UNIX, but SUNOS need them !
;

available_options = BYTARR(10)
channels = BYTARR(7)
valid_channels = LONARR(7,2)

device,pseudo=8

filter = '*.gbt*'

;
; First call for a normal SADIST-2 GBT file
; 

get_sadist2_gbt, filter, gbt_header, $
  nadir_ir37, nadir_ir11, nadir_ir12, nadir_v16, nadir_v555, nadir_v670, nadir_v870, $
  frwrd_ir37, frwrd_ir11, frwrd_ir12, frwrd_v16, frwrd_v555, frwrd_v670, frwrd_v870, $
  latitude_grid, longitude_grid, $
  nadir_x_offsets, nadir_y_offsets, forward_x_offsets, forward_y_offsets, $
  nadir_cloud_flags, frwrd_cloud_flags, $
  available_options = available_options, $
  channels = channels, $
  valid_channels = valid_channels

Print,' '
Print,'Printout of values from call to GET_SADIST2_GBT'
Print,'Current file filter : ', filter
Print,'File name : ', gbt_header.file_name
Print,'Available options : ' , available_options

ind = WHERE( available_options EQ 1, count)
IF count GT 0 THEN PRINT,'File options present : ', options(ind)
ind = WHERE( available_options EQ 0, count)
IF count GT 0 THEN PRINT,'File options missing : ', options(ind)

Print,'Channels in file :   ', channels[0:6]

Print,'No. of valid Nadir Pixels      : ', valid_channels[0:6,0]

ind = WHERE( valid_channels(0:6,0) GT  0, count)
IF count GT 0 THEN PRINT,'Valid nadir view pixels for : ', channel_names(ind)

Print,'No. of valid Forward Pixels    : ', valid_channels[0:6,1]

ind = WHERE( valid_channels(0:6,1) GT  0, count)
IF count GT 0 THEN PRINT,'Valid forward view pixels for : ', channel_names(ind)



window, 1, xsize = 512, ysize = 512

loadct,0

tv,255B-hist_equal(bytscl(abs(nadir_ir11),min=27000,max=32000))

;
; Now  call for an NRT  SADIST-2 GBT file with all options as separate files
; 

filter ='*'

get_sadist2_gbt, filter, gbt_header, $
  nadir_ir37, nadir_ir11, nadir_ir12, nadir_v16, nadir_v555, nadir_v670, nadir_v870, $
  frwrd_ir37, frwrd_ir11, frwrd_ir12, frwrd_v16, frwrd_v555, frwrd_v670, frwrd_v870, $
  latitude_grid, longitude_grid, $
  nadir_x_offsets, nadir_y_offsets, forward_x_offsets, forward_y_offsets, $
  nadir_cloud_flags, frwrd_cloud_flags, $
  available_options = available_options, $
  channels = channels, $
  valid_channels = valid_channels, /nrt

window, 1, xsize = 512, ysize = 512

tv,255B-hist_equal(bytscl(abs(nadir_ir11),min=27000,max=32000))

Print,' '
Print,'Printout of values from call to GET_SADIST2_GBT'
Print,'File name : ', gbt_header.file_name
Print,'Available options : ' , available_options

ind = WHERE( available_options EQ 1, count)
IF count GT 0 THEN PRINT,'File options present : ', options(ind)
ind = WHERE( available_options EQ 0, count)
IF count GT 0 THEN PRINT,'File options missing : ', options(ind)

Print,'Channels in file :   ', channels[0:6]

Print,'No. of valid Nadir Pixels      : ', valid_channels[0:6,0]

ind = WHERE( valid_channels(0:6,0) GT  0, count)
IF count GT 0 THEN PRINT,'Valid nadir view pixels for : ', channel_names(ind)

Print,'No. of valid Forward Pixels    : ', valid_channels[0:6,1]

ind = WHERE( valid_channels(0:6,1) GT  0, count)
IF count GT 0 THEN PRINT,'Valid forward view pixels for : ', channel_names(ind)

END
 
