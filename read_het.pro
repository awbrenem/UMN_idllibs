;+
;FUNCTION: read_het.pro 
;
;PURPOSE: To read het data files in ASCII and output data in matrix form
;
;ARGUMENTS:
;       FILE_NAME    -> name of file to be input
;       DATA_ARRAY   <- name of variable for data to be stored in
;       NAME_ARRAY   <- name of variable for quantity names (and units) to be
;                       stored in
;
;KEYWORDS: N/A
;
;RETURNS: Status
;            0 - Failure
;            1 - Success
;
;CALLING SEQUENCE: status=read_het('file_name',data,name)
;
;NOTES: data has fields: bogus,yr,mth,dy,hhmm,elow,emid,ehigh,
;       efluxlow,efluxmid,efluxhigh
;	Will return one matrix with times in index 0 (UNIX time) and
;	data in the rest
;
;CREATED BY: John Dombeck April 2007
;
;MODIFICATION HISTORY: 
;  04/23/07-J.Dombeck	original creation - adapted from read_ssc
;-
;INCLUDED MODULES:
;   read_het
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   None
;
;-



;*** MAIN *** : * READ_HET *

function read_het,file_name,data_array,name_array


; Open file

  openr,unit,file_name,error=err,/get_lun

  if (err ne 0)  then begin
    message,'file not found',/cont
    return,0
  endif


; Set # of data quantities

  ncol=11


; Set names and units

  names=['Bogus','Year','Month','Day','Hour/Minute',$
    'e - low','e - mid','e - high',$
    'e flux - low','e flux - mid','e flux - high']
  units=['','','','','','#/s','#/s','#/s','eV/s','eV/s','eV/s']


; Find beginning of data

  line=''
  r=0
  while(not eof(unit) and (r ne 1))do begin
        readf,unit,line
    r=stregex(line,'#End',/boolean)
  endwhile
  
  if r ne 1 then begin
    free_lun,unit
    message,'Not HET file',/cont
    return,0
  endif


; Read data

  t=0l
  nrow=0l
  while not eof(unit) do begin
    readf,unit,line
    nrow=nrow+1
    line=strcompress(line)
    ele=strsplit(line,' ',/extract)
    yr=fix(ele[1])
    case ele[2] of
      'Jan':mth=1
      'Feb':mth=2
      'Mar':mth=3
      'Apr':mth=4
      'May':mth=5
      'Jun':mth=6
      'Jul':mth=7
      'Aug':mth=8
      'Sep':mth=9
      'Oct':mth=10
      'Nov':mth=11
      'Dec':mth=12
    endcase
    dy=fix(ele[3])
    hr=fix(strmid(ele[4],0,2))
    mn=fix(strmid(ele[4],2,2))
    timestr=string( $
      format='(I4.4,"-",I2.2,"-",I2.2,"/",I2.2,":",I2.2,":00")',$
      yr,mth,dy,hr,mn)
    time=time_double(timestr)
    new_line=[time,ele[5:ncol-1]]
    if (t eq 0) then data_array=new_line $
                else data_array=[[data_array],[new_line]]
    t=t+1
  endwhile

  data_array=transpose(data_array)

; Sets garbage values to NAN

  idx=where(data_array ge 1.0000e33,cnt)
  if cnt ne 0 then data_array[idx]=!values.d_nan

  free_lun,unit


; Set name_array

  name_array=[['UT',names[5:ncol-1]],['sec',units[5:ncol-1]]]

return,1
end        ;*** MAIN *** : * READ_HET *

