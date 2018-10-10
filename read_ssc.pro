;+
;FUNCTION: read_ssc.pro 
;
;PURPOSE: To read ssc data files in ASCII and output data in matrix form
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
;CALLING SEQUENCE: status=read_ssc('file_name',data,name)
;
;NOTES: data has fields: yr,mon,dy,hr,mn,sc,ms,
;       vx,vygsm,vzgsm,np,dyn,vth,x,y,gsm,z,gsm and others
;       uses data file to determine the number of columns.  
;	Will return one matrix with times in index 0 (UNIX time) and
;	data in the rest
;
;CREATED BY: Lisa Rassel Jun 2001
;
;MODIFICATION HISTORY: 
;  06/06/01-L.Rassel	original creation
;  01/17/02-J.Dombeck   Changed labels search from '#' to 'NAME' and needed to convert
;                         year creation, since, evidently, SSC changed their format
;-
;INCLUDED MODULES:
;   read_ssc
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   None
;
;-



;*** MAIN *** : * READ_SDT *

function read_ssc,file_name,data_array,name_array


; Open file

  openr,unit,file_name,error=err,/get_lun

  if (err ne 0)  then begin
    message,'file not found',/cont
    return,0
  endif


; Find # of data quantities

  line= ' '
  q=0
  while (not eof(unit) and (q eq 0))do begin
    readf,unit,line
    q=stregex(line,'NCOLS',/boolean)
  endwhile
  
  if q ne 1 then begin
    free_lun,unit
    message,'Bad file type',/cont
    return,0
  endif

  line=strcompress(line)
  ln_elmts=strsplit(line,' ',/extract)
  ncol=fix(ln_elmts[2])


; Find names and units

  p=0
  while (not eof(unit) and (p ne 1)) do begin
    readf,unit,line
    p=stregex(line,'NAME',/boolean)
  endwhile
  
  if p ne 1 then begin
    free_lun,unit
    message,'Premature end of data',/cont
    return,0
  endif

  i=0
  j=0
  names=['']
  units=['']
  while (not eof(unit) and (j ne ncol)) do begin
    readf,unit,line          
    line=strcompress(line)
    line_elts=strsplit(line,' ',/extract)
    if(i eq 0) then begin
      str1=line_elts[1]
      str2=strmid(line,7,21)
      names=[str1]
      units=['sec']
      i=i+1
    endif else begin
      str1=line_elts[1]
      str2=line_elts[2]
      names=[names,str1]
      units=[units,str2]
    endelse

    j=j+1

  endwhile
  
  if eof(unit) then begin
    free_lun,unit
    message,'Premature end of data',/cont
    return,0
  endif

  name_array=[[names],[units]]


; Find beginning of data

  r=0
  while(not eof(unit) and (r ne 1))do begin
        readf,unit,line
    r=stregex(line,'DATA:',/boolean)
  endwhile
  
  if r ne 1 then begin
    free_lun,unit
    message,'Premature end of data',/cont
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
    yr=fix(ele[0])
    if yr lt 100 then ele[0]=strcompress('19'+string(yr),/remove_all)
    timestr=string( $
            format='(I4.4,"-",I2.2,"-",I2.2,"/",I2.2,":",I2.2,":",I2.2,".",I3.3)',$
                   ele[0],ele[1],ele[2],ele[3],ele[4],ele[5],ele[6])
    time=time_double(timestr)
    new_line=[time,ele[7:ncol+5]]
    if (t eq 0) then data_array=new_line $
                else data_array=[[data_array],[new_line]]
    t=t+1
  endwhile

  data_array=transpose(data_array)

; Sets garbage values to NAN

  idx=where(data_array ge 1.0000e33,cnt)
  if cnt ne 0 then data_array[idx]=!values.d_nan

  free_lun,unit

return,1
end        ;*** MAIN *** : * READ_SSC *

