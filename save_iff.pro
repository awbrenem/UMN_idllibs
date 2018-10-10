;+
;PROCEDURE: save_iff.pro 
;
;PURPOSE: Save U of M standard data types in IDL File Format (IFF)
;
;ARGUMENTS:
;	DATA     -> Data array
;       NAMES    -> Quantity names and units
;       VALUES   -> Values for multi-line and spectra plots
;
;KEYWORDS:
;      FILENAME  -> Name of file to store data in
;      TITLE     -> Title for data stored (string)
;      COMMENTS  -> Comments describing data stored (string)
;
;CALLING SEQUENCE: 
;      save_iff,data,names,values,filename='filename',title='TITLE'
;
;NOTES:
;      If FILENAME is not specified NAMES[0,0]+time_string(DATA[0,0]) will 
;      be used
;
;CREATED BY: John Dombeck August 2001
;
;MODIFICATION HISTORY: 
;  8/01/2001-J. Dombeck      Creation
;-
;INCLUDED MODULES:
;   save_iff
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;   time_string
;
;-



;*** MAIN *** : * SAVE_IFF *

pro save_iff,data,names,values,filename=filename,title=title,comments=comments

   
; Check input


; Check Data

  dtype=data_type(data)     ;(5=double, 10=pointer)
  if dtype ne 5 and dtype ne 10 then begin
    message,'DATA not proper type',/cont
    return
  endif


; Check Names

  if n_elements(names) eq 0 then begin
    message,'NAMES required',/cont
    return
  endif

  if data_type(names) ne 7 then begin
    message,'NAMES requires string array',/cont
    return
  endif


; Check Title

  if n_elements(title) eq 0 then begin
    title=''
  endif else if data_type(title) ne 7 then begin
    message,'TITLE requires string',/cont
    return
  endif

   
; Check Comments

  if n_elements(comments) eq 0 then begin
    comments=''
  endif else if data_type(comments) ne 7 then begin
    message,'COMMENTS requires string',/cont
    return
  endif


; Determine Filename

  if n_elements(filename) eq 0 then begin
    filename=strcompress(names[0,0],/remove_all)
    if dtype eq 5 then filename=filename+time_string(data[0,0]) $
                  else filename=filename+time_string((*data[0])[0,0])
    filename=filename+'.iff'
  endif else begin
    if n_elements(filename) ne 1 or data_type(filename) ne 7 then begin
      message,'FILENAME require string',/cont
      return
    endif
 
    if strmid(filename,strlen(filename)-4,4) ne '.iff' then $
      filename=filename+'.iff'
  endelse


; Find other data quantities

  if dtype eq 10 then begin
    datatype=0 
      

  ; Find intersection times

    itimes=sdt_intersect(data)
    if n_elements(itimes) eq 1 then begin    ; Times don't intersect
      istarttime=time_string(0.0d) 
      iendtime=time_string(0.0d) 
    endif else begin
      istarttime=time_string(itimes[0])
      iendtime=time_string(itimes[1])
    endelse


  ; Find first start and last end times

    starttime=(*data[0])[0,0]
    endtime=(*data[0])[n_elements((*data[0])[*,0])-1,0]
    for xx=1,n_elements(data)-1 do begin
      tmpstart=(*data[xx])[0,0]
      tmpend=(*data[xx])[n_elements((*data[xx])[*,0])-1,0]
      if tmpstart lt starttime then starttime=tmpstart
      if tmpend lt endtime then endtime=tmpend
    endfor
    starttime=time_string(starttime)
    endtime=time_string(endtime)
    
  endif else begin
    starttime=time_string(data[0,0])
    endtime=time_string(data[n_elements(data[*,0])-1,0])
    if n_elements(values) eq 0 then datatype=1 $
    else datatype=2
  endelse


; Save data

  case datatype of 
    0 : begin
        save,filename=filename,title,comments,datatype,starttime,endtime,$
             istarttime,iendtime,data,names
      end
    1 : begin
        save,filename=filename,title,comments,datatype,starttime,endtime,$
             data,names
      end
    2 : begin
        save,filename=filename,title,comments,datatype,starttime,endtime,$
             data,names,values
      end
    else : begin
        message,"Incorrect case value"
      end
  endcase



return
end        ;*** MAIN *** : * SAVE_IFF *

