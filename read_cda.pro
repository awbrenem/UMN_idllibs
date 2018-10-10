;+
;FUNCTION: read_cda.pro
;
;PURPOSE: To read CDA data files and output data in matrix form.
;
;ARGUMENTS:
;       FILE_NAME    -> name of file to be input
;       DATA_ARRAY   <- name of variable for data to be stored in
;       NAME_ARRAY   <- name of variable quantity names (and units) to be
;                       stored in
;
;KEYWORDS: N/A
;
;RETURNS: Status
;            0 - Failure
;            1 - Success
;
;CALLING SEQUENCE: status=read_cda('file_name',data,name)
;
;NOTES: needs fix_name.pro function to work
;	gives one matrix contining the time and data plots
;
;CREATED BY:   Lisa Rassel Jun 2001
;
;MODIFICATION HISTORY: 
;	06/11/01-L. Rassel 	created
;       04/18/02-J. Dombeck     made search for 'Epoch' case insensitive
;-
;INCLUDED MODULES:
;   rdcda_fix_name
;   read_cda
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   None
;
;-


;* RDCDA_FIX_NAME *  Reformats the name array

function rdcda_fix_name,line,string

  l=strlen(line)
  idx=l & idx_arr=0 & b=0
  p=strpos(line,' ',idx,/reverse_search)
  idx=p
  ctr=p


; Find the position of all spaces

  while ((idx ne 0) and (ctr ne 0))do begin
    p=strpos(line,' ',idx,/reverse_search)
    idx=p
    if (idx eq l) then begin
      idx_arr=p
    endif else begin
      idx_arr=[idx_arr,p]
    endelse
    idx=idx-1
    ctr=ctr-1
  endwhile


; Find the number of elements in the space arr

  num_a=n_elements(idx_arr)

  idx_arr=reverse(idx_arr)


; Extract position of first space

  for i=0,num_a-2 do begin
    if ((idx_arr[i]+2) lt idx_arr[i+1])then b=[b,idx_arr[i+1]]
  endfor

  delim_arr=[b,strlen(line)]

  d=n_elements(delim_arr)-1
  str_brk=make_array(d,/string)
  names=make_array(d-1,/string)


; Break names apart

  for xx=0,(d-1) do begin
    str_brk[xx]=strmid(string,delim_arr(xx),delim_arr(xx+1)-delim_arr(xx))
  endfor


; Shifts names so str_brk ne ' '

  str_brk=[str_brk[0],str_brk[2:d-1]]
  names=strtrim(str_brk,2)

return,names
end     ;* RDCDA_FIX_NAME *



;*** MAIN *** : * READ_CDA *

function read_cda,file_name,data_array,name_array


; Open file

  openr,unit,file_name,error=err,/get_lun

  if (err ne 0)  then begin
    message,'file not found',/cont
    return,0
  endif


; Finds names and units

  line= ' '
  i=0l
  while (not eof(unit) and (i lt 2)) do begin
    readf,unit,line
    p=stregex(line,'Epoch',/boolean,/fold_case)
    q=stregex(line,'dd',/boolean)
    if (p eq 1) then begin                    
      name1=line                 ;saves name line for later use 
      p=0                        ;see (rdcda_fix_name)
      i=i+1
    endif
    if ((q eq 1) and (strcmp(line,' ',1) ne 1)) then begin
      name2=line                 ;saves unit line as string for later use
      q=0
      i=i+1
    endif
  endwhile
  
  if eof(unit) then begin
    message,"Incorrect file type",/cont
    free_lun,unit
    return,0
  endif


; Read in data

  t=0l
  r=0
  nrow=0
  while (not eof(unit) and (r ne 1)) do begin
    readf,unit,line
    r=stregex(line,'Key',/boolean)    ;checks that line Not Equal footer

  ; Data Line

    if ((strcmp(line,' ',1) ne 1) and (r ne 1)) then begin
      nrow=nrow+1
      elmts=strcompress(line)
      elmts=strsplit(line,' ',/extract)
      str1=strsplit(elmts[0],'-',/extract)       ;reformats time
      new_str1=[str1[2],str1[1],str1[0]]
      elmts[0]=strjoin(new_str1,'-')
      timestr=elmts[0]+'/'+elmts[1]
      time=time_double(timestr)                  ;sets to UNIX time
  
      if(t eq 0) then begin
        test_line=line                  ;saves a test line for FIX_NAME
        ncol=n_elements(elmts)-1        ;only executes one time through loop
      endif
  
      for x=2,ncol do begin
       if (x eq 2) then data=[time,double(elmts[x])] $
                      else data=[data,double(elmts[x])]
      endfor
      if (t eq 0) then data_array=data else data_array=[[data_array],[data]]
      t=t+1
    endif

  endwhile
 
  free_lun,unit

  if r ne 1 then begin
    message,'Missing standard end of CDA file',/cont
    return,0
  endif

  data_array=transpose(data_array)


; Sets garbage values to NAN

  idx=where(data_array le -1.0000e30,cnt)
  if cnt ne 0 then data_array[idx]=!values.d_nan
  

; Fix names

  if((strcmp(line,' ',1) ne 1) and (strcmp(test_line,' ') ne 1)) then begin
    name1=rdcda_fix_name(test_line,name1)
    name2=rdcda_fix_name(test_line,name2)
    name1[0]='UT'
    name2[0]='seconds'
    name_array=[[name1],[name2]]
  endif else begin
    message,'Bad name format,/cont
    return,0
  endelse

return,1
end        ;*** MAIN *** : * READ_CDA *

  
