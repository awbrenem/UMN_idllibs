;+
;FUNCTION: remove_nan.pro
;
;PURPOSE: Removing columns with NAN data points.  Has uniform time increments
;
;ARGUMENTS: 
;    DATA_ARRAY  -> Data to clear NaN columns from
;    CNT         <- Count of columns with NANs
;
;RETURNS: Array cleared of any NAN columns
;
;KEYWORDS: 
;
;CALLING SEQUENCE: array=remove_nan(data_array,cnt)
;
;NOTE: returns 0 if NAN's removed and nothing is left
;	clears columns
;
;CREATED BY: Lisa Rassel Jun 2001
;
;MODIFICATION HISTORY: 
;	06/11/01-L. Rassel	creation
;	05/10/04-J. Dombeck	clearly handle null input
;	08/11/11-J. Dombeck	added CNT 
;-
;INCLUDED MODULES:
;   remove_nan
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   None
;
;-


;*** MAIN *** : * REMOVE_NAN *

function remove_nan,data_array,cnt

  n=size(data_array,/dimensions)

  if n[0] le 1 then begin
    message,'DATA_ARRAY requires array',/cont
    return,0
  endif

  idx=0l
  cnt=0l
  new_arr=data_array

  for j=0l,(n[0]-1) do begin      ;columns
    if (total(finite(data_array[j,*],/nan)) eq 0) then begin
      new_arr[idx,*]=data_array[j,*]
      idx=idx+1
    endif else cnt=cnt+1
  endfor    

  if idx eq 0 then return,0

return,new_arr[0:idx-1,*]
end        ;*** MAIN *** : * REMOVE_NAN *

