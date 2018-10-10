;+
;FUNCTION: sdt_union.pro
;
;PURPOSE: To find the union of a set of sdt pointer arrays
;
;ARGUMENTS:
;       PTR_ARR	  ->  SDT pointer array
;
;RETURNS: union data times [start time,end time]
;           ( 0 on failure)
;
;KEYWORDS: N/A
;
;CALLING SEQUENCE: times=sdt_union(ptr_arr)
;
;NOTES: None
;
;CREATED BY: John Dombeck Oct.,02 2001
;
;MODIFICATION HISTORY:
;  10/02/01-J. Dombeck  Original writing
;-
;INCLUDED MODULES:
;   sdt_union
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;
;-



;*** MAIN *** : * SDT_UNION *


function sdt_union,ptr_arr

  if data_type(ptr_arr) ne 10 then begin
    message,'PTR_ARRAY requires sdt pointer array',/cont
    return,0
  endif

  num_ptr=n_elements(ptr_arr)
  sz=n_elements((*ptr_arr[0])[*,0])
  
  start_pt=(*ptr_arr[0])[0,0]
  final_pt=(*ptr_arr[0])[sz-1,0]
  
  for p=1,num_ptr-1 do begin

    sz=n_elements((*ptr_arr[p])[*,0])
    tmp_start_pt=(*ptr_arr[p])[0,0]
    tmp_final_pt=(*ptr_arr[p])[sz-1,0]

    if (tmp_start_pt le start_pt) then start_pt=tmp_start_pt
    if (tmp_final_pt ge final_pt) then final_pt=tmp_final_pt
  
  endfor
  
  union=[start_pt,final_pt]
 
return,union
end        ;*** MAIN *** : * SDT_UNION *

