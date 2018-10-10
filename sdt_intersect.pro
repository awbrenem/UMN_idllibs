;+
;FUNCTION: sdt_intersect.pro
;
;PURPOSE: To find the intersection among a set of sdt pointer arrays
;
;ARGUMENTS:
;       PTR_ARR	  ->  SDT pointer array
;
;RETURNS: interesection data times [start time,end time]
;           ( 0 on failure)
;
;KEYWORDS: N/A
;
;CALLING SEQUENCE: times=sdt_intersect(ptr_arr)
;
;NOTES: None
;
;CREATED BY: Lisa Rassel July,09 2001
;
;MODIFICATION HISTORY:
;  07/09/01-L.Rassel	Original writing
;-
;INCLUDED MODULES:
;   sdt_intersect
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;
;-



;*** MAIN *** : * SDT_INTERSECT *


function sdt_intersect,ptr_arr

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
    inta=where(start_pt ge (*ptr_arr[p])[*,0],cnt1)
    intb=where(final_pt le (*ptr_arr[p])[*,0],cnt2)
  
    if (cnt1 eq sz) or (cnt2 eq sz) then begin
      message,'data sets do not intersect',/cont
      return,0
    endif

    if (cnt1 eq 0) then start_pt=(*ptr_arr[p])(0,0)
    if (cnt2 eq 0) then final_pt=(*ptr_arr[p])(sz-1,0)
  
  endfor
  
  intsec=[start_pt,final_pt]
 
return,intsec
end        ;*** MAIN *** : * SDT_INTERSECT *

