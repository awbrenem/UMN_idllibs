;+
;PROCEDURE: sdt_free.pro 
;
;PURPOSE: Deallocate memory used by sdt pointer array
;
;ARGUMENTS:
;	SDT_PTR_ARRAY  <> SDT pointer array to be deallocated
;
;KEYWORDS: N/A
;
;CALLING SEQUENCE: sdt_free,sdt_ptr_array
;
;CREATED BY: Lisa Rassel May 2001
;
;MODIFICATION HISTORY: 
;  5/18/2001-L.Rassel	original creation
;-
;INCLUDED MODULES:
;   sdt_free
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;   undefine
;
;-



;*** MAIN *** : * SDT_FREE *

pro sdt_free,sdt_ptr_array

  if data_type(sdt_ptr_array) ne 10 then begin
    message,'SDT_PTR_ARRAY not proper type',/cont
    return
  endif

  size=n_elements(sdt_ptr_array)
  for i=0,size-1 do begin
    ptr_free,sdt_ptr_array[i]
  endfor

  undefine,sdt_ptr_array

return
end        ;*** MAIN *** : * SDT_FREE *

