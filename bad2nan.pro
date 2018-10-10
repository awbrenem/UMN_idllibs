;+
;FUNCTION: bad2nan.pro
;
;PURPOSE: Removing columns with NAN data points.  Has uniform time increments
;
;ARGUMENTS: 
;    DATA_ARRAY  -> Data to clear NaN columns from
;    BAD_PTS     -> Array of possible bad data points
;
;RETURNS: Array with "bad" data points replaced with NaNs
;
;KEYWORDS: N/A
;
;CALLING SEQUENCE: array=bad2nan(data_array,bad_pts)
;                   0 on failure
;
;NOTES:
;
;CREATED BY: John Dombeck July 2001
;
;MODIFICATION HISTORY: 
;	07/23/01-J. Dombeck       Creation
;-
;INCLUDED MODULES:
;   bad2nan
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;
;-


;*** MAIN *** : * BAD2NAN *

function bad2nan,data_array,bad_pts


; Check input

  n=size(data_array)

  if data_type(data_array) ne 5 or n[0] ne 2 then begin
    message,"DATA_ARRAY requires 2D array of doubles",/cont
    return,0
  endif

  if n_elements(bad_pts) ne n[1] then begin
    message,"DATA_ARRAY / BAD_PTS number of elements mismatch",/cont
    return,0
  endif


; Replace bad points

  new_arr=data_array
  this_badpts=bad_pts

  for xx=1,n[1]-1 do begin

    idx=where(this_badpts mod 2 eq 1,cnt)

    if cnt ne 0 then begin
      new_arr[idx,xx]=!values.d_nan
      this_badpts[idx]=this_badpts[idx]-1
    endif

    this_badpts=this_badpts / 2

  endfor
    
return,new_arr
end        ;*** MAIN *** : * BAD2NAN *

