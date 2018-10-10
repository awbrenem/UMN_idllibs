;+
;FUNCTION: interp_nan.pro
;
;PURPOSE: Removing columns with NAN data points.  Has uniform time increments
;
;ARGUMENTS: 
;    DATA_ARRAY  -> Data to interpolate NaNs in
;    BAD_PTS     <- Array of possible bad points
;
;
;RETURNS: Array with NaNs interpolated
;           0 on failure
;
;KEYWORDS:
;    CUBIC    /  Does a cubic spline rather than a linear interpolation

;
;CALLING SEQUENCE: array=interp_nan(data_array,bad_pts,/cubic)
;
;NOTES: [*,0] row of DATA_ARRAY needs to be desired abscissa values
;
;CREATED BY: John Dombeck July 2001
;
;MODIFICATION HISTORY: 
;	07/27/01-J. Dombeck              Created
;-
;INCLUDED MODULES:
;   interp_nan
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   None
;
;-


;*** MAIN *** : * INTERP_NAN *

function interp_nan,data_array,bad_pts,cubic=cubic


; Check input

  n=size(data_array,/dimensions)

  if n_elements(n) ne 2 then begin
    message,"DATA_ARRAY requires 2D array",/cont
    return,0
  endif

  delta=difference(data_array[*,0])
  bogus=where(delta le 0.,cnt)
  if cnt ne 0 then begin
    message,"Abscissa values (DATA_ARRAY[*,0]) require monotonic increase",/cont
    return,0
  endif


; Fill NaNs

  new_arr=data_array
  bad_pts=make_array(n[0],/long,value=0l)

  for j=1,(n[1]-1) do begin      ;rows

    nans=finite(data_array[*,j],/nan)

    if total(nans) ne 0 then begin

    ; Find array to interpolate over w/o NaNs

      idx=where(nans eq 0)
      old_absc=data_array[idx,0]
      values=data_array[idx,j]

    
    ; Interpolate

      nan_idx=where(nans ne 0)
      if keyword_set(cubic) then $
        new_arr[nan_idx,j]=spline(old_absc,values,data_array[nan_idx,0]) $
      else $
        new_arr[nan_idx,j]=interpol(values,old_absc,data_array[nan_idx,0])


    ; Set BAD_PTS array

      bad_pts[nan_idx]=bad_pts[nan_idx]+2l^(j-1)

    endif

  endfor    

return,new_arr
end        ;*** MAIN *** : * INTERP_NAN *

