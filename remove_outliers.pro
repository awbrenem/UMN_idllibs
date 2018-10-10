;+
;FUNCTION: remove_outliers.pro
;
;PURPOSE: To replace outlying data points with NaNs.
;
;ARGUMENTS:
;	DATA_ARRAY    -> array of data to be checked
;	ERROR_BOUND   -> error_bound muliplier for outliers (default=2)
;
;RETURNS: a data_array free of any outlying data points
;
;KEYWORD(S): 
;	None
;
;CALLING SEQUENCE: var=remove_outliers(data_array,error_bound)
;
;NOTES:  None 
;
;CREATED BY: Lisa Rassel Jun 2001
;
;MODIFICATION HISTORY: 
;	06/15/01- L. Rassel	incorporated: sub_array->set_nan and typeA
;-
;INCLUDED MODULES:
;   rmvo_set_nan
;   rmvo_sub_array
;   remove_outliers
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;   difference
;
;-



;* RMVO_SET_NAN *  To replace outlying data points with NAN

function rmvo_set_nan,array,error_bound,keepzero=keepzero

  n=n_elements(array)
  if n le 4 then return,array


; Checks first data point

  if (not keyword_set(keepzero) or (array[0] ne 0)) then begin
    if ((abs(array[0]) lt abs(array[1]/error_bound))       $
    and (abs(array[0]) lt abs(array[2]/error_bound)))      $
    or ((abs(array[0]) gt abs(array[1]*error_bound))       $
    and (abs(array[0]) gt abs(array[2]*error_bound))) then $
      array[0]=!values.d_nan
  endif


; Checks last data point


  if (not keyword_set(keepzero) or (array[n-1] ne 0)) then begin
    if ((abs(array[n-1]) lt abs(array[n-2]/error_bound))       $
    and (abs(array[n-1]) lt abs(array[n-3]/error_bound)))      $
    or ((abs(array[n-1]) gt abs(array[n-2]*error_bound))       $
    and (abs(array[n-1]) gt abs(array[n-3]*error_bound))) then $
      array[n-1]=!values.d_nan
  endif


; Check points in between

  for i=1,(n-2) do begin
    test=array[i]
    prev=array[i-1]
    next=array[i+1]


;XXXXXXXXX  NEED TO FIX THIS ALGORITHM  XXXXXXXXXXXXX

  ; If no first point yet, use following 2 points
  ; If NaN in middle find first good point before NaN

    if (finite(prev,/nan) eq 1) then begin
      if (i gt 1) then begin
        prev=array[i+2]
      endif else begin
        prev=next
        next=array[i+2]
      endelse
    endif


  ; Handles final NaN

    if (finite(next,/nan) eq 1) then begin
      prev=array[i-2]
      next=prev
    endif


  ; Check the point


    if ((abs(test) lt abs(prev/error_bound))         $
    and (abs(test) lt abs(next/error_bound)))        $
    or ((abs(test) gt abs(prev*error_bound))         $
    and (abs(test) gt abs(next*error_bound))) then   $
      array[i]=!values.d_nan

  endfor

return,array
end     ;* RMVO_SET_NAN *


;* RMVO_SUB_ARRAY *  To decompose arrays with NaNs into sub-arrays between
;                    NaNs, then call RMVO_SET_NAN on the sub_arrays

function rmvo_sub_array,array,error_bound


; Find the NAN indices

  idx=where(finite(array,/nan),num_nan)


; No NaNs

  if num_nan eq 0 then begin
    ret_array=rmvo_set_nan(array,error_bound)
    return,ret_array
  endif


; All NaNs

  num=n_elements(array)
  if num eq num_nan then return,array


; Find sub-array(s)


; Find Sub-arrays

  nan_array=make_array(num,/integer,value=0)
  nan_array(idx)=1

  idx_dif=difference(nan_array)

  a=where(idx_dif eq -1,a_cnt)+1  ; a = array of sub_array start points
  b=where(idx_dif eq 1,b_cnt)     ; b = array of sub_array end points


; Correct ends

  if b_cnt eq 0 then b=[num-1]  $
  else if a_cnt eq 0 then a=[0] $
  else begin
    if b[b_cnt-1] lt a[a_cnt-1] then b=[b,num-1]
    if a[0] gt b[0] then a=[0,a]
  endelse


; Find bad points on sub-arrays

  ret_array=array
  a_cnt=n_elements(a)
  b_cnt=n_elements(b)


; Internal diagnostic

  if a_cnt ne b_cnt then $
    message,"a_cnt ne b_cnt"


; Loop thru sub-array(s)

  for i=0,a_cnt-1 do begin
    new_array=[array[a[i]:b[i]]]
    fixed_arr=rmvo_set_nan(new_array,error_bound)
    ret_array[a[i]:b[i]]=fixed_arr
  endfor

return,ret_array
end     ;* RMVO_SUB_ARRAY *



;*** MAIN *** : * REMOVE_OUTLIERS *

function remove_outliers,data_array,error_bound


; Check input

  if data_type(data_array) ne 5 then begin
    message,"DATA_ARRAY requires array of DOUBLEs",/cont
    return,0
  endif

  delta=difference(data_array[*,0])
  mm=min(delta)
  bogus=where(mm*1.5 lt delta,num)

  if(num gt 0) then begin
    message,'input times do have gaps.',/cont
    return,0
  endif

  if n_elements(error_bound) eq 0 then error_bound=2


; Remove outlying points

  n=size(data_array)   ;n[0]=dimens, n[1]=col,n[2]=rows,n[3]=type,n[4]=elemts
  my_array=data_array
  
  for k=1,n[2]-1 do begin
    my_array[*,k]=rmvo_sub_array(my_array[*,k],error_bound)
  endfor

return,my_array
end        ;*** MAIN *** : * REMOVE_OUTLIERS *

