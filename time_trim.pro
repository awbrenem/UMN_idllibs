;+
;FUNCTION: time_trim.pro
;
;PURPOSE: To find the sub array, given a start time and end time
;
;ARGUMENTS:
;       ARR	->  original array
;	T1	->  desired start time
;	T2	->  desired end time
;
;RETURNS: New sub_array
;         0 - failure to match times
;
;KEYWORDS: 
;	PLUS	-> sets number of spaces added to intersection
;	IDX 	/  returns idices of times to keep rather than array 
;
;CALLING SEQUENCE:  new_array=time_trim(array,start_time,end_time)
;
;NOTES: Times should to be entered in UT
;
;CREATED BY: Lisa Rassel July,31 2001
;
;MODIFICATION HISTORY:
;  07/31/01-L.Rassel    Original writing
;  04/20/09-J.Dombeck   renamed input time caribles so program TIME_TRIM
;                         didn't overwrite them
;  11/29/16-J.Dombeck   added IDX keyword
;-
;INCLUDED MODULES:
;   None
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   sdt_intersect
;   data_type
;   time_double
;
;-



;*** MAIN *** : * TIME_TRIM *


function time_trim,arr,t1,t2,plus=plus,idx=idx


; Changes input start and end times to be in UT doubles

  dtype=data_type(t1)
  if dtype eq 7 then start_t=time_double(t1) else start_t=t1
  dtype=data_type(t2)
  if dtype eq 7 then end_t=time_double(t2) else end_t=t2


; If time input incorrect.

  if data_type(start_t) ne 5 or data_type(end_t) ne 5 then begin
    message,'Improper time input.',/cont
    return,0
  endif


; Sets data type and initializes NUM_PTR

  dtype=data_type(arr)
  num_ptr=0


; If pointer

  if dtype eq 10 then begin
    times=(*arr(0))[*,0]    
    num_ptr=n_elements(arr)
  endif $
  else begin
    times=arr[*,0]
    num_ptr=1
  endelse


; Checks that plus is a valid integer
 
  if keyword_set(plus) then begin
    if (plus lt 0) or (data_type(plus) ne 2) then begin
      message,'Keyword PLUS must be a positive integer.  PLUS reset to 0',/cont
      plus=0
    endif
  endif


; Trims array



  for i=0,num_ptr-1 do begin

    if i gt 0 then times=(*arr(i))[*,0]

    sz=n_elements(times)

    first=where(start_t gt times,cnt1)
    last =where(end_t lt times,cnt2)


    if (cnt1 eq sz) or (cnt2 eq sz) then begin
      message,'Start/End times are not contained within the set',/cont
      if (dtype eq 10) and (i gt 0) then ptr_free,new_array
      return,0
    endif


  ; Set s_pt and e_pt eq to the indicies of the intersection with the array

    if (cnt1 eq 0) then s_pt=0  $
     else if (cnt1 lt sz) then $
      s_pt=first[cnt1-1]+1
    if (cnt2 eq 0) then e_pt=sz-1 $
     else if (cnt2 lt sz) then e_pt=last[0]-1

    if (s_pt eq e_pt) or (s_pt gt e_pt) then begin
      message,'Intersection of START_T and END_T contains no values',/cont
      if (dtype eq 10) and (i gt 0) then ptr_free,new_array
      return,0
    endif
 
  ; Adjusts indicies of the sub array if keyword PLUS is set.

    if (keyword_set(plus)) then begin
      if dtype ne 10 then begin
        idx1=where(arr[s_pt,0] eq arr[*,0],num1)  
        idx2=where(arr[e_pt,0] eq arr[*,0],num2)
      endif else begin
        idx1=where((*arr(i))[s_pt,0] eq (*arr(0))[*,0],num1)  
        idx2=where((*arr(i))[e_pt,0] eq (*arr(0))[*,0],num2)
      endelse

      if (idx1[0]-plus) ge 0 then s_pt=idx1[0]-plus else begin
          s_pt=0
         message,'PLUS is out of bounds.  START_T eq index 0 of ARR.',/cont
      endelse
      if (idx2[0]+plus) le sz-1 then e_pt=idx2[0]+plus else begin
          e_pt=sz-1
        message,'PLUS is out of bounds.  END_T eq last index of ARR.',/cont
      endelse
    endif

  ; Makes the new array

    if dtype ne 10 then begin                   ; If not pointer
      if keyword_set(idx) then begin
        new_array=lindgen(e_pt-s_pt+1)+s_pt
      endif else begin
        new_array=[arr[s_pt:e_pt,*]]    
      endelse
    endif else begin
      if (i eq 0) then begin                        ; If pointer 
        new_array=ptrarr(num_ptr,/allocate_heap)
        *new_array(i)=(*arr(i))(s_pt:e_pt,*)
      endif else        $
        *new_array(i)=(*arr(i))(s_pt:e_pt,*) 
    endelse
  endfor

return,new_array

end    ;*** MAIN *** : * TIME_TRIM *
  
