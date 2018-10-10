;+
; FUNCTION even_rate
;
; PURPOSE:     The purpose of EVEN_RATE is to create a one-dimensional time
;              array with an evenly spaced sample rate. This function can 
;              work in two different ways:
;                 1) It can create a time set based on simple inputs, or
;                 2) It can create a time set based on analyzing a given data
;                    array.
;
; ARGUMENTS:
;       DATA  -> The data array for which the time is to be created.
;                  (OPTIONAL)
;
; KEYWORDS:
;       TIMES ->  [start time,end time] for time series
;       RATE  ->  the uniform data rate to be used
;       INDEX ->  index of the DATA series from which to take times/rate
;       UNION /   creates time set over the union of all time ranges
;
; RETURNS: an array of double precision times from start time to end time
;          at RATE spacing. 
;          (0 on error)
;
; CALLING SEQUENCES:
;              new_times = even_rate(data)
;              new_times = even_rate(data,times=[t1,t2])
;              new_times = even_rate(data,rate=0.1)
;              new_times = even_rate(times=times,rate=rate)
;
; NOTES: Start time is inclusive, end time is not neccessarily inclusive
;        By default uses intersection times of pointer array
;        By default, RATE is minimum even rate of time series
;        TIMES and RATE combination overrides data related values
;        INDEX overrides UNION
;
; CREATED BY:  Jed J. Heisserer and John Dombeck  Sept, 2001
;
; LAST MODIFIED:
;       09/29/01-J. Heisserer   created
;       10/03/01-J. Dombeck     organized logic
;
;-
;INCLUDED MODULES:
;   even_rate
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;   difference
;   find_datarate
;
;-



;*** MAIN *** : * EVEN_RATE *

function even_rate,data,times=times,rate=rate,index=index,union=union


; Check input types

  d_set=n_params()
  t_set=n_elements(times) gt 0
  r_set=n_elements(rate) gt 0
  i_set=n_elements(index) gt 0
  u_set=n_elements(union) gt 0


; Data (determine times)


; Times given

  if t_set then begin
    if n_elements(times) ne 2 or data_type(times) ne 5 then begin
      message, "TIMES must have 2 times in double-precision",/cont
      return,0
    endif

    if times[0] gt times[1] then eventimes=reverse(times) $
                            else eventimes=times
  endif


; Rate given

  if r_set then begin
    if rate lt 0 then begin
      message, "RATE requires >0",/cont
      return,0
    endif

    evenrate=rate
  endif


; Use DATA to find times and/or rate

  if not r_set or not t_set then begin

    if not d_set then begin
      message, "DATA or TIMES/RATE combination required",/cont
      return,0
    endif

    d_type=data_type(data)


  ; Pointers

    if d_type eq 10 then begin
      n_data=n_elements(data)


    ; Only 1 element

      if n_data eq 1 then begin
        dtimes=(*data[0])[*,0]
        if not t_set then eventimes=[dtimes[0],dtimes[n_elements(dtimes)-1]]
        if not r_set then begin
          evenrate=find_datarate(dtimes)
          if evenrate eq 0. then return,0
        endif


    ; Index given

      endif else if i_set then begin
        if index gt n_data-1 or index lt 0 then begin
          message, "INDEX out of range",/cont
          return,0 
        endif
        dtimes=(*data[index])[*,0]
        if not t_set then eventimes=[dtimes[0],dtimes[n_elements(dtimes)-1]]
        if not r_set then begin
          evenrate=find_datarate(dtimes)
          if evenrate eq 0. then return,0
        endif


    ; Union/Intersect (Intersect = Default)

      endif else begin
        if not t_set then begin
          if u_set then eventimes=sdt_union(data) $
                   else eventimes=sdt_intersect(data)
          if n_elements(eventimes) eq 1 then return,0
        endif

        if not r_set then begin
          message,'RATE/INDEX not given, using shortest even data rate',/cont

          startrate=eventimes[1]-eventimes[0]
          evenrate=startrate
          for xx=0,n_data-1 do begin
            dtimes=(*data[xx])[*,0]
            tmprate=find_datarate(dtimes)
            if evenrate ne 0. then $
              if tmprate lt evenrate then evenrate=tmprate
          endfor

          if evenrate eq startrate then begin
            message,'No even data rate(s)',/cont
            return,0
          endif
        endif
      endelse
            

  ; Doubles

    endif else if d_type eq 5 then begin
      dtimes=data[*,0]
      bogus=where(difference(dtimes) lt 0.,cnt)
      if cnt ne 0 then begin
        message,'DATA[*,0] not monotonically increasing',/cont
        return,0
      endif

      if not t_set then eventimes=[dtimes[0],dtimes[n_elements(dtimes)-1]]
      if not r_set then begin
        evenrate=find_datarate(dtimes)
        if evenrate eq 0. then return,0
      endif


  ; Bad DATA

    endif else begin
      message,'DATA requires double array or sdt pointer structure',/cont
      return,0
    endelse

  endif


; Compute time series

  if eventimes[1]-eventimes[0] lt 2.*evenrate then begin
    message,'time range to short for RATE',/cont
    return,0
  endif

  n_points=round((eventimes[1]-eventimes[0])/evenrate)+1
  even_series=dindgen(n_points)*evenrate+eventimes[0]
  even_series=even_series[where(even_series le eventimes[1]+0.1*evenrate)]

return,even_series
end     ;*** MAIN *** : * EVEN_RATE *
