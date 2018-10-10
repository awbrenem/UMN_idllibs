;+
;FUNCTION: find_datarate.pro
;
;PURPOSE: To find predominant data rate of a time series
;
;ARGUMENTS:
;       TIME_SERIES ->  Time series of which to determine data rate
;                         (also determines if there are gaps in data)
;       GAPS        <-  Flag whether there are data gaps or not
;                          0 = even data rate, no gaps
;                          1 = even data rate, w/ gap(s)
;                          2 = even data rate, w/ uneven gap(s)
;                          3 = uneven data rate (failure)
;
;RETURNS: Predominant data rate
;           ( 0 on failure)
;
;KEYWORDS:
;       PCT_TI_REQ ->  The minimum percetage of time intervals that need to be the determined
;                        data rate (+/- 10%) to be considered an even rate (default=90)
;
;CALLING SEQUENCE: rate=find_datarate(times)
;
;NOTES: Considered constant rate if w/i 10%
;
;CREATED BY: John Dombeck Oct.,03 2001
;
;MODIFICATION HISTORY:
;  10/03/01-J. Dombeck  Original writing
;  11/01/01-J. Dombeck  Added gap type 2
;  06/13/07-J. Dombeck  Fixed crash when 2 datapoints have same time
;-
;INCLUDED MODULES:
;   find_datarate
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;   difference
;
;-



;*** MAIN *** : * FIND_DATARATE *


function find_datarate,time_series,gaps,pct_ti_req=pct_ti_req


; Check TIME_SERIES (and calclate difference array)

  d_type=data_type(time_series)
  if (d_type ne 4 and d_type ne 5) or n_elements(time_series) lt 2 then begin
    message,'TIME_SERIES requires array of doubles or floats',/cont
    return,0
  endif

  diff=difference(time_series)

  bogus=where(diff le 0.,cnt)
  if cnt ne 0 then begin
    message,'TIMES_SERIES not monotonically increasing',/cont
    return,0
  endif

  if n_elements(pct_ti_req) ne 1 then maxbadti=0.1 $
  else begin
    d_type=data_type(pct_ti_req)
    if (d_type lt 2 or d_type gt 5) then begin
      message,'PCT_TI_REQ requires a number type',/cont
      return,0
    endif
    if (pct_ti_req lt 0. or pct_ti_req gt 100.) then begin
      message,'PCT_TI_REQ requires a number between [0,100] inclusive',/cont
      return,0
    endif else maxbadti=1.-float(pct_ti_req)/100.
  endelse

; Compute predominant data rate

  min_rate=min(diff)
  rate=mean(diff[where(diff lt min_rate*1.2)])


; Check for even rate and gaps

  bogus=where(diff-rate gt rate*0.1,cnt)
  if cnt ne 0 then begin
    bogus=where(abs(diff/rate-round(diff/rate)) gt 0.1,cnt)
    if cnt ne 0 then begin
      if float(cnt)/n_elements(diff) gt maxbadti then begin
        gaps=3
        message,'Uneven data rate',/cont
        return,0
      endif else gaps=2
    endif else gaps=1
  endif else gaps=0

return,rate
end        ;*** MAIN *** : * FIND_DATARATE *

