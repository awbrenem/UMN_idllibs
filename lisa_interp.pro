;+
;FUNCTION: interp.pro
;
;PURPOSE: to interpolate the data from read_sdt and mreadsdt
;
;ARGUMENTS:
;	mdata_array	-> multiple pointer array from mread_sdt
;	star_t		-> start time
;	end_t		-> end time
;	time_step	-> time step
;	bad		<- array to contain suspected bad points
;
;RETURNS: a matrix containing the interpolated values for each file
;
;KEYWORDS:return_flg=ret_flg	/returns 0 if interpolation gives garbage data
;         fix_err=fx_err	/asks for user input when 
;				start times and end times exceed data set 
;	  typeA=tyA		/sets bad values to NAN
;
;CALLING SEQUENCE: 
;	interp_vls=interp(data,start,end,.25,bad,/fix_err,/return_flag)
;
;NOTES: input start,end,and time variables MUST BE DOUBLES       
;       so they need a 'd' after the input values. 
;       Default will return value and msg if error encountered 
;	during interpolation.  
;	does not work with multiple data sets at once
;	bad array is only returned when key word typeA is not set
;
;CREATED BY: Lisa Rassel May,2001
;
;LAST MODIFIED: 
;	07/11/01-L.Rassel	included var bad to print array of bad values
;	06/13/01-L. Rassel	updated so it works with multiple files
;-
function interp,mdata_array,start_t,end_t,time_step,bad,fix_err=fx_err,$
	 return_flag=ret_flg,typeA=tyA
num_files=0l & index=0 & size_array=0l & W=0

  tmp=size(mdata_array)  ;finds num of pointers in data array
  num_files=tmp[1]  ;assumes use of 1D array(see size fcn discription)
  for p=0,(num_files-1) do begin
    size_array=size(*mdata_array[p],/dimensions)
    new_array=make_array(size_array[0],size_array[1])
    new_array=*mdata_array[p]
    times=dblarr(size_array[0])
    data_val=dblarr(size_array[0])
 
    for i=0l,(size_array[0]-1) do begin   ;sets 'X' value of spline fcn
      times[i]=new_array[i,0]
    endfor

    for j=0l,(size_array[0]-1) do begin   ;sets 'Y' value of spline fcn
      data_val[j]=new_array[j,1]     ;NOW READS IN ALL DATA QTYS
    endfor
  
    end_t=double(end_t)
    start_t=double(start_t)

    time_step=double(time_step)
     if(end_t lt start_t) then begin       ;makes sure start time > end time
      temp=start_t
      start_t=end_t
      end_t=temp
    endif

    if(keyword_set(fx_err)) then begin
      if(start_t lt times[0] or start_t gt times[(size_array[1]-1)]) then begin
        print,'invalid start time entered.  Please choose value within range:'
        print,format='(f20.10)',times[0] 
        print,format='(f20.10)',times[size_array[0]-1]
        R=' '
        read,R,Prompt='Please enter new start time value: '
        start_t=double(R)
      endif
      if(end_t gt times[(size_array[0]-1)] or end_t lt times[0]) then begin
        print,'invalid end time entered.  Please choose value within range:'
        print,times[0] 
        print,times[size_array[0]-1]
        R=' '
        read,R,Prompt='Please enter new end time value: '
        end_t=double(R)
      endif
     endif

    if((end_t-start_t) lt time_step) then begin  ;makes sure tm_stp<time int
      time_step=end_t-start_t
      print, 'invalid time_step.Reset to start-end time.'
    endif 

    number_iter=long((end_t-start_t)/time_step)  ;sets 'T' values of spline fcn
    T=dblarr(number_iter)
    for k=0l,(number_iter-1) do begin
      if (k eq 0) then begin
          start1=start_t
      endif
      T[k]=start1
      start1=start1+time_step
    endfor 

    !except=0
    interp_values=spline(times,data_val,T)        ;do cubic interpolation    

    if(keyword_set(ret_flg)) then begin     ;returns 0 if garbage interpolation
      math_val=check_math()
      if(math_val ne 0) then return,0
    endif

    D=[interp_values]

    if (p eq 0) then begin           ;these should only happen the first time
      badarr=make_array(n_elements(T),/long,value=0)
      flag=1
    endif
    rate=min(difference((*mdata_array[p])(*,0))) 
    strpt=0l
    while((*mdata_array[p])(strpt,0) lt T[0]) do strpt=strpt+1
    endpt=strpt
    while((*mdata_array[p])(endpt,0) lt T[number_iter-1]) do endpt=endpt+1
    for yy=strpt,endpt-1 do $
      if((*mdata_array[p])[yy+1,0]-(*mdata_array[p])[yy,0]) gt 1.1*rate $
      then begin
        badpts=where(T gt (*mdata_array[p])[yy,0] and $
                     T lt (*mdata_array[p])[yy+1,0],cnt)
        if cnt ne 0 then begin
          if(keyword_set(tyA)) then $
            D[badpts]=!values.d_nan $
          else badarr[badpts]=badarr[badpts]+flag
        endif
      endif
    flag=2*flag
    bad=badarr

    if(p eq 0) then W=[[T],[D]] else W=[[W],[D]]
stop
  endfor

   if(check_math() ne 0) then begin
      print,'Program caused arithmetic error.  Returned values uncertain. '
   endif
return, W  ;returns an array of times in index 0, and corresponding
end         ;interpolated values