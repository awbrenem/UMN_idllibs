;+
;PROCEDURE totplot.pro
;
;PURPOSE: Store common data formats as TPLOT quantities
;
;ARGUMENTS:
;	DATA     -> Data to convert to TPLOT
;	NAMES    -> Array of variable names and units
;                    (Optional if /SINGLE or /SPEC plot)
;
;KEYWORDS: 
;       SINGLE   /  Saves all data as one multiline panel (or spectra)
;       SPEC     /  Saves data as a spectral plot
;       NAME     -> Name of panel for /SINGLE or /SPEC plot
;       VALUES   -> Data values for multiline or spectral plot
;       NOZLOG   /  Plot Z linearly rather than log with /SPEC
;
;CALLING SEQUENCE: totplot,data,names
;
;NOTES: None
;
;CREATED BY: John Dombeck  July,2001
;
;LAST MODIFIED: 
;	07/25/01-J. Dombeck     created 
;	07/27/01-J. Dombeck     Added multiline/spectral plot handling
;	09/04/01-J. Dombeck     Added x/y no_interp for spectral plots
;                               Added NOZLOG keyword
;-
;INCLUDED MODULES:
;   totplot
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;   store_data
;
;-



;*** MAIN *** : * TOTPLOT *

pro totplot,data,names,single=single,spec=spec,name=name,values=values,$
      nozlog=nozlog


; Check input

  dtype=data_type(data)    ; (5=double, 10=pointer)
  if dtype ne 5 and dtype ne 10 then begin
    message,"DATA requires either double array or pointer array",/cont
    return
  endif


; Multi-quantity plots?

  if keyword_set(spec) or keyword_set(single) then multi=1 else multi=0


; Find number of quantities (or channels)

  if dtype eq 5 then begin  ; Double array
    sz=size(data)
    num=sz[2]
  endif else begin          ; Pointer array
    if multi eq 1 then begin
      message,"DATA requires double array with /SINGLE or /SPEC",/cont
      return
    endif
    num=n_elements(data)
  endelse
    
  ndtype=data_type(names)   ; (7=string)


; Check input for multi-quantity plots

  if multi eq 1 then begin
    
    if data_type(name) ne 7 then begin
      message,"NAME required with /SINGLE or /SPEC ",/cont
      return
    endif

    if ndtype ne 7 and ndtype ne 0 then begin
      message,"NAMES requires string array",/cont
      return
    endif

    vnum=n_elements(values)
    if vnum ne 0 and vnum ne num-1 $
       and vnum ne (num-1)*n_elements(data[*,0]) then begin
      message,"Number of VALUES mismatch",/cont
      return
    endif

    if vnum eq 0 and keyword_set(spec) then begin
      message,"VALUES required with /SPEC",/cont
      return
    endif


; Check input for single quantity plots

  endif else begin

    if ndtype ne 7 then begin
      message,"NAMES requires string array",/cont
      return
    endif

    if n_elements(name) ne 0 or n_elements(values) ne 0 then begin
      message,"NAME and VALUES keywords not allowed w/o /SINGLE or /SPEC",/cont
      return
    endif

  endelse


; Check if NAMES contains quantity names and units

  namnum=n_elements(names)

  if namnum eq 2*num then begin
    if multi eq 1 then begin
      units=names[1,1]
      bogus=where(names[1:num-1,1] ne units,cnt)
      if cnt ne 0 then begin
        message,"Units mismatch with /SINGLE or /SPEC",/cont
        return
      endif
    endif
    unitflg=1
  endif else if namnum eq num or namnum eq 0 then begin
    unitflg=0
  endif else if namnum ne 0 or multi ne 1 then begin
    message,"DATA / NAMES number of elements mismatch",/cont
    return
  endif
    

; Multi-quantity (spec or multiline) plot

  if multi eq 1 then begin

    id=strcompress(strlowcase(name),/remove_all)

    if vnum eq 0 then dat={x:data[*,0],y:data[*,1:num-1]} $
                 else dat={x:data[*,0],y:data[*,1:num-1],v:values}

    store_data,id,data=dat


  ; Find appropriate labels

    if unitflg eq 0 then begin
      if namnum ne 0 then labels=names[1:num-1]
      title=name
    endif else begin
      labels=names[1:num-1,0]
      if units ne 'na' and units ne 'Na' and units ne 'NA' $
          and units ne 'N/A' and units ne 'n/a' then begin
        if keyword_set(spec) then begin
          title=name
          zlabel=units
        endif else begin
          title=name+'!C'+units
        endelse
      endif else begin
        title=name
      endelse
    endelse

    options,id,'ytitle',title
    if keyword_set(spec) then begin
      options,id,'spec',1
      options,id,'x_no_interp',1
      options,id,'y_no_interp',1
      if n_elements(zlabel) ne 0 then options,id,'ztitle',zlabel
      if not keyword_set(nozlog) then options,id,'zlog',1 $
                                 else options,id,'zlog',0
    endif
    if n_elements(labels) ne 0 then options,id,'labels',labels


; (Multiple) single line plot(s)

  endif else begin


  ; Setup names
  
    if unitflg eq 1 then begin
      tnames=names[*,0]
      tunits=names[*,1]
      na_idx=where(tunits eq 'na' or tunits eq 'Na' or tunits eq 'NA' $
                or tunits eq 'N/A' or tunits eq 'n/a',cnt)
      if cnt ne 0 then tunits[na_idx]=''
    endif else begin
      tnames=names
      tunits=tnames
      tunits[*]=''
    endelse
  
    tids=strcompress(strlowcase(tnames),/remove_all)
    bogus=where(tids eq '',cnt)
    if cnt ne 0 then begin
      message,"Non-blank NAMES required",/cont
      return
    endif
  
  
  ; Store TPLOT quantities
  
    if dtype eq 5 then start=1 $
                  else start=0

    for xx=start,num-1 do begin
      if dtype eq 5 then dat={x:data[*,0],y:data[*,xx]} $
                    else dat={x:(*data[xx])[*,0],y:(*data[xx])[*,1]}
      store_data,tids[xx],data=dat
      if tunits[xx] eq '' then title=tnames[xx] $
                          else title=tnames[xx]+'!C'+tunits[xx]
      options,tids[xx],'ytitle',title
    endfor
 
  endelse
 
return
end        ;*** MAIN *** : * TOTPLOT *

