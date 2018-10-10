;+
;FUNCTION: read_iff.pro 
;
;PURPOSE: Read U of M standard data types from IDL File Format (IFF)
;
;ARGUMENTS:
;       FILENAME -> Name of file to read data from 
;	DATA     <- Data array
;       NAMES    <- Quantity names and units
;       VALUES   <- Values for multi-line and spectra plots
;
;KEYWORDS: None
;
;RETURNS: Status
;            0 - Failure
;            1 - Success
;
;CALLING SEQUENCE:   status=read_iff('file.iff',data,names,values)
;
;NOTES: None
;
;CREATED BY: John Dombeck August 2001
;
;MODIFICATION HISTORY: 
;  8/01/2001-J. Dombeck      Creation
;-
;INCLUDED MODULES:
;   read_iff
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;
;-



;*** MAIN *** : * READ_IFF *

function read_iff,filename,data,names,values

   
; Check input

  if n_elements(filename) ne 1 or data_type(filename) ne 7 then begin
    message,'FILENAME require string',/cont
    return,0
  endif

  if strmid(filename,strlen(filename)-4,4) ne '.iff' then $
    wholefilename=filename+'.iff' $
  else $
    wholefilename=filename


; Check if file exists

  openr,unit,wholefilename,error=err,/get_lun

  if (err ne 0)  then begin
    message,'file '+wholefilename+' not found',/cont
    return,0
  endif

  free_lun,unit


; Read data

  restore,wholefilename


return,1
end        ;*** MAIN *** : * READ_IFF *

