; *******************************************************************
;
;
; *******************************************************************

function savedata, file
common files, extension, directory, outputdirectory
common datainfo, filenames, alphastart, alphaend, intervalle, date
common rawdata, nalpha, ntheta, alpha, twotheta, data
ON_IOERROR, IOERROR

filename = directory + file
openw, /XDR, 1, filename
; basic info
writeu, 1, strlen(filenames)
writeu, 1, filenames
writeu, 1, alphastart
writeu, 1, alphaend
writeu, 1, intervalle
writeu, 1, strlen(date)
writeu, 1, date
; data
writeu, 1, nalpha
writeu, 1, ntheta
writeu, 1, alpha
writeu, 1, twotheta
writeu, 1, data
close, 1
return, 1
IOERROR:close, 1
return, !ERR_STRING
end


function readfile, file
common datainfo, filenames, alphastart, alphaend, intervalle, date
common rawdata, nalpha, ntheta, alpha, twotheta, data
ON_IOERROR, BADINPUT
openr, /XDR, 1, file
                                ; basic info
length = fix(0)
alphastart = fix(0)
alphaend = fix(0)
intervalle = fix(0)
readu, 1, length
filenames = STRING(REPLICATE(32B,length))
readu, 1, filenames
readu, 1, alphastart
readu, 1, alphaend
readu, 1, intervalle
readu, 1, length
date = STRING(REPLICATE(32B,length))
readu, 1, date
                                ; data
nalpha = fix(0)
ntheta = fix(0)
readu, 1, nalpha
readu, 1, ntheta
twotheta = fltarr(ntheta)
alpha = fltarr(nalpha)
data = fltarr(nalpha,ntheta)
readu, 1, alpha
readu, 1, twotheta
readu, 1, data
close, 1
return, 1
BADINPUT: close, 1
return, !ERR_STRING
end

pro slicedata, min, max
common rawdata, nalpha, ntheta, alpha, twotheta, data
; finding the twotheta indexes
tmp = WHERE(twotheta GT min, count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT max, count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR = tmp[count-1]
; slicing the data
for i=indexL, indexR do $
	data(*,i) = data(*,indexL) + $
			(data(*,indexR)- data(*,indexL)) * $
			(twotheta(i)-twotheta(indexL)) / $
			(twotheta(indexR)-twotheta(indexL))
end

