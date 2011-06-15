; *******************************************************************
;
; function READDATA
;   Reads the data out of one .chi file.
; send the file name
; returns the data as 2 columns table, first col. = angles,
; 2nd colums = intensities
;
; *******************************************************************

function readdata, file
tmp = read_ascii(file, DATA_START=4)
data = tmp.field1
return, data
end

; *******************************************************************
;
; function NDATA
;   Counts the number of data point in a .chi file
; send the file name
; Returns the number of points
;
; *******************************************************************

function ndata, file
tmp = read_ascii(file, DATA_START=4)
data = tmp.field1
data_size=size(data)
return, data_size(2)
end

; *******************************************************************
;
; subroutine READ_ONE
;   to read the content of one .chi file
; send the file name (without the .chi extension)
;
; *******************************************************************

pro read_one, name
common rawdata, nalpha, ntheta, alpha, twotheta, data
common datainfo, filenames, alphastart, alphaend, intervalle, date
common files, extension, directory, outputdirectory
inputname = name
nalpha = 1
file = directory + name  + extension
file = STRCOMPRESS(file, /REMOVE_ALL)
; print, "Reading ", file
ntheta = ndata(filedata = fltarr(nalpha,ntheta))
twotheta = fltarr(ntheta)
alpha = fltarr(nalpha)
tmp =  readdata(file)
data(0,*) = tmp(1,*)
twotheta = tmp(0,*)
alpha(0) = 0.0
; saving basic informations about the data
filenames = directory + name
alphastart = 0.0
alphaend = 0.0
intervalle = 0
date = systime(0)
end

; *******************************************************************
;
; subroutine READ_ALL
;   to read the content of a serie .chi files
;   for instance, file_10.chi file_15.chi file_20.chi file_25.chi
;   where 10 15 20 25 are the values of the azimuth angle
; send the base file name, izero, iend, iset
;   for instance READ_ALL, 'file_', 10, 25, 5
;
; *******************************************************************

function read_all, name, izero, iend, istep, log
common rawdata, nalpha, ntheta, alpha, twotheta, data
common datainfo, filenames, alphastart, alphaend, intervalle, date
common files, extension, directory, outputdirectory
ON_IOERROR, BADINPUT
nalphatmp = fix((iend-izero)/istep+1)
nalpha = nalphatmp[0]
file = directory + name + "_" + STRING(izero) + extension
file = STRCOMPRESS(file, /REMOVE_ALL)
fileinfo = FILE_TEST(file)
if (fileinfo ne 1) then begin
    return, "File " + file + " does not exist!"
endif
ntheta = ndata(file)
data = fltarr(nalpha,ntheta)
twotheta = fltarr(ntheta)
alpha = fltarr(nalpha)
tmp =  readdata(file)
data(0,*) = tmp(1,*)
twotheta = tmp(0,*)
alpha(0) = izero
endloop = (nalpha(0)-1)
for i = 1, endloop do begin
    alpha(i) = izero + i*istep
    ii = fix(alpha(i))
    file = directory + name + "_" + STRING(ii) + extension
    file = STRCOMPRESS(file, /REMOVE_ALL)
    ; logit, log, "Reading " + file
    fileinfo = FILE_TEST(file)
    if (fileinfo ne 1) then begin
        return, "File " + file + " does not exist!"
    endif
    tmp =  readdata(file)
    data(i,*) = tmp(1,*)
endfor
                                ; saving basic informations about the data
filenames = directory + name
alphastart = izero
alphaend = iend
intervalle = istep
date = systime(0)
return, 1
BADINPUT: return, !ERR_STRING
end

; *******************************************************************
;
; subroutine READ_ALL_FIXED_DIGITS
;   to read the content of a serie .chi files, with the same number of digits for all numbers
;     and always separated by one
;   for instance, file_010.chi file_011.chi file_012.chi file_013.chi
;   This is more intendend for series of diffraction images fully integrated
; send the base file name, izero, iend, number of digits
;   for instance READ_ALL_FIXED_DIGITS, 'file_', 10, 25, 5
;
; *******************************************************************

function read_all_fixed_digits, name, izero, iend, idigits, log
common rawdata, nalpha, ntheta, alpha, twotheta, data
common datainfo, filenames, alphastart, alphaend, intervalle, date
common files, extension, directory, outputdirectory
ON_IOERROR, BADINPUT
istep = 1
nalphatmp = fix((iend-izero)/istep+1)
nalpha = nalphatmp[0]
filebase = directory + name + "_"
fileindex = intformat(izero,idigits);
file = strtrim(filebase) + strtrim(fileindex) + extension
file = STRCOMPRESS(file, /REMOVE_ALL)
fileinfo = FILE_TEST(file)
if (fileinfo ne 1) then begin
    return, "File " + file + " does not exist!"
endif
ntheta = ndata(file)
data = fltarr(nalpha,ntheta)
twotheta = fltarr(ntheta)
alpha = fltarr(nalpha)
tmp =  readdata(file)
data(0,*) = tmp(1,*)
twotheta = tmp(0,*)
alpha(0) = izero
endloop = (nalpha(0)-1)
for i = 1, endloop do begin
    alpha(i) = izero + i*istep
	fileindex = intformat(fix(alpha(i)),idigits);
	file = strtrim(filebase) + strtrim(fileindex) + extension
    file = STRCOMPRESS(file, /REMOVE_ALL)
    ; logit, log, "Reading " + file
    fileinfo = FILE_TEST(file)
    if (fileinfo ne 1) then begin
        return, "File " + file + " does not exist!"
    endif
    tmp =  readdata(file)
    data(i,*) = tmp(1,*)
endfor
                                ; saving basic informations about the data
filenames = directory + name
alphastart = izero
alphaend = iend
intervalle = 1
date = systime(0)
return, 1
BADINPUT: return, !ERR_STRING
end
