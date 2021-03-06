; *******************************************************************
; Multfit efficient processing of 2D diffraction images
; Copyright (C) 2000-2014 S. Merkel, Universite Lille 1
; http://merkel.zoneo.net/Multifit/
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;
; *******************************************************************


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
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory
inputname = name
nalpha = 1
file = datadirectory + name  + extension
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
filenames = datadirectory + name
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
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory
ON_IOERROR, BADINPUT
nalphatmp = fix((iend-izero)/istep+1)
nalpha = nalphatmp[0]
file = datadirectory + name + "_" + STRING(izero) + extension
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
    file = datadirectory + name + "_" + STRING(ii) + extension
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
filenames = datadirectory + name
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
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory
ON_IOERROR, BADINPUT
istep = 1
nalphatmp = fix((iend-izero)/istep+1)
nalpha = nalphatmp[0]
filebase = datadirectory + name + "_"
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
filenames = datadirectory + name
alphastart = izero
alphaend = iend
intervalle = 1
date = systime(0)
return, 1
BADINPUT: return, !ERR_STRING
end

; *******************************************************************
;
; subroutine READ_multichi
;   to read the content of a serie .chi files created by fit2d multichi output
;   for instance, file_00001.chi file_00002.chi ....
; send the base file name, start azimuth,end azimuth, and number of slices
;   for instance READ_multichi, 'file_', 0, 360, 72
;
; *******************************************************************

function read_multichi, name, startAz, endAz, nAz, log
common rawdata, nalpha, ntheta, alpha, twotheta, data
common datainfo, filenames, alphastart, alphaend, intervalle, date
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory
ON_IOERROR, BADINPUT
nalpha = nAz
intervalle = 1.0*(endAz-startAz)/nAz
alphastart = startAz + intervalle/2.
alphaend = endAz - intervalle/2.
alpha = fltarr(nalpha)
alpha[0] = alphastart
fileindex = intformat(1,5);
file = datadirectory + name + "_" + fileindex + extension
file = STRCOMPRESS(file, /REMOVE_ALL)
fileinfo = FILE_TEST(file)
if (fileinfo ne 1) then begin
    return, "File " + file + " does not exist!"
endif
ntheta = ndata(file)
data = fltarr(nalpha,ntheta)
twotheta = fltarr(ntheta)
tmp =  readdata(file)
data[0,*] = tmp[1,*]
twotheta = tmp[0,*]
for i = 1,nAz-1  do begin
    alpha[i] = alpha[i-1]+intervalle
    ii = intformat(i+1,5);
    file = datadirectory + name + "_" + ii + extension
    file = STRCOMPRESS(file, /REMOVE_ALL)
    ; logit, log, "Reading " + file
    fileinfo = FILE_TEST(file)
    if (fileinfo ne 1) then begin
        return, "File " + file + " does not exist!"
    endif
    tmp =  readdata(file)
    data[i,*] = tmp[1,*]
endfor
                                ; saving basic informations about the data
filenames = datadirectory + name
date = systime(0)
return, 1
BADINPUT: return, !ERR_STRING
end