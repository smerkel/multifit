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

function savedata, file
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common datainfo, filenames, alphastart, alphaend, intervalle, date
common rawdata, nalpha, ntheta, alpha, twotheta, data
ON_IOERROR, IOERROR

filename = datadirectory + file
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

