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

pro encode_key, email, duration
email = strtrim(strlowcase(email),2)
expir = strtrim(string(long(systime(/julian)+duration*366)),2)
expirb = byte(expir)
nc = N_ELEMENTS(expirb)
print, expirb
key = ''
for i=0,nc-1 do begin
	digit = fix(long(expirb[i])-48); digit
	tmp = (digit+3) mod 10
	key += strtrim(tmp,2)
endfor
print, 'Expiration time: ', expir
print, 'Email: ', email
print, 'Key: ', key
str = key + email
strb = byte(str)
nc = N_ELEMENTS(strb)
testcode = ''
for i=0,nc-1 do begin
	; enc += string(long(strb[i]),FORMAT='(I03)')
	testcode += strtrim(string(fix(long(strb[i])*3 mod 10)),2)
endfor
print, 'Code: ', testcode
; Trying to decode key
keyb = byte(key)
nc = N_ELEMENTS(keyb)
decode = ''
for i=0,nc-1 do begin
	digit = fix(long(keyb[i])-48); digit
	tmp = digit-3
	if (tmp lt 0) then tmp=10+tmp
	decode += strtrim(tmp,2)
endfor
print, 'Decoded: ', decode
CALDAT, decode, Month1, Day1, Year1
print, 'Decoded and readable: ', Month1, Day1, Year1
end
