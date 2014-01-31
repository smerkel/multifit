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
; function GET_INDEX
;   to know the index of a given two theta value in the twotheta table
; send the two theta value you are looking for, the twotheta table and
;   the number of two theta values
; returns the index
;
; *******************************************************************

function getIndex, x, twotheta, ntheta
; test the direction of the array
if (twotheta[0] lt twotheta[1]) then begin
  index = 0
  for i = 0, (ntheta-1) do begin
    if (x gt twotheta[i]) then begin
        index = i
    endif
  endfor
endif else begin
  index = 0
  for i = 0, (ntheta-1) do begin
    if (x lt twotheta[i]) then begin
      index = i
    endif
  endfor
endelse
return, index
end
