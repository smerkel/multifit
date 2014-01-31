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

function intformat, n, ndigits
case ndigits of
	'1': s = string(n,format='(I1.1)')
	'2': s = string(n,format='(I2.2)')
	'3': s = string(n,format='(I3.3)')
	'4': s = string(n,format='(I4.4)')
	'5': s = string(n,format='(I5.5)')
	'6': s = string(n,format='(I6.6)')
	'7': s = string(n,format='(I7.7)')
	'8': s = string(n,format='(I8.8)')
	'9': s = string(n,format='(I9.9)')
	'10': s = string(n,format='(I10.10)')
endcase
return, s
end
