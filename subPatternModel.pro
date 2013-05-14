; *******************************************************************
; Multfit efficient processing of 2D diffraction images
; Copyright (C) 2000-2011 S. Merkel, Universite Lille 1
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


; data range (array of integers)
; number of peaks
; array of objects peaks

PRO SubPatternModel__DEFINE 
	struct = { subPatternModel, npeaks: 0, deltarange: PTR_NEW(), peakinfo: PTR_NEW() } 
END

; Init method
function SubPatternModel::Init
return, 1
end

; Cleanup method
pro SubPatternModel::Cleanup
end

FUNCTION SubPatternModel::setNPeaks, n
	if (self.npeaks ne 0) then return, 'Sorry, this subPattern object is used already'
	self.npeaks = n
	self.peakinfo = PTR_NEW(fltarr(n))
RETURN, 1
END

FUNCTION SubPatternModel::nPeaks
RETURN, self.nPeaks
END

FUNCTION SubPatternModel::nDelta
RETURN, N_ELEMENTS(*self.deltarange)
END

FUNCTION SubPatternModel::delta, i
RETURN, (*self.deltarange)(i)
END

FUNCTION SubPatternModel::peakmodel, i
RETURN, (*(self.peakinfo))(i)
END

FUNCTION SubPatternModel::fromFile, log, filename, peakprofile
	ncolperpeak = 4
	logit, log, 'Reading data from ' + filename
	; The Read_ascii command is giving me non consistent shit from one file 
	; to the next. I just wrote my own. 
	OPENR, lun, filename, /GET_LUN
	header = STRARR(2)
	READF, lun, header
	row = STRARR(1)
	READF, lun, row
	result = STRSPLIT(row, /extract)
	npfloat = 1.0*(n_elements(result)-1)/ncolperpeak
	if (fix(npfloat) ne npfloat) then return, "Wrong number of columns. You probably did not save the peak half-widths."
	self.npeaks = (n_elements(result)-1)/ncolperpeak
	azimuth = FLTARR(400)
	twotheta = FLTARR(self.npeaks,400)
	hwidth = FLTARR(self.npeaks,400)
	intensity = FLTARR(self.npeaks,400)
	count = 0
	WHILE (NOT EOF(lun)) DO BEGIN
		azimuth(count) = float(result(0))
		for i=0,self.npeaks-1 do begin
			twotheta(i,count) = float(result(i*ncolperpeak+1))
			intensity(i,count) = float(result(i*ncolperpeak+3))
			hwidth(i,count) = float(result(i*ncolperpeak+4))
		endfor
		READF, lun, row
		result = STRSPLIT(row, /extract)
		count = count + 1 
	ENDWHILE
	close, lun
	free_lun, lun
	; 23 nov 2011
	; Last line may not have been processed properly
	if (n_elements(result) gt 1) then BEGIN
	 azimuth(count) = float(result(0))
    for i=0,self.npeaks-1 do begin
      twotheta(i,count) = float(result(i*ncolperpeak+1))
      intensity(i,count) = float(result(i*ncolperpeak+3))
      hwidth(i,count) = float(result(i*ncolperpeak+4))
    endfor
    count = count + 1
	endif
	; End modification 23 nov 2011
	; Ok, data has been read. We have to create a list of azimuth angles for 
	; which we have data and create a model for each peak
	; Let's make sure we do have data for more than a few orientations...
	if (count lt 5) then return, "You do not have enough data in there! I found" + STRING(count,/print) + " datapoints. You need at least 5."
	; Azimuth angles are converted to INTEGERS! This is critical for comparisons
	; later.
	self.deltarange=PTR_NEW(intarr(count))
	*(self.deltarange)=fix(azimuth(0:count-1))
	self.peakinfo = PTR_NEW(OBJARR(self.npeaks))
	for i=0,self.npeaks-1 do begin
		logit, log, 'Creating model for peak ' + string(i,/print)
		(*(self.peakinfo))(i) = OBJ_NEW('PeakModel')
		testit = (*self.peakinfo)(i)->fromData(log, count, azimuth, twotheta(i,*), intensity(i,*), hwidth(i,*))
		testit = (*self.peakinfo)(i)->setPeakProfile(peakprofile)
	endfor
RETURN, 1
END 
  
FUNCTION SubPatternModel::readFromAscii, lun
	on_ioerror, bad
	row = readascii(lun,com='#')
	self.nPeaks = fix(row)
	ndelta = fix(readascii(lun, com='#'))
	self.deltarange=PTR_NEW(intarr(ndelta[0]))
	for i=0, ndelta[0]-1 do (*self.deltarange)(i) = fix(readascii(lun, com='#'))
	self.peakinfo = PTR_NEW(OBJARR(self.npeaks))
	for i=0, self.nPeaks-1 do begin
		(*(self.peakinfo))(i) = OBJ_NEW('PeakModel')
		noerror = (*self.peakinfo)(i)->readFromAscii(lun)
		if (noerror ne 1) then return, noerror
	endfor
RETURN, 1 
bad: return, !ERR_STRING
END

FUNCTION SubPatternModel::saveToAscii, lun
	printf, lun, '# Number of peaks'
	printf, lun, STRING(self.nPeaks, /PRINT)
	printf, lun, '# Range in azimuth: nvalues, and values'
	printf, lun, STRING(N_ELEMENTS((*self.deltarange)), /PRINT)
	for i=0, N_ELEMENTS(*(self.deltarange))-1 do begin
		printf, lun, STRING((*(self.deltarange))(i), /PRINT)
	endfor
	printf, lun, '# Peak models'
	for i=0, self.nPeaks-1 do begin
		printf, lun, '# peak number ' + STRING(i, /PRINT)
		noerror = (*self.peakinfo)(i)->saveToAscii(lun)
		if (noerror ne 1) then return, noerror
	endfor
RETURN, 1
END