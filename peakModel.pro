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


; data range (array of integers)
; number of peaks
; array of objects peaks

PRO PeakModel__DEFINE 
	struct = { PeakModel, nterms : 0, peakprofile:0, thistwotheta: PTR_NEW(), thisintensity: PTR_NEW(), thishwidth: PTR_NEW(), thisweightGL: PTR_NEW() } 
END

; Init method
function PeakModel::Init
	self.nterms = 15
return, 1
end

; Cleanup method
pro PeakModel::Cleanup
end

FUNCTION PeakModel::setPeakProfile, profile
	self.peakprofile=profile
	if (profile eq 1) then test = self->initPseudoVoigt()
return, 1
end

FUNCTION PeakModel::initPseudoVoigt
	self.thisweightGL = PTR_NEW(fltarr(self.nterms*2+1))
	(*self.thisweightGL)(0) = 0.5
	for i=1, self.nterms*2 do begin
		(*self.thisweightGL)(i) = 0.
	endfor
return, 1
end

FUNCTION PeakModel::fromData, log, ndata, dataazimuth, datatwotheta, dataintensity, datahwidth
	; Creating peak models: Fourier transform of experimental data
	self.thistwotheta = PTR_NEW(fltarr(self.nterms*2+1))
	self.thisintensity = PTR_NEW(fltarr(self.nterms*2+1))
	self.thishwidth = PTR_NEW(fltarr(self.nterms*2+1))
	az = dataazimuth(0:(ndata-1))*!pi/360.
	theta = datatwotheta(0:(ndata-1))
	int = dataintensity(0:(ndata-1))
	w = datahwidth(0:(ndata-1))
	; Model for 2 theta
	sigma = 0.0001*(theta)
	guess = fltarr(self.nterms*2+1)
	guess[0] = mean(theta)
	*(self.thistwotheta) = MPFITFUN('FOURIER', az, theta, sigma, guess, /quiet)
	logit, log, '  2 theta model is ready'
	; Model for intensities
	sigma = 0.0001*(int)
	guess = fltarr(self.nterms*2+1)
	guess[0] = mean(int)
	*(self.thisintensity) = MPFITFUN('FOURIER', az, int, sigma, guess, /quiet)
	logit, log, '  Intensity model is ready'
	; Model for peak width 
	sigma = 0.0001*(w)
	guess = fltarr(self.nterms*2+1)
	guess[0] = mean(w)
	*(self.thishwidth) = MPFITFUN('FOURIER', az, w, sigma, guess, /quiet)
	logit, log, '  Half-width model is ready'
RETURN, 1
END

FUNCTION PeakModel::fromDataNoLog, ndata, dataazimuth, datatwotheta, dataintensity, datahwidth
	; Creating peak models: Fourier transform of experimental data
	self.thistwotheta = PTR_NEW(fltarr(self.nterms*2+1))
	self.thisintensity = PTR_NEW(fltarr(self.nterms*2+1))
	self.thishwidth = PTR_NEW(fltarr(self.nterms*2+1))
	az = dataazimuth(0:(ndata-1))*!pi/360.
	theta = datatwotheta(0:(ndata-1))
	int = dataintensity(0:(ndata-1))
	w = datahwidth(0:(ndata-1))
	; Model for 2 theta
	sigma = 0.0001*(theta)
	guess = fltarr(self.nterms*2+1)
	guess[0] = mean(theta)
	*(self.thistwotheta) = MPFITFUN('FOURIER', az, theta, sigma, guess, /quiet)
	; Model for intensities
	sigma = 0.0001*(int)
	guess = fltarr(self.nterms*2+1)
	guess[0] = mean(int)
	*(self.thisintensity) = MPFITFUN('FOURIER', az, int, sigma, guess, /quiet)
	; Model for peak width 
	sigma = 0.0001*(w)
	guess = fltarr(self.nterms*2+1)
	guess[0] = mean(w)
	*(self.thishwidth) = MPFITFUN('FOURIER', az, w, sigma, guess, /quiet)
RETURN, 1
END

; Fit of the weight between gaussian and lorentzian. It is put separatly 
; it is only use with the pseudo-voigt peak profile
FUNCTION PeakModel::fitWeightGL, ndata, dataazimuth, dataweightgl
	self.thisweightGL = PTR_NEW(fltarr(self.nterms*2+1))
	az = dataazimuth(0:(ndata-1))*!pi/360.
	we = dataweightgl(0:(ndata-1))
	sigma = (we)+0.01
	guess = fltarr(self.nterms*2+1)
	guess[0] = mean(we)
	*(self.thisweightGL) = MPFITFUN('FOURIER', az, we, sigma, guess, /quiet)
RETURN, 1
END

FUNCTION PeakModel::intensity, az
  ; print, "Here, PeakModel::intensity", az
	y = fourier(az*!pi/360., *(self.thisintensity))
  ; print, "ready to return, PeakModel::intensity", az
  ; print, "Returning:intensity ", y
	return, y
END

FUNCTION PeakModel::twotetha, az
  ; print, "Here, PeakModel::twotetha", az
	y = fourier(az*!pi/360., *(self.thistwotheta))
  ; print, "ready to return, PeakModel::twotetha", az
  ; print, "Returning: ", y
	return, y
END

FUNCTION PeakModel::hwidth, az
	y = fourier(az*!pi/360., *(self.thishwidth))
	return, y
END

FUNCTION PeakModel::weightGL, az
	y = fourier(az*!pi/360., *(self.thisweightGL))
	return, y
END

FUNCTION PeakModel::readFromAscii, lun
	on_ioerror, bad
	self.nterms = fix(readascii(lun,com="#"))
	self.peakprofile = fix(readascii(lun,com="#"))
	self.thistwotheta = PTR_NEW(fltarr(self.nterms*2+1))
	self.thisintensity = PTR_NEW(fltarr(self.nterms*2+1))
	self.thishwidth = PTR_NEW(fltarr(self.nterms*2+1))
	self.thisweightGL = PTR_NEW(fltarr(self.nterms*2+1))
	for i=0, self.nterms*2 do begin
		(*self.thistwotheta)(i) = float(readascii(lun,com='#'))
	endfor
	for i=0, self.nterms*2 do begin
		(*self.thisintensity)(i) = float(readascii(lun,com='#'))
	endfor
	for i=0, self.nterms*2 do begin
		(*self.thishwidth)(i) = float(readascii(lun,com='#'))
	endfor
	if (self.peakprofile eq 1) then begin
		for i=0, self.nterms*2 do begin
			(*self.thisweightGL)(i) = float(readascii(lun,com='#'))
		endfor
	endif
RETURN, 1
bad: return, !ERR_STRING
END

FUNCTION PeakModel::saveToAscii, lun
	printf, lun, '# Number terms in Fourier expension'
	printf, lun, STRING(self.nterms, /PRINT)
	printf, lun, '# Peak profile: 0- Gauss 1-Pseudo Voigt 2-Lorentz'
	printf, lun, STRING(self.peakprofile, /PRINT)
	printf, lun, '# Fourier coefficients for 2 theta'
	for i=0, self.nterms*2 do begin
		printf, lun, STRING((*self.thistwotheta)(i), /PRINT)
	endfor
	printf, lun, '# Fourier coefficients for intensities'
	for i=0, self.nterms*2 do begin
		printf, lun, STRING((*self.thisintensity)(i), /PRINT)
	endfor
	printf, lun, '# Fourier coefficients for half-widths'
	for i=0, self.nterms*2 do begin
		printf, lun, STRING((*self.thishwidth)(i), /PRINT)
	endfor
	if (self.peakprofile eq 1) then begin
		printf, lun, '# Fourier coefficients for weight between Gauss and Lorentz'
		for i=0, self.nterms*2 do begin
			printf, lun, STRING((*self.thisweightGL)(i), /PRINT)
		endfor
	endif
RETURN, 1
END