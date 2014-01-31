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

PRO FitSubPatternObject__DEFINE 
	struct = { FitSubPatternObject, peakprofile: 0, npeaks: 0, ndelta: 0, widthfactor: 4.0, deltarange: PTR_NEW(), peakmodel: PTR_NEW(), twotheta: PTR_NEW(), intensity: PTR_NEW(), hwidth: PTR_NEW(), weightGL: PTR_NEW(), limits: PTR_NEW(), bgcoefs: PTR_NEW()} 
END

; Basic init method
function FitSubPatternObject::Init
return, 1
end

; Build from a model
function FitSubPatternObject::fromModel, model, peakprofile
self.npeaks = model->nPeaks()
self.peakprofile = peakprofile
self.ndelta = model->nDelta()
self.deltarange=PTR_NEW(intarr(self.ndelta))
for i=0,self.ndelta-1 do begin
	(*(self.deltarange))(i) = model->delta(i)
endfor
self.twotheta=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.intensity=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.hwidth=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.weightGL=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.limits=PTR_NEW(fltarr(self.ndelta,2))
self.bgcoefs=PTR_NEW(fltarr(self.ndelta,2))
self.peakmodel = PTR_NEW(OBJARR(self.npeaks))
for i=0,self.npeaks-1 do begin
	(*(self.peakmodel))(i) = OBJ_NEW('PeakModel')
	(*(self.peakmodel))(i) = model->peakmodel(i)
endfor
return, 1
end

; Build from a JCPDS guess
; We assume that the data is available over the all delta range
;	startZ: first 2theta of the subpattern
;	endZ: last 2theta of the  subpattern
;	peaksZ: array with peak positions
;	npeaksZ: number of peaks
;	width: 2theta width used to define zones, half-width of peaks will be assume width/4
;	delta: list of azimuth angles over which the fit will be performed
;	twotheta: array with 2theta values
;	intensity: array with diffraction intensities for first azimuth
function FitSubPatternObject::fromJCPDS, startZ, endZ, peaksZ, npeaksZ, width, peakprofile, delta, twotheta, intensity
self.npeaks = npeaksZ
self.peakprofile = peakprofile
self.ndelta = N_ELEMENTS(delta)
self.deltarange=PTR_NEW(intarr(self.ndelta))
for i=0,self.ndelta-1 do begin
	(*(self.deltarange))(i) = delta[i]
endfor
self.twotheta=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.intensity=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.hwidth=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.weightGL=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.limits=PTR_NEW(fltarr(self.ndelta,2))
self.bgcoefs=PTR_NEW(fltarr(self.ndelta,2))
self.peakmodel = PTR_NEW(OBJARR(self.npeaks))
minI = min(intensity)
for i=0,self.npeaks-1 do begin
	peakposition = peaksZ[i]
	index = where(twotheta gt peaksZ[i])
	if (N_ELEMENTS(index) eq 1) then if (index eq -1) then return, "Some peaks are not in 2theta range!"
	(*self.twotheta)[i,0] = peakposition
	(*self.intensity)[i,0] = intensity[index[0]] -minI
	(*self.hwidth)[i,0] = width/4.
	(*self.weightGL)[i,0] = 0.5
endfor
return, 1
end

;Build from a .dat file 
;reads and assigns the values from a .dat file to a subpattern object 
;most of it is copied/adapted from customized read ascii in subpatternmodel.pro
;added 13/05/2013 N Hilairet 
function FitSubPatternObject::fromDat, log, filename, peakprofile
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
; Let's make sure we do have data for more than a few orientations...
if (count lt 5) then return, "You do not have enough data in there! I found" + STRING(count,/print) + " datapoints. You need at least 5."
; Azimuth angles are converted to INTEGERS! This is critical for comparisons later.
self.deltarange=PTR_NEW(intarr(count))
*(self.deltarange)=fix(azimuth(0:count-1))
; end of copy from subpatternmodel.pro

self.ndelta = count     
self.peakprofile = peakprofile

self.twotheta=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.intensity=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.hwidth=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.weightGL=PTR_NEW(fltarr(self.npeaks,self.ndelta))
self.limits=PTR_NEW(fltarr(self.ndelta,2))
self.bgcoefs=PTR_NEW(fltarr(self.ndelta,2))

; passing values
; NB: this function is intended for building .fit files suitables for polydefix, which does not need weightGL, 
; limits and bgcoeffs so dummy values (1's) are assigned
for i=0,self.npeaks-1 do begin
  (*self.twotheta)[i,*] = twotheta[i,0:count-1]
  (*self.intensity)[i,*] = intensity[i,0:count-1]
  (*self.hwidth)[i,*] = hwidth[i,0:count-1]
  (*self.weightGL)=replicate(1,self.npeaks,self.ndelta)
  (*self.limits)=replicate(1,self.ndelta,2)
  (*self.bgcoefs)=replicate(1,self.ndelta,2)
endfor

return, 1
end

; Cleanup method
pro FitSubPatternObject::Cleanup
end

function FitSubPatternObject::nPeaks
return, self.npeaks
end

function FitSubPatternObject::getDelta, i
return, (*self.deltarange)
end

function FitSubPatternObject::getIntensity, i
return, (*self.intensity)(i,*)
end

function FitSubPatternObject::getTheta, i
return, (*self.twotheta)(i,*)
end

function FitSubPatternObject::getHalfWidth, i
return, (*self.hwidth)(i,*)
end

function FitSubPatternObject::optimizeOneDeltaWithCurrentDataset, loop, i, plotlevel
; This is the raw data
common rawdata, nalpha, ntheta, alpha, twotheta, data
; finding index in the dataset
indexdelta = WHERE(alpha  EQ (*self.deltarange)(i))
; starting values
fit = fltarr(self.npeaks, 4)
; print, "Starting value"
for peak = 0, self.npeaks -1 do begin
  ; print, "Peak ", peak
  ; print, "Starting value",  fit[peak,*]
  ; help, (*(self.peakmodel)) 
  ; help, (*(self.peakmodel))[peak]
	fit[peak,0] = (*(self.peakmodel))[peak]->twotetha((*self.deltarange)(i))
	; print, fit[peak,0]
	; print, "Intensity", (*(self.peakmodel))[peak]->intensity((*self.deltarange)(i))
	fit[peak,1] = (*(self.peakmodel))[peak]->intensity((*self.deltarange)(i))
  ; print, fit[peak,1]
	fit[peak,2] = (*(self.peakmodel))[peak]->hwidth((*self.deltarange)(i))
  ; print, fit[peak,2]
	; print, fit[peak,*]
	if (self.peakprofile eq 1) then fit[peak,3] = minmaxval(0,1,(*(self.peakmodel))(peak)->weightGL((*self.deltarange)(i))) ; Weight between gaussian and Lorentzian
	; print, fit[peak,*]
endfor
; temporary values of intensity vs 2theta restricted to the right interval
; We use the largest peak width as a reference, and put a minimum of 
; 0.05 degrees for peak width
width = max(fit(*,2))
if (width lt 0.05) then width = 0.05
twothetal = min(fit(*,0))- self.widthfactor *width
twothetar = max(fit(*,0))+ self.widthfactor *width
tmp = WHERE(twotheta  GT twothetal, count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT twothetar, count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR = tmp[count-1]
; print, "We want two theta from " + STRING(twothetal,/PRINT) + " to " + STRING(twothetar,/PRINT) 
; print, "We find index " + STRING(indexl,/PRINT)+ " to " + STRING(indexr,/PRINT)+ " out of " + STRING(N_ELEMENTS(twotheta),/PRINT)
xtemp = twotheta(indexL:indexR)
ytemp = data(indexdelta,indexL:indexR)
(*self.limits)(i,0) = twotheta(indexL)
(*self.limits)(i,1) = twotheta(indexR)
; We are ready to fit: optimizing the parameters in the array 'fit'... 
; For this, we call a new function, fitMultiplePeaks
bgcoefs = fltarr(2)
fitMultiplePeaks, xtemp, ytemp, self.npeaks, fit, bgcoefs, self.peakprofile, loop, (*self.deltarange)(i), plotlevel
(*self.bgcoefs)(i,0) = bgcoefs(0)
(*self.bgcoefs)(i,1) = bgcoefs(1)
; print, "Ending value"
for peak = 0, self.npeaks -1 do begin
	(*self.twotheta)(peak,i) = fit(peak,0)
	(*self.intensity)(peak,i) = fit(peak,1) 
	(*self.hwidth)(peak,i) = fit(peak,2)
	(*self.weightGL)(peak,i) = fit(peak,3)
	; print, fit(peak,*)
endfor
return, 1
end

; We use the current dataset in the 'rawdata' common block to optimize the fit.
; What we will do is use the model as starting value, then 
;   - we optimize the fit for each value of alpha with the fitnGauss routine and a miminum
;       number of loops 
;   - we then fit a model (using Fourier transforms) to smooth the results
;   - do it over and over until we are happy
; + send a WIDGET_TEXT 'log' to log the state of the fit...
; + Send a plotlevel:
;     0: plot only once: fit for all values of azimuth during the last loop only
;     1: plot once per loop: fit for all values of azimuth at all loops 
;     2: verbose plot: plots everything, including intermediary fits.
; + Send a width factor: this number is used to define the width of the region 
;     to fit. Default value is 4.0. If you find that the width of the region 
;     where we do the fit is too narrow, make this value larger...
; + Send sub-pattern number (for display only)
pro FitSubPatternObject::optimizeWithCurrentDataset, log, plotlevel, wfactor, subpatn
; Info about the active dataset (file name)
common inputfiles, inputfiles, activeset
common fitoptions, basescale, smallDetection, nLoop, startSmall, endSmall
; Setting fit options for the fitting routine
basescale = 3.0
smalldetection = 100
nLoop = 3
startSmall = 0
endSmall = 100
; Setting variables that will be used during the fit...
self.widthfactor = wfactor
filename = inputfiles(activeset)
nloops = 2-1
for i=0, nloops do begin
	txt = filename + ', sub-pat. ' + strtrim(string(subpatn, /print),2) + ', loop ' + strtrim(string(i, /print),2) 
	WIDGET_CONTROL, log, SET_VALUE=txt, /APPEND
	if ((i eq nloops) and (plotlevel eq 0)) then plotlevel = 1
	; We optimize the fit for each value of azimuth
	for j=0, self.ndelta-1 do begin
		;txt = filename + ', sub-pat. ' + strtrim(string(subpatn, /print),2) + ', loop ' + strtrim(string(i, /print),2) + ', az. n. ' + strtrim(string(j, /print),2)
		;WIDGET_CONTROL, log, SET_VALUE=txt, /APPEND
		test = self->optimizeOneDeltaWithCurrentDataset(i, j, plotlevel)
	end
	; fit a new model
	for peak = 0, self.npeaks -1 do begin
		test = (*(self.peakmodel))(peak)->fromDataNoLog(self.ndelta, (*(self.deltarange)), (*self.twotheta)(peak,*), (*self.intensity)(peak,*), (*self.hwidth)(peak,*))
		if (self.peakprofile eq 1) then test = (*(self.peakmodel))(peak)->fitWeightGL(self.ndelta, (*(self.deltarange)), (*self.weightGL)(peak,*))
	endfor
endfor
end

; optimize educated guess for azimuth number i
; + width: 2theta width over which to look at
; + peaksLabels: array with peak labels (for display, optional)
function FitSubPatternObject::optimizeJCPDSOneDeltaWithCurrentDataset, i, plotlevel, width, labels=labels
; This is the raw data
common rawdata, nalpha, ntheta, alpha, twotheta, data
if (keyword_set(labels)) then addlabel = 1 else addlabel = 0
; finding index in the dataset
indexdelta = WHERE(alpha  EQ (*self.deltarange)(i))
; starting values
fit = fltarr(self.npeaks, 4)
; print, "Starting value"
for peak = 0, self.npeaks -1 do begin
	fit(peak,0) = (*self.twotheta)[peak,i]
	fit(peak,1) = (*self.intensity)[peak,i]
	fit(peak,2) = (*self.hwidth)[peak,i]
	if (self.peakprofile eq 1) then fit(peak,3) = (*self.weightGL)(peak,i) ; Weight between gaussian and Lorentzian
	; print, fit(peak,*)
endfor
twothetal = min(fit(*,0))-width/2.
twothetar = max(fit(*,0))+width/2.
for kk=0, N_ELEMENTS(twotheta)-1 do begin
	if (twotheta[kk] gt twothetal) then break
endfor
if (kk eq N_ELEMENTS(twotheta)-1) then indexL = 0 else indexL = kk
for kk=0, N_ELEMENTS(twotheta)-1 do begin
	if (twotheta[kk] gt twothetar) then break
endfor
indexR = kk
; print, "We want two theta from " + STRING(twothetal,/PRINT) + " to " + STRING(twothetar,/PRINT) 
; print, "We find index " + STRING(indexl,/PRINT)+ " to " + STRING(indexr,/PRINT)+ " out of " + STRING(N_ELEMENTS(twotheta),/PRINT)
xtemp = twotheta(indexL:indexR)
ytemp = data(indexdelta,indexL:indexR)
(*self.limits)(i,0) = twotheta(indexL)
(*self.limits)(i,1) = twotheta(indexR)
; We are ready to fit: optimizing the parameters in the array 'fit'... 
; For this, we call a new function, fitMultiplePeaks
bgcoefs = fltarr(2)
; loop is 0 in this case
loop = 0
if (addlabel eq 0) then $
	fitMultiplePeaks, xtemp, ytemp, self.npeaks, fit, bgcoefs, self.peakprofile, loop, (*self.deltarange)(i), plotlevel $
else $
	fitMultiplePeaks, xtemp, ytemp, self.npeaks, fit, bgcoefs, self.peakprofile, loop, (*self.deltarange)(i), plotlevel, extralabels=labels
(*self.bgcoefs)(i,0) = bgcoefs(0)
(*self.bgcoefs)(i,1) = bgcoefs(1)
; print, "Ending value"
for peak = 0, self.npeaks -1 do begin
	(*self.twotheta)(peak,i) = fit(peak,0)
	(*self.intensity)(peak,i) = fit(peak,1) 
	(*self.hwidth)(peak,i) = fit(peak,2)
	(*self.weightGL)(peak,i) = fit(peak,3)
	; print, fit(peak,*)
endfor
return, 1
end


; We use the current dataset in the 'rawdata' common block to optimize the fit.
; What we will do is use the model as starting value, then 
;   - we optimize the fit for the first value of azimuth with the fitnGauss routine 
;    and a miminum number of loops using the JCPDS guesses
; + Send a plotlevel:
;     0: plot only once: fit for all values of azimuth during the last loop only
;     1: plot once per loop: fit for all values of azimuth at all loops 
;     2: verbose plot: plots everything, including intermediary fits
; + widthregion: 2theta width over which to look at
; + wfactor: scale for base region of peaks, in multiples of peak width

pro FitSubPatternObject::optimizeJCPDSWithCurrentDatasetFirstDelta, plotlevel, widthregion, wfactor
; Info about the active dataset (file name)
common inputfiles, inputfiles, activeset
common fitoptions, basescale, smallDetection, nLoop, startSmall, endSmall
; Setting fit options for the fitting routine
basescale = wfactor
smalldetection = 100
nLoop = 3
startSmall = 0
endSmall = 100
filename = inputfiles(activeset)
; Only one loop, therefore we plot at the end of this loop and plotlevel is 1
; plot level 0 is used for multiple loops
if (plotlevel eq 0) then plotlevel = 1
; We optimize the fit for each value of azimuth
test = self->optimizeJCPDSOneDeltaWithCurrentDataset(0, plotlevel,widthregion)
end

; We use the current dataset in the 'rawdata' common block to optimize the fit.
; What we will do is use the model as starting value, then 
;   - we optimize the fit for all azimuth with the fitnGauss routine 
;    and a miminum number of loops using the JCPDS guesses
; + Send a plotlevel:
;     0: plot only once: fit for all values of azimuth during the last loop only
;     1: plot once per loop: fit for all values of azimuth at all loops 
;     2: verbose plot: plots everything, including intermediary fits.
; + widthregion: 2theta width over which to look at
; + wfactor: scale for base region of peaks, in multiples of peak width
; + peaksLabels: array with peak labels (for display)

pro FitSubPatternObject::optimizeJCPDSWithCurrentDataset, plotlevel, widthregion, wfactor, peaklabels
; Info about the active dataset (file name)
common inputfiles, inputfiles, activeset
common fitoptions, basescale, smallDetection, nLoop, startSmall, endSmall
; Setting fit options for the fitting routine
basescale = wfactor
smalldetection = 100
nLoop = 3
startSmall = 0
endSmall = 100
filename = inputfiles(activeset)
; Only one loop, therefore we plot at the end of this loop and plotlevel is 1
; plot level 0 is used for multiple loops
if (plotlevel eq 0) then plotlevel = 1
; We optimize the fit for each value of azimuth
labels = strarr(self.npeaks)
for peak = 0, self.npeaks -1 do labels[peak] = peaklabels[peak]
for j=0, self.ndelta-1 do begin
	if (j gt 0) then begin
		; if not on first image, use result of previous fit as guess for the next one
		for peak = 0, self.npeaks -1 do begin
			(*self.twotheta)(peak,j) = (*self.twotheta)(peak,j-1)
			(*self.intensity)(peak,j) = (*self.intensity)(peak,j-1)
			(*self.hwidth)(peak,j) = (*self.hwidth)(peak,j-1)
			(*self.weightGL)(peak,j) = (*self.weightGL)(peak,j-1)
		endfor
	endif
	test = self->optimizeJCPDSOneDeltaWithCurrentDataset(j, plotlevel,widthregion,labels=labels)
endfor
end





; Build a synthethic dataset from the fit we have
; Send
;    + value of azimtuh
;    + twotheta array
;    + number of two theta angles
; If we have no fit for this value of alpha, we just send back an array of 0's
; Otherwise, array of numbers
function FitSubPatternObject::syntheticdata, alpha,twotheta, ntheta
spectrum = fltarr(ntheta)
; Find the index for azimuth
tmp = WHERE((*(self.deltarange)) EQ alpha, count)
if (count lt 1) then return, spectrum ; No corresponding alpha
indexalpha = tmp[0]
; where are we in 2theta?
tmp = WHERE (twotheta GT (*self.limits)(indexalpha,0), count) 
if (count lt 1) then return, spectrum ; No corresponding 2theta
tmp = WHERE (twotheta LT (*self.limits)(indexalpha,1), count) 
if (count lt 1) then return, spectrum ; No corresponding 2theta
tmp = WHERE(twotheta GT (*self.limits)(indexalpha,0), count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT (*self.limits)(indexalpha,1), count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR = tmp[count-1]
; Build a model
; background
spectrum(indexL:indexR) = spectrum(indexL:indexR) + (*self.bgcoefs)(indexalpha,0) + (*self.bgcoefs)(indexalpha,1)*twotheta(indexL:indexR)
; peaks
for peak=0, self.nPeaks-1 do begin
	inten = (*self.intensity)(peak,indexalpha)
	pos = (*self.twotheta)(peak,indexalpha)
	hwidth = (*self.hwidth)(peak,indexalpha)
	if (self.peakprofile eq 2) then begin
		weight = 0.0 ; Lorentz
	endif else if (self.peakprofile eq 1) then begin
		weight = (*self.weightGL)(peak,indexalpha) ; Voigt
	endif else begin
		weight = 1.0 ; Gauss
	end
	u = ((twotheta(indexL:indexR)-pos)/hwidth)^2
	lorentz =  inten / (u + 1)
	gauss = inten * exp (- alog(2) * u)
	spectrum(indexL:indexR) = spectrum(indexL:indexR) + weight*gauss + (1-weight)*lorentz
endfor
return, spectrum
end
 
; Saves fit to Ascii for later processing
function FitSubPatternObject::saveToAscii, lun
	printf, lun, '# Number of peaks'
	printf, lun, STRING(self.nPeaks, /PRINT)
	printf, lun, '# Peak profile'
	printf, lun, STRING(self.peakprofile, /PRINT)
	printf, lun, '# Width factor'
	printf, lun, STRING(self.widthfactor, /PRINT)
	printf, lun, '# Range in azimuth: nvalues, and values'
	printf, lun, STRING(self.ndelta, /PRINT)
	for i=0, self.ndelta-1 do begin
		printf, lun, STRING((*(self.deltarange))(i), /PRINT)
	endfor
	printf, lun, '# background positions, in 2 theta (left/right)'
	for i=0, self.ndelta-1 do begin
		printf, lun, STRING((*self.limits)(i,0), /PRINT) + ' ' + STRING((*self.limits)(i,1), /PRINT)
	endfor
	printf, lun, '# background coefficients'
	for i=0, self.ndelta-1 do begin
		printf, lun, STRING((*self.bgcoefs)(i,0), /PRINT) + ' ' + STRING((*self.bgcoefs)(i,1), /PRINT)
	endfor
	printf, lun, '# Peak positions (2 theta), intensity, half-width, weight Gauss/Lorentz (for pseudo-voigt), '
	for peak=0, self.nPeaks-1 do begin
		printf, lun, '# peak number ' + STRING(peak, /PRINT)
		for i=0, self.ndelta-1 do begin
			printf, lun, STRING((*self.twotheta)(peak,i), /PRINT) + ' ' + STRING((*self.intensity)(peak,i), /PRINT) + ' ' + STRING((*self.hwidth)(peak,i), /PRINT) + ' ' + STRING((*self.weightGL)(peak,i), /PRINT) 
		endfor
	endfor
RETURN, 1
end


; Read previous fit from Ascii file
function FitSubPatternObject::readFromascii, lun
	on_ioerror, bad
	; Number of peaks
	row = readascii(lun,com='#')
	self.nPeaks = fix(row)
	; Peak profile
	self.peakprofile = fix(readascii(lun, com='#'))
	; Width factor 
	self.widthfactor = float(readascii(lun,com='#'))
	; Azimuth angles
	self.ndelta = fix(readascii(lun, com='#'))
	;print, 'Npeaks = ' + STRING(self.nPeaks, /PRINT)
	;print, 'Ndelta = ' + STRING(self.ndelta, /PRINT)
	self.deltarange=PTR_NEW(intarr(self.ndelta))
	for i=0, self.ndelta-1 do (*self.deltarange)(i) = fix(readascii(lun, com='#'))
	; Setting up arrays
	self.twotheta=PTR_NEW(fltarr(self.npeaks,self.ndelta))
	self.intensity=PTR_NEW(fltarr(self.npeaks,self.ndelta))
	self.hwidth=PTR_NEW(fltarr(self.npeaks,self.ndelta))
	self.weightGL=PTR_NEW(fltarr(self.npeaks,self.ndelta))
	self.limits=PTR_NEW(fltarr(self.ndelta,2))
	self.bgcoefs=PTR_NEW(fltarr(self.ndelta,2))
	self.peakmodel = PTR_NEW(OBJARR(self.npeaks))
	for i=0,self.npeaks-1 do begin
		(*(self.peakmodel))(i) = OBJ_NEW('PeakModel')
		;print, 'peakmodel(' + STRING(i, /PRINT) + ') = ' + STRING((*(self.peakmodel))(i), /PRINT)
	endfor
	; Background sides
	for i=0, self.ndelta-1 do begin
		row = strsplit(readascii(lun, com='#'), /extract)
		(*self.limits)(i,0) = float(row[0])
		(*self.limits)(i,1) = float(row[1])
	endfor
	; Background coefficients
	for i=0, self.ndelta-1 do begin
		row = strsplit(readascii(lun, com='#'), /extract)
		(*self.bgcoefs)(i,0) = float(row[0])
		(*self.bgcoefs)(i,1) = float(row[1])
	endfor
	; Peak positions, intensity, half-width, weight Gauss/Lorentz
	for peak=0, self.nPeaks-1 do begin
		for i=0, self.ndelta-1 do begin
			row = strsplit(readascii(lun, com='#'), /extract)
			(*self.twotheta)(peak,i) = float(row[0])
			(*self.intensity)(peak,i) = float(row[1])
			(*self.hwidth)(peak,i) = float(row[2])
			(*self.weightGL)(peak,i) = float(row[3])
		endfor
	endfor
	; build a new fit model from the data in the file
	for peak = 0, self.npeaks -1 do begin
		test = (*(self.peakmodel))(peak)->fromDataNoLog(self.ndelta, (*(self.deltarange)), (*self.twotheta)(peak,*), (*self.intensity)(peak,*), (*self.hwidth)(peak,*))
		if (self.peakprofile eq 1) then test = (*(self.peakmodel))(peak)->fitWeightGL(self.ndelta, (*(self.deltarange)), (*self.weightGL)(peak,*))
	endfor
	return, 1
bad: return, !ERR_STRING
end