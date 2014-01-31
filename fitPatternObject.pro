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

PRO FitPatternObject__DEFINE 
	struct = { FitPatternObject, nsubpat:0, peakprofile: 0, subpatterns: PTR_NEW() } 
END

; basic init method
function FitPatternObject::Init
return, 1
end

; build from a model
function FitPatternObject::fromModel, model
self.nsubpat = model->nPat()
self.peakprofile = model->peakProfile()
self.subpatterns = PTR_NEW(OBJARR(self.nsubpat))
for i=0, self.nsubpat-1 do begin
	(*(self.subpatterns))(i) = OBJ_NEW('FitSubPatternObject')
	test = (*(self.subpatterns))(i)->fromModel((model->subPattern(i)), self.peakprofile)
endfor
return, 1
end


; build from a JCPDS guess
; We assume that the data is available over the all delta range
; we assume that peak intensity is the intensity at where we find the peak
;	nzones: number of subpatterns
;	startZ: array with first 2theta of each subpattern
;	endZ: array with last 2theta of each subpattern
;	peaksZ: 2-Darray with peak positions for each subpattern
;	npeaksZ: array with number of peaks for each subpattern
;	width: 2theta width used to define zones, half-width of peaks will be assume width/4
;	delta: list of azimuth angles over which the fit will be performed
;	twotheta: array with 2theta values
;	intensity: array with diffraction intensities for first azimuth
function FitPatternObject::fromJCPDS, nzones, startZ, endZ, peaksZ, npeaksZ, width, delta, twotheta, intensity
self.nsubpat = nzones
self.peakprofile = 1 ; assume pseudo-voigt
self.subpatterns = PTR_NEW(OBJARR(self.nsubpat))
for i=0, self.nsubpat-1 do begin
	(*(self.subpatterns))(i) = OBJ_NEW('FitSubPatternObject')
	test = (*(self.subpatterns))(i)->fromJCPDS(startZ[i], endZ[i], peaksZ[i,*], npeaksZ[i], width, self.peakprofile, delta, twotheta, intensity)
	if (test ne 1) then return, test
endfor
return, 1
end

;Build from chosen .dat files (1 dat file = 1 subpattern) for direct use into polydefix
;assumes gaussian peak profile since polydefix does not take into account the peak profiles 
;added 13th may 2013 N. Hilairet
function FitPatternObject::fromDat, log, ninputfiles, outputdirectory, inputfiles       
self.nsubpat = ninputfiles
self.peakprofile = 0    ; assume gaussian 
self.subpatterns = PTR_NEW(OBJARR(self.nsubpat))
for i=0, self.nsubpat-1, 1 do begin
  filename = outputdirectory + inputfiles(i)
    print, 'working on inputfile', filename
  (*(self.subpatterns))(i) = OBJ_NEW('FitSubPatternObject')
  test = (*(self.subpatterns))(i)->fromDat(log, filename, self.peakprofile)
  if (test ne 1) then return, test
endfor
return, 1
end

; Cleanup method
pro FitPatternObject::Cleanup
end

; We use the current dataset in the '' common block to optimize the fit.
; this is done for each sub-pattern one after the other.
; + send a WIDGET_TEXT 'log' to log the state of the fit...
; + Send a plotlevel:
;     0: plot only once: fit for all values of azimuth during the last loop only
;     1: plot once per loop: fit for all values of azimuth at all loops 
;     2: verbose plot: plots everything, including intermediary fits.
; + Send a width factor: this number is used to define the width of the region 
;     to fit. Default value is 4.0. If you find that the width of the region 
;     where we do the fit is too narrow, make this value larger...
pro FitPatternObject::optimizeWithCurrentDataset, log, plotlevel, widthfactor
for i=0, self.nsubpat-1 do begin
	thisplotlevel = plotlevel
	; print, "Here, working on nsubpattern ", i
	(*(self.subpatterns))(i)->optimizeWithCurrentDataset, log, thisplotlevel, widthfactor, (i+1)
endfor
end

; We use the current dataset in the '' common block to optimize the fit.
; this is done for each sub-pattern one after the other.
; This will optimize the guess from JCPDS on the first azimuth
; + widthregion: 2theta width over which to look at
; + wfactor: scale for base region of peaks, in multiples of peak width
pro FitPatternObject::optimizeJCPDSWithCurrentDatasetFirstDelta, widthregion, wfactor
for i=0, self.nsubpat-1 do begin
	thisplotlevel = 0
	(*(self.subpatterns))(i)->optimizeJCPDSWithCurrentDatasetFirstDelta, thisplotlevel, widthregion, wfactor
endfor
end

; We use the current dataset in the '' common block to optimize the fit.
; this is done for each sub-pattern one after the other.
; This will optimize the guess from JCPDS for all azimuth
; + widthregion: 2theta width over which to look at
; + wfactor: scale for base region of peaks, in multiples of peak width
; + peaksLabels: array with peak labels (for display)
pro FitPatternObject::optimizeJCPDSWithCurrentDataset, widthregion, wfactor, peakLabels
for i=0, self.nsubpat-1 do begin
	thisplotlevel = 0
	(*(self.subpatterns))(i)->optimizeJCPDSWithCurrentDataset, thisplotlevel, widthregion, wfactor, peakLabels(i,*)
endfor
end

; Use the fit results to build a synthetic dataset.
; send 
;       + the array of azimuth angles
;       + the array of 2theta
;       + the number of azimuth angles
;       + the number of 2theta values
; If the azimuth angles are different that those in the fit, it will fail!
function FitPatternObject::buildSynthethicData, alpha, twotheta, nalpha, ntheta
builtdata = fltarr(nalpha,ntheta)
for i=0, nalpha-1 do begin
	for j=0, self.nsubpat-1 do begin
		builtdata(i,*) = builtdata(i,*) + (*(self.subpatterns))(j)->syntheticdata(alpha(i),twotheta,ntheta)
	endfor
endfor
return, builtdata
end

; Returns a list of fitted peaks, in a list of strings
; Name is the subpattern number followed by the peak number
function FitPatternObject::listPeaksByName
npeaks = 0
for i=0, self.nsubpat-1 do begin
	npeaks = npeaks + (*self.subpatterns)(i)->nPeaks()
endfor
peakname = strarr(npeaks)
n = 0
for i=0, self.nsubpat-1 do begin
	npeaks = (*self.subpatterns)(i)->nPeaks()
	for j=0, npeaks-1 do begin
		peakname[n] = "Subpat. " + strtrim(string(i+1,/print),2) + ", peak " + strtrim(string(j+1,/print),2)
		n = n + 1
	endfor
endfor
return, peakname
end

function FitPatternObject::findPeak, i
npeaks = fltarr(self.nsubpat)
for j=0, self.nsubpat-1 do begin
	npeaks[j] = (*self.subpatterns)(j)->nPeaks()
endfor
total = 0
subpat = -1
while (total lt i+1) do begin
	subpat = subpat + 1
	total = total + npeaks[subpat]
endwhile
index = fix(i-total+npeaks[subpat])
return, [subpat,index]
end

; Returns azimtuh angles for peak number i
function FitPatternObject::getDelta, i
index = self->findPeak(i)
return, (*self.subpatterns)(index[0])->getDelta(index[1])
end

; Returns intensities of peak number i
function FitPatternObject::getIntensity, i
index = self->findPeak(i)
return, (*self.subpatterns)(index[0])->getIntensity(index[1])
end

; Returns 2theta of peak number i
function FitPatternObject::getTheta, i
index = self->findPeak(i)
return, (*self.subpatterns)(index[0])->getTheta(index[1])
end

; Returns half-width of peak number i
function FitPatternObject::getHalfWidth, i
index = self->findPeak(i)
return, (*self.subpatterns)(index[0])->getHalfWidth(index[1])
end

; Save fit in Ascii file for later processing
function FitPatternObject::saveToAscii, lun
	printf, lun, '# Fit pattern file'
	printf, lun, '# File version'
	printf, lun, '1'
	printf, lun, '# Peak profile (0: gauss, 1: pseudo-voigt, 2: lorentz)'
	printf, lun, STRING(self.peakprofile, /PRINT)
	printf, lun, '# Number of sub-patterns'
	printf, lun, STRING(self.nsubpat, /PRINT)
	for i=0, self.nsubpat-1 do begin
		printf, lun, '# Sub-patterns ' + STRING(i, /PRINT)
		noerror = (*self.subpatterns)(i)->saveToAscii(lun)
		if (noerror ne 1) then return, noerror
	endfor
RETURN, 1
end


; Loads results of a fit from an Ascii file
function FitPatternObject::readFromAscii, lun
	on_ioerror, bad
    messages = "Loading fit data. Please wait..."
	progressBar = Obj_New("SHOWPROGRESS", message=messages)
	progressBar->Start
	; file version
	row = readascii(lun,com='#')
	if (fix(row) ne 1) then return, 'Sorry, we can only read file format 1 at this time'
	; peak profile
	row = readascii(lun,com='#')
	self.peakprofile = fix(row)
	; number of subpatterns
	row = readascii(lun,com='#')
	self.nsubpat = fix(row)
	self.subpatterns = PTR_NEW(OBJARR(self.nsubpat))
	; subpatterns
	for i=0, self.nsubpat-1 do begin
		;print, 'Reading subpattern' + STRING(i, /PRINT)
		(*(self.subpatterns))(i) = OBJ_NEW('FitSubPatternObject')
		noerror = (*self.subpatterns)(i)->readFromAscii(lun)
		percent = 100.*i/(self.nsubpat-1)
		progressBar->Update, percent
		if (noerror ne 1) then begin
			progressBar->Destroy
			Obj_Destroy, progressBar
			return, noerror
		endif
	endfor
	progressBar->Destroy
	Obj_Destroy, progressBar
RETURN, 1
bad:
progressBar->Destroy
Obj_Destroy, progressBar
return, !ERR_STRING
end