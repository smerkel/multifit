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

; *******************************************************************
; subroutine fitMultiplePeaks
; 
; Create 06/2005 by S. Merkel, starting from his old routine, gaussNpeaksbg
;   it was becoming to messy (to many options) so I decided to start a new 
;   one...
;
; Fits N gauss, lorentz or pseudo-voigt peaks to a given x-y data set 
; using the mpfit subroutine and a linear background. The background is 
; taken with the 10 first datapoints on each side...
; 
; send x, y, npeaks, fit
; Fit parameters will be returned in the array fit as follow
;   fit(i,0) = position for peak i
;   fit(i,1) = intensity for peak i
;   fit(i,2) = half width for peak i
;
; parameters:
;  x: two theta, in general
;  y: corresponding intensities
;  npeaks: number of peaks
;  fit: fit parameters (see above for format) of the previous fits or
;     array of zeros
;  bgcoefs: variables to save background in: bg = bgcoefs(0) + bgcoefs(1)*twotheta
;  peakmodel: = 1 -> pseudo-voigt peak profile
;     = 2 -> lorentzian peak profile
;     else gaussian peak profile
; loop: loop number (for display only)
; azimuth: azimuth value (for display only)
; plotlevel: 0- no plot, 1- only once (at the end), 2- verbose!
; extralabels: array of string with extra labels to add on plots
; *******************************************************************


pro fitMultiplePeaks, x, y, npeaks, fit, bgcoefs, peakmodel, loop, azimuth, plotlevel, extralabels = extralabels
common fitoptions, basescale, smallDetection, nLoop, startSmall, endSmall

if (keyword_set(extralabels)) then addlabel = 1 else addlabel = 0

; peak profile setup
; default, gaussian peak profile
; This variable is used to set the number of terms to fit (different with
; different peak profiles)
shiftNterms  = 0
; restriction of region to fit for an individual peak
; basescaling defines the region to include around the peak position
; in the fit (baseScaling*peakwidth). The rest of the spectrum is
; simply ignored.
; default, gaussian peak profile
baseScaling = 1.0*basescale
; if lorentzian peak profile
if (peakmodel eq 2) then begin
; lorentzians have no extra term, and
; they are pretty wide at the base, so we give them more space for the
; fit
    shiftNterms = 0
    baseScaling = 1.0*basescale
endif
; if pseudo-Voigt peak profile
if (peakmodel eq 1) then begin
; there is one more term with a pseudo-voigt function (weigth between
; Gauss and Lorentz)
; they are pretty wide at the base, so we give them more space for the
; fit
    shiftNterms = 1
    baseScaling = 1.0*basescale
endif

;
; Adding datapoints in x, if necessary: if the data is too restricted, 
; we will not be able to fit all parameters. So, if necessary, we add new 
; points on the way by simple linear interpolation!
;
ylength =  N_ELEMENTS(y)
minScale = 60
scaling = 1
if (ylength lt (minScale+npeaks*minScale)) then begin
    scaling = fix((minScale+npeaks*minScale)/ylength)
endif
scaledylength = ylength+(ylength-1)*(scaling-1)
scaledx = fltarr(scaledylength)
scaledy = fltarr(scaledylength)
for i=0, scaledylength-2 do begin
    indexInf = fix(i/scaling)
    indexSup = fix(i/scaling)+1
    scaledy(i) = 1.0*(y(indexInf)+1.0*(1.0*i-scaling*indexInf)* $
                      (y(indexSup)-y(indexInf))/scaling)
    scaledx(i) = 1.0*(x(indexInf)+1.0*(1.0*i-scaling*indexInf)* $
                      (x(indexSup)-x(indexInf))/scaling)
endfor
scaledy(scaledylength-1) = y(ylength-1)
scaledx(scaledylength-1) = x(ylength-1)
oldx = x
oldy = y
y = scaledy
x = scaledx
ylength = scaledylength
;
; end scaling
;

; Setting up various variables and pointers...
indexthetaL = intarr(npeaks)    ; index of left side of data interval for each peak
indexthetaR = intarr(npeaks)    ; index of right side of data interval for each peak
yfit = ptrarr(npeaks)      ; will contain the fitted profile, for each peak
estimates = ptrarr(npeaks) ; estimates before the fit, for each peak
coeffs = ptrarr(npeaks)    ; results of the fit, for each peak
xtmp = ptrarr(npeaks)      ; temporary data to fit - x-axis, for each peak
ytmp = ptrarr(npeaks)      ; temporary data to fit - y-axis, for each peak

; parameters used in plots:
minX = min(x)
maxX = max(x)
minY = 0
maxY = max(y) 

; fitting the background: straigh line between the sides of the plot!
bgdegree = 1
bg = fltarr(n_elements(x))
fitBgX = fltarr(20)
fitBgY = fltarr(20)
for j = 0, 9 do begin
	fitBgX(j) = x(j)
	fitbgX(10+j) = x(N_ELEMENTS(x)-1-j)
	fitBgY(j) = y(j)
	fitbgY(10+j) = y(N_ELEMENTS(x)-1-j)
endfor
thisbgCoefs = POLY_FIT(fitBgX,fitBgY,bgdegree)
for deg=0, bgdegree do begin
	bgcoefs(deg) = thisbgCoefs(deg)
	bg = bg + bgcoefs(deg)*x^deg
endfor
if (plotlevel gt 1) then begin
	plot, x, y, background=255, color = 0, xrange=[minX,maxX], yrange=[minY,maxY], ystyle=1, xstyle=1
	oplot, x, bg, color=100
	xleg = maxX-0.3*(maxX-minX)
	yleg1 = maxY-0.07*(maxY-minY)
	yleg2 = maxY-0.14*(maxY-minY)
	yleg3 = maxY-0.21*(maxY-minY)
	xyouts, xleg, yleg1, "az = "+STRING(azimuth,format='(F6.1)'),  color = 0, charsize=2, charthick=3, font=1
	xyouts, xleg, yleg2, "Loop "+STRING(loop,format='(I3)'),  color = 0, charsize=2, charthick=3, font=1
	xyouts, xleg, yleg3, "Background ", color = 0, charsize=2, charthick=3, font=1
	wait, 0.3
endif
	
; Substract the background
y = y - bg
maxY = max(y)
; For plots...
xleg = maxX-0.3*(maxX-minX)
yleg1 = maxY-0.07*(maxY-minY)
yleg2 = maxY-0.14*(maxY-minY)
yleg3 = maxY-0.21*(maxY-minY)

; first loop to initialize all pointers, variables, and peak positions
; We use the results from previous fits as starting model. So we have
; to regenerate the peak profiles from their position, half-width and intensity.
for peak = 0, npeaks-1 do begin
	estimates(peak) = PTR_NEW(/ALLOCATE_HEAP)
	xtmp(peak) = PTR_NEW(/ALLOCATE_HEAP)
	ytmp(peak) = PTR_NEW(/ALLOCATE_HEAP)
	coeffs(peak) = PTR_NEW(/ALLOCATE_HEAP)
	yfit(peak) = PTR_NEW(/ALLOCATE_HEAP)
	*estimates(peak) =  fltarr(5+shiftNterms)
	(*estimates(peak))(1) = fit(peak,0) ; position
	(*estimates(peak))(0) = fit(peak,1) ; intensity
	(*estimates(peak))(2) = fit(peak,2) ; half width
	if (peakmodel eq 1) then (*estimates(peak))(3) = fit(peak,3) ; weight Gauss-Lorentz
	; Setting data to fit
	tmpy = y
	; remove the profile of the previous peaks
	for j = 0, peak-1 do begin
		tmpy(indexthetaL(j):indexthetaR(j)) = tmpy(indexthetaL(j):indexthetaR(j)) - (*yfit(j))(0:indexthetaR(j)-indexthetaL(j))
	endfor
	; we limit the zone where the peak resides
	tmp = WHERE(x  LT (fit(peak,0)-baseScaling*fit(peak,2)), count)
	if (count eq 0) then indexthetaL(peak) = 0 else indexthetaL(peak) = tmp[count-1]
	tmp = WHERE(x  GT (fit(peak,0)+baseScaling*fit(peak,2)), count)
	if (count eq 0) then indexthetaR(peak) = N_ELEMENTS(x)-1 else indexthetaR(peak) = tmp[0]
	;print, "x[0] is " + STRING(x[0],/PRINT) + ", xmax is " + STRING(x[N_ELEMENTS(x)-1],/PRINT) + " with " + STRING(N_ELEMENTS(x),/PRINT) + " elements"
	;print, "We want " + STRING((fit(peak,0)-baseScaling*fit(peak,2)),/PRINT) + " to " + STRING((fit(peak,0)+baseScaling*fit(peak,2)),/PRINT)
	;print, "And we find the following interval: " + STRING(indexthetaL(peak),/PRINT) + " to " + STRING(indexthetaR(peak),/PRINT)
	*xtmp(peak) = x(indexthetaL(peak):indexthetaR(peak))
	*ytmp(peak) = tmpy(indexthetaL(peak):indexthetaR(peak))
	; generate peak with no background
	; Gauss: y = intensity * exp (-0.5 *((x-center)/half-width)^2)
	; Lorentz: y = intensity / (1 + ((x-center)/half-width)^2)
	; Pseudo-voigt: y = a * gauss + (1-a) * lorentz
	if (peakmodel eq 2) then begin
		*yfit(peak) = fit(peak,1)/(1+((*xtmp(peak)-fit(peak,0))/fit(peak,2))^2)
	endif else if (peakmodel eq 1) then begin
		y1 = fit(peak,1)*exp(-0.5*((*xtmp(peak)-fit(peak,0))/fit(peak,2))^2)
		y2 = fit(peak,1)/(1+((*xtmp(peak)-fit(peak,0))/fit(peak,2))^2)
		*yfit(peak) = fit(peak,3)*y1 + (1-fit(peak,3))*y2
	endif else begin
		*yfit(peak) = fit(peak,1)*exp(-0.5*((*xtmp(peak)-fit(peak,0))/fit(peak,2))^2)
	endelse
	*coeffs(peak) = *estimates(peak)
	if (plotlevel gt 1) then begin
		plot, x, y, background=255, color = 0, xrange=[minX,maxX], yrange=[minY,maxY], ystyle=1, xstyle=1
		oplot, *xtmp(peak), *ytmp(peak), color = 100
		oplot, *xtmp(peak), *yfit(peak), color = 200
		xyouts, xleg, yleg1, "az = "+STRING(azimuth,format='(F6.1)'),  color = 0, charsize=2, charthick=3
		xyouts, xleg, yleg2, "Loop "+STRING(loop,format='(I3)'),  color = 0, charsize=2, charthick=3
		xyouts, xleg, yleg3, "Starting model, peak "+STRING(peak,format='(I3)'), color = 0, charsize=2, charthick=3
		wait, 0.3
	endif
endfor

; Optimization loop!
weighttmp = fltarr(N_ELEMENTS(y))
for i = 0, nLoop do begin
	for peak = 0, npeaks-1 do begin
		; we take the original data, minus the other peaks and the bg
		tmpy = y
		for j = 0, npeaks-1 do begin
			if (j ne peak) then $
				tmpy(indexthetaL(j):indexthetaR(j)) = tmpy(indexthetaL(j):indexthetaR(j)) - (*yfit(j))(0:indexthetaR(j)-indexthetaL(j))
		endfor
		; we have to apply a weight, otherwise the fit tends to explode, 
		; if the fit from other peaks is high, the weight is small, so 
		; the peaks don't shift towards each other
		weighttmp(*) = 0.0
		for j = 0, npeaks-1  do begin
			if (j ne peak) then $
				weighttmp(indexthetaL(j):indexthetaR(j)) = weighttmp(indexthetaL(j):indexthetaR(j)) + abs((*yfit(j))(0:indexthetaR(j)-indexthetaL(j)))
				; COEF TO PLAY ON! 
		endfor
		; We apply another weight to give more importance to the top of the peak..
		; -> Removed it: did not help at all!
		; Normalization...
		maxweigth = max(weighttmp)
		if (maxweigth lt 1) then maxweigth = 1
		; Coefficient to change to strength of the weighting... 
		coeffWeight = 3.
		; COEF TO PLAY ON!
		weighttmp(*) = (coeffWeight*weighttmp(*) + 0.1 * maxweigth)/coeffWeight
		; results from previous fit on this peak becomes estimation
		*estimates(peak) = *coeffs(peak)
		; avoid getting a fitting zone that is too small: put a minimum 
		; of 0.05 degrees to the peak width
		(*estimates(peak))(2) = max([0.05,(*estimates(peak))(2)])
		; COEF TO PLAY ON!
		; we limit the zone to fit the peak, so it doesn't fit everything
		; This part is tricky...
		; At first, I was restricting at baseScaling * peak width, but it
		; does not work well if one peak is much smaller that the other... 
		; So now, it is weighted by the peak intensity...
		; find Max Intensity
		if (npeaks gt 1) then begin
			maxIntensity = 0.0
			minIntensity = 100000000000
 			for j = 0, npeaks-1  do begin
				if ((*coeffs(j))(0) gt maxIntensity) then maxIntensity=(*coeffs(j))(0)
				if ((*coeffs(j))(0) lt minIntensity) then minIntensity=(*coeffs(j))(0)
			endfor
			; factor for zoneWidth is baseScaling for peak with maxIntensity, and
			; 0.5*baseScaling for lowest peak
			thisScaling = (0.5 + 0.5*((*coeffs(peak))(0)-minIntensity)/(maxIntensity-minIntensity)) * baseScaling
			; COEF TO PLAY ON!
		endif else thisScaling = baseScaling
		; we limit the zone where the peak resides
		thismintheta = (*estimates(peak))(1)-2.*thisScaling*(*estimates(peak))(2)
		thismaxtheta = (*estimates(peak))(1)+2.*thisScaling*(*estimates(peak))(2)
		; COEF TO PLAY ON!
		tmp = WHERE(x  LT thismintheta, count)
		if (count eq 0) then indexthetaL(peak) = 0 else indexthetaL(peak) = tmp[count-1]
		tmp = WHERE(x  GT thismaxtheta, count)
		if (count eq 0) then indexthetaR(peak) = N_ELEMENTS(x)-1 else indexthetaR(peak) = tmp[0]
		*xtmp(peak) = x(indexthetaL(peak):indexthetaR(peak))
		*ytmp(peak) = tmpy(indexthetaL(peak):indexthetaR(peak))
		weighttmp2 = weighttmp(indexthetaL(peak):indexthetaR(peak))
		; Fit the actual peak to the data we just defined
		parinfo = replicate({limited:[0,0], limits:[0,0]}, 3+shiftNterms)
		; force the peaks width to be positive. Forcing peak height to
		; be positive causes crashes... I can't understand why, and it's a
		; real problem
		parinfo(1).limited(0) = 1
		parinfo(1).limits(0)  = 0.0
		if (peakmodel eq 1) then begin
			nterms = (3+shiftNterms)
			*yfit(peak) = MPFITPEAK(*xtmp(peak),*ytmp(peak),(*coeffs(peak)), $
										nterms=nterms, $
										estimates=(*estimates(peak)), error = weighttmp2, $
										parinfo = parinfo, /pseudoV, STATUS=status, ERRMSG=errmsg)
		endif else if (peakmodel eq 2) then begin
			nterms = (3+shiftNterms)
			*yfit(peak) = MPFITPEAK(*xtmp(peak),*ytmp(peak),(*coeffs(peak)), $
										nterms=nterms, estimates=(*estimates(peak)), /LORENTZIAN, STATUS=status, ERRMSG=errmsg)
		endif else begin
			nterms = (3+shiftNterms)
			*yfit(peak) = MPFITPEAK(*xtmp(peak),*ytmp(peak),(*coeffs(peak)), $
										nterms=nterms, $
										estimates=(*estimates(peak)), error = weighttmp2, STATUS=status, ERRMSG=errmsg)
		endelse
		; catching an error in MPFITPEAK and throwing a full error
		if (status le 0) then begin
			MESSAGE, "Error in MPFIT function: " + errmsg
		endif
		; 
		if (plotlevel gt 1) then begin
			plot, x, y, background=255, color = 0, xrange=[minX,maxX], yrange=[minY,maxY], ystyle=1, xstyle=1
			oplot, *xtmp(peak), *ytmp(peak), color = 100
			oplot, *xtmp(peak), *yfit(peak), color = 200
			xyouts, xleg, yleg1, "az = "+STRING(azimuth,format='(F6.1)'),  color = 0, charsize=2, charthick=3
			xyouts, xleg, yleg2, "Loop "+STRING(loop,format='(I3)'),  color = 0, charsize=2, charthick=3
			xyouts, xleg, yleg3, "Fitting, peak "+STRING(peak,format='(I3)'), color = 0, charsize=2, charthick=3
			wait, 0.3
		endif
	endfor
endfor
; Fit has been optimized
; Plotting results
if (plotlevel gt 0) then begin
	minY = minY - 0.15 * maxY ; Space for residuals!
	maxY = maxY + 0.15 * maxY ; Space for legends!
	plot, x, y, background=255, color = 0, xrange=[minX,maxX], yrange=[minY,maxY], ystyle=1, xstyle=1
	xleg = maxX-0.3*(maxX-minX)
	yleg1 = maxY-0.07*(maxY-minY)
	yleg2 = maxY-0.14*(maxY-minY)
	xyouts, xleg, yleg1, "az = "+STRING(azimuth,format='(F6.1)'),  color = 0, charsize=2, charthick=3, font=1
	xyouts, xleg, yleg2, "Loop "+STRING(loop),  color = 0, charsize=2, charthick=3, font=1
	if (addlabel eq 1) then begin
		for i=0, n_elements(extralabels)-1 do begin
			yleg2 -= 0.07*(maxY-minY)
			xyouts, xleg, yleg2, extralabels[i],  color = 0, charsize=2, charthick=3, font=1
		endfor
	endif
	; Global fit
	globalfit = fltarr(N_ELEMENTS(y))
	for j = 0, npeaks-1 do globalfit(indexthetaL(j):indexthetaR(j)) = globalfit(indexthetaL(j):indexthetaR(j)) + (*yfit(j))(0:indexthetaR(j)-indexthetaL(j))
	oplot, x, globalfit, color = 100
	; Individual peaks
	for peak = 0, npeaks-1 do oplot, *xtmp(peak), *yfit(peak), color = 200
	; Residuals
	residuals = y-globalfit-.1*maxY
	oplot, x, residuals, color=50
	wait, 0.3
endif
; prepare the results to send them back...
for peak = 0, npeaks-1 do begin
	fit(peak,0) = (*coeffs(peak))(1)  ; position
	fit(peak,1) = (*coeffs(peak))(0)  ; intensity
	fit(peak,2) =  (*coeffs(peak))(2) ; half width
	if (peakmodel eq 1) then fit(peak,3) =  (*coeffs(peak))(3) ; Weight between Gaussian and Lorentzian
endfor
; Clean up memory
for peak = 0, npeaks-1 do ptr_free, estimates(peak),  xtmp(peak), ytmp(peak), coeffs(peak), yfit(peak)
; We are done!
end
