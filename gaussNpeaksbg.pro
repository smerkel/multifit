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
; subroutine GAUSSNPEAKSBG
;
; Modified on 12/20/2001 by S. Merkel
;    Try to zoom in when there are less than minScale+npeaks*minScale points.
;    I just multiply the number of points in x by a number
;    and interpolate y in between known values.
;    This is done because the fit is unstable otherwise, and it
;    tends to make it faster.
; Modified on 12/21/2001 by S. Merkel
;    Added the possibility to change peak profile.
;    If parameter peakmodel = 1 -> pseudo-voigt
;    if parameter peakmodel = 2 -> lorentzian
;       else gaussian.
; Modified 02/2005 by S. Merkel
;    Added a common block with fit improvement parameters so they can
;    be easily set in a GUI.
; Modified 03/2005 by S. Merkel
;    Changed plots, they are now in degrees. All calculations are done
;    in pixels but we calculate a 2theta scale for plotting. It makes
;    it much easier to identify peaks...
;
;
; Fits N gaussians to a given x-y data set using the mpfit subroutine
; and a second degree poly for the background
; send x, y, npeaks, fit
; Fit parameters will be returned in the array fit as follow
;   fit(i,0) = position for peak i
;   fit(i,1) = intensity for peak i
;   fit(i,2) = half width for peak i
; If called with keyword AUTO, it will use previous parameters
; given in the fit table to estimate peak positions, otherwise
; user will be asked to define the top and  first peak and half
; width for each peak
;
; parameters:
;  theta: 2 theta values
;  x: index (pixel numbers) of the section of spectra to fit
;  y: corresponding intensities
;  npeaks: number of peaks
;  fit: fit parameters (see above for format) of the previous fits or
;     array of zeros
;  hardbg: if equals to 1, background will be fitted by user using a
;     polynomial function a begining of iterations
;  sidebg: if equals to 1, background = line between the few points
;     on the side of the domain to fit
;  automatic: if set, program uses the coefficients of the previous
;     fits (as sent in the fit array), otherwise, user defines
;     the peaks by clicking on top and half-width
;  peakmodel: = 1 -> pseudo-voigt peak profile
;     = 2 -> lorentzian peak profile
;     else gaussian peak profile
;
; parameters to be set in common block 'fitoptions'
;  - basescale: sets the restriction of region to fit, (should be 5, by
;  default)
;  - smallDetection: detection of small peaks (should be 3, by
;    default)
;  - nloops: number of loops (20)
;  - startSmall: 2
;  - endSmall: 10
; *******************************************************************

; REMEMEBER: ALL FITS ARE DONE IN PIXELS! THERE ARE CONVERSIONS TO
; 2THETA FOR EASY DISPLAY, BUT THE MATHS ARE IN PIXELS!

pro gaussNpeaksbg, theta, x, y, npeaks, fit, hardbg, sidebg, peakmodel, alpha, AUTO = automatic
common bginfo, bgdegree, bgCoefs
common fitoptions, basescale, smallDetection, nLoop, startSmall, endSmall
if (keyword_set(automatic)) then auto = 1 else auto = 0

; Parameters for the fit
fitBg = 10
;startSmall = 2
;endSmall = 12
;nLoop = 20

; peak profile setup
; default, gaussian peak profile
shiftNterms  = 0

; restriction of region, number of terms to fit
; basescaling defines the region to include around the peak position
; in the fit (baseScaling*peakwidth). The rest of the spectrum is
; simply ignored.
; default, gaussian peak profile
baseScaling = 1.0*basescale
shiftNterms  = 0
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
; x are integers than indicate the pixel numbers for the center of
; the imaging plate
;
; they are converted to whatever they mean in another subroutine
;
; y are floats that represent the intensity of diffraction at this
; pixel
;

ylength =  N_ELEMENTS(y)

;
; added for scaling
;
minScale = 60
scaling = 1
if (ylength lt (minScale+npeaks*minScale)) then begin
    scaling = fix((minScale+npeaks*minScale)/ylength)
endif
; scaling = 2 means for instance:
;       x = 10        11        12        13
;       y = 0.0       0.1       0.2       0.3
; scaledx = 10   11   12   13   14   15   16
; scaledy = 0.0  0.05 0.1  0.15 0.2  0.25 0.3
;print, 'x-scaling = ', scaling
scaledylength = ylength+(ylength-1)*(scaling-1)
scaledx = fltarr(scaledylength)
scaledy = fltarr(scaledylength)
scaledtheta = fltarr(scaledylength)
for i=0, scaledylength-2 do begin
    indexInf = fix(i/scaling)
    indexSup = fix(i/scaling)+1
    scaledx(i) = min(x)+i
    scaledy(i) = 1.0*(y(indexInf)+1.0*(1.0*i-scaling*indexInf)* $
                      (y(indexSup)-y(indexInf))/scaling)
    scaledtheta(i) = 1.0*(theta(indexInf)+1.0*(1.0*i-scaling*indexInf)* $
                      (theta(indexSup)-theta(indexInf))/scaling)
endfor
scaledx(scaledylength-1) = min(x)+scaledylength-1
scaledy(scaledylength-1) = y(ylength-1)
scaledtheta(scaledylength-1) = theta(ylength-1)
oldx = x
oldy = y
y = scaledy
x = scaledx
ylength = scaledylength
;
; end scaling
;

minX = min(x)
maxX = max(x)
minY = min(y)
minYA = [replicate(miny,ylength)]
y = y - minYA
tofittmp = fltarr(maxX+1,2)
weighttmp = fltarr(maxX+1)
tofittmp(*,0) = findgen(maxX+1)
yfit = ptrarr(npeaks)
estimates = ptrarr(npeaks)
coeffs = ptrarr(npeaks)
xtmp = ptrarr(npeaks)
ytmp = ptrarr(npeaks)
thetatmp = ptrarr(npeaks)
if (auto eq 0) then begin
	yplotmin = min(y)-0.05*(max(y)-min(y))
	yplotmax = max(y)+0.05*(max(y)-min(y))
	xplotmin = min(scaledtheta)
	xplotmax = max(scaledtheta)
	!X.STYLE = 1
	!Y.STYLE = 1
	!P.NOERASE = 0
    plot, scaledtheta, y, background=255, color = 0, yrange=[yplotmin,yplotmax]
endif

; hard backgound?? THIS HAS TO BE CORRECTED, WAS WRITTEN AT A TIME
; WHERE THE PLOTS WERE IN PIXELS
if (hardbg eq 1) then begin
    if (auto eq 0) then begin
        print, 'n elements in x 1:', n_elements(x)
        read, bgdegree, prompt = 'Degree of polynomial for the background:'
        print, "  Click points for the background, right click to end."
        xCbg = fltarr(100)
        yCbg = fltarr(100)
        iBg = -1
        !err=0
        while (!err ne 4)do begin
            cursor,xCurs,yCurs,/down
            if !err ne 4 then begin
                iBg = iBg + 1
                print, iBg
                xCbg(iBg) = xCurs
                yCbg(iBg) = yCurs
            endif
        endwhile
        fitBgX = xCbg (0:iBg)
        fitBgY = yCbg (0:iBg)
        bgCoefs = POLY_FIT(fitBgX,fitBgY,bgdegree)
        bg = fltarr(n_elements(x))
        for deg=0, bgdegree do begin
            bg = bg + bgCoefs(deg)*x^deg
        endfor
        plot, x, y-bg, background=255, color = 0
    endif else begin
        bg = fltarr(n_elements(x))
        for deg=0, bgdegree do begin
            bg = bg + bgCoefs(deg)*x^deg
        endfor
    endelse
endif else begin
    bgdegree = 1
    bg = fltarr(n_elements(x))
    if (sidebg eq 1) then begin
        fitBgX = fltarr(20)
        fitBgY = fltarr(20)
        for j = 0, 9 do begin
            fitBgX(j) = x(j)
            fitbgX(10+j) = x(N_ELEMENTS(x)-1-j)
            fitBgY(j) = y(j)
            fitbgY(10+j) = y(N_ELEMENTS(x)-1-j)
        endfor
        bgCoefs = POLY_FIT(fitBgX,fitBgY,bgdegree)
        for deg=0, bgdegree do begin
            bg = bg + bgCoefs(deg)*x^deg
        endfor
    endif
endelse

; first loop to initialize all pointers, variables, and peak positions

; if we have to initialize it by hand, we ask the user to
; click on the peaks one by one, a we try t fit them
diff = fltarr(maxX+1)
if (auto eq 0) then begin
	xleg = xplotmin+0.05*(xplotmax-xplotmin)
    for peak = 0, npeaks-1 do begin
        estimates(peak) = PTR_NEW(/ALLOCATE_HEAP)
        xtmp(peak) = PTR_NEW(/ALLOCATE_HEAP)
        ytmp(peak) = PTR_NEW(/ALLOCATE_HEAP)
        thetatmp(peak) = PTR_NEW(/ALLOCATE_HEAP)
        coeffs(peak) = PTR_NEW(/ALLOCATE_HEAP)
        yfit(peak) = PTR_NEW(/ALLOCATE_HEAP)
                                ; get position of this peak from the user
        string = "Click on peak " + string(peak + 1, /print)
		yleg = yplotmax-0.04*(yplotmax-yplotmin)
		xyouts, xleg,yleg, string,  color = 0, charsize=1.5, charthick=2
        cursor,xC,yC,/down
		yleg = yleg-0.04*(yplotmax-yplotmin)
		xyouts, xleg,yleg, "Define half-width", color = 0, charsize=1.5, charthick=2
        cursor,xH,yH,/down
        xC = theta2pixels(scaledtheta, N_ELEMENTS(scaledtheta), xC)+minX
        xH = theta2pixels(scaledtheta, N_ELEMENTS(scaledtheta), xH)+minX
        *estimates(peak) =  fltarr(5+shiftNterms)
        (*estimates(peak))(1) = xC ; position
        (*estimates(peak))(0) = yC ; intensity
        (*estimates(peak))(2) = max([2,abs(xC-xH)]) ; half width

                                ; we remove the peaks found before
        tofittmp(*,1) = 0.0
        tofittmp(x,1) = y-bg
        for j = 0, peak-1 do begin
            tofittmp((*xtmp(j)),1) = (tofittmp((*xtmp(j)),1)-(*yfit(j)))
        endfor
                                ; we limit the zone to fit the peak, so it doesn't
                                ; fit everything
        minZoneX = minmaxval(minX,maxX-1,(*estimates(peak))(1)-baseScaling*(*estimates(peak))(2))
        maxZoneX = minmaxval(minX,maxX-1,(*estimates(peak))(1)+baseScaling*(*estimates(peak))(2))
                                ; print, N_ELEMENTS(tofittmp), XC2, estimates2(2), minxx, maxxx
        *xtmp(peak) = tofittmp(minZonex:maxZonex,0)
        *ytmp(peak) = tofittmp(minZonex:maxZonex,1)
        *thetatmp(peak) = scaledtheta(minZonex-x(0):maxZonex-x(0))
                                ; fit the gaussian or pseudovoigt with
                                ; a linear background

                                ; force the peaks width to be
                                ; positive... Forcing peak height to
                                ; be positive causes crashes... I
                                ; can't understand why, and it's a
                                ; real problem
        parinfo = replicate({limited:[0,0], $
                             limits:[0,0]}, 3+shiftNterms)
        parinfo(1).limited(0) = 1
        parinfo(1).limits(0)  = 0.0

        if (peakmodel eq 1) then begin
            *yfit(peak) = MPFITPEAK(*xtmp(peak),*ytmp(peak),(*coeffs(peak)), $
                                    nterms=5+shiftNterms, estimates=(*estimates(peak)), /pseudoV, parinfo = parinfo)
        endif else if (peakmodel eq 2) then begin
            *yfit(peak) = MPFITPEAK(*xtmp(peak),*ytmp(peak),(*coeffs(peak)), $
                                    nterms=5+shiftNterms, estimates=(*estimates(peak)), /LORENTZIAN, parinfo = parinfo)
        endif else begin
            *yfit(peak) = MPFITPEAK(*xtmp(peak),*ytmp(peak),(*coeffs(peak)), $
                                    nterms=5+shiftNterms, estimates=(*estimates(peak)), parinfo = parinfo)
        endelse
                                ; show the result
        diff(x) = y-bg
        for j = 0, peak do begin
            diff((*xtmp(j))) = (diff((*xtmp(j)))-(*yfit(j)))
        endfor
        diff((*xtmp(peak))) = (diff((*xtmp(peak)))+(*coeffs(peak))(3+shiftNterms)+(*xtmp(peak))*(*coeffs(peak))(4+shiftNterms))
		yplotmin = min(y-bg) - 0.05* (max(y-bg)-min(y-bg))
		yplotmax = max(y-bg) + 0.05* (max(y-bg)-min(y-bg))
		!X.STYLE = 1
		!Y.STYLE = 1
		!P.NOERASE = 0
        plot, scaledtheta, y-bg, background=255, color = 0, yrange=[yplotmin,yplotmax]
        oplot, *thetatmp(peak), *yfit(peak), color = 100
        oplot, scaledtheta, diff(x), color=200
                                ; we remove the linear background from the fit
        *yfit(peak) = *yfit(peak)-(*coeffs(peak))(3+shiftNterms)-(*xtmp(peak)) *(*coeffs(peak))(4+shiftNterms)
    endfor
endif

; We show the result of the pre-fit with user input
if (auto eq 0) then begin
    plot, scaledtheta, y, background=255, color = 0
    for peak = 0, npeaks-1 do $
      oplot, *thetatmp(peak), *yfit(peak), color = 100
    oplot, scaledtheta, bg, color = 200
                                ; plot, x, y, background=255, color = 0
                                ; tofittmp(*,1) = 0.0
                                ; tofittmp(x,1) =  bg
                                ; for j = 0, npeaks-1 do begin
                                ;     tofittmp((*xtmp(j)),1) = tofittmp((*xtmp(j)),1)+(*yfit(j))
                                ; endfor
                                ; newfit = tofittmp(minX:maxX,1)
                                ; oplot, x, newfit, color = 200
endif

; We use the results from previous fits as starting model. So we have
; to regenerate the peak profiles from their position, half-width and intensity.
if (auto ne 0) then begin
    for peak = 0, npeaks-1 do begin
        estimates(peak) = PTR_NEW(/ALLOCATE_HEAP)
        xtmp(peak) = PTR_NEW(/ALLOCATE_HEAP)
        ytmp(peak) = PTR_NEW(/ALLOCATE_HEAP)
        coeffs(peak) = PTR_NEW(/ALLOCATE_HEAP)
        yfit(peak) = PTR_NEW(/ALLOCATE_HEAP)
        thetatmp(peak) = PTR_NEW(/ALLOCATE_HEAP)
        *estimates(peak) =  fltarr(5+shiftNterms)
                                ; guess for position and half width have to be scaled
        position = minX+(fit(peak,0)-minX)*scaling
        halfwidth = max([5,fit(peak,2)])*scaling
        intensity = fit(peak,1)
        (*estimates(peak))(1) = position ; position
        (*estimates(peak))(0) = intensity ; intensity
        (*estimates(peak))(2) = halfwidth ; half width
                                ; we don't fit anything, just regenerate the peak profiles
                                ; we remove the peaks found before
        tofittmp(*,1) = 0.0
        tofittmp(x,1) = y-bg
        for j = 0, peak-1 do begin
            tofittmp((*xtmp(j)),1) = (tofittmp((*xtmp(j)),1)-(*yfit(j)))
        endfor
                                ; we limit the zone where the peak resides
        minZoneX = minmaxval(minX,maxX-1,(*estimates(peak))(1)-baseScaling*(*estimates(peak))(2))
        maxZoneX = minmaxval(minX,maxX-1,(*estimates(peak))(1)+baseScaling*(*estimates(peak))(2))
        *xtmp(peak) = tofittmp(minZonex:maxZonex,0)
        *ytmp(peak) = tofittmp(minZonex:maxZonex,1)
        *thetatmp(peak) = scaledtheta(minZonex-x(0):maxZonex-x(0))
                                ; generate the gaussian or pseudovoigt with no background
                                ; since we lost the weigth between
                                ; gaussian and lorentzian, we just use
                                ; a gaussian model
                                ; y = intensity * exp (-0.5 *((x-center)/half-width)^2)
        *yfit(peak) = intensity*exp(-0.5*((*xtmp(peak)-position)/halfwidth)^2)
        *coeffs(peak) = *estimates(peak)
    endfor
endif


;print, "Optimization..."
; optimization loop
doSmall = 0
compteBg = 0
doPeak = intArr(npeaks)
for i = 0, nLoop do begin
                                ; If there is a very small peak, we
                                ; don't optimize it at the same time,
                                ; we do
                                ;   - the big ones from 0 to startSmall
                                ;   - the small ones from startSmall
                                ;     to endSmall
                                ;   - the big ones later

                                ; Also, we we are in doSmall mode, the
                                ; peak width is automatically divided
                                ; by 2 before refitting...
                                ; In that case, we also fit a local
                                ; background in order to avoid the
                                ; effects of the surrounding peaks...

                                ; Small peak is also ignored when
                                ; fitting the rest...
                                ; Factor for detecting small peak
    if (npeaks gt 1) then begin
        maxIntensity = 0.0
        minIntensity = 100000000000
        for j = 0, npeaks-1  do begin
            if ((*coeffs(j))(0) gt maxIntensity) then maxIntensity=(*coeffs(j))(0)
            if ((*coeffs(j))(0) lt minIntensity) then minIntensity=(*coeffs(j))(0)
        endfor
        if (maxIntensity gt (smallDetection*minIntensity)) then begin
            if ((i lt startSmall) or (i gt endSmall)) then begin
                doSmall = 0
                for j = 0, npeaks-1  do begin
                    if ((*coeffs(j))(0) lt (smallDetection*minIntensity)) then doPeak(j) = 0 else doPeak(j) = 1
                endfor
            endif else begin
                doSmall = 1
                for j = 0, npeaks-1  do begin
                    if ((*coeffs(j))(0) lt (smallDetection*minIntensity)) then doPeak(j) = 1 else doPeak(j) = 0
                endfor
            endelse
        endif else begin
            doSmall = 0
            for j = 0, npeaks-1  do doPeak(j) = 1
        endelse
    endif else begin
        doSmall = 0
        doPeak(0) = 1
    endelse

    if ((compteBg eq fitBg) and (sidebg eq 0)) then begin
                                ; we fit a quadratic background to the
                                ; data minus the two previous fits
        tofittmp(*,1) = 0.0
        tofittmp(x,1) =  y
        for j = 0, npeaks-1 do begin
            tofittmp((*xtmp(j)),1) = tofittmp((*xtmp(j)),1)-(*yfit(j))
        endfor
        xbg = x
        ybg = tofittmp(minX:maxX,1)
        bgCoefs = POLY_FIT(xbg,ybg,bgdegree)
        bg = fltarr(n_elements(x))
        for deg=0, bgdegree do begin
            bg = bg + bgCoefs(deg)*x^deg
        endfor
    endif
    compteBg = compteBg + 1
                                ;plot, xbg, ybg, background=255, color = 0
                                ;oplot, x, bg, color = 100
                                ;cursor,xH,yH,/down
                                ; loop on peaks
    for peak = 0, npeaks-1 do begin
        if (doPeak(peak) eq 1) then begin
                                ; we take the original data, minus the other peaks and the bg
            tofittmp(*,1) = 0.0
            tofittmp(x,1) =  y-bg
                                ; If there is a very small beak, we
                                ; ignore it when fitting the others
                                ; If we are fitting a small peak, we
                                ; take care of everything
                                ; Also, if we are working on a small
                                ; peak, the background has to be
                                ; lifted up because of the surrounding
                                ; ones, otherwise the fit fucks up
                                ; This is done later once, we have
                                ; restricted the zone...
            if (doSmall eq 1) then begin
                for j = 0, npeaks-1  do begin
                    if (j ne peak)then $
                      tofittmp((*xtmp(j)),1) = tofittmp((*xtmp(j)),1)-(*yfit(j))
                endfor
            endif else begin
                for j = 0, npeaks-1  do begin
                    if ((j ne peak) and (doPeak(j) eq 1))then $
                      tofittmp((*xtmp(j)),1) = tofittmp((*xtmp(j)),1)-(*yfit(j))
                endfor
            endelse
                                ; we have to apply a weight, otherwise, it fucks, if the fit from
                                ; other peaks is high, the weight is small, so the peaks don't
                                ; shift towards each other
            weighttmp(*) = 0.0
            for j = 0, npeaks-1  do begin
                if (j ne peak) then $
                  weighttmp((*xtmp(j))) = weighttmp((*xtmp(j)))+abs((*yfit(j)))
            endfor

                                ; We apply another weight to give more
                                ; importance to the top of the peak..
                                ; weighttmp((*xtmp(peak))) =
                                ; weighttmp((*xtmp(peak)))-abs((*yfit(peak)))
                                ; Removed it does not help at all...
                                ; Normalization...
            maxweigth = max(weighttmp)
            if (maxweigth lt 1) then maxweigth = 1
                                ; Coefficient to change to strength of the weighting...
            coeffWeight = 3.
            weighttmp(*) = (coeffWeight*weighttmp(*) + 0.1 * maxweigth)/coeffWeight
                                ; results from previous fit on this peak becomes estimation
            *estimates(peak) = *coeffs(peak)
                                ; avoid getting a fitting zone that is too small
            (*estimates(peak))(2) = max([5,(*estimates(peak))(2)])

                                ; we limit the zone to fit the peak, so it doesn't
                                ; fit everything
                                ; This part is tricky...
                                ; At first, I was restricting at
                                ; baseScaling * peak width, but it
                                ; works badly if one peak is much
                                ; smaller that the other... So now, it
                                ; is weighted by the peak intensity...
                                ; find Max Intensity
            if (npeaks gt 1) then begin
                maxIntensity = 0.0
                minIntensity = 100000000000
                for j = 0, npeaks-1  do begin
                    if ((*coeffs(j))(0) gt maxIntensity) then maxIntensity=(*coeffs(j))(0)
                    if ((*coeffs(j))(0) lt minIntensity) then minIntensity=(*coeffs(j))(0)
                endfor
                                ; factor for zoneWidth is baseScaling
                                ; for peak with maxIntensity, and
                                ; 0.5*baseScaling for lowest peak
                thisScaling = (0.5 + 0.5*((*coeffs(peak))(0)-minIntensity)/(maxIntensity-minIntensity)) * baseScaling
                                ; We restrict the zone
            endif else thisScaling = baseScaling


                                ; If we are working on a small peak,
                                ; we divide its width by 3 before
                                ; refiting
            if (doSmall eq 1) then (*estimates(peak))(2) = (*estimates(peak))(2)/2.


            minZoneX = minmaxval(minX,maxX-1,(*estimates(peak))(1)-4.*thisScaling*(*estimates(peak))(2))
            maxZoneX = minmaxval(minX,maxX-1,(*estimates(peak))(1)+4.*thisScaling*(*estimates(peak))(2))
            if ((maxZoneX- minZoneX) lt 5) then minZoneX = minzoneX -10
            minZoneX = minmaxval(0,maxX-1,minZoneX)
            if ((maxZoneX- minZoneX) lt 5) then  maxZoneX = maxZoneX + 10

            *xtmp(peak) = tofittmp(minZonex:maxZonex,0)
            *ytmp(peak) = tofittmp(minZonex:maxZonex,1)
            *thetatmp(peak) = scaledtheta(minZonex-x(0):maxZonex-x(0))
                                ; If it is a small peak, we take a
                                ; straight line between left and right
                                ; to remove it otherwise the peak is
                                ; to high
            if (doSmall eq 1) then begin
                bg2 = fltarr(n_elements(*xtmp(peak)))
                fitBgX2 = fltarr(4)
                fitBgY2 = fltarr(4)
                for j = 0, 1 do begin
                    fitBgX2(j) = (*xtmp(peak))(j)
                    fitbgX2(2+j) = (*xtmp(peak))(N_ELEMENTS(*xtmp(peak))-1-j)
                    fitBgY2(j) = (*ytmp(peak))(j)
                    fitbgY2(2+j) = (*ytmp(peak))(N_ELEMENTS(*xtmp(peak))-1-j)
                endfor
                bgCoefs2 = POLY_FIT(fitBgX2,fitBgY2,1)
                for deg=0, 1 do begin
                    bg2 = bg2 + bgCoefs2(deg)*(*xtmp(peak))^deg
                endfor
                                ; Another weight... So the intensity
                                ; of the not so small peaks are not reduced to much...
                coeffBg = (minIntensity*smallDetection-(*estimates(peak))(1))/(smallDetection*minIntensity)
                (*ytmp(peak)) = (*ytmp(peak)) - coeffBg * bg2
            endif

            weigthtmp2 = fltarr(maxZonex-minZonex+1)
            weighttmp2 = weighttmp(minZonex:maxZonex)
                                ; print, 'le poids  ', weighttmp2
                                ;print, minZoneX, maxZoneX
                                ; fit the gaussian with no background

            parinfo = replicate({limited:[0,0], $
                                 limits:[0,0]}, 3+shiftNterms)
                                ; force the peaks width to be
                                ; positive... Forcing peak height to
                                ; be positive causes crashes... I
                                ; can't understand why, and it's a
                                ; real problem
            parinfo(1).limited(0) = 1
            parinfo(1).limits(0)  = 0.0

            if (peakmodel eq 1) then begin
                *yfit(peak) = MPFITPEAK(*xtmp(peak),*ytmp(peak),(*coeffs(peak)), $
                                        nterms=3+shiftNterms, $
                                        estimates=(*estimates(peak)), error = weighttmp2, $
                                        parinfo = parinfo, /pseudoV)
            endif else if (peakmodel eq 2) then begin
                *yfit(peak) = MPFITPEAK(*xtmp(peak),*ytmp(peak),(*coeffs(peak)), $
                                        nterms=3+shiftNterms, estimates=(*estimates(peak)), /LORENTZIAN)
            endif else begin
                *yfit(peak) = MPFITPEAK(*xtmp(peak),*ytmp(peak),(*coeffs(peak)), $
                                        nterms=3+shiftNterms, $
                                        estimates=(*estimates(peak)), error = weighttmp2)
            endelse
                                ; check the result... If we get
                                ; negative height, we make it 0
                                ; does not work, still need to find
                                ; something better. Has been a problem
                                ; for 3 years...
                                ;if ((*coeffs(peak))(0)<0.0) then begin
                                ;    print, 'Warning: negative peak for number', peak, '... Canceling...'
                                ;    (*coeffs(peak))(0) = 0.0
                                ;    *yfit(peak) = *yfit(peak)-*yfit(peak)
                                ;endif


                                ; *yfit(peak) = *yfit(peak)-(*coeffs(peak))(3)
                                ; plot, *xtmp(peak), *ytmp(peak), background=255, color = 0
                                ; oplot, *xtmp(peak), *yfit(peak), color = 100
                                ; cursor,xH,yH,/down
        endif
    endfor
endfor
; print, 'le poids  ', weighttmp2
; preparing to plot the result...
; results of the fit = bg + all peaks
tofittmp(*,1) = 0.0
tofittmp(x,1) =  bg
for j = 0, npeaks-1 do begin
    tofittmp((*xtmp(j)),1) = tofittmp((*xtmp(j)),1)+(*yfit(j))
endfor
newfit = tofittmp(minX:maxX,1)
; plot the original curve
plotmin = min(y)-.3*(max(y)-min(y))
plotmax = max(y)+.1*(max(y)-min(y))
xmin = min(scaledtheta)
xmax = max(scaledtheta)
plot, scaledtheta, y, background=255, color = 0, xrange=[xmin,xmax], yrange = [plotmin,plotmax], ystyle=1, xstyle=1
xleg = xmax-0.5*(xmax-xmin)
yleg = plotmax-0.05*(plotmax-plotmin)
; print, xleg, yleg
xyouts, xleg,yleg, "az = "+STRING(alpha,format='(F6.1)'),  color = 0, charsize=2, charthick=3
yleg = plotmax-0.10*(plotmax-plotmin)
xyouts, xleg,yleg, "scale fac. x = "+STRING(scaling,format='(I3)'),  color = 0, charsize=1.5, charthick=2
; global fit
oplot, scaledtheta, newfit, color = 100
; and the gaussians
for peak = 0, npeaks-1 do begin
    oplot, *thetatmp(peak), *yfit(peak), color = 200
endfor
; plot the residuals
diff(x) = y-bg-.1*(max(y)-min(y))
for j = 0, npeaks-1 do begin
    diff((*xtmp(j))) = (diff((*xtmp(j)))-(*yfit(j)))
endfor
oplot, scaledtheta, diff(x), color=50
; prepare the results to send them back...
fit = fltarr(npeaks,3)
for peak = 0, npeaks-1 do begin
                                ; position has to be corrected for scaling
    scaledposition = (*coeffs(peak))(1)
    position = minX + (scaledposition-minX)/scaling
                                ; same for half width
    scaledHWidth = (*coeffs(peak))(2)
    hWidth = scaledHWidth/scaling
    fit(peak,0) = position      ; position
    fit(peak,1) = (*coeffs(peak))(0) ; intensity
    fit(peak,2) = hWidth        ; half width
    ptr_free, estimates(peak),  xtmp(peak), ytmp(peak), coeffs(peak), yfit(peak)
endfor
wait, 0.3
end
