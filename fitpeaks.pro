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


;************************************************************ routines to fit 1 spectrum *****************

function theta2pixels, twotheta, ntheta, x
return, getIndex(x,  twotheta, ntheta)
end


function fitit, manual, theta, xx, yy, npeaks, alpha, fit, hardbg, sidebg, peakmodel
fitted = 0
if (manual eq 1) then begin
    gaussNpeaksbg, theta, xx, yy, npeaks, fit, hardbg, sidebg, peakmodel, alpha
endif else begin
    gaussNpeaksbg, theta, xx, yy, npeaks, fit, hardbg, sidebg, peakmodel, alpha, /AUTO
endelse
info = dialog_message(/cancel, 'Fit for alpha = ' + STRING(alpha))
if (info eq 'Cancel') then begin
    info = againDialog(Label="Try again?")
    if (info eq 0) then fitted = 2 else if (info eq 1) then fitted = 1 else fitted = 3
endif else fitted = 0
return, fitted
end

; SOME PARAMETERS:
;  Those 3 parameters have no effect if AUTOCHECK keyword is not set
;  - stopNegIntensity: 1 to stop automatic fitting if negative intensities
;  - stopTooWide: 1 to stop automatic fitting if peak width changes by more than 2
;  - stopIntensityChge: 1 to stop automatic fitting if intensity changes by more than 2
; 05/2011 new parameters: stopNegIntensity, stopTooWide, stopIntensityChge
function fitandcheck, theta, xx, yy, npeaks, alpha, fit, hardbg, sidebg, peakmodel, stopNegIntensity, stopTooWide, stopIntensityChge
fitted = 0
oldfit = fit
gaussNpeaksbg, theta, xx, yy, npeaks, fit, hardbg, sidebg, peakmodel, alpha, /AUTO
strange = 0
for peak = 0, npeaks-1 do begin
                                ; intensity < 0 ----> strange
    if ((fit(peak,1) lt 0) and (stopNegIntensity eq 1)) then strange = 1
                                ; half-width 2 times bigger then before ----> strange
    if ((fit(peak,2) gt (2.*oldfit(peak,2))) and (stopTooWide eq 1)) then strange = 1
                                ; peak intensity 3 times bigger than before ----> strange
    if ((fit(peak,1) gt (3.*oldfit(peak,1))) and (stopIntensityChge eq 1)) then strange = 1
endfor
if (strange eq 1) then begin
    ;info = dialog_message(/cancel, $
    ;                      'Fit for alpha = ' + STRING(alpha) + " is strange, Try again manually?" )
    ;if (info eq 'Cancel') then fitted = 2 else fitted = 1
    info = againDialog(Label="Fit for alpha = " + STRING(alpha) + " is strange, Try again manually?")
    if (info eq 0) then fitted = 2 else if (info eq 1) then fitted = 1 else fitted = 3
endif else fitted = 0
return, fitted
end

; let the use define a zone to perform the fit
function findzone, manual, theta, minx, ytmp, xx, yy, thetatmp
common datafindzone, xr, xl     ; this keep the last input from the user in memory for the next time
common datafit, fit, fitcenter  ; results from fits of peak position, are used to re-center fitting zone
if (manual eq 1) then begin
    doit = 1
    miny = min(ytmp)-0.05*(max(ytmp)-min(ytmp))
    maxy = max(ytmp)+0.05*(max(ytmp)-min(ytmp))
	!X.STYLE = 1
	!Y.STYLE = 1
	!P.NOERASE = 0
    plot, theta, ytmp, background=255, color=0, yrange=[miny,maxy]
    minmaxy = max(ytmp)-min(ytmp)
    minmaxyG = max(ytmp(0:fix(N_ELEMENTS(ytmp)/3)))$
      -min(ytmp(0:fix(N_ELEMENTS(ytmp)/3)))
    minmaxyD = max(ytmp(fix(2.*N_ELEMENTS(ytmp)/3.):(N_ELEMENTS(ytmp)-1)))$
      -min(ytmp(fix(2.*N_ELEMENTS(ytmp)/3.):(N_ELEMENTS(ytmp)-1)))
    if ((minmaxy lt 1) or (minmaxyG lt 1) or (minmaxyD lt 1)) then begin
        info = againDialog(Label="Bad data, keep going?")
        if (info eq 0) then begin
            doit = 0
            xr = N_ELEMETS(theta)+ minx
            xl = N_ELEMETS(theta)+ minx
        endif else if (info eq -1) then begin
            ; we forget about everything
            return, 0
        endif
    endif
    if (doit eq 1) then begin
        ; the user defines the zone
		xleg = min(theta)+0.05*(max(theta)-min(theta))
		yleg = maxy-0.04*(maxy-miny)
		xyouts, xleg,yleg, "Click on left side of peaks",  color = 0, charsize=1.5, charthick=2
        cursor,xl,yl,/down
		yleg = maxy-0.08*(maxy-miny)
		xyouts, xleg,yleg, "Click on right side of peaks",  color = 0, charsize=1.5, charthick=2
        cursor,xr,yr,/down
        xr = theta2pixels(theta, N_ELEMENTS(theta), xr)
        xl = theta2pixels(theta, N_ELEMENTS(theta), xl)
        tmp = [xr,xl]
        xr = minmaxval(0, minx + N_ELEMENTS(theta), fix(max(tmp)))
        xl = minmaxval(0, minx + N_ELEMENTS(theta), fix(min(tmp)))
    endif
endif else begin
; zone is guessed from previous fits
    width = fix((xr-xl)/2)
    xl = minmaxval(minx, minx + N_ELEMENTS(theta), (fitcenter - width)) - minx
    xr = minmaxval(minx, minx + N_ELEMENTS(theta), (fitcenter + width)) - minx
    ; print, fitcenter, width,  xtmp(0),  xl, xr
endelse
; print, xl, xr,  N_ELEMENTS(xtmp)
yy = ytmp(xl:xr)
xx = findgen(xr-xl+1)+xl+minx
thetatmp = theta(xl:xr)
return, 1
end


; PARAMETERS:
;  Those 3 parameters have no effect if AUTOCHECK keyword is not set
;  - stopNegIntensity: 1 to stop automatic fitting if negative intensities
;  - stopTooWide: 1 to stop automatic fitting if peak width changes by more than 2
;  - stopIntensityChge: 1 to stop automatic fitting if intensity changes by more than 2
; 05/2011 new parameters: stopNegIntensity, stopTooWide, stopIntensityChge
function fitalpha, i, npeaks, center0, width, auto, position, intensity, halfwidth, good, hardbg, sidebg, peakmodel, stopNegIntensity, stopTooWide, stopIntensityChge, AUTOCHECK = autocor
common rawdata, nalpha, ntheta, alpha, twotheta, data
common datafit, fit, fitcenter
common anythingdoneyet, anythingdoneyet
if (keyword_set(autocor)) then autocorrect = 1 else autocorrect = 0
ytemp = data(i,*)
minx = minmaxval(0, ntheta-1, (center0-3*width))
maxx = minmaxval(0, ntheta-1, (center0+3*width))
ytmp = ytemp(minx:maxx)
xtmp = findgen(maxx-minx)+minx
completed = 0
thetatmp = twotheta(minx:maxx)
                                ; manual adjustment of peak positions ??
if ((auto eq 0) or (i eq 0) or (anythingdoneyet eq 0)) then manual = 1 else  manual = 0
while (completed eq 0) do begin
                                ; narrow the zone to fit
    test = findzone(manual, thetatmp, minx, ytmp, xx, yy, restrictheta)
    if (test eq 0) then return, 0
                                ; is there any data to fit?
    minmaxy = max(yy)-min(yy)
    minmaxyG = max(yy(0:fix(N_ELEMENTS(yy)/3)))$
               -min(yy(0:fix(N_ELEMENTS(yy)/3)))
    minmaxyD = max(yy(fix(2.*N_ELEMENTS(yy)/3.):(N_ELEMENTS(yy)-1)))$
               -min(yy(fix(2.*N_ELEMENTS(yy)/3.):(N_ELEMENTS(yy)-1)))
    if ((minmaxy lt 1) or (minmaxyG lt 1) or (minmaxyD lt 1)) then begin
        plot, xx, yy, background=255, color=0, yrange = [min(yy),max(yy)]
        info = againDialog(Label="Bad data, keep going?")
        if (info eq 0) then begin
            ; We just forget about this spectrum and go to the next one
            good(i) = 0
            if (anythingdoneyet eq 1) then begin
                for peak = 0, npeaks -1 do begin
                    position(peak,i) = position(peak,i-1)
                    intensity(peak,i) = intensity(peak,i-1)
                    halfwidth(peak,i) = halfwidth(peak,i-1)
                    fit(peak,0) = position(peak,i-1)
                    fit(peak,1) = intensity(peak,i-1)
                    fit(peak,2) = halfwidth(peak,i-1)
                endfor
            endif
            completed = 1
        endif else if (info eq 1) then begin
            ; we will do the fit
            completed = 0
        endif else begin
            ; We forget about everything and cancel the fitting procedure
            return, 0
        endelse
    endif else completed = 0
    if (completed eq 0) then begin
                                ; fit the peaks
        if ((autocorrect eq 0) or (manual eq 1)) then begin
            fitted = fitit(manual, restrictheta, xx, yy, npeaks, alpha(i), fit, hardbg, sidebg, peakmodel)
        endif else begin
            fitted = fitandcheck(restrictheta, xx, yy, npeaks, alpha(i), fit, hardbg, sidebg, peakmodel, stopNegIntensity, stopTooWide, stopIntensityChge)
        endelse
        if (fitted eq 0) then begin
                                ; the fit worked, we save everyting and go to the next one
            good(i) = 1
            for peak = 0, npeaks -1 do begin
                position(peak,i) = fit(peak,0)
                intensity(peak,i) = fit(peak,1)
                halfwidth(peak,i) = fit(peak,2)
            endfor
            completed = 1
            anythingdoneyet = 1
        endif
                                ; the fit did not work and we will try again
        if (fitted eq 1) then begin
            completed = 0
            manual = 1
        endif
                                ; the fit did not work and we will not try again
        if (fitted eq 2) then begin
            good(i) = 0
            if (anythingdoneyet eq 1) then begin
                for peak = 0, npeaks -1 do begin
                    position(peak,i) = position(peak,i-1)
                    intensity(peak,i) = intensity(peak,i-1)
                    halfwidth(peak,i) = halfwidth(peak,i-1)
                    fit(peak,0) = position(peak,i-1)
                    fit(peak,1) = intensity(peak,i-1)
                    fit(peak,2) = halfwidth(peak,i-1)
                endfor
            endif
            completed = 1
        endif
                              ; the fit did not work and we completely give up fitting
        if (fitted eq 3) then begin
            return, 0
        endif
    endif
endwhile
                                ; we locate the center of this fit, this will be used to
                                ; estimate the next one
fitcenter =  0
for peak = 0, npeaks -1 do fitcenter = fitcenter + position(peak,i)
fitcenter = fix(fitcenter/npeaks)
return, 1
end



; *******************************************************************
;
; Subroutine fitNpeak...
;
; To fit N peaks in the pattern.
; if called with the keyword AUTO, will try to do that automatically:
;   For the first spectrum, you will have to select the zone twice,
;   and then define each peak (position, half-width). Then the program
;   will use the results from previous fits to guess the parameters
;   for the new ones.
;   For each fit, click on 'Ok' if you are happy, 'Cancel'
;   if not. If you clicked 'Cancel', you will be allowed to try again
;   manually. If you click cancel all the way the fit will not be
;   considered and the program will go to the next spectrum.
; otherwise, manually:
;   For the first spectrum, you will have to select the zone twice
;   and then define each peak (position, half-width).
;   Atferwards for each spectrum , you'll have to define the zone once,
;   and then define each peak (position, half-width).
;
; PARAMETERS:
;  - npeaks: number of peaks
;  - include: azimuth values for which to fit the data (indices)
;  - peakmodel: 0 for Gaussian, 1 for Voigt, 2 for pseudo-voigt
;  - savehalfwidth: 1 to save half-width, 0 otherwise
;  - stopNegIntensity: 1 to stop automatic fitting if negative intensities
;  - stopTooWide: 1 to stop automatic fitting if peak width changes by more than 2
;  - stopIntensityChge: 1 to stop automatic fitting if intensity changes by more than 2
; 05/2011 new parameters: stopNegIntensity, stopTooWide, stopIntensityChge
; *******************************************************************


pro fitNpeaks, npeaks, include, peakmodel, savehalfwidth, stopNegIntensity, stopTooWide, stopIntensityChge, AUTOMATIC = automatic, AUTOCHECK = autocor, $
               BG = backgd, SIDEBG = sidebg
common rawdata, nalpha, ntheta, alpha, twotheta, data
common anythingdoneyet, anythingdoneyet
if (keyword_set(automatic)) then auto = 1 else auto = 0
if (keyword_set(autocor)) then autocorrect = 1 else autocorrect = 0
if (keyword_set(backgd)) then hardbg = 1 else hardbg = 0
if (keyword_set(sidebg)) then sidebg = 1 else sidebg = 0
if (not keyword_set(alphamin)) then alphamin = alpha(0)
if (not keyword_set(alphamax)) then alphamax = alpha(nalpha-1)

                                ; init arrays
position = fltarr(npeaks,nalpha)
intensity = fltarr(npeaks,nalpha)
halfwidth = fltarr(npeaks,nalpha)
good = intarr(nalpha)
                                ; start of the loop on alpha values...
anythingdoneyet = 0
for i=0, nalpha-1 do begin
    test = where(include eq i)
    if (test(0) ge 0) then begin
                                ; If it is the first spectrum to fit, we need to get an interval from the user...
        if (anythingdoneyet eq 0) then begin
			miny = min(data(i,*)) - 0.05*(max(data(i,*))-min(data(i,*)))
			maxy = max(data(i,*)) + 0.05*(max(data(i,*))-min(data(i,*)))
			minx = min(twotheta)
			maxx = max(twotheta)
			!X.STYLE = 1
			!Y.STYLE = 1
			!P.NOERASE = 0
            plot,twotheta,data(i,*), background=255, color=0, yrange=[miny,maxy]
			xleg = minx+0.05*(maxx-minx)
			yleg = maxy-0.04*(maxy-miny)
			xyouts, xleg,yleg, "Click on left side of peaks",  color = 0, charsize=1.5, charthick=2
            cursor,xl,yl,/down
			xleg = minx+0.05*(maxx-minx)
			yleg = maxy-0.08*(maxy-miny)
			xyouts, xleg,yleg, "Click on right side of peaks",  color = 0, charsize=1.5, charthick=2
            cursor,xr,yr,/down
            xl = theta2pixels(twotheta, ntheta, xl)
            xr = theta2pixels(twotheta, ntheta, xr)
            width = abs(fix((xr-xl)/2))+1
            center0 = fix((xr+xl)/2)
        endif
        if ((autocorrect eq 0) or (anythingdoneyet eq 0)) then begin
            keepgoing = fitalpha(i, npeaks, center0, width, auto, position, intensity, halfwidth, good, hardbg, sidebg, peakmodel, stopNegIntensity, stopTooWide, stopIntensityChge)
        endif else begin
            keepgoing = fitalpha(i, npeaks, center0, width, auto, position, intensity, halfwidth, good, hardbg, sidebg, peakmodel, stopNegIntensity, stopTooWide, stopIntensityChge, /AUTOCHECK)
        endelse
        if (keepgoing eq 0) then goto, FITCANCELLED
    endif
endfor
                                ; oooof, we are done with the fits, just need to format the results...
calculateTD, include, npeaks, good, position, intensity,  halfwidth, results
                                ; plot the results
min2theta = min(results(*,0,*))
max2theta = max(results(*,0,*))
minInt = min(results(*,2,*))
maxInt = max(results(*,2,*))
minHW = min(results(*,3,*))
maxHW = max(results(*,3,*))
alphaplot = alpha(include)
!X.STYLE = 1
; used a multiplot to have all plots on the save page, but it was buggy. 
; So changed it to hard coded plot positions... Please do not go back to
; multiplot!
plot, alphaplot,  results(*,0,0), background=255, color=0, yrange=[min2theta,max2theta],$
      xtitle = 'delta', ytitle='2 theta', charsize=1.5, position= [0.15, 0.73, 0.97, 0.99]
for j=1, npeaks -1 do oplot,  alphaplot, results(*,0,j), color = 0
plot, alphaplot,  results(*,2,0), background=255, color=0, yrange=[minInt,maxInt],$
      xtitle = 'delta', ytitle='intensity', charsize=1.5, position= [0.15, 0.40, 0.97, 0.66], /noerase
for j=1, npeaks -1 do oplot,  alphaplot, results(*,2,j), color = 0
plot, alphaplot,  results(*,3,0), background=255, color=0, yrange=[minHW,maxHW],$
      xtitle = 'delta', ytitle='half-width', charsize=1.5, position= [0.15, .07, 0.97, 0.33], /noerase
for j=1, npeaks -1 do oplot,  alphaplot, results(*,3,j), color = 0
!P.MULTI = 0
; eventually, save it into a file
info = dialog_message('Save results in a file?', /QUESTION)
if (info eq 'Yes') then saveresults, include, npeaks, good, results, savehalfwidth
return
FITCANCELLED:
info = dialog_message('Fit was cancel')
return
end


;************************************************************ finalize and export results *****************

pro calculateTD, include, npeaks, good, position, intensity, halfwidth, results
common rawdata, nalpha, ntheta, alpha, twotheta, data
common experiment, wavelength, detectordistance
ttheta = fltarr(nalpha,npeaks)
d =  fltarr(nalpha,npeaks)
halfW = fltarr(nalpha,npeaks)
results = fltarr(N_ELEMENTS(include),4,npeaks)
thispeak = -1
for i=0, nalpha-1 do begin
    test = where(include eq i)
    if (test(0) ge 0) then begin
        thispeak = thispeak+1
                                ; to avoid ARRAY OUT OF RANGE errors
        for peak = 0, npeaks -1 do  $
          position(peak,i) = minmaxval(0,  ntheta-1, position(peak,i))
        for peak = 0, npeaks -1 do begin
                                ; calculate 2theta from the peak
                                ; pos. in pixels
            ttheta(i,peak) = twotheta(fix(position(peak,i)))* $
                             (1+fix(position(peak,i))-position(peak,i)) + $
                             twotheta(fix(position(peak,i)+1))* $
                             (position(peak,i)-fix(position(peak,i)))
                                ; deduce the d-spacing
            d(i,peak) = wavelength/(2*sin(ttheta(i,peak)*!PI/360))
            pos = position(peak,i)-halfwidth(peak,i)
            tthetaMiHWidth = twotheta(fix(pos))* $
                             (1+fix(pos)-pos) + $
                             twotheta(fix(pos+1))* $
                             (pos-fix(pos))
            pos = position(peak,i)+halfwidth(peak,i)
            tthetaMaHWidth = twotheta(fix(pos))* $
                             (1+fix(pos)-pos) + $
                             twotheta(fix(pos+1))* $
                             (pos-fix(pos))
            halfW(i,peak) = tthetaMaHWidth - tthetaMiHWidth
        endfor
                                ; sort all of this mess
        sortt = sort(ttheta(i,*))
        for j=0, npeaks -1 do begin
            results(thispeak,0,j) = ttheta(i,sortt(j))
            results(thispeak,1,j) = d(i,sortt(j))
            results(thispeak,2,j) = intensity(sortt(j),i)
            results(thispeak,3,j) = halfW(i,sortt(j))
        endfor
                                ; print it (if the fit was good
                                ; REMOVE ON 10/29/2003: too slow and not very usefull
                                ; str = strarr(3*npeaks+1)
                                ; if (good(i) eq 1) then begin
                                ;    str(0) = alpha(i)
                                ;    for j=0, npeaks -1 do begin
                                ;        str(3*j+1) = results(i-alphastart,0,j)
                                ;        str(3*j+2) = results(i-alphastart,1,j)
                                ;        str(3*j+3) = results(i-alphastart,2,j)
                                ;    endfor
                                ;    print, str
                                ; endif
    endif
endfor
end


pro saveresults, include, npeaks, good, results, savehalfwidth
common rawdata, nalpha, ntheta, alpha, twotheta, data
common files, extension, directory, outputdirectory
outfile = pickfile_dir_ext(outputdirectory, 'dat', title='Save as')
if (outfile ne '') then begin
    titre=''
    titre = TextBox(Title="Title for this file", Label='Title for this file: ', Cancel=cancelled, XSize=200, Value='')
    openw, lun, outfile, /get_lun
    printf, lun, '# ' + titre
    if (savehalfwidth eq 1) then $
      printf, lun, '# delta, then 2 theta, d, intensity, and half-width (and maybe relative weight gauss/lorentz) for each peak' $
    else $
      printf, lun, '# delta, then 2 theta, d, and intensity (and maybe relative weight gauss/lorentz) for each peak'
    thispeak=-1
    for i=0, nalpha-1 do begin
        test = where(include eq i)
        if (test(0) ge 0) then begin
            thispeak = thispeak + 1
            if (good(i) eq 1) then begin
                toprint = string(format='(f8.2)', alpha(i))
                for j=0, npeaks -1 do begin
                    toprint = toprint + " " + string(format='(f12.4)', results(thispeak,0,j))
                    toprint = toprint + " " + string(format='(f8.4)', results(thispeak,1,j))
                    toprint = toprint + " " + string(format='(f20.2)', results(thispeak,2,j))
                    if (savehalfwidth eq 1) then $
                      toprint = toprint + " " + string(format='(f16.8)', results(thispeak,3,j))
                endfor
                printf, lun, toprint
            endif
        endif
    endfor
    free_lun, lun
endif
end


