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

function contourlevel, min, max
ncontours = 30
range = float(max[0])-float(min[0])
levels = sqrt(range)/(ncontours-1)
contours = fltarr(ncontours)
for i=0,ncontours-1 do begin
	contours[i] = float(min[0]) + i * i * levels * levels
endfor
return, contours
end

pro plotlegend, contours
min = min(contours)
max = max(contours)
ndata = N_ELEMENTS(contours)
data = fltarr(2,ndata)
data(0,*) = 0.
data(1,*) = 1.
for i=0,ndata-1 do begin
	data(*,i) = contours[i]
endfor
nlabels=5
ticklabels = strarr(nlabels)
for i=0,nlabels-1 do begin
	j = fix(i*(ndata-1)/(nlabels-1))
	ticklabels[i] = strtrim(string(contours[j], /print, FORMAT='(F10.0)'),2)
endfor
ticksinterval = ndata/(nlabels-1)
contour, data, /NOERASE ,/FILL, levels = contours, ystyle=1, xstyle=1, background=255, color = 0, charsize = 1.1, xticks=0.0, xtickinterval=100, xtickname=['',''], ytickinterval=ticksinterval, ytickname=ticklabels
end

pro changeFitFile, fitFileEntry
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory
common fitresults, fitdata
result=dialog_pickfile(title='Select fit results', path=outputdirectory, DIALOG_PARENT=base, FILTER=['*.fit','*.*'], /must_exist)
if (result ne '') then begin
	FDECOMP, result, disk, dir, name, qual, version
	filename = outputdirectory + name + "." + Qual
	filenameshort = name + "." + Qual
	if (FILE_TEST(filename)) then begin
		openr, lun, filename, /get_lun
		fitdata = OBJ_NEW('FitPatternObject')
		a = fitdata->readFromAscii(lun)
		free_lun, lun
		if (a ne 1) then begin
			tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
			return
		endif
		WIDGET_CONTROL, fitFileEntry, SET_VALUE=filenameshort
	endif else tmp = DIALOG_MESSAGE(filenameshort + " is not in your output directory.", /ERROR)
endif
end

; Uses the plot window defined in plotwindow.pro
;
PRO plotDataContour, oldbase, plotmin, plotmax, thetamin, thetamax, azmin, azmax
common rawdata, nalpha, ntheta, alpha, twotheta, data
common inputfiles, inputfiles, activeset
common plotit, def, base, draw
; preparing plot window
if (exist(def) eq 0) then def=0
if (def ne 1) then begin
	base = WIDGET_BASE(Title='Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=oldbase) 
	draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500) 
	stash = {base:base, draw:draw}
	WIDGET_CONTROL, base, SET_UVALUE=stash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotit', base
	def = 1
endif
if (WIDGET_INFO(draw, /VALID_ID) ne 1) then begin
	base = WIDGET_BASE(Title='Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=oldbase) 
	draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500)  
	stash = {base:base, draw:draw}
	WIDGET_CONTROL, base, SET_UVALUE=stash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotit', base
	def = 1
endif
WIDGET_CONTROL, draw, GET_VALUE = index 
WSET, index
; prepare the plot
file = inputfiles(activeset)
tmp = WHERE(twotheta  GT thetamin[0], count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT thetamax[0], count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR=tmp[count-1]
tmp  = WHERE(alpha  GT azmin[0], count)
if (count eq 0) then indexB = 0 else indexB = tmp[0]
tmp = WHERE(alpha  LT azmax[0], count)
if (count eq 0) then indexT = N_ELEMENTS(alpha)-1 else indexT=tmp[count-1]
datatmp = data(indexB:indexT,indexL:indexR)
plotdata = ROTATE(datatmp,4) ; 12/2015, this should be 4 to have 2 theta from left to right and azimuth from bottom to top
contours = contourlevel(plotmin, plotmax)
rangetheta = float(azMax[0])-float(azMin[0])
if (rangetheta le 30) then begin
	ticksep = 5
endif else if (rangetheta le 180) then begin 
	ticksep = 30
endif else if (rangetheta le 360) then begin
	ticksep = 45
endif else ticksep = 90
!P.REGION = [0., 0., 0.8, 1.]
contour, plotdata, twotheta(indexL:indexR), alpha(indexB:indexT), /FILL, levels = contours, ystyle=1, xstyle=1, background=255, color = 0, charsize = 1.1, xtitle='2 theta', ytitle='Azimuth', title=file, ytickinterval=ticksep
!P.REGION = [0.75, 0.5, 1., 1.]
plotlegend, contours
!P.REGION = [0., 0., 1., 1.]
end

PRO plotFitContour, plotmin, plotmax, thetamin, thetamax, azmin, azmax
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fitresults, fitdata
tmp = WHERE(twotheta  GT thetamin[0], count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT thetamax[0], count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR=tmp[count-1]
tmp  = WHERE(alpha  GT azmin[0], count)
if (count eq 0) then indexB = 0 else indexB = tmp[0]
tmp = WHERE(alpha  LT azmax[0], count)
if (count eq 0) then indexT = N_ELEMENTS(alpha)-1 else indexT=tmp[count-1]
fitarray = fitdata->buildSynthethicData(alpha(indexB:indexT),twotheta(indexL:indexR), indexT-indexB+1, indexR-indexL+1)
fit = ROTATE(fitarray,4)
contours = contourlevel(plotmin, plotmax)
rangetheta = float(azMax[0])-float(azMin[0])
if (rangetheta le 30) then begin
	ticksep = 5
endif else if (rangetheta le 180) then begin 
	ticksep = 30
endif else if (rangetheta le 360) then begin
	ticksep = 45
endif else ticksep = 90
!P.REGION = [0., 0., 0.8, 1.]
contour, fit, twotheta(indexL:indexR), alpha(indexB:indexT), /FILL, levels = contours, ystyle=1, xstyle=1, background=255, color = 0, charsize = 1.5, xtitle='two theta', ytitle='azimuth', ytickinterval=ticksep
!P.REGION = [0.75, 0.5, 1., 1.]
plotlegend, contours
!P.REGION = [0., 0., 1., 1.]
end

PRO plotBothContour, plotmin, plotmax, thetamin, thetamax, azmin, azmax
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fitresults, fitdata
tmp = WHERE(twotheta  GT thetamin[0], count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT thetamax[0], count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR=tmp[count-1]
fitarray = fitdata->buildSynthethicData(alpha,twotheta(indexL:indexR), nalpha, indexR-indexL+1)
fit = ROTATE(fitarray,4)
datatmp = data(*,indexL:indexR)
thedata = ROTATE(datatmp,4)
both = fltarr(indexR-indexL+1,2*nalpha)
both(*,0:(nalpha-1)) = thedata(*,*)
both(*,nalpha:(2*nalpha-1)) = fit(*,*)
alpha2 = fltarr(nalpha*2)
alpha2(0:(nalpha-1)) = alpha(*)
alpha2(nalpha:(2*nalpha-1)) = alpha(*)+max(alpha)
contours = contourlevel(plotmin, plotmax)
rangetheta = max(alpha2)-min(alpha2)
if (rangetheta le 30) then begin
	ticksep = 5
endif else if (rangetheta le 180) then begin 
	ticksep = 30
endif else if (rangetheta le 360) then begin
	ticksep = 45
endif else ticksep = 90
!P.REGION = [0., 0., 0.8, 1.]
contour, both, twotheta(indexL:indexR), alpha2, /FILL, levels = contours, ystyle=1, xstyle=1, background=255, color = 0, charsize = 1.5, xtitle='two theta', ytitle='azimuth', ytickinterval=ticksep
!P.REGION = [0.75, 0.5, 1., 1.]
plotlegend, contours
!P.REGION = [0., 0., 1., 1.]
end

PRO plotSubstractContour, plotmin, plotmax, thetamin, thetamax, azmin, azmax
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fitresults, fitdata
tmp = WHERE(twotheta  GT thetamin[0], count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT thetamax[0], count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR=tmp[count-1]
fitarray = fitdata->buildSynthethicData(alpha,twotheta(indexL:indexR), nalpha, indexR-indexL+1)
datatmp = data(*,indexL:indexR)
; we take the subtsraction only where there is a fit, if the fit is equal to zero, we set the 
; substraction to 0
dude = datatmp-fitarray;
zeros = WHERE(fitarray EQ 0.0)
dude[zeros] = 0.0  
dudeplot = ROTATE(dude,4)
contours = contourlevel(plotmin, plotmax)
rangetheta = max(alpha)-min(alpha)
if (rangetheta le 30) then begin
	ticksep = 5
endif else if (rangetheta le 180) then begin 
	ticksep = 30
endif else if (rangetheta le 360) then begin
	ticksep = 45
endif else ticksep = 90
!P.REGION = [0., 0., 0.8, 1.]
!P.REGION = [0., 0., 0.8, 1.]
contour, dudeplot, twotheta(indexL:indexR), alpha, /FILL, levels=contours, ystyle=1, xstyle=1, background=255, color = 0, charsize = 1.5, xtitle='two theta', ytitle='azimuth', ytickinterval=ticksep
!P.REGION = [0.75, 0.5, 1., 1.]
plotlegend, contours
!P.REGION = [0., 0., 1., 1.]
end

PRO plotSubstractDataContour, plotmin, plotmax, thetamin, thetamax, azmin, azmax
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fitresults, fitdata
tmp = WHERE(twotheta  GT thetamin[0], count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT thetamax[0], count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR=tmp[count-1]
fitarray = fitdata->buildSynthethicData(alpha,twotheta(indexL:indexR), nalpha, indexR-indexL+1)
datatmp = data(*,indexL:indexR)
; we take the subtsraction only where there is a fit, if the fit is equal to zero, we set the 
; substraction to 0
dude = datatmp-fitarray;
zeros = WHERE(fitarray EQ 0.0)
dude[zeros] = 0.0  
dude = ROTATE(dude,4)
datatmp = ROTATE(datatmp,4)
both = fltarr(indexR-indexL+1,2*nalpha)
both(*,0:(nalpha-1)) = datatmp(*,*)
both(*,nalpha:(2*nalpha-1)) = dude(*,*)
contours = contourlevel(plotmin, plotmax)
alpha2 = fltarr(nalpha*2)
alpha2(0:(nalpha-1)) = alpha(*)
alpha2(nalpha:(2*nalpha-1)) = alpha(*)+max(alpha)
rangetheta = max(alpha2)-min(alpha2)
if (rangetheta le 30) then begin
	ticksep = 5
endif else if (rangetheta le 180) then begin 
	ticksep = 30
endif else if (rangetheta le 360) then begin
	ticksep = 45
endif else ticksep = 90
!P.REGION = [0., 0., 0.8, 1.]
contour, both, twotheta(indexL:indexR), alpha2, /FILL, levels = contours, ystyle=1, xstyle=1, background=255, color = 0, charsize = 1.5, xtitle='two theta', ytitle='azimuth', ytickinterval=ticksep
!P.REGION = [0.75, 0., 1., 0.5]
plotlegend, contours
!P.REGION = [0., 0., 1., 1.]
end

PRO exitCompareFitWindow, base
loadct, 15
WIDGET_CONTROL, base, /DESTROY
end

PRO updateRangeLabels, stash
common rawdata, nalpha, ntheta, alpha, twotheta, data
mindata = min(data)
maxdata = max(data)
mintheta = min(twotheta)
maxtheta = max(twotheta)
minAz = min(alpha)
maxAz = max(alpha)
widget_control, stash.zLa1, SET_VALUE=strtrim(string(mindata,/print),2)
widget_control, stash.zLa2, SET_VALUE=strtrim(string(maxdata,/print),2)
widget_control, stash.thetaLa1, SET_VALUE=strtrim(string(mintheta,/print),2)
widget_control, stash.thetaLa2, SET_VALUE=strtrim(string(maxtheta,/print),2)
widget_control, stash.azLa1, SET_VALUE=strtrim(string(minAz,/print),2)
widget_control, stash.azLa2, SET_VALUE=strtrim(string(maxAz,/print),2)
end

PRO compareFitWindow_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
WIDGET_CONTROL, stash.plotMin, GET_VALUE=plotmin
WIDGET_CONTROL, stash.plotMax, GET_VALUE=plotmax
WIDGET_CONTROL, stash.thetaMin, GET_VALUE=thetamin
WIDGET_CONTROL, stash.thetaMax, GET_VALUE=thetamax
WIDGET_CONTROL, stash.azMin, GET_VALUE=azmin
WIDGET_CONTROL, stash.azMax, GET_VALUE=azmax
CASE uval OF
    'PREVIOUSDATASET': BEGIN
		movebackActiveSet, stash.log, stash.listsets
		updateRangeLabels, stash
		plotDataContour, ev.TOP, plotmin, plotmax, thetamin, thetamax, azmin, azmax
		END
    'NEXTDATASET': BEGIN
		advanceActiveSet, stash.log, stash.listsets
		updateRangeLabels, stash
		plotDataContour, ev.TOP, plotmin, plotmax, thetamin, thetamax, azmin, azmax
		END
    'PLOTDATA': plotDataContour, ev.TOP, plotmin, plotmax, thetamin, thetamax, azmin, azmax
    'PLOTFIT': plotFitContour, plotmin, plotmax, thetamin, thetamax, azmin, azmax
    'PLOTBOTH': plotBothContour, plotmin, plotmax, thetamin, thetamax, azmin, azmax
    'SUBSTRACT': plotSubstractContour, plotmin, plotmax, thetamin, thetamax, azmin, azmax
    'SUBSTRACTDATA': plotSubstractDataContour, plotmin, plotmax, thetamin, thetamax, azmin, azmax
    'CSCALE': xloadct
    'FITFILE': changeFitFile, stash.fitFile
    'EXIT': exitCompareFitWindow, ev.TOP
	ELSE:
ENDCASE
END

; if /nofit is set, we do not show options regarding the fit, plot data only
; send parent, log, and dataset list widget IDs
PRO compareFitWindow, parent, log, listsets, nofit = nofit
; force creation of a new plot window
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fonts, titlefont, boldfont, mainfont, avFontHeight

if (KEYWORD_SET(nofit)) then showfit = 0 else showfit = 1
; Load better color scale
loadct, 5
; 
mindata = min(data)
maxdata = max(data)
mintheta = min(twotheta)
maxtheta = max(twotheta)
minAz = min(alpha)
maxAz = max(alpha)
def = 0
; base GUI
base = WIDGET_BASE(Title='Mapplot options',/COLUMN, GROUP_LEADER=parent)
options = WIDGET_BASE(base,FRAME=1, ROW=4, /ALIGN_CENTER, /GRID_LAYOUT)
la1 = WIDGET_LABEL(options, VALUE='')
la1 = WIDGET_LABEL(options, VALUE='Min data')
la1 = WIDGET_LABEL(options, VALUE='Max data')
la1 = WIDGET_LABEL(options, VALUE='Min plot')
la1 = WIDGET_LABEL(options, VALUE='Max data')
; Z Range
la1 = WIDGET_LABEL(options, VALUE='Intensity')
zLa1 = WIDGET_LABEL(options, VALUE=strtrim(string(mindata,/print),2))
zLa2 = WIDGET_LABEL(options, VALUE=strtrim(string(maxdata,/print),2))
plotMin = WIDGET_TEXT(options, VALUE=strtrim(string(mindata,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
plotMax = WIDGET_TEXT(options, VALUE=strtrim(string(maxdata,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
; 2 theta Range
la1 = WIDGET_LABEL(options, VALUE='2 theta')
thetaLa1 = WIDGET_LABEL(options, VALUE=strtrim(string(mintheta,/print),2))
thetaLa2 = WIDGET_LABEL(options, VALUE=strtrim(string(maxtheta,/print),2))
thetaMin = WIDGET_TEXT(options, VALUE=strtrim(string(mintheta,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
thetaMax = WIDGET_TEXT(options, VALUE=strtrim(string(maxtheta,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
; azimuth Range
la1 = WIDGET_LABEL(options, VALUE='Azimuth')
azLa1 = WIDGET_LABEL(options, VALUE=strtrim(string(minAz,/print),2))
azLa2 = WIDGET_LABEL(options, VALUE=strtrim(string(maxAz,/print),2))
azMin = WIDGET_TEXT(options, VALUE=strtrim(string(minAz,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
azMax = WIDGET_TEXT(options, VALUE=strtrim(string(maxAz,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
; Fit
if (showfit eq 1) then begin
	fit = WIDGET_BASE(base, /COL, FRAME=1, /ALIGN_CENTER, /GRID_LAYOUT)
	fitLa = WIDGET_LABEL(fit, VALUE='Fit data')
	fitEn = WIDGET_BASE(fit, /ROW, FRAME=1, /ALIGN_CENTER)
	fitFile = WIDGET_TEXT(fitEn, VALUE='' ,/ALIGN_LEFT, XSIZE=30)
	fitBu = WIDGET_BUTTON(fitEn, VALUE='Change', UVALUE='FITFILE')
endif
; ACTION BUTTONS
if (showfit eq 1) then begin
	buttons = WIDGET_BASE(base,ROW=3, /ALIGN_CENTER, /GRID_LAYOUT)
	plotDataBut = WIDGET_BUTTON(buttons, VALUE='Plot Data', UVALUE='PLOTDATA')
	plotFitBut = WIDGET_BUTTON(buttons, VALUE='Plot Fit', UVALUE='PLOTFIT')
	plotBothBut = WIDGET_BUTTON(buttons, VALUE='Plot Both', UVALUE='PLOTBOTH')
	plotSubstractBut = WIDGET_BUTTON(buttons, VALUE='Substract', UVALUE='SUBSTRACT')
	plotSubstractBut = WIDGET_BUTTON(buttons, VALUE='Sub. and Data', UVALUE='SUBSTRACTDATA')
	emLa = WIDGET_LABEL(buttons, VALUE=' ')
	colorBut = WIDGET_BUTTON(buttons, VALUE='Color scale', UVALUE='CSCALE')
	emLa = WIDGET_LABEL(buttons, VALUE=' ')
	closeBut = WIDGET_BUTTON(buttons, VALUE='Close', UVALUE='EXIT')
endif else begin
	buttons2 = WIDGET_BASE(base,/ALIGN_CENTER, /ROW, /GRID_LAYOUT)
	previous = WIDGET_BUTTON(buttons2, VALUE='Previous dataset', UVALUE='PREVIOUSDATASET')
	next = WIDGET_BUTTON(buttons2, VALUE='Next dataset', UVALUE='NEXTDATASET')
	buttons = WIDGET_BASE(base,/ALIGN_CENTER, /ROW)
	plotDataBut = WIDGET_BUTTON(buttons, VALUE='Plot Data', UVALUE='PLOTDATA')
	colorBut = WIDGET_BUTTON(buttons, VALUE='Color scale', UVALUE='CSCALE')
	closeBut = WIDGET_BUTTON(buttons, VALUE='Close', UVALUE='EXIT')
endelse
; Create an anonymous structure to hold widget IDs
if (showfit eq 1) then begin
	stash = {base:base, log:log, listsets:listsets,  zLa1:zLa1, zLa2:zLa2, thetaLa1:thetaLa1, thetaLa2: thetaLa2, azLa1:azLa1, azLa2:azLa2, plotMin: plotMin, plotMax: plotMax, thetaMin: thetaMin, thetaMax: thetaMax, azMin: azMin, azMax: azMax, fitFile: fitFile}
endif else begin
	stash = {base:base, log:log, listsets:listsets,  zLa1:zLa1, zLa2:zLa2, thetaLa1:thetaLa1, thetaLa2: thetaLa2, azLa1:azLa1, azLa2:azLa2, plotMin: plotMin, plotMax: plotMax, thetaMin: thetaMin, thetaMax: thetaMax, azMin: azMin, azMax: azMax}
endelse
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'compareFitWindow', base
END
