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
contour, data, /NOERASE ,/FILL, levels = contours, ystyle=1, xstyle=1, background=255, color = 0, charsize = 1.5, xticks=0.0, xtickinterval=100, xtickname=['',''], ytickinterval=ticksinterval, ytickname=ticklabels
end

pro changeFitFile, fitFileEntry
common files, extension, directory, outputdirectory
common fitresults, fitdata
result=dialog_pickfile(title='Select fit results', path=outputdirectory, DIALOG_PARENT=base, DEFAULT_EXTENSION='.fit', FILTER=['*.fit'])
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
PRO plotDataContour, oldbase, plotmin, plotmax, thetamin, thetamax
common rawdata, nalpha, ntheta, alpha, twotheta, data
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
tmp = WHERE(twotheta  GT thetamin[0], count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT thetamax[0], count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR=tmp[count-1]
datatmp = data(*,indexL:indexR)
sqrtdata = ROTATE(datatmp,3)
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
contour, sqrtdata, twotheta(indexL:indexR), alpha, /FILL, levels = contours, ystyle=1, xstyle=1, background=255, color = 0, charsize = 1.5, xtitle='two theta', ytitle='azimuth', ytickinterval=ticksep
!P.REGION = [0.75, 0.5, 1., 1.]
plotlegend, contours
!P.REGION = [0., 0., 1., 1.]
end

PRO plotFitContour, plotmin, plotmax, thetamin, thetamax
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fitresults, fitdata
tmp = WHERE(twotheta  GT thetamin[0], count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT thetamax[0], count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR=tmp[count-1]
fitarray = fitdata->buildSynthethicData(alpha,twotheta(indexL:indexR), nalpha, indexR-indexL+1)
fit = ROTATE(fitarray,3)
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
contour, fit, twotheta(indexL:indexR), alpha, /FILL, levels = contours, ystyle=1, xstyle=1, background=255, color = 0, charsize = 1.5, xtitle='two theta', ytitle='azimuth', ytickinterval=ticksep
!P.REGION = [0.75, 0.5, 1., 1.]
plotlegend, contours
!P.REGION = [0., 0., 1., 1.]
end

PRO plotBothContour, plotmin, plotmax, thetamin, thetamax
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fitresults, fitdata
tmp = WHERE(twotheta  GT thetamin[0], count)
if (count eq 0) then indexL = 0 else indexL = tmp[0]
tmp = WHERE(twotheta  LT thetamax[0], count)
if (count eq 0) then indexR = N_ELEMENTS(twotheta)-1 else indexR=tmp[count-1]
fitarray = fitdata->buildSynthethicData(alpha,twotheta(indexL:indexR), nalpha, indexR-indexL+1)
fit = ROTATE(fitarray,3)
datatmp = data(*,indexL:indexR)
thedata = ROTATE(datatmp,3)
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

PRO plotSubstractContour, plotmin, plotmax, thetamin, thetamax
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
dudeplot = ROTATE(dude,3)
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

PRO plotSubstractDataContour, plotmin, plotmax, thetamin, thetamax
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
dude = ROTATE(dude,3)
datatmp = ROTATE(datatmp,3)
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

PRO compareFitWindow_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
WIDGET_CONTROL, stash.plotMin, GET_VALUE=plotmin
WIDGET_CONTROL, stash.plotMax, GET_VALUE=plotmax
WIDGET_CONTROL, stash.thetaMin, GET_VALUE=thetamin
WIDGET_CONTROL, stash.thetaMax, GET_VALUE=thetamax
CASE uval OF
    'PLOTDATA': plotDataContour, ev.TOP, plotmin, plotmax, thetamin, thetamax
    'PLOTFIT': plotFitContour, plotmin, plotmax, thetamin, thetamax
    'PLOTBOTH': plotBothContour, plotmin, plotmax, thetamin, thetamax
    'SUBSTRACT': plotSubstractContour, plotmin, plotmax, thetamin, thetamax
    'SUBSTRACTDATA': plotSubstractDataContour, plotmin, plotmax, thetamin, thetamax
    'CSCALE': xloadct
    'FITFILE': changeFitFile, stash.fitFile
    'EXIT': exitCompareFitWindow, ev.TOP
	ELSE:
ENDCASE
END

PRO compareFitWindow, parent 
; force creation of a new plot window
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fonts, titlefont, boldfont, mainfont, avFontHeight
; Load better color scale
loadct, 5
; 
mindata = min(data)
maxdata = max(data)
mintheta = min(twotheta)
maxtheta = max(twotheta)
def = 0
; base GUI
base = WIDGET_BASE(Title='Multipeak comparison window',/COLUMN, GROUP_LEADER=parent)
titleLa = WIDGET_LABEL(base, VALUE='Multipeak comparison functions', /ALIGN_CENTER, font=titlefont)
; Z Range
dataRange = WIDGET_BASE(base,COLUMN=4, FRAME=1, /ALIGN_CENTER, /GRID_LAYOUT)
dataLa1 = WIDGET_LABEL(dataRange, VALUE='Min. Z Data')
dataLa2 = WIDGET_LABEL(dataRange, VALUE=strtrim(string(mindata,/print),2))
dataLa3 = WIDGET_LABEL(dataRange, VALUE='Max. Z Data')
dataLa4 = WIDGET_LABEL(dataRange, VALUE=strtrim(string(maxdata,/print),2))
dataLa5 = WIDGET_LABEL(dataRange, VALUE='Min. in Plot')
plotMin = WIDGET_TEXT(dataRange, VALUE=strtrim(string(mindata,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
dataLa6 = WIDGET_LABEL(dataRange, VALUE='Max. in Plot')
plotMax = WIDGET_TEXT(dataRange, VALUE=strtrim(string(maxdata,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
; 2 theta Range
thetaRange = WIDGET_BASE(base,COLUMN=4, FRAME=1, /ALIGN_CENTER, /GRID_LAYOUT)
thetaLa1 = WIDGET_LABEL(thetaRange, VALUE='Min. 2theta')
thetaLa2 = WIDGET_LABEL(thetaRange, VALUE=strtrim(string(mintheta,/print),2))
thetaLa3 = WIDGET_LABEL(thetaRange, VALUE='Max. 2theta')
thetaLa4 = WIDGET_LABEL(thetaRange, VALUE=strtrim(string(maxtheta,/print),2))
thetaLa5 = WIDGET_LABEL(thetaRange, VALUE='Min. in Plot')
thetaMin = WIDGET_TEXT(thetaRange, VALUE=strtrim(string(mintheta,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
dataLa6 = WIDGET_LABEL(thetaRange, VALUE='Max. max Plot')
thetaMax = WIDGET_TEXT(thetaRange, VALUE=strtrim(string(maxtheta,/print),2), /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
; Fit
fit = WIDGET_BASE(base, /COL, FRAME=1, /ALIGN_CENTER, /GRID_LAYOUT)
fitLa = WIDGET_LABEL(fit, VALUE='Fit data')
fitEn = WIDGET_BASE(fit, /ROW, FRAME=1, /ALIGN_CENTER)
fitFile = WIDGET_TEXT(fitEn, VALUE='' ,/ALIGN_LEFT, XSIZE=30)
fitBu = WIDGET_BUTTON(fitEn, VALUE='Change', UVALUE='FITFILE')
; ACTION BUTTONS
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
; Create an anonymous structure to hold widget IDs
stash = {base:base, plotMin: plotMin, plotMax: plotMax, thetaMin: thetaMin, thetaMax: thetaMax, fitFile: fitFile} 
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'compareFitWindow', base
END
