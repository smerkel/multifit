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


PRO doplotcomparison1d, include, stretchit, stretch, thetait, thetaMin, thetaMax, legendeskipit, legendeskip
common fitresults, fitdata
common rawdata, nalpha, ntheta, alpha, twotheta, data
common datainfo, filenames, alphastart, alphaend, intervalle, date
if (not keyword_set(titre)) then titre=filenames
if (not keyword_set(offset)) then offset = 0.0
if (not keyword_set(yplotmax)) then yplotmax = -100000
if (not keyword_set(yplotmin)) then yplotmin = 0
if (not keyword_set(charsize)) then charsize = 1
if (not keyword_set(charthick)) then charthick = 1
if (stretchit eq 0) then begin
    sepfactor = float(1)
endif else begin
    sepfactor = 1.0*float(stretch[0])
endelse
if (legendeskipit eq 0) then begin
    legendesep = 1
endif else begin
    legendesep = fix(legendeskip)
endelse
if (thetait eq 0) then begin
    thetamin = twotheta(0)
    thetamax = twotheta(ntheta-1)
endif else begin
    thetamin = float(thetamin)
    thetamax = float(thetamax)
endelse
xStart = getIndex(thetamin,  twotheta, ntheta)
xEnd = getIndex(thetamax,  twotheta, ntheta)
; use INCLUDE to select datasets to actually plot
; define legende for each plot
; add an offset in Y so all plots do not overlapp
dataplot = fltarr(nalpha, xEnd-xStart+1)
ntoplot = 0
leg = intarr(nalpha)
offsets = fltarr(nalpha)
for i=0, (nalpha-1) do begin
    test = where(include eq i)
    if (test(0) ge 0) then begin
        if (ntoplot gt 0) then offset = offset + 1.0*max(dataplot(ntoplot-1,*)-offset)/sepfactor
        dataplot(ntoplot,*) = data(i,xStart:xEnd) + offset - min(data(i,xStart:xEnd))
		offsets[i] = offset-min(data(i,xStart:xEnd))
        leg(ntoplot) = alpha(i)
        ntoplot = ntoplot + 1
    endif
endfor
; basic plot characteritics (scale...)
max = max(dataplot(*,*))
plotYmax = max
if (yplotmax gt 0) then plotYmax = yplotmax
plotXmin = thetamin
plotXmax = thetamax+0.1*(1.+.5*charsize)*(thetamax-thetamin)
texte = ' ' + strcompress(''+leg(0),/REMOVE_ALL)
; Plot datasets
plot, twotheta(xStart:xEnd), dataplot(0,*), background=255, color=0, $ 
      yrange = [yplotmin,plotYmax], xrange = [plotXmin,plotXmax], $
	  xtitle = '2 theta', ytitle='intensity', title = titre, ystyle=1, $
	  xstyle=1, charsize=charsize, $
	  ytickname=[" ", " ", " ", " ", " ", " ", " ", " ", " "], $
	  charthick=charthick, linestyle=1, thick=4
plotYmin = 0
xyouts, thetamax, dataplot(0,xEnd-xStart), texte, color=0, $
      charsize=charsize, charthick=charthick
index = 0
legendeindex = 0
for i=1, (ntoplot-1) do begin
	index = index + 1
	texte = ' ' + strcompress(''+leg(i),/REMOVE_ALL)
	oplot, twotheta(xStart:xEnd), dataplot(i,*) , color=0, linestyle=1, thick=4
	legendeindex = legendeindex + 1
    if (legendeindex eq legendesep) then begin
		xyouts, thetamax, dataplot(i,xEnd-xStart), ''+texte, color=0, $
		     charsize=charsize, charthick=charthick
        legendeindex = 0
	endif
endfor
; add the fits
thisntheta = xEnd-xStart+1
for i=0, (nalpha-1) do begin
    test = where(include eq i)
    if (test(0) ge 0) then begin
		; create a simulated dataset...
		fit = fitdata->buildSynthethicData(alpha[i], twotheta[xStart:xEnd], 1, thisntheta)
		; add the offset
		fit = fit + offsets[i]
		; plot!
		oplot, twotheta(xStart:xEnd), fit(0,*) , color=100
	endif
endfor
end

PRO plotcomparison1D_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
; resize event?
if (ev.id eq ev.top) then begin
	; resize the plot
	widget_control, stash.draw, draw_xsize=ev.x, draw_ysize=ev.y
	; replot
	plotcomparison1D, stash.stash
endif
END

PRO plotcomparison1D, stash
common rawdata, nalpha, ntheta, alpha, twotheta, data
common plotcomparison1D, def, base, draw
common fitresults, fitdata
; Get plot parameters
toplot = WIDGET_INFO(stash.listID, /LIST_SELECT)
if (toplot(0) eq -1) then begin
	tmp = DIALOG_MESSAGE('Select at least one spectrum!', /ERROR)
	return
endif
stretchit = WIDGET_INFO(stash.stretchBut, /BUTTON_SET)
WIDGET_CONTROL, stash.stretchVa, GET_VALUE=stretch
legendeskipit = WIDGET_INFO(stash.legendeBut, /BUTTON_SET)
WIDGET_CONTROL, stash.legendeVa, GET_VALUE=legendeskip
thetait = WIDGET_INFO(stash.thetaBut, /BUTTON_SET)
WIDGET_CONTROL, stash.thetaMinVa, GET_VALUE=thetaMin
WIDGET_CONTROL, stash.thetaMaxVa, GET_VALUE=thetaMax
; create plot window if necessary 
if (def ne 1) then begin
	base = WIDGET_BASE(Title='Comparison Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
	draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500) 
	stash = {base:base, draw:draw, stash:stash} 
	WIDGET_CONTROL, base, SET_UVALUE=stash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotcomparison1D', base
	def = 1
endif
if (WIDGET_INFO(draw, /VALID_ID) ne 1) then begin
	base = WIDGET_BASE(Title='Comparison Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
	draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500)  
	stash = {base:base, draw:draw, stash:stash} 
	WIDGET_CONTROL, base, SET_UVALUE=stash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotcomparison1D', base
	def = 1
endif 
WIDGET_CONTROL, draw, GET_VALUE = index 
WSET, index
doplotcomparison1d, toplot, stretchit, float(stretch), thetait, thetaMin, thetaMax, legendeskipit, legendeskip
END

PRO plotdynamiccomparison1D, stash
common rawdata, nalpha, ntheta, alpha, twotheta, data
common plotcomparison1D, def, base, draw
common fitresults, fitdata
; Get plot parameters
toplot = WIDGET_INFO(stash.listID, /LIST_SELECT)
if (toplot(0) eq -1) then begin
	tmp = DIALOG_MESSAGE('Select at least one spectrum!', /ERROR)
	return
endif
stretchit = WIDGET_INFO(stash.stretchBut, /BUTTON_SET)
WIDGET_CONTROL, stash.stretchVa, GET_VALUE=stretch
legendeskipit = WIDGET_INFO(stash.legendeBut, /BUTTON_SET)
WIDGET_CONTROL, stash.legendeVa, GET_VALUE=legendeskip
thetait = WIDGET_INFO(stash.thetaBut, /BUTTON_SET)
WIDGET_CONTROL, stash.thetaMinVa, GET_VALUE=thetaMin
WIDGET_CONTROL, stash.thetaMaxVa, GET_VALUE=thetaMax
; create plot window if necessary 
if (def ne 1) then begin
	base = WIDGET_BASE(Title='Comparison Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
	draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500) 
	stash = {base:base, draw:draw, stash:stash} 
	WIDGET_CONTROL, base, SET_UVALUE=stash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotcomparison1D', base
	def = 1
endif
if (WIDGET_INFO(draw, /VALID_ID) ne 1) then begin
	base = WIDGET_BASE(Title='Comparison Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
	draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500)  
	stash = {base:base, draw:draw, stash:stash} 
	WIDGET_CONTROL, base, SET_UVALUE=stash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotcomparison1D', base
	def = 1
endif 
WIDGET_CONTROL, draw, GET_VALUE = index 
WSET, index
; instead of plotting everything in the same plot, we just plot it in a 
; row...
for i=0,N_ELEMENTS(toplot)-1 do begin
	doplotcomparison1d, toplot[i], stretchit, float(stretch), thetait, thetaMin, thetaMax, legendeskipit, legendeskip
	wait, 0.3
endfor
END

PRO exitCompareFit1DWindow, base
WIDGET_CONTROL, base, /DESTROY
end

PRO compareFit1DWindow_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE uval OF
	'STRETCH': chgStretch, stash ; routine from plotwindow.pro
	'LEGENDE': chgLegend, stash ; routine from plotwindow.pro
	'THETA':  chgTheta, stash ; routine from plotwindow.pro
	'LISTDELTA':
	'PLOT': plotcomparison1D, stash
	'DYNAMICPLOT': plotdynamiccomparison1D, stash
    'FITFILE': changeFitFile, stash.fitFile ; routine from compareFitWindow.pro
    'EXIT': exitCompareFit1DWindow, ev.TOP
	ELSE:
ENDCASE
END

PRO compareFit1DWindow, parent 
common plotcomparison1D, def, base, draw
common datainfo, filenames, alphastart, alphaend, intervalle, date
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fonts, titlefont, boldfont, mainfont, avFontHeight
; force creation of a new plot window
def = 0
; base GUI
base = WIDGET_BASE(Title='2-D Fit comparison window',/COLUMN, GROUP_LEADER=parent)
titleLa = WIDGET_LABEL(base, VALUE='2-D Fit comparison functions', font=titlefont)
; top container
top = WIDGET_BASE(base,/ROW)
; display default parameters
dataBase =  WIDGET_BASE(top,/COLUMN, FRAME=1)
alphaminLabel = 'Delta min: ' + STRTRIM(STRING(alphastart,/PRINT),1)
alphaminLa = WIDGET_LABEL(dataBase, VALUE=alphaminLabel, /ALIGN_LEFT)
alphamaxLabel = 'Delta max: ' + STRTRIM(STRING(alphaend,/PRINT),1)
alphamaxLa = WIDGET_LABEL(dataBase, VALUE=alphamaxLabel, /ALIGN_LEFT)
intervalLabel = 'Interval: ' + STRTRIM(STRING(intervalle,/PRINT),1)
intervalLa = WIDGET_LABEL(dataBase, VALUE=intervalLabel, /ALIGN_LEFT)
; list of delta values
listBase =  WIDGET_BASE(top,/COLUMN, FRAME=1)
alphaText = strarr(nalpha)
alphaU = intarr(nalpha)
for i=0, nalpha[0]-1 do begin
	alphaText(i) = 'delta = ' + STRTRIM(STRING(alpha(i),/PRINT))
	alphaU(i) = i
endfor
listID = Widget_List(listBase, VALUE=alphaText, UVALUE='LISTDELTA', YSIZE=20, /MULTIPLE)
; Plot options
optBase = WIDGET_BASE(top,/COLUMN, FRAME=1)
; streching
stretchBase = WIDGET_BASE(optBase,/ROW)
stretchCh = Widget_Base(stretchBase, /NonExclusive)
stretchBut = Widget_Button(stretchCh, Value='Stretch', UVALUE='STRETCH')
Widget_Control, stretchBut, Set_Button=0
stretchVa = WIDGET_TEXT(stretchBase, VALUE='1',/ALIGN_LEFT, XSIZE=10)
; legendeskip
legendeBase = WIDGET_BASE(optBase,/ROW)
legendeCh = Widget_Base(legendeBase, /NonExclusive)
legendeBut = Widget_Button(legendeCh, Value='Legende skip', UVALUE='LEGENDE')
Widget_Control, legendeBut, Set_Button=0
legendeVa = WIDGET_TEXT(legendeBase, VALUE='1',/ALIGN_LEFT, XSIZE=10)
; range in 2theta
thetaBase = WIDGET_BASE(optBase,/ROW)
tlb = Widget_Base(thetaBase, /NonExclusive)
thetaBut = Widget_Button(tlb, Value='2theta range', UVALUE='THETA')
Widget_Control, thetaBut, Set_Button=0
thetaMinVa = WIDGET_TEXT(thetaBase, VALUE='0',/ALIGN_LEFT, XSIZE=10)
thetarangeLa = WIDGET_LABEL(thetaBase, VALUE='to', /ALIGN_LEFT)
thetaMaxVa = WIDGET_TEXT(thetaBase, VALUE='20',/ALIGN_LEFT, XSIZE=10)
; Fit
fit = WIDGET_BASE(base, /COL, FRAME=1, /ALIGN_CENTER, /GRID_LAYOUT)
fitLa = WIDGET_LABEL(fit, VALUE='Fit data')
fitEn = WIDGET_BASE(fit, /ROW, FRAME=1, /ALIGN_CENTER)
fitFile = WIDGET_TEXT(fitEn, VALUE='' ,/ALIGN_LEFT, XSIZE=30)
fitBu = WIDGET_BUTTON(fitEn, VALUE='Change', UVALUE='FITFILE')
; ACTION BUTTONS
butBase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER)
plotBut = WIDGET_BUTTON(butBase, VALUE='Plot', UVALUE='PLOT',xsize=80)
plotBut = WIDGET_BUTTON(butBase, VALUE='Dynamic Plot', UVALUE='DYNAMICPLOT',xsize=80)
closeBut = WIDGET_BUTTON(butBase, VALUE='Close', UVALUE='EXIT',xsize=80)
; Create an anonymous structure to hold widget IDs
stash = {base:base, listID: listID, stretchBut:stretchBut, stretchVa:stretchVa, $
			legendeBut:legendeBut, legendeVa:legendeVa, $
			thetaBut:thetaBut, thetaMinVa:thetaMinVa, thetaMaxVa:thetaMaxVa, fitFile:fitFile}
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'compareFit1DWindow', base
END
