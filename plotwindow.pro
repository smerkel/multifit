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


; ********************************* PLOTITFUNCTION ***************

PRO plotit_event, ev
	WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
	; resize event?
	if (ev.id eq ev.top) then begin
		; resize the plot
		widget_control, stash.draw, draw_xsize=ev.x, draw_ysize=ev.y
		; replot
		plotit, stash.stash
	endif
END

PRO plotit, stash
	common rawdata, nalpha, ntheta, alpha, twotheta, data
	common plotit, def, base, draw
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
		base = WIDGET_BASE(Title='Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
		draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500) 
		stash = {base:base, draw:draw, stash:stash} 
		WIDGET_CONTROL, base, SET_UVALUE=stash
		WIDGET_CONTROL, base, /REALIZE
		XMANAGER, 'plotit', base
		def = 1
	endif
	if (WIDGET_INFO(draw, /VALID_ID) ne 1) then begin
		base = WIDGET_BASE(Title='Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
		draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500)  
		stash = {base:base, draw:draw, stash:stash} 
		WIDGET_CONTROL, base, SET_UVALUE=stash
		WIDGET_CONTROL, base, /REALIZE
		XMANAGER, 'plotit', base
		def = 1
	endif
	WIDGET_CONTROL, draw, GET_VALUE = index 
	WSET, index
	plotdata, toplot, stretchit, float(stretch), thetait, thetaMin, thetaMax, legendeskipit, legendeskip
END

; ********************************* PLOT WINDOW GUI EVENT PROCESSING ***************

PRO chgStretch, stash
	doit = WIDGET_INFO(stash.stretchBut, /BUTTON_SET)
	if (doit) THEN BEGIN
		WIDGET_CONTROL, stash.stretchVa, EDITABLE=1
	endif else begin
		WIDGET_CONTROL, stash.stretchVa, EDITABLE=0
	endelse
END

PRO chgLegend, stash
	doit = WIDGET_INFO(stash.legendeBut, /BUTTON_SET)
	if (doit) THEN BEGIN
		WIDGET_CONTROL, stash.legendeVa, EDITABLE=1
	endif else begin
		WIDGET_CONTROL, stash.legendeVa, EDITABLE=0
	endelse
END

PRO chgTheta, stash
	doit = WIDGET_INFO(stash.thetaBut, /BUTTON_SET)
	if (doit) THEN BEGIN
		WIDGET_CONTROL, stash.thetaMinVa, EDITABLE=1
		WIDGET_CONTROL, stash.thetaMaxVa, EDITABLE=1
	endif else begin
		WIDGET_CONTROL, stash.thetaMinVa, EDITABLE=0
		WIDGET_CONTROL, stash.thetaMaxVa, EDITABLE=0
	endelse
END

; ****************************************** MAIN PLOT WINDOW GUI ***************

PRO plotWindow_event, ev
	; Get the 'stash' structure.
	WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
	WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
	CASE uval OF
		'STRETCH': chgStretch, stash
		'LEGENDE': chgLegend, stash
		'THETA':  chgTheta, stash
		'LISTDELTA':
		'PLOT': plotit, stash
		'EXIT': WIDGET_CONTROL, ev.TOP, /DESTROY
	ENDCASE
END

PRO plotWindow, parent
	common datainfo, filenames, alphastart, alphaend, intervalle, date
	common rawdata, nalpha, ntheta, alpha, twotheta, data
	common plotit, def, base, draw
	
	; force creation of a new plot window
	def = 0
	; base GUI
	base = WIDGET_BASE(Title='Multipeak plot window',/COLUMN, GROUP_LEADER=parent)
	titleLa = WIDGET_LABEL(base, VALUE='Multipeak plotting functions')
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
	; ACTION BUTTONS
	butBase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER)
	plotBut = WIDGET_BUTTON(butBase, VALUE='Plot', UVALUE='PLOT')
	closeBut = WIDGET_BUTTON(butBase, VALUE='Close', UVALUE='EXIT')
	; Create an anonymous structure to hold widget IDs
	stash = {base:base, listID: listID, stretchBut:stretchBut, stretchVa:stretchVa, $
			legendeBut:legendeBut, legendeVa:legendeVa, $
			thetaBut:thetaBut, thetaMinVa:thetaMinVa, thetaMaxVa:thetaMaxVa} 
	
	WIDGET_CONTROL, base, SET_UVALUE=stash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotWindow', base
END
