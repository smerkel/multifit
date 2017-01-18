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


; ********************************* PLOTITFUNCTION ***************

PRO plotit, stash
	common rawdata, nalpha, ntheta, alpha, twotheta, data
	; Get plot parameters
	toplot = WIDGET_INFO(stash.listID, /LIST_SELECT)
	if (toplot(0) eq -1) then begin
		tmp = DIALOG_MESSAGE('Select at least one spectrum!', /ERROR)
		return
	endif
	
	; Set up CATCH error handler for conversion issues
	Catch, theError
	;IF theError NE 0 THEN BEGIN
	;  Catch, /Cancel
	;  bad_io:
	;    tmp = DIALOG_MESSAGE('Some numbers are not correct in your settings', /ERROR)
	;    return
	;ENDIF
	; Set up file I/O error handling.
	;ON_IOError, bad_io
	
	separate_Y = WIDGET_INFO(stash.stretchBut, /BUTTON_SET)
	WIDGET_CONTROL, stash.stretchVa, GET_VALUE=sep
	separation = float(sep[0])
	addlegend = WIDGET_INFO(stash.legendeBut, /BUTTON_SET)
	stack = WIDGET_INFO(stash.stackBut, /BUTTON_SET)
	addnav = WIDGET_INFO(stash.navBut, /BUTTON_SET)
	base = stash.base
	rangeSet = WIDGET_INFO(stash.thetaBut, /BUTTON_SET)
	if (rangeSet eq 1) then begin
		WIDGET_CONTROL, stash.thetaMinVa, GET_VALUE=sep
		tthetamin = float(sep[0])
		WIDGET_CONTROL, stash.thetaMaxVa, GET_VALUE=sep
		tthetamax = float(sep[0])
	endif else begin
		tthetamin = min(twotheta)
		tthetamax = max(twotheta)
	endelse
	
	if (stack eq 1) then nplots = 1 else nplots=n_elements(toplot)
	xdata = twotheta
	ydata= fltarr(nplots,n_elements(twotheta))
	leg = strarr(nplots)
	
	if (stack eq 0) then begin
		offset = 0.
		for i=0,nplots-1 do begin
			if ((i gt 0) and (separate_Y eq 1)) then offset = offset + 1.0*separation
			ydata[i,*] = data[toplot[i],*] + offset
			leg[i] = alpha[toplot[i]]
		endfor
	endif else begin
		ydata[0,*] = data[toplot[0],*]
		for i=1,n_elements(toplot)-1 do ydata[0,*] += data[toplot[i],*]
		leg[0] = "Staked data"
		addlegend = 0
	endelse
	if (addnav eq 1) then begin ; if navigation, we force the legend
		plotinteractive1D, base, xdata, ydata, xlabel='2 theta', ylabel='Intensity', legend = leg, range=[tthetamin, tthetamax], nav = 1
	endif else if (addlegend eq 1) then begin
		plotinteractive1D, base, xdata, ydata, xlabel='2 theta', ylabel='Intensity', legend = leg, range=[tthetamin, tthetamax]
	endif else begin
		plotinteractive1D, base, xdata, ydata, xlabel='2 theta', ylabel='Intensity', range=[tthetamin, tthetamax]
	endelse
	; plotdata, toplot, stretchit, float(stretch), thetait, thetaMin, thetaMax, legendeskipit, legendeskip
END


; ****************************************** MAIN PLOT WINDOW GUI ***************

PRO plotWindow_event, ev
	; Get the 'stash' structure.
	WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
	WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
	CASE uval OF
		'PLOT': plotit, stash
		'EXIT': WIDGET_CONTROL, ev.TOP, /DESTROY
		else: 
	ENDCASE
END

PRO plotWindow, parent
	common datainfo, filenames, alphastart, alphaend, intervalle, date
	common rawdata, nalpha, ntheta, alpha, twotheta, data
	
	; force creation of a new plot window
	def = 0
	; base GUI
	base = WIDGET_BASE(Title='Multipeak plot window',/COLUMN, GROUP_LEADER=parent)
	titleLa = WIDGET_LABEL(base, VALUE='Multipeak plotting functions')
	; top container
	top = WIDGET_BASE(base,/ROW)
	; display default parameters
	dataBase =  WIDGET_BASE(top,/COLUMN, FRAME=1)
	alphaminLabel = 'Min azimuth: ' + STRTRIM(STRING(alphastart,/PRINT),1)
	alphaminLa = WIDGET_LABEL(dataBase, VALUE=alphaminLabel, /ALIGN_LEFT)
	alphamaxLabel = 'Max azimuth: ' + STRTRIM(STRING(alphaend,/PRINT),1)
	alphamaxLa = WIDGET_LABEL(dataBase, VALUE=alphamaxLabel, /ALIGN_LEFT)
	intervalLabel = 'Interval: ' + STRTRIM(STRING(intervalle,/PRINT),1)
	intervalLa = WIDGET_LABEL(dataBase, VALUE=intervalLabel, /ALIGN_LEFT)
	
	; list of azimuth values
	listBase =  WIDGET_BASE(top,/COLUMN, FRAME=1)
	alphaText = strarr(nalpha)
	alphaU = intarr(nalpha)
	for i=0, nalpha[0]-1 do begin
		alphaText(i) = 'Az = ' + STRTRIM(STRING(alpha(i),/PRINT))
		alphaU(i) = i
	endfor
	listID = Widget_List(listBase, VALUE=alphaText, UVALUE='LISTDELTA', YSIZE=20, /MULTIPLE)
	
	; Plot options
	optBase = WIDGET_BASE(top,/COLUMN, FRAME=1)
	; streching
	stretchBase = WIDGET_BASE(optBase,/ROW)
	stretchCh = Widget_Base(stretchBase, /NonExclusive)
	stretchBut = Widget_Button(stretchCh, Value='Y-Separation', UVALUE='DUMMY')
	Widget_Control, stretchBut, Set_Button=0
	stretchVa = WIDGET_TEXT(stretchBase, VALUE='100',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
	; legende
	legendeBase = WIDGET_BASE(optBase,/ROW)
	legendeCh = Widget_Base(legendeBase, /NonExclusive)
	legendeBut = Widget_Button(legendeCh, Value='Include legend', UVALUE='DUMMY')
	Widget_Control, legendeBut, Set_Button=0
	; stacking
	stackBase = WIDGET_BASE(optBase,/ROW)
	stackCh = Widget_Base(stackBase, /NonExclusive)
	stackBut = Widget_Button(stackCh, Value='Stack datasets', UVALUE='DUMMY')
	Widget_Control, stackBut, Set_Button=0
	; navigation
	navBase = WIDGET_BASE(optBase,/ROW)
	navCh = Widget_Base(navBase, /NonExclusive)
	navBut = Widget_Button(navCh, Value='Navigate datasets', UVALUE='DUMMY')
	Widget_Control, navBut, Set_Button=0
	; range in 2theta
	mintt = strtrim(string(min(twotheta)),2)
	maxtt = strtrim(string(max(twotheta)),2)
	thetaBase = WIDGET_BASE(optBase,/ROW)
	tlb = Widget_Base(thetaBase, /NonExclusive)
	thetaBut = Widget_Button(tlb, Value='2theta range', UVALUE='THETA')
	Widget_Control, thetaBut, Set_Button=0
	thetaMinVa = WIDGET_TEXT(thetaBase, VALUE=mintt,/ALIGN_LEFT, XSIZE=10, /EDITABLE)
	thetarangeLa = WIDGET_LABEL(thetaBase, VALUE='to', /ALIGN_LEFT)
	thetaMaxVa = WIDGET_TEXT(thetaBase, VALUE=maxtt,/ALIGN_LEFT, XSIZE=10, /EDITABLE)
	; ACTION BUTTONS
	butBase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER)
	plotBut = WIDGET_BUTTON(butBase, VALUE='Plot', UVALUE='PLOT')
	closeBut = WIDGET_BUTTON(butBase, VALUE='Close', UVALUE='EXIT')
	; Create an anonymous structure to hold widget IDs
	stash = {base:base, listID: listID, stretchBut:stretchBut, stretchVa:stretchVa, $
			legendeBut:legendeBut, stackBut:stackBut,  navBut:navBut, thetaBut:thetaBut, thetaMinVa:thetaMinVa, thetaMaxVa:thetaMaxVa} 
	WIDGET_CONTROL, base, SET_UVALUE=stash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotWindow', base
END
