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

PRO plotResults_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
; resize event?
if (ev.id eq ev.top) then begin
	; resize the plot
	widget_control, stash.draw, draw_xsize=ev.x, draw_ysize=ev.y
	; replot
	plotResults, stash.stash
endif
END

PRO plotResults, stash
common plotresults, def, base, draw, fitdata
; Get plot parameters
toplot = WIDGET_INFO(stash.listID, /LIST_SELECT)
if (toplot(0) eq -1) then begin
	tmp = DIALOG_MESSAGE('Select at least one spectrum!', /ERROR)
	return
endif
alphait = WIDGET_INFO(stash.alphaBut, /BUTTON_SET)
WIDGET_CONTROL, stash.alphaMinVa, GET_VALUE=alphaMin
WIDGET_CONTROL, stash.alphaMaxVa, GET_VALUE=alphaMax
; create plot window if necessary 
if (def ne 1) then begin
	base = WIDGET_BASE(Title='Results Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
	draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500) 
	newstash = {base:base, draw:draw, stash:stash} 
	WIDGET_CONTROL, base, SET_UVALUE=newstash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotResults', base
	def = 1
endif
if (WIDGET_INFO(draw, /VALID_ID) ne 1) then begin
	base = WIDGET_BASE(Title='Results Plots',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
	draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=500)  
	newstash = {base:base, draw:draw, stash:stash} 
	WIDGET_CONTROL, base, SET_UVALUE=newstash
	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'plotResults', base
	def = 1
endif 
WIDGET_CONTROL, draw, GET_VALUE = index 
WSET, index
; Fetch the data to plot
nplots = N_ELEMENTS(toplot)
x = ptrarr(nplots)
y = ptrarr(nplots)
for i=0, nplots-1 do begin
	x(i) = PTR_NEW(/ALLOCATE_HEAP)
	y(i) = PTR_NEW(/ALLOCATE_HEAP)
	*x(i) = fitdata->getDelta(toplot[i])
	if (WIDGET_INFO(stash.thetaa, /BUTTON_SET) eq 1) then begin
		*y(i) = fitdata->getTheta(toplot[i])
		ytitle = '2 theta'
	endif else if (WIDGET_INFO(stash.intensity, /BUTTON_SET) eq 1) then begin
		*y(i) = fitdata->getIntensity(toplot[i])
		ytitle = 'Intensity'
	endif else begin
    	*y(i) = fitdata->getHalfWidth(toplot[i])
		ytitle = 'half-width'
	endelse
endfor
; Prepare for the plot
if (alphait eq 0) then begin
	yalphamin = 100000000
	alphamax = -100000000
	for i=0, nplots-1 do begin
		alphamin = min([alphamin,min(*x(0))])
		alphamax = max([alphamax,max(*x(0))])
	endfor
endif
ymin = 1000000000
ymax = -1000000000
for i=0, nplots-1 do begin
	ymin = min([ymin,min(*y(i))])
	ymax = max([ymax,max(*y(i))])
endfor
; Do the plot!
!X.STYLE = 1
!Y.STYLE = 1
!P.NOERASE = 0
plot, *x(0), *y(0), background=255, color=0, xtitle = 'Azimtuth', ytitle=ytitle, xrange = [alphaMin,alphaMax], yrange=[ymin,ymax], charsize=1.5
for i=1, nplots-1 do begin
	color = i*50
	oplot, *x(i), *y(i), color=color
endfor
END

PRO chgAlpha, stash
	doit = WIDGET_INFO(stash.alphaBut, /BUTTON_SET)
	if (doit) THEN BEGIN
		WIDGET_CONTROL, stash.alphaMinVa, EDITABLE=1
		WIDGET_CONTROL, stash.alphaMaxVa, EDITABLE=1
	endif else begin
		WIDGET_CONTROL, stash.alphaMinVa, EDITABLE=0
		WIDGET_CONTROL, stash.alphaMaxVa, EDITABLE=0
	endelse
END

PRO plotResultsWindow_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE uval OF
	'ALPHA':  chgAlpha, stash 
	'LISTDELTA':
	'PLOT': plotResults, stash
    'EXIT': WIDGET_CONTROL, ev.TOP, /DESTROY
	ELSE:
ENDCASE
END

PRO plotResultsWindow, parent 
common plotresults, def, base, draw, fitdata
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common fonts, titlefont, boldfont, mainfont, avFontHeight
; Load fit data
result=dialog_pickfile(title='Select fit results', path=outputdirectory, DIALOG_PARENT=parent, FILTER=['*.fit'], /must_exist)
if (result eq '') then return
FDECOMP, result, disk, dir, name, qual, version
filename = outputdirectory + name + "." + Qual
filenameshort = name + "." + Qual
if (FILE_TEST(filename) eq 0) then begin
	tmp = DIALOG_MESSAGE(filenameshort + " is not in your output directory.", /ERROR)
	return
endif
openr, lun, filename, /get_lun
fitdata = OBJ_NEW('FitPatternObject')
a = fitdata->readFromAscii(lun)
free_lun, lun
if (a ne 1) then begin
	tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
	return
endif
; force creation of a new plot window
def = 0
; base GUI
base = WIDGET_BASE(Title='Plot results',/COLUMN, GROUP_LEADER=parent)
titleLa = WIDGET_LABEL(base, VALUE='Plot results', font=titlefont)
; top container
top = WIDGET_BASE(base,/ROW)
; list of delta values
listBase =  WIDGET_BASE(top,/COLUMN, FRAME=1)
listPeaks = fitdata->listPeaksByName()
listID = Widget_List(listBase, VALUE=listPeaks, UVALUE='LISTPEAKS', YSIZE=20, /MULTIPLE)
; Plot options
optBase = WIDGET_BASE(top,/COLUMN, FRAME=1)
; What to plot
plotLa = WIDGET_LABEL(optBase, VALUE='Plot', /ALIGN_LEFT)
plot = Widget_Base(optBase, Column=1, /Exclusive)
theta = Widget_Button(plot, Value='2 theta', UVALUE='THETA')
intensity = Widget_Button(plot, Value='Intensity', UVALUE='INTENSITY')
hwidth = Widget_Button(plot, Value='Half-Width', UVALUE='HWIDTH')
Widget_Control, theta, Set_Button=1
; Range in azimuth angles
alphaBase = WIDGET_BASE(optBase,/ROW)
tlb = Widget_Base(alphaBase, /NonExclusive)
alphaBut = Widget_Button(tlb, Value='Azimuth range', UVALUE='ALPHA')
Widget_Control, alphaBut, Set_Button=0
alphaMinVa = WIDGET_TEXT(alphaBase, VALUE='0',/ALIGN_LEFT, XSIZE=10)
alpharangeLa = WIDGET_LABEL(alphaBase, VALUE='to', /ALIGN_LEFT)
alphaMaxVa = WIDGET_TEXT(alphaBase, VALUE='360',/ALIGN_LEFT, XSIZE=10)
; ACTION BUTTONS
butBase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
plotBut = WIDGET_BUTTON(butBase, VALUE='Plot', UVALUE='PLOT',xsize=80)
closeBut = WIDGET_BUTTON(butBase, VALUE='Close', UVALUE='EXIT',xsize=80)
; Create an anonymous structure to hold widget IDs
stash = {base:base, listID: listID, alphaBut:alphaBut, alphaMinVa:alphaMinVa, alphaMaxVa:alphaMaxVa, thetaa: theta, intensity: intensity, hwidth:hwidth}
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'plotResultsWindow', base
END
