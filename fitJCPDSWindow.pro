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


; *********** GUI THAT CONTROL AUTOMATIC FITTING BASED ON JCPDS CARD***************
; In those fitting procedures, we fit an image based on peak positions 
; defined in a JCPDS card
; There is a gui to select the peaks to fit and the pressure to be used
;


; ****************************************************************
; Try to fit the the data at all angles using selected peaks
; from the JCPDS card
; Uses the active dataset in the main window
; Uses the plot window defined in plotwindow.pro
;
pro fitautoJCPDS, oldbase, jcpds, fitselect, pressureentry, widthentry, wfactorentry
common rawdata, nalpha, ntheta, alpha, twotheta, data
common experiment, wavelength, detectordistance, experimenttype
common plotit, def, base, draw
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
; getting parameters
WIDGET_CONTROL, pressureentry, GET_VALUE=pressure
WIDGET_CONTROL, widthentry, GET_VALUE=width
WIDGET_CONTROL, wfactorentry, GET_VALUE=wfactor
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
; getting peak list
peaklist = jcpds->peaklist(fitselect, pressure, wavelength)
nn = size(peaklist,/DIMENSIONS)
npeaks = nn[0]
; Preparing fit sub-pattern
nzones = 0;
startZ = fltarr(50)
endZ = fltarr(50)
npeaksZ = intarr(50)
peaksZ = fltarr(50,50)
peakLabels = strarr(50,50)
for i=0,npeaks-1 do begin
	tt = float(peaklist(i,1))
	if (nzones eq 0) then begin
		startZ[0] = tt-width/2.
		endZ[0] = tt+width/2.
		peaksZ[0,0] = tt
		peakLabels[0,0] = peaklist(i,0)
		npeaksZ[0] = 1
		nzones += 1
	endif else begin
		if (tt-width/2. lt endZ[nzones-1]) then begin
			endZ[nzones-1] = tt+width/2.
			peaksZ[nzones-1,npeaksZ[nzones-1]] = tt
			peakLabels[nzones-1,npeaksZ[nzones-1]] = peaklist(i,0)
			npeaksZ[nzones-1] += 1
		endif else begin
			startZ[nzones] = tt-width/2.
			endZ[nzones] = tt+width/2.
			peaksZ[nzones,0] = tt
			npeaksZ[nzones] = 1
			peakLabels[nzones,0] = peaklist(i,0)
			nzones += 1
		endelse
	endelse
endfor
; Prepare fit
; We assume that the data is available over the all delta range
; we assume that peak intensity is the intensity at where we find the peak
fitobject = OBJ_NEW('fitPatternObject')
test = fitobject->fromJCPDS(nzones, startZ, endZ, peaksZ, npeaksZ, width, alpha, twotheta, data[0,*])
if (test ne 1) then begin
	test = DIALOG_MESSAGE(test, /error)
	return
endif
; optimize the guess
fitobject->optimizeJCPDSWithCurrentDataset, width, wfactor, peakLabels
; do we save the fit?
Result = DIALOG_MESSAGE( "Fit is finished. Save in a file?", /QUESTION)
if (Result eq "Yes") then begin
	filename = pickfile_dir_ext(outputdirectory, 'fit', parent=base)
	if (filename ne '') then begin
		openw, lun, filename, /get_lun
		a = fitobject->saveToAscii(lun)
		free_lun, lun
		if (a ne 1) then begin
			tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
			return
		endif
	endif
endif
end


; ****************************************************************
; Try to fit the the data at the first angle using selected peaks
; from the JCPDS card
; Uses the active dataset in the main window
; Uses the plot window defined in plotwindow.pro
;
pro testJCPDSFit, oldbase, jcpds, fitselect, pressureentry, widthentry, wfactorentry
common rawdata, nalpha, ntheta, alpha, twotheta, data
common experiment, wavelength, detectordistance, experimenttype
common plotit, def, base, draw
; getting parameters
WIDGET_CONTROL, pressureentry, GET_VALUE=pressure
WIDGET_CONTROL, widthentry, GET_VALUE=width
WIDGET_CONTROL, wfactorentry, GET_VALUE=wfactor
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
; getting peak list
peaklist = jcpds->peaklist(fitselect, pressure, wavelength)
nn = size(peaklist,/DIMENSIONS)
npeaks = nn[0]
; Preparing fit sub-pattern
nzones = 0;
startZ = fltarr(50)
endZ = fltarr(50)
npeaksZ = intarr(50)
peaksZ = fltarr(50,50)
for i=0,npeaks-1 do begin
	tt = float(peaklist(i,1))
	if (nzones eq 0) then begin
		startZ[0] = tt-width/2.
		endZ[0] = tt+width/2.
		peaksZ[0,0] = tt
		npeaksZ[0] = 1
		nzones += 1
	endif else begin
		if (tt-width/2. lt endZ[nzones-1]) then begin
			endZ[nzones-1] = tt+width/2.
			peaksZ[nzones-1,npeaksZ[nzones-1]] = tt
			npeaksZ[nzones-1] += 1
		endif else begin
			startZ[nzones] = tt-width/2.
			endZ[nzones] = tt+width/2.
			peaksZ[nzones,0] = tt
			npeaksZ[nzones] = 1
			nzones += 1
		endelse
	endelse
endfor
; Prepare fit
; We assume that the data is available over the all delta range
; we assume that peak intensity is the intensity at where we find the peak
fitobject = OBJ_NEW('fitPatternObject')
test = fitobject->fromJCPDS(nzones, startZ, endZ, peaksZ, npeaksZ, width, alpha, twotheta, data[0,*])
if (test ne 1) then begin
	test = DIALOG_MESSAGE(test, /error)
	return
endif
; optimize the guess
fitobject->optimizeJCPDSWithCurrentDatasetFirstDelta, width, wfactor
; prepare plot to show the final fit...
x = twotheta
y = data[0,*]
azimuth = alpha[0]
minX = min(x)
maxX = max(x)
maxX = maxX + 0.1*(maxX-minX)
realMinY = min(y)
realMaxY = max(y)
diff = realMaxY-realMinY
maxY = realMaxY + 0.1*diff
minY = realMinY - 0.2*diff
xleg = maxX-0.3*(maxX-minX)
yleg1 = maxY-0.07*(maxY-minY)
yleg2 = min(y)-0.01*diff
plot, x, y, background=255, color = 0, xrange=[minX,maxX], yrange=[minY,maxY], ystyle=1, xstyle=1
; oplot, *xtmp(peak), *ytmp(peak), color = 100
; oplot, *xtmp(peak), *yfit(peak), color = 200
xyouts, xleg, yleg1, "az = "+STRING(azimuth,format='(F6.1)'),  color = 0, charsize=2, charthick=3, font=1
yleg1 -= 0.07*(maxY-minY)
xyouts, xleg, yleg1, "P = "+STRING(pressure,format='(F4.1)')+" GPa",  color = 0, charsize=2, charthick=3, font=1
for i=0,npeaks-1 do begin
	label = peaklist(i,0)
	tt = float(peaklist(i,1))
	xx = [tt, tt]
	yy = [realMinY,realMaxY-diff*0.3]
	oplot, xx, yy, color = 100
	xx = [tt-width/2.,tt+width/2.]
	tmp = realMinY+diff/5.
	yy = [tmp,tmp]
	oplot, xx, yy, color = 100, thick=2
	xyouts, tt, yleg2, label,  color = 100, charsize=1, charthick=1, alignment=1., orientation=90., font=1
endfor
fitY = fitobject->buildSynthethicData(alpha[0], twotheta, 1, N_ELEMENTS(twotheta))
oplot, x, fitY, color=100
; xyouts, xleg, yleg3, "Fitting, peak "+STRING(peak,format='(I3)'), color = 0, charsize=2, charthick=3
wait, 0.3
end


; ****************************************************************
; Plot the data at the first angle and the location of selected peaks
; from the JCPDS card
; Uses the active dataset in the main window
; Uses the plot window defined in plotwindow.pro
;
pro plotJCPDSData, oldbase, jcpds, fitselect, pressureentry, widthentry, wfactor
common rawdata, nalpha, ntheta, alpha, twotheta, data
common experiment, wavelength, detectordistance, experimenttype
common plotit, def, base, draw
; getting parameters
WIDGET_CONTROL, pressureentry, GET_VALUE=pressure
WIDGET_CONTROL, widthentry, GET_VALUE=width
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
; getting peak list
peaklist = jcpds->peaklist(fitselect, pressure, wavelength)
nn = size(peaklist,/DIMENSIONS)
npeaks = nn[0]
; prepare plot
x = twotheta
y = data[0,*]
azimuth = alpha[0]
minX = min(x)
maxX = max(x)
maxX = maxX + 0.1*(maxX-minX)
realMinY = min(y)
realMaxY = max(y)
diff = realMaxY-realMinY
maxY = realMaxY + 0.1*diff
minY = realMinY - 0.2*diff
xleg = maxX-0.3*(maxX-minX)
yleg1 = maxY-0.07*(maxY-minY)
yleg2 = min(y)-0.01*diff
plot, x, y, background=255, color = 0, xrange=[minX,maxX], yrange=[minY,maxY], ystyle=1, xstyle=1
; oplot, *xtmp(peak), *ytmp(peak), color = 100
; oplot, *xtmp(peak), *yfit(peak), color = 200
xyouts, xleg, yleg1, "az = "+STRING(azimuth,format='(F6.1)'),  color = 0, charsize=2, charthick=3, font=1
yleg1 -= 0.07*(maxY-minY)
xyouts, xleg, yleg1, "P = "+STRING(pressure,format='(F4.1)')+" GPa",  color = 0, charsize=2, charthick=3, font=1
for i=0,npeaks-1 do begin
	label = peaklist(i,0)
	tt = float(peaklist(i,1))
	xx = [tt, tt]
	yy = [realMinY,realMaxY-diff*0.3]
	oplot, xx, yy, color = 100
	xx = [tt-width/2.,tt+width/2.]
	tmp = realMinY+diff/5.
	yy = [tmp,tmp]
	oplot, xx, yy, color = 100, thick=2
	xyouts, tt, yleg2, label,  color = 100, charsize=1, charthick=1, alignment=1., orientation=90., font=1
endfor
; xyouts, xleg, yleg3, "Fitting, peak "+STRING(peak,format='(I3)'), color = 0, charsize=2, charthick=3
wait, 0.3
end

; ****************************************************************
; Resize JCPDS main window
;

PRO resizeJCPDS, base, stash
	widget_control, base, TLB_GET_SIZE=size
	; resizing top part
	tmpsize = widget_info(stash.param, /GEOMETRY)
	tmpsize2 = widget_info(stash.buttons, /GEOMETRY)
	ysize = size[1]-(tmpsize2.SCR_YSIZE + (2*tmpsize2.MARGIN))-(tmpsize.SCR_YSIZE + (2*tmpsize.MARGIN))
	; Need to finish this... We want to resize the peak list, actually...
end

; ****************************************************************
; Event manager, processes events sent by the main gui window
;
PRO fitJCPDSWindowMain_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.ID OF
	stash.base: resizeJCPDS, stash.base, stash
	else: begin
		CASE uval OF
			'PLOT': plotJCPDSData, stash.base, stash.jcpds, stash.fitselect, stash.pressure, stash.width, stash.wfactor
			'TEST': testJCPDSFit, stash.base, stash.jcpds, stash.fitselect, stash.pressure, stash.width, stash.wfactor
		    'FIT': fitautoJCPDS, stash.base, stash.jcpds, stash.fitselect, stash.pressure, stash.width, stash.wfactor
	    	'EXIT': WIDGET_CONTROL, ev.TOP, /DESTROY
			ELSE:
		endcase
	end
ENDCASE
END

; ****************************************************************
; Build main gui with selection on hkl lines to fit
;
PRO fitJCPDSWindowMain, parent, jcpds
common inputfiles, inputfiles, activeset
common fonts, titlefont, boldfont, mainfont, avFontHeight
; force creation of a new plot window
def = 0
; base GUI
base = WIDGET_BASE(Title='Multipeak: fit from JCPDS',/COLUMN, GROUP_LEADER=parent, /TLB_SIZE_EVENTS)
titleLa = WIDGET_LABEL(base, VALUE='Multipeak: fit from JCPDS', /ALIGN_CENTER, font=titlefont)
;
; Object summary
fitselect = jcpds->summaryFrame(base)
; pressure and width
param = WIDGET_BASE(base, COLUMN=2, /GRID_LAYOUT)
wLa = WIDGET_LABEL(param, VALUE='Pressure (GPa)', /ALIGN_LEFT)
wLa = WIDGET_LABEL(param, VALUE='Fit region (degrees)', /ALIGN_LEFT)
wFLa = WIDGET_LABEL(param, VALUE='Peak region width factor (mult. half-width)', /ALIGN_LEFT)
pressure = WIDGET_TEXT(param, VALUE='0.0', /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
width = WIDGET_TEXT(param, VALUE='1.0', /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
wfactor= WIDGET_TEXT(param, VALUE='4.0', /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
; ACTION BUTTONS
buttons = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
plotBut = WIDGET_BUTTON(buttons, VALUE='Plot', UVALUE='PLOT',xsize=80)
testBut = WIDGET_BUTTON(buttons, VALUE='Test Fit', UVALUE='TEST',xsize=80)
fitBut = WIDGET_BUTTON(buttons, VALUE='Fit all', UVALUE='FIT',xsize=80)
closeBut = WIDGET_BUTTON(buttons, VALUE='Close', UVALUE='EXIT',xsize=80)
; Create an anonymous structure to hold widget IDs
stash = {base:base, param: param, buttons: buttons, fitselect:fitselect, pressure: pressure, width: width, jcpds:jcpds, wfactor: wfactor}
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'fitJCPDSWindowMain', base
END

; ****************************************************************
; This is what is called first. We try to find a JCPDS and 
; build the interface afterwards
;
PRO fitJCPDSWindow, parent
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
test = 0
while (test eq 0) do begin
	result=dialog_pickfile(title='Enter JCPDS card', path=jcpdsdirectory, DIALOG_PARENT=parent)
	if (result ne '') then begin
		FDECOMP, result, disk, dir, name, qual, version
		jcpdsdirectory = disk+dir
		openr, lun, result, /get_lun
		jcpds = OBJ_NEW('JCPDSObject')
		test2 = jcpds->fromFile(lun, result)
		close, lun
		if (test2 ne 1) then tmp=DIALOG_MESSAGE(test2, /error) else test = 1
	endif else begin
		return
	endelse
endwhile
fitJCPDSWindowMain, parent, jcpds
END