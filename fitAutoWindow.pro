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


; ********************************* GUI THAT CONTROL AUTOMATIC FITTING ***************
; In those fitting procedures, we start from a model from a previous fit.
; The model describes
;    - the number of 2theta subranges to look at
;    - the number of peaks in each 2theta subranges 
;    - the properties of each peak in the previous fits, in terms of Fourier coefficients
;

PRO loadfitmodel, base, widget
	common files, extension, directory, outputdirectory
	result=dialog_pickfile(title='Select input file', path=outputdirectory, DIALOG_PARENT=base, DEFAULT_EXTENSION='.mdl', FILTER=['*.mdl','*.fit'])
	if (result ne '') then begin
		FDECOMP, result, disk, dir, name, qual, version
		filename = outputdirectory + name + "." + Qual
		filenameshort = name + "." + Qual
		if (FILE_TEST(filename)) then begin
			if ((Qual eq 'model') or (Qual eq 'mdl')) then begin 
				openr, lun, filename, /get_lun
				testModel = OBJ_NEW('FitPatternModel')
				a = testModel->readFromAscii(lun)
				free_lun, lun
				if (a ne 1) then begin
					tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
					return
				endif
			endif else begin
				openr, lun, filename, /get_lun
				testModel = OBJ_NEW('fitPatternObject')
				a = testModel->readFromAscii(lun)
				free_lun, lun
				if (a ne 1) then begin
					tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
					return
				endif
			endelse
			WIDGET_CONTROL, widget, SET_VALUE=filenameshort
		endif else tmp = DIALOG_MESSAGE(filenameshort + " is not in your output directory.", /ERROR)
	endif
END

PRO fitauto,  fitwindow, base, listSets, fitmodel, plot0, plot1, plot2, mapplot, noplot, wVa, autosaveY, autosaveN, stoponerrorY, stoponerrorN
	common files, extension, directory, outputdirectory
	common inputfiles, inputfiles, activeset
	common fitresults, fitdata
	common rawdata, nalpha, ntheta, alpha, twotheta, data
	; What sets to do we have to fit?
	tofit = WIDGET_INFO(listSets, /LIST_SELECT)
	; plot level
	if (WIDGET_INFO(plot2, /BUTTON_SET) eq 1) then begin
		plotlevel = 2
	endif else if (WIDGET_INFO(plot1, /BUTTON_SET) eq 1) then begin
		plotlevel = 1
	endif else if (WIDGET_INFO(plot0, /BUTTON_SET) eq 1) then begin
		plotlevel = 0
	endif else if (WIDGET_INFO(mapplot, /BUTTON_SET) eq 1) then begin
		plotlevel = -1
	endif else begin
		plotlevel = -2
	endelse
	; print, "plotlevel is " + string(plotlevel, /print)
	if (WIDGET_INFO(autosaveY, /BUTTON_SET) eq 1) then begin
		autosave = 1
	endif else begin
		autosave = 0
	endelse
	if (WIDGET_INFO(stoponerrorY, /BUTTON_SET) eq 1) then begin
		stoponerror = 1
	endif else begin
		stoponerror = 0
	endelse
	; Fit region width factor
	WIDGET_CONTROL, wVa, GET_VALUE=widthfactor
	; Create a log window so we can follow the fit...
	logbase = WIDGET_BASE(Title='Autofit log window',/COLUMN, GROUP_LEADER=fitwindow)
	titleLa = WIDGET_LABEL(logbase, VALUE='Autofit log', /ALIGN_CENTER)
	log = WIDGET_TEXT(logbase, XSIZE=60, YSIZE=30, /ALIGN_CENTER, /EDITABLE, /WRAP, /SCROLL)
	WIDGET_CONTROL, logbase, /REALIZE
	txt = "Loading fit model (this may take a few seconds)"
	WIDGET_CONTROL, log, SET_VALUE=txt, /APPEND
	; Load the fitmodel
	WIDGET_CONTROL, fitmodel, GET_VALUE=filenameshort
	filename = outputdirectory + filenameshort
	FDECOMP, filenameshort, disk, dir, name, qual, version
	if (FILE_TEST(filename)) then begin
		openr, lun, filename, /get_lun
		if ((qual eq 'model') or (qual eq 'mdl')) then begin
			testModel = OBJ_NEW('FitPatternModel')
			a = testModel->readFromAscii(lun)
		endif else begin 
			fitobject = OBJ_NEW('fitPatternObject')
			a = fitobject->readFromAscii(lun)
		endelse
		free_lun, lun
		if (a ne 1) then begin
			tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
			return
		endif
	endif else begin
		tmp = DIALOG_MESSAGE("File " + filename + " not found", /ERROR)
	endelse
	; create a fit object with the model
	if ((qual eq 'model') or (qual eq 'mdl')) then begin
		fitobject = OBJ_NEW('fitPatternObject')
		test = fitobject->fromModel(testModel)
	endif
	; Perform fit
	; Loop on datasets
	top = N_ELEMENTS(tofit)-1
	for i=0, top do begin
		txt = "Working on " + STRING(inputfiles(tofit(i)),/PRINT)
		WIDGET_CONTROL, log, SET_VALUE=txt, /APPEND
		; Loading the dataset
		file =  directory +inputfiles(tofit(i))
		res = readfile(file)
		activeset = tofit(i)
		if (res ne 1) then begin
			tmp = DIALOG_MESSAGE(res, /ERROR)
    		txt = "Read active set in MULTIFIT format from " + inputfiles(tofit(i)) + ": failed"
			WIDGET_CONTROL, log, SET_VALUE=txt, /APPEND
			return
		endif
		; Optimize fit (with a catch for errors...
		CATCH, Error_status  
		;This statement begins the error handler:  
		IF Error_status NE 0 THEN BEGIN 
			txt = "Error with " + inputfiles(tofit(i)) + ": " + !ERROR_STATE.MSG 
			WIDGET_CONTROL, log, SET_VALUE=txt, /APPEND
			if (stoponerror eq 1) then begin
				tmp = DIALOG_MESSAGE(txt, /ERROR)
				return
			endif else begin
				txt = "Nothing saved for " + inputfiles(tofit(i)) 
				WIDGET_CONTROL, log, SET_VALUE=txt, /APPEND
				goto, endloop
			endelse
 		ENDIF  
		fitobject->optimizeWithCurrentDataset, log, plotlevel, widthfactor
		; Save
		if (autosave eq 1) then begin
			FDECOMP, file, disk, dir, name, qual, version
			filename = outputdirectory + name + ".fit"
			openw, lun, filename, /get_lun
			a = fitobject->saveToAscii(lun)
			free_lun, lun
			if (a ne 1) then begin
				tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
				return
			endif
    		txt = "Saved fit in " + name + ".fit"
			WIDGET_CONTROL, log, SET_VALUE=txt, /APPEND
		endif else begin
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
		endelse
		; If plotlevel is -1, we need a mapplot
		if (plotlevel eq -1) then begin 
			fitdata = fitobject
			mindata = min(data)
			maxdata = max(data)
			mintheta = min(twotheta)
			maxtheta = max(twotheta)
			; uses a function from compareFitWindow!!
			plotBothContour, mindata, maxdata, mintheta, maxtheta
		endif
		endloop:
	endfor
	txt = "End of fit!!"
	WIDGET_CONTROL, log, SET_VALUE=txt, /APPEND
	;endloop: WIDGET_CONTROL, fitwindow, /DESTROY
END

PRO fitAutoWindow_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE uval OF
    'FITMODEL': loadfitmodel, stash.base, stash.fitmodelText
    'FIT': fitauto, ev.Top, stash.base, stash.listSets, stash.fitmodelText, stash.plot0, stash.plot1, stash.plot2, stash.mapplot, stash.noplot, stash.wVa, stash.autosaveY, stash.autosaveN, stash.stoponerrorY, stash.stoponerrorN
    'EXIT': WIDGET_CONTROL, ev.TOP, /DESTROY
	ELSE:
ENDCASE
END

PRO fitAutoWindow, parent, nplots
common inputfiles, inputfiles, activeset
common fonts, titlefont, boldfont, mainfont, avFontHeight
; force creation of a new plot window
def = 0
; base GUI
base = WIDGET_BASE(Title='Multipeak automatic fit window',/COLUMN, GROUP_LEADER=parent)
titleLa = WIDGET_LABEL(base, VALUE='Multipeak automated fitting functions', /ALIGN_CENTER, font=titlefont)
; 
; top container
top = WIDGET_BASE(base,/ROW)
; List of sets
if (nplots lt 2) then begin
	listSets = Widget_List(top, VALUE=inputfiles, UVALUE='LISTSETS', YSIZE=20)
endif else begin
	listSets = Widget_List(top, VALUE=inputfiles, UVALUE='LISTSETS', YSIZE=20, /MULTIPLE)
endelse 
; Fit parameters
param = WIDGET_BASE(top,/COLUMN, FRAME=1)
; Fit model
fitmodel = WIDGET_BASE(param,/ROW, FRAME=1)
fitmodelLa = WIDGET_LABEL(fitmodel, VALUE='Starting fit model: ', /ALIGN_LEFT)
fitmodelText = WIDGET_TEXT(fitmodel, VALUE='', XSIZE=40)
fitmodelBu = WIDGET_BUTTON(fitmodel, VALUE='Change', UVALUE='FITMODEL',xsize=80)
; visual output?
plots = Widget_Base(param, Column=1)
plotLa = WIDGET_LABEL(plots, VALUE='Level of plots: ', /ALIGN_LEFT)
plotlevel = Widget_Base(plots, Column=1, /Exclusive)
noplot = Widget_Button(plotlevel, Value='No plot', UVALUE='mapplot')
mapplot = Widget_Button(plotlevel, Value='Mapplot', UVALUE='mapplot')
plot0 = Widget_Button(plotlevel, Value='1-D: Minimal', UVALUE='plot0')
plot1 = Widget_Button(plotlevel, Value='1-D: Intermediate', UVALUE='plot1')
plot2 = Widget_Button(plotlevel, Value='1-D: Verbose', UVALUE='plot2')
WIDGET_CONTROL,  plot0, /SET_BUTTON
; autosave?
autosave = Widget_Base(param, Column=1)
autosaveLa = WIDGET_LABEL(autosave, VALUE='Save fits automatically: ', /ALIGN_LEFT)
autosaveCh = Widget_Base(autosave, Column=1, /Exclusive)
autosaveY = Widget_Button(autosaveCh, Value='Yes', UVALUE='y')
autosaveN = Widget_Button(autosaveCh, Value='No', UVALUE='n')
WIDGET_CONTROL,  autosaveN, /SET_BUTTON
; Error handling
error = Widget_Base(param, Column=1)
errorLa = WIDGET_LABEL(error, VALUE='Error handling: ', /ALIGN_LEFT)
errorCh = Widget_Base(error, Column=1, /Exclusive)
stoponerrorY = Widget_Button(errorCh, Value='Stop calculations', UVALUE='y')
stoponerrorN = Widget_Button(errorCh, Value='Proceed to next dataset', UVALUE='n')
WIDGET_CONTROL, stoponerrorY, /SET_BUTTON
; Width factor
wBase = WIDGET_BASE(param,/ROW)
wLa = WIDGET_LABEL(wBase, VALUE='Fit region width factor: ', /ALIGN_LEFT)
wVa = WIDGET_TEXT(wBase, VALUE='4.0', /EDITABLE ,/ALIGN_LEFT, XSIZE=10)
; ACTION BUTTONS
buttons = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
plotBut = WIDGET_BUTTON(buttons, VALUE='Fit', UVALUE='FIT',xsize=80)
closeBut = WIDGET_BUTTON(buttons, VALUE='Close', UVALUE='EXIT',xsize=80)
; Create an anonymous structure to hold widget IDs
stash = {base:base, listSets:listSets, fitmodelText: fitmodelText, noplot: noplot, mapplot: mapplot, plot0: plot0, plot1: plot1, plot2: plot2, wVa: wVa, autosaveY:autosaveY, autosaveN:autosaveN, stoponerrorY:stoponerrorY, stoponerrorN:stoponerrorN} 
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'fitAutoWindow', base
END
