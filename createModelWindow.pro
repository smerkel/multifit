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


; ******************************  GUI THAT CONTROLS THE MODEL WINDOW ***************

PRO addInputModelWindow, base, listinput
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common inputModelFiles, inputfiles, ninputfiles
result=dialog_pickfile(title='Select input file(s)', path=outputdirectory, DIALOG_PARENT=base, FILTER=['*.dat','*.*'], /must_exist)
if (result ne '') then begin
	FDECOMP, result, disk, dir, name, qual, version
	filename = outputdirectory + name + "." + Qual
	filenameshort = name + "." + Qual
	res =  FILE_TEST(filename)
	if (res) then begin
		inputtmp = strarr(ninputfiles+1)
		for i=0,ninputfiles-1 do begin
			inputtmp(i) = inputfiles(i)
		endfor
		inputtmp(ninputfiles) = filenameshort
		WIDGET_CONTROL, listinput, SET_VALUE=inputtmp
		inputfiles = inputtmp
		ninputfiles = ninputfiles + 1
	endif else begin
		tmp = DIALOG_MESSAGE(filenameshort + " is not in your output directory.", /ERROR)
	endelse
endif 
END 

PRO removeInputModelWindow, listinput
common inputModelFiles, inputfiles, ninputfiles
active = WIDGET_INFO(listinput, /LIST_SELECT)
n = n_elements(active)
for i=0,n-1 do begin
	inputfiles = array_pop(inputfiles,active(n-1-i))
endfor
ninputfiles = ninputfiles-n
WIDGET_CONTROL, listinput, SET_VALUE=inputfiles
END   

PRO createModel, base, log, gauss, lorentz, voigt
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common inputModelFiles, inputfiles, ninputfiles
if (WIDGET_INFO(gauss, /BUTTON_SET) eq 1) then begin
    profile = 0
endif else if (WIDGET_INFO(lorentz, /BUTTON_SET) eq 1) then begin
    profile = 2
endif else begin
    profile = 1
endelse
testModel = OBJ_NEW('FitPatternModel')
a = testModel->setPeakProfile(profile)
a = testModel->setNSubPat(ninputfiles)
if (a) then begin
	for i=0,ninputfiles-1 do begin
		filename = outputdirectory + inputfiles(i)
		a = testModel->setSubPat(log,i,filename)
		if (a ne 1) then begin
			tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
			goto, ERROR
		endif
	endfor
	result = pickfile_dir_ext(outputdirectory, 'mdl', parent=base, title='Select file to save model')
	if (result ne '') then begin
		openw, lun, result, /get_lun
		test = testModel->saveToAscii(lun)
		free_lun, lun
		WIDGET_CONTROL, base, /DESTROY
	endif
endif else begin
	tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
endelse
ERROR:
END

PRO createModelWindow_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE uval OF
    'ADD': addInputModelWindow, stash.base, stash.listinput
    'REMOVE': removeInputModelWindow, stash.listinput
	'CREATEMODEL': createModel, stash.base, stash.log, stash.gauss, stash.lorentz, stash.voigt
    'EXIT': WIDGET_CONTROL, ev.TOP, /DESTROY
	else:
ENDCASE
END

PRO createModelWindow, parent
common inputModelFiles, inputfiles, ninputfiles
common fonts, titlefont, boldfont, mainfont, avFontHeight
ninputfiles = 0
base = WIDGET_BASE(Title='Creation of new fit model',/COLUMN, GROUP_LEADER=parent)
titleLa = WIDGET_LABEL(base, VALUE='Creation of new fit model', /ALIGN_CENTER, font=titlefont)
; We need: peak profile, input data file, and textlog
model  = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
prof = WIDGET_BASE(model,/COLUMN, FRAME=1)
la1 = WIDGET_LABEL(prof, VALUE='Peak profile', /ALIGN_LEFT)
peakprofile = Widget_Base(prof, Column=1, /Exclusive)
gauss = Widget_Button(peakprofile, Value='Gauss', UVALUE='PROFILE')
voigt = Widget_Button(peakprofile, Value='Pseudo-Voigt', UVALUE='PROFILE')
lorentz = Widget_Button(peakprofile, Value='Lorentz', UVALUE='PROFILE')
input = WIDGET_BASE(model,/COLUMN, FRAME=1)
la2 = WIDGET_LABEL(input, VALUE='Results from previous fit', /ALIGN_LEFT)
input2= WIDGET_BASE(input,/ROW)
listinput = Widget_List(input2, VALUE='', XSIZE=20, YSIZE=10, /MULTIPLE, uval='LISTINPUT')
inputBut = WIDGET_BASE(input2,/COLUMN, /ALIGN_CENTER, /GRID_LAYOUT)
plotBut = WIDGET_BUTTON(inputBut, VALUE='Add', UVALUE='ADD',xsize=80)
closeBut = WIDGET_BUTTON(inputBut, VALUE='Remove', UVALUE='REMOVE',xsize=80)
textLog = WIDGET_TEXT(base, XSIZE=60, YSIZE=10, /ALIGN_CENTER, /EDITABLE, /WRAP, /SCROLL)
; ACTION BUTTONS
butBase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
plotBut = WIDGET_BUTTON(butBase, VALUE='Create model', UVALUE='CREATEMODEL',xsize=80)
closeBut = WIDGET_BUTTON(butBase, VALUE='Close', UVALUE='EXIT',xsize=80)
; Create an anonymous structure to hold widget IDs
stash = {base:base, log: textLog, gauss:gauss, voigt:voigt, lorentz:lorentz, listinput:listinput} 
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'createModelWindow', base
END
