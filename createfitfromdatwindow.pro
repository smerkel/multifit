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
;
;
; This window is copied/adapted from createmodelwindow.pro 
; It converts .dat files into a .fit format readable by polydefix.
; This is meant to be useful when data are difficult to (re)fit by the automatic procedure.   
; Background coefficients and peakprofiles are not used by polydefix, hence there are dummy values at their usual place. 
; 
; 13th May 2013 1st version, N. Hilairet
; 
; ******************************  GUI THAT CONTROLS THE CONVERSION WINDOW ***************

PRO addInputConvertWindow, base, listinput
common files, extension, directory, outputdirectory
common inputModelFiles, inputfiles, ninputfiles
result=dialog_pickfile(title='Select input file(s)', path=outputdirectory, DIALOG_PARENT=base, DEFAULT_EXTENSION='.dat', FILTER=['*.dat'])
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

PRO removeInputConvertWindow, listinput
common inputModelFiles, inputfiles, ninputfiles
active = WIDGET_INFO(listinput, /LIST_SELECT)
n = n_elements(active)
for i=0,n-1 do begin
	inputfiles = array_pop(inputfiles,active(n-1-i))
endfor
ninputfiles = ninputfiles-n
WIDGET_CONTROL, listinput, SET_VALUE=inputfiles
END   

;this part 
;- passes the filenames and paths
;- creates a fitPatternObject containing one subPatternObject per .dat file
;- saves the fitpatternObject to an ascii file in .fit format
PRO makefitfile, base, log
common files, extension, directory, outputdirectory
common inputModelFiles, inputfiles, ninputfiles
testdata = OBJ_NEW('fitPatternObject')
a = testdata->fromDat(log,ninputfiles, outputdirectory, inputfiles)
if (a) then begin
	result = pickfile_dir_ext(outputdirectory, 'fit', parent=base, title='Select file to write .fit')
	if (result ne '') then begin
		openw, lun, result, /get_lun
		test = testdata->saveToAscii(lun)
		free_lun, lun
		WIDGET_CONTROL, base, /DESTROY
	endif
endif else begin
	tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
endelse
END

PRO createFitfromdatWindow_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE uval OF
    'ADD': addInputConvertWindow, stash.base, stash.listinput
    'REMOVE': removeInputConvertWindow, stash.listinput
	  'WRITEFITFILE': makefitfile, stash.base, stash.log   
    'EXIT': WIDGET_CONTROL, ev.TOP, /DESTROY
	else:
ENDCASE
END

PRO createFitfromdatWindow, parent
common inputModelFiles, inputfiles, ninputfiles
common fonts, titlefont, boldfont, mainfont, avFontHeight
ninputfiles = 0
base = WIDGET_BASE(Title='Conversion of .dat files into .fit file',/COLUMN, GROUP_LEADER=parent)
titleLa = WIDGET_LABEL(base, VALUE='Conversion of .dat files', /ALIGN_CENTER, font=titlefont)
; We need: input data files, and textlog
fram  = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
input = WIDGET_BASE(fram,/COLUMN, FRAME=1)
la2 = WIDGET_LABEL(input, VALUE='Results from previous fit', /ALIGN_LEFT)
input2= WIDGET_BASE(input,/ROW)
listinput = Widget_List(input2, VALUE='', XSIZE=20, YSIZE=10, /MULTIPLE, uval='LISTINPUT')
inputBut = WIDGET_BASE(input2,/COLUMN, /ALIGN_CENTER, /GRID_LAYOUT)
plotBut = WIDGET_BUTTON(inputBut, VALUE='Add', UVALUE='ADD',xsize=80)
closeBut = WIDGET_BUTTON(inputBut, VALUE='Remove', UVALUE='REMOVE',xsize=80)
textLog = WIDGET_TEXT(fram, XSIZE=60, YSIZE=10, /ALIGN_CENTER, /EDITABLE, /WRAP, /SCROLL)
; ACTION BUTTONS
butBase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
plotBut = WIDGET_BUTTON(butBase, VALUE='write .fit', UVALUE='WRITEFITFILE',xsize=80)
closeBut = WIDGET_BUTTON(butBase, VALUE='Close', UVALUE='EXIT',xsize=80)
; Create an anonymous structure to hold widget IDs
stash = {base:base, log: textLog, listinput:listinput} 
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'createFitfromdatWindow', base
END
