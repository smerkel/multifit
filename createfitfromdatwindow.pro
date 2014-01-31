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
; 
; Its purpose is to convert .dat files into a .fit format readable by polydefix.
; 
; This is meant to be useful when data are difficult to (re)fit by the automatic procedure.   
; 
; Background coefficients and peakprofiles are not used by polydefix, hence dummy values fill their usual place. 
; 
; In case of conversion into one .fit only the files selected by hand will be considered
; 
; In case of multiple conversions (.dat sets for several diffractions):
; The .dat files need to have 
; - the same base name for each subpattern
; - the same number of subpatterns (each can contain several peaks)
; - the diffraction number at the end 
; e.g.: for the diffraction number 13, sp1_013.dat and sp2_013.dat 
;       for the diffraction number 14: sp1_014.dat and sp2_014.dat
; these will be converted into .fit files which
; - base names are indentical and chosen by the user 
; - end of names are the diffraction numbers
; e.g. the .dat files in the example above will give: run1_013.fit and run1_014.fit
; 
; 13th May 2013 1st version, N. Hilairet
; 29th May 2013 added conversion for multiple sets N. Hilairet
; 
; ******************************  GUI THAT CONTROLS THE CONVERSION WINDOW ***************

PRO addInputConvertWindow, base, listinput
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
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

;makefitfiles : 
;  - passes the filenames and paths
;  - creates a fitPatternObject containing one subPatternObject per .dat file
;  - saves the fitpatternObject to an ascii file in .fit format
;  - the "multiples" variable is set to the .dat file names for a given diffraction number
PRO makefitfiles, base, log, multiples, fitfilename
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common inputModelFiles, inputfiles, ninputfiles

testdata = OBJ_NEW('fitPatternObject')

if multiples[0] ne '' then begin
a = testdata->fromDat(log,ninputfiles, outputdirectory, multiples)

  if (a) then begin
      openw, lun, fitfilename, /get_lun
      b = testdata->saveToAscii(lun)
      free_lun, lun
      if (b ne 1) then begin
        tmp = DIALOG_MESSAGE("Error: " + b, /ERROR)
        return
      endif
      logit, log, "Save dat files in .fit format: finished for "
	endif
endif else begin
	tmp = DIALOG_MESSAGE("Error: " + a, /ERROR)
endelse

END

; routine called for a single .fit to write - 
; the difference with "makefitfiles" is here you get to chose the name and the directory of output
; To be done later: condensed version of both! 
;  - passes the filenames and paths
;  - creates a fitPatternObject containing one subPatternObject per .dat file
;  - saves the fitpatternObject to an ascii file in .fit format
PRO makefitfile, base, log
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
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

;this procedure converts several sets of .dat files (one set per diffraction) 
;into one .fit file per diffraction (see top of this file for full explanation)
PRO multiplefitsfromdats, base, log, starti, endi, ndg, interv, rootname
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common inputModelFiles, inputfiles, ninputfiles

; for some reason an array is passed instead of a single element - need to select the first element of it 
      startimg = FIX(starti[0])
      endimg = FIX(endi[0])
      ndigits = FIX(ndg[0])
      int = FIX(interv[0])

; get the .dat basenames (one basename per subpattern)  
if inputfiles[0] ne '' then begin     
     inputfiles = strtrim(inputfiles)
     wherecut = strpos(inputfiles ,'_' ,/reverse_search)
     basenames= strarr(ninputfiles)
     for j = 0, ninputfiles-1 do begin
       basenames[j] = strmid(inputfiles[j], 0, wherecut[j]+1)
     endfor
     print, 'basenames list for fits is ', basenames
  
; loop on the diffractions   
     for i = startimg, endimg, int do begin
       str = intformat(i, ndigits)
       endname = replicate(str, ninputfiles)
       
       multiples = strarr(ninputfiles)
       multiples = basenames + endname + '.dat'
       fitfilename = outputdirectory + rootname +'_'+ endname +".fit"
     
       makefitfiles, base, log, multiples, fitfilename 
 
       logit, log, 'created .fit file '+ fitfilename + ' from ' + multiples 
     endfor
       
    logit, log, "Save multiple sets of dat files in .fit format: finished"
    return
endif else begin
    logit, log, "Get series of files: canceled"
    WIDGET_CONTROL, ev.TOP, /DESTROY
endelse
error: tmp = DIALOG_MESSAGE(result, /ERROR)
END


; 
PRO createFitfromdatWindow_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE uval OF
    'ADD': addInputConvertWindow, stash.base, stash.listinput
    'REMOVE': removeInputConvertWindow, stash.listinput
	  'WRITEFITFILE': makefitfile, stash.base, stash.log, ''  
    'EXIT': WIDGET_CONTROL, ev.TOP, /DESTROY
	else:
ENDCASE
If (uval eq 'WRITEFITFILES') then begin
    if (stash.listinput ne '') then begin
      WIDGET_CONTROL, stash.startImText, GET_VALUE=starti
      WIDGET_CONTROL, stash.endImText, GET_VALUE=endi
      WIDGET_CONTROL, stash.intervImText, get_value=int
      WIDGET_CONTROL, stash.digitsImText, GET_VALUE=ndg
      WIDGET_CONTROL, stash.RootnameText, GET_VALUE=rootname
      multiplefitsfromdats, stash.base, stash.log, starti, endi, ndg, int, rootname
    endif ;else begin
      ;logit, log, 'you need to select .dat files first'
    ;endelse
endif
END

PRO createFitfromdatWindow, parent
common inputModelFiles, inputfiles, ninputfiles
common fonts, titlefont, boldfont, mainfont, avFontHeight
ninputfiles = 0
base = WIDGET_BASE(Title='Conversion of .dat files into .fit file',/COLUMN, GROUP_LEADER=parent)
titleLa = WIDGET_LABEL(base, VALUE='Conversion of .dat files', /ALIGN_CENTER, font=titlefont)
; We need: input data files, and textlog
fram  = WIDGET_BASE(base,/COLUMN, /ALIGN_CENTER, /GRID_LAYOUT)
input = WIDGET_BASE(fram,/ROW, FRAME=1)
input1= WIDGET_BASE(input,/COLUMN)
la1 = WIDGET_LABEL(input1, VALUE='Results from previous manual fits', /ALIGN_LEFT)
listinput = Widget_List(input1, VALUE='', XSIZE=20, YSIZE=10, /MULTIPLE, uval='LISTINPUT')
inputBut = WIDGET_BASE(input1,/COLUMN, /ALIGN_CENTER, /GRID_LAYOUT)
plotBut = WIDGET_BUTTON(inputBut, VALUE='Add', UVALUE='ADD',xsize=80)
closeBut = WIDGET_BUTTON(inputBut, VALUE='Remove', UVALUE='REMOVE',xsize=80)
textLog = WIDGET_TEXT(fram, XSIZE=100, YSIZE=10, /ALIGN_CENTER, /EDITABLE, /WRAP, /SCROLL)
; editable inputs for multiple sets
input2 = WIDGET_BASE(input, /column, FRAME=1)
title1 = WIDGET_LABEL(input2, VALUE='Choosing multiple sets of dat files', /ALIGN_CENTER, font=titlefont)
nameBase =  WIDGET_BASE(input2,COLUMN=2, /GRID_LAYOUT)
startImLa = WIDGET_LABEL(nameBase, VALUE='First image file number', /ALIGN_LEFT)
endImLa = WIDGET_LABEL(nameBase, VALUE='Last image file number', /ALIGN_LEFT)
intervImLa = WIDGET_LABEL(nameBase, VALUE='interval between image files number', /ALIGN_LEFT)
digitsImLa = WIDGET_LABEL(nameBase, VALUE='Number of digits for image files', /ALIGN_LEFT)
RootnameLa = WIDGET_LABEL(nameBase, VALUE='Base name for your fit files', /ALIGN_LEFT)
startImText = WIDGET_TEXT(nameBase, XSIZE=3, /EDITABLE)
endImText = WIDGET_TEXT(nameBase, XSIZE=3, /EDITABLE)
intervImText = WIDGET_TEXT(nameBase, XSIZE=3, /EDITABLE)
digitsImText = WIDGET_TEXT(nameBase, XSIZE=3, /EDITABLE)
RootnameText = WIDGET_TEXT(nameBase, XSIZE=3, /EDITABLE)
; ACTION BUTTONS
butBase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
plotBut = WIDGET_BUTTON(butBase, VALUE='write one .fit', UVALUE='WRITEFITFILE',xsize=170)
closeBut = WIDGET_BUTTON(butBase, VALUE='Close', UVALUE='EXIT',xsize=80)
plotBut2 = WIDGET_BUTTON(butBase, VALUE='write multiple .fits', UVALUE='WRITEFITFILES',xsize=170)
; Create an anonymous structure to hold widget IDs
stash = {base:base, log: textLog, listinput:listinput, startImText:startImText,endImText:endImText,digitsImText:digitsImText, intervImText:intervImText,RootnameText:RootnameText  } 
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
XMANAGER, 'createFitfromdatWindow', base
END
