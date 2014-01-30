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


; ******************************************************************
; Saves and reads basic information about multifit in hidden file for future
; uses
;

function readdefault
common jcpds, jcpds_directory
common workdirectory, workdirectory
defaultdir = GETENV('HOME')
jcpds_directory = GETENV('HOME')
; If we are using UNIX, try to see if we saved a default file
if (STRUPCASE(!VERSION.OS_FAMILY) eq 'UNIX') then begin
  default = GETENV('HOME') + "/" + ".multifit"
  if (file_test(default)) then begin
    openr, lun, default, /get_lun
    while ~ EOF(lun) do begin
      row = ""
      readf, lun, row
      word = strsplit(row, COUNT=nn, /EXTRACT)
      if (nn gt 1) then begin
        label = STRUPCASE(word[0])
        CASE label OF
          "WORK_DIRECTORY:": defaultdir = word[1]
          "JCPDS_DIRECTORY:": jcpds_directory = word[1]
          else:
        endcase
      endif
    endwhile
    free_lun, lun
  endif else begin
    openw, lun, default, /get_lun
    printf, lun, '# Default information for multifit'
    free_lun, lun
  endelse
endif
workdirectory = defaultdir
; print, "Returning " + defaultdir
return, defaultdir
end

pro setDefaultItem, item, value
if (STRUPCASE(!VERSION.OS_FAMILY) eq 'UNIX') then begin
  default = GETENV('HOME') + "/" + ".multifit"
  if (file_test(default)) then begin
    str = ""
    done = 0
    openr, lun, default, /get_lun
    while ~ EOF(lun) do begin
      row = ""
      readf, lun, row
      word = strsplit(row, COUNT=nn, /EXTRACT)
      label = STRUPCASE(word[0])
      CASE label OF
        item: begin
          str = str + item + " " + value + "|"
          done = 1
        end
        else: str = str + row + "|"
      endcase
    endwhile
    free_lun, lun
    if (done eq 0) then str = str + item + " " + value + "|"
    ; print, "Trying to write " + str
    openw, lun, default, /get_lun
    word = strsplit(str, "|", COUNT=nn, /EXTRACT)
    for i=0,nn-1 do printf, lun, word[i]
    free_lun, lun
  endif
endif
end

pro setDefaultJCPDSdir, dir
setDefaultItem, "JCPDS_DIRECTORY:", dir
end

pro setDefaultWorkDir, dir
setDefaultItem, "WORK_DIRECTORY:", dir
end

; ********************************* LOAD_DEFAULTS *********************************

PRO load_defaults_startup
common fonts, titlefont, boldfont, mainfont, avFontHeight
common files, extension, directory, outputdirectory
common experiment, wavelength, detectordistance
common default, defaultdir
; default color palette and font for plots
OS   = strupcase(!VERSION.OS)
OS   = strmid(OS,0,3)
if (OS ne 'WIN') then device, true_color=24
device, decomposed=0
loadct, 15, /SILENT
DEVICE, SET_FONT='Helvetica Bold', /TT_FONT, SET_CHARACTER_SIZE=[8,10]
; default for plots
!P.MULTI = 0
; directories
defaultdir = readdefault()
directory = defaultdir
outputdirectory = defaultdir
; other things
extension = '.chi'
wavelength=0.4000
detectordistance=200.
; Fonts
if (OS ne 'WIN') then begin
  mainfont = '-*-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*'
  titlefont = '-*-helvetica-bold-r-*-*-14-*-*-*-*-*-*-*'
  boldfont = '-*-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*'
  ; calculating conversion between font size in and number of pixels
  ; need for some user interface stuff
  scale = !D.X_PX_CM ; how many pixels in a cm
  ; There are 72 pt per inch. Main font is 12 pt, therefore, average font
  ; height will be
  avFontHeight = fix(12.*scale*2.54/72.)
endif else begin
  titlefont = 'helvetica*bold*14'
  boldfont = 'helvetica*bold*12'
  mainfont = 'helvetica*12'
  ; calculating conversion between font size in and number of pixels
  ; need for some user interface stuff
  scale = !D.X_PX_CM ; how many pixels in a cm
  ; There are 72 pt per inch. Main font is 12 pt, therefore, average font
  ; height will be
  avFontHeight = fix(12.*scale*2.54/72.)
  ; we divide it by 2 in windows. Things look better
  avFontHeight = fix(avFontHeight/2)
endelse
WIDGET_CONTROL,  DEFAULT_FONT=mainfont
end

; ****************************************** about window **************

PRO aboutWindow_event, ev
WIDGET_CONTROL, ev.TOP, /DESTROY
END

pro aboutWindow, base
common fonts, titlefont, boldfont, mainfont, avFontHeight
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base, Title='About Multifit')
infobase =  WIDGET_BASE(basedialog,/COLUMN)
la = WIDGET_LABEL(infobase, VALUE='Multifit', /ALIGN_LEFT, font=titlefont)
la = WIDGET_LABEL(infobase, VALUE='Multifit v4.4, build 8, compiled 14th may 2013', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='Multifit is a software to process multiple diffraction images', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='Copyright S. Merkel, Universite Lille 1, France', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='http://merkel.ZoneO.net/Multifit/', /ALIGN_LEFT)
la = WIDGET_LABEL(infobase, VALUE='', /ALIGN_LEFT)
buttons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK', xsize=80)
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'aboutWindow', basedialog
end


; ****************************************** LOGIT **************

PRO logit, log, txt
text = txt
WIDGET_CONTROL, log, SET_VALUE=text, /APPEND
END

; ****************************************** SAVEPARAMS ********************************
PRO saveparams, base, log, inputDirText, outputDirText, waveText, ipDistanceText
common files, extension, directory, outputdirectory
common experiment, wavelength, detectordistance
common workdirectory, workdirectory
common inputfiles, inputfiles, activeset
filename=dialog_pickfile(title='Filename', path=workdirectory, DIALOG_PARENT=base, DEFAULT_EXTENSION='.par', FILTER=['*.par'], /WRITE)
if (filename ne '') then begin
	filename = addextension(filename, 'par')
	openw, lun, filename, /get_lun
	printf, lun, '# Parameter file for multfit'
	printf, lun, '# Directory with chi and idl files'
	printf, lun, 'directory|'+directory
	printf, lun, '# Directory with image fits'
	printf, lun, 'outputdirectory|'+outputdirectory
	printf, lun, '# Wavelength'
	printf, lun, 'wavelength|'+ STRING(wavelength, /PRINT)
	printf, lun, '# Detector distance'
	printf, lun, 'detectordistance|' + STRING(detectordistance, /PRINT)
	if (size(inputfiles, /TYPE) ne 0) then begin
		input = STRJOIN( inputfiles, ';' )
		printf, lun, '# Input files'
		printf, lun, 'inputfiles|'+input
	endif
	free_lun, lun
    logit, log, "Parameters save in " + filename
	FDECOMP, filename, disk, dir, name, qual, version
	workdirectory = disk+dir
	setDefaultWorkDir, workdirectory
endif
END

PRO readparams, base, log, inputDirText, outputDirText, waveText, ipDistanceText, listSets
common workdirectory, workdirectory
filename=dialog_pickfile(title='Filename', path=workdirectory, DIALOG_PARENT=base, DEFAULT_EXTENSION='.par', FILTER=['*.par'])
if (filename ne '') then begin
  ; print, "Want to open ", filename
	openr, lun, filename, /get_lun
	while ~ EOF(lun) do begin
		row = ""
		readf, lun, row
		words = strsplit(row, '|', /EXTRACT)
		case words[0] of
			'directory': chgInputDirBasic, log, inputDirText, words[1]
			'outputdirectory': chgOutputDirBasic, log, outputDirText, words[1]
			'wavelength': chgWavelengthBasic, log, waveText, words[1]
			'detectordistance': chgDetectordistanceBasic, log, ipDistanceText, words[1]
			'inputfiles': inpufilesFromList, log, listSets, strsplit(words[1], ';', /EXTRACT)
			else:
		endcase
	endwhile 
	close, lun
  logit, log, "Parameters read from " + filename
	FDECOMP, filename, disk, dir, name, qual, version
	workdirectory = disk+dir
	setDefaultWorkDir, workdirectory
endif
end

; ****************************************** CHGINPUTDIR *********************************
PRO chgInputDir, base, log, widget
common files, extension, directory, outputdirectory
result=dialog_pickfile(/DIRECTORY,title='Select input directory', path=directory, DIALOG_PARENT=base)
if (result ne '') then begin
    directory = result
    inputDirLabel = 'Input directory: ' + directory
    WIDGET_CONTROL, widget, SET_VALUE=directory
    logit, log, inputDirLabel
endif
END

PRO chgInputDirBasic, log, widget, result
common files, extension, directory, outputdirectory
if (result ne '') then begin
    directory = result
    inputDirLabel = 'Input directory: ' + directory
    WIDGET_CONTROL, widget, SET_VALUE=directory
    logit, log, inputDirLabel
endif
END

; ****************************************** CHGOUTPUTDIR *************************************
PRO chgOutputDir, base, log, widget
common files, extension, directory, outputdirectory
result=dialog_pickfile(/DIRECTORY,title='Select output directory', path=outputdirectory, DIALOG_PARENT=base)
if (result ne '') then begin
    outputdirectory=result
    outputDirLabel = 'Output directory: ' + outputdirectory
    WIDGET_CONTROL, widget, SET_VALUE=outputdirectory
    logit, log, outputDirLabel
endif
END

PRO chgOutputDirBasic, log, widget, result
common files, extension, directory, outputdirectory
if (result ne '') then begin
    outputdirectory=result
    outputDirLabel = 'Output directory: ' + outputdirectory
    WIDGET_CONTROL, widget, SET_VALUE=outputdirectory
    logit, log, outputDirLabel
endif
END

; ****************************************** CHGWAVELENGTH *************************************
PRO chgWavelengthBasic, log, widget, result
common experiment, wavelength, detectordistance
wavelength = float(result)
waveLabel = 'Wavelength: ' + STRING(wavelength, /PRINT) + ' angstroms'
WIDGET_CONTROL, widget, SET_VALUE=STRTRIM(STRING(wavelength, /PRINT),2)
logit, log, waveLabel
end

PRO chgWavelength_event, ev
common experiment, wavelength, detectordistance
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.waveText, GET_VALUE=wavelength
    wavlength = float(wavelength)
    waveLabel = 'Wavelength: ' + STRING(wavelength, /PRINT) + ' angstroms'
    WIDGET_CONTROL, stash.widget, SET_VALUE=STRTRIM(STRING(wavelength, /PRINT),2)
    logit, stash.log, waveLabel
endif
WIDGET_CONTROL, ev.TOP, /DESTROY
END

PRO chgWavelength, base, log, widget
common experiment, wavelength,  detectordistance
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
waveBase =  WIDGET_BASE(basedialog,/ROW)
waveLa = WIDGET_LABEL(waveBase, VALUE='Wavelength (in angstroms)', /ALIGN_LEFT)
waveText = WIDGET_TEXT(waveBase, XSIZE=10, VALUE=STRING(wavelength,/PRINT), /EDITABLE)
waveButtons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(waveButtons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(waveButtons, VALUE='Cancel', UVALUE='CANCEL')
stash = {widget:widget, waveText:waveText, log:log}
WIDGET_CONTROL, basedialog, SET_UVALUE=stash
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'chgWavelength', basedialog
END

; ****************************************** CHGDETECTORDISTANCE *************************************

PRO chgDetectorDistanceBasic, log, widget, result
common experiment, wavelength, detectordistance
detectordistance = float(result)
waveLabel = 'Detector Distance: ' + STRING(detectordistance, /PRINT) + ' mm'
WIDGET_CONTROL, widget, SET_VALUE=STRTRIM(STRING(detectordistance, /PRINT),2)
logit, log, waveLabel
end

PRO chgDetectorDistance_event, ev
common experiment, wavelength, detectordistance
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.waveText, GET_VALUE=detectordistance
    detectordistance = float(detectordistance)
    waveLabel = 'Detector Distance: ' + STRING(detectordistance, /PRINT) + ' mm'
    WIDGET_CONTROL, stash.widget, SET_VALUE=STRTRIM(STRING(detectordistance, /PRINT),2)
    logit, stash.log, waveLabel
endif
WIDGET_CONTROL, ev.TOP, /DESTROY
END

PRO chgDetectorDistance, base, log, widget
common experiment, wavelength,  detectordistance
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
waveBase =  WIDGET_BASE(basedialog,/ROW)
waveLa = WIDGET_LABEL(waveBase, VALUE='Sample-Detector distance (in mm)', /ALIGN_LEFT)
waveText = WIDGET_TEXT(waveBase, XSIZE=10, VALUE=STRING(detectordistance,/PRINT), /EDITABLE)
waveButtons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(waveButtons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(waveButtons, VALUE='Cancel', UVALUE='CANCEL')
stash = {widget:widget, waveText:waveText, log:log}
WIDGET_CONTROL, basedialog, SET_UVALUE=stash
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'chgDetectorDistance', basedialog
END

; ****************************************** READ AND CONVERT CHI FILES **************

PRO convertonechi_event, ev
common files, extension, directory, outputdirectory
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
log=stash.log
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.startiText, GET_VALUE=starti
    WIDGET_CONTROL, stash.endiText, GET_VALUE=endi
    WIDGET_CONTROL, stash.intervalText, GET_VALUE=interval
    WIDGET_CONTROL, stash.nameText, GET_VALUE=basename
    logit, log, "Read series of text files: " + basename + " with angles from " + starti + " to " + endi + " with interval " +  interval
    startiInt = FIX(FLOAT(starti))
    endiInt = FIX(FLOAT(endi))
    intervalInt = FIX(FLOAT(interval))
    result = read_all(basename, startiInt, endiInt, intervalInt, log)
    if (result eq 1) then begin
        logit, log, "Read series of text files: success"
	outputname = basename + ".idl"
	logit, log, "Saving data in MULTIFIT format into " + outputname
	res = savedata(outputname)
	if (res eq 1) then begin
		logit, log, "Save data in MULTIFIT format to " + outputname + ": success"
		WIDGET_CONTROL, ev.TOP, /DESTROY
	endif else begin
	        tmp = DIALOG_MESSAGE(res, /ERROR)
	        logit, log, "Save data in MULTIFIT format: failed"
	endelse
    endif else begin
        tmp = DIALOG_MESSAGE(result, /ERROR)
        logit, log, "Read series of text files: failed"
    endelse
endif else begin
    logit, log, "Read series of text files: canceled"
    WIDGET_CONTROL, ev.TOP, /DESTROY
endelse
END

PRO convertonechi, base, log
common files, extension, directory, outputdirectory
common inputinfo, string
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
nameBase =  WIDGET_BASE(basedialog,COLUMN=2, /GRID_LAYOUT, FRAME=1)
nameLa = WIDGET_LABEL(nameBase, VALUE='Base for name of chi files', /ALIGN_LEFT)
startiLa = WIDGET_LABEL(nameBase, VALUE='First azimuth angle', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Last azimuth angle', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Interval', /ALIGN_LEFT)
nameText = WIDGET_TEXT(nameBase, XSIZE=10, /EDITABLE)
startiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
endiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
intervalText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
extButtons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(extButtons, VALUE='Ok', UVALUE='OK', xsize=80)
cancel = WIDGET_BUTTON(extButtons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
stash = {log:log, nameText:nameText, intervalText:intervalText, startiText:startiText, endiText:endiText}
WIDGET_CONTROL, basedialog, SET_UVALUE=stash
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'convertonechi', basedialog
END

PRO convertfileseries_event, ev
common files, extension, directory, outputdirectory
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
log=stash.log
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.startiText, GET_VALUE=starti
    WIDGET_CONTROL, stash.endiText, GET_VALUE=endi
    WIDGET_CONTROL, stash.numberDigitsText, GET_VALUE=numberDigits
    WIDGET_CONTROL, stash.nameText, GET_VALUE=basename
    logit, log, "Read series of chi files: " + basename + " starting from " + starti + " to " + endi
    startiInt = FIX(FLOAT(starti))
    endiInt = FIX(FLOAT(endi))
    numberDigitsInt = FIX(FLOAT(numberDigits))
    result = read_all_fixed_digits(basename, startiInt, endiInt, numberDigitsInt, log)
    if (result eq 1) then begin
        logit, log, "Read series of text files: success"
	outputname = basename + ".idl"
	logit, log, "Saving data in MULTIFIT format into " + outputname
	res = savedata(outputname)
	if (res eq 1) then begin
		logit, log, "Save data in MULTIFIT format to " + outputname + ": success"
		WIDGET_CONTROL, ev.TOP, /DESTROY
	endif else begin
	        tmp = DIALOG_MESSAGE(res, /ERROR)
	        logit, log, "Save data in MULTIFIT format: failed"
	endelse
    endif else begin
        tmp = DIALOG_MESSAGE(result, /ERROR)
        logit, log, "Read series of text files: failed"
    endelse
endif else begin
    logit, log, "Read series of text files: canceled"
    WIDGET_CONTROL, ev.TOP, /DESTROY
endelse
END

PRO convertfileseries, base, log
common files, extension, directory, outputdirectory
common inputinfo, string
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
nameBase =  WIDGET_BASE(basedialog,COLUMN=2, /GRID_LAYOUT, FRAME=1)
nameLa = WIDGET_LABEL(nameBase, VALUE='Base for name of files', /ALIGN_LEFT)
startiLa = WIDGET_LABEL(nameBase, VALUE='First file', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Last file', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Number of digits for files', /ALIGN_LEFT)
nameText = WIDGET_TEXT(nameBase, XSIZE=10, /EDITABLE)
startiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
endiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
numberDigitsText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
extButtons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(extButtons, VALUE='Ok', UVALUE='OK', xsize=80)
cancel = WIDGET_BUTTON(extButtons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
stash = {log:log, nameText:nameText, numberDigitsText:numberDigitsText, startiText:startiText, endiText:endiText}
WIDGET_CONTROL, basedialog, SET_UVALUE=stash
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'convertfileseries', basedialog
END


PRO convertmultiplechi_event, ev
common files, extension, directory, outputdirectory
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
log=stash.log
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.startiText, GET_VALUE=starti
    WIDGET_CONTROL, stash.endiText, GET_VALUE=endi
    WIDGET_CONTROL, stash.intervalText, GET_VALUE=interval
    WIDGET_CONTROL, stash.nameText, GET_VALUE=basename
    WIDGET_CONTROL, stash.startImText, GET_VALUE=startImage
    WIDGET_CONTROL, stash.endImText, GET_VALUE=endImage
    WIDGET_CONTROL, stash.ndigits, GET_VALUE=ndigits
    logit, log, "Read multiple series of chi files: " + basename + " from number " + startImage + " to number " + endImage + " with angles from " + starti + " to " + endi + " with interval " +  interval
    startiInt = FIX(FLOAT(starti))
    endiInt = FIX(FLOAT(endi))
    intervalInt = FIX(FLOAT(interval))
    startImageInt = FIX(FLOAT(startImage[0]))
    endImageInt = FIX(FLOAT(endImage[0])) 
    ndigitsInt = FIX(FLOAT(ndigits))
	nchi = STRTRIM(STRING((endiInt-startiInt)*(endImageInt-startImageInt)/intervalInt,/PRINT), 2)
    messages = nchi + " chi files to process. Please wait..."
	progressBar = Obj_New("SHOWPROGRESS", message=messages)
	progressBar->Start
    for i=startImageInt, endImageInt do begin
		fileindex = intformat(i,ndigitsInt);
		chibase = strtrim(basename) + "_" + strtrim(fileindex)
		logit, log, "Read series of text files: " + chibase + " with angles from " + starti + " to " + endi + " with interval " +  interval
		result = read_all(chibase, startiInt, endiInt, intervalInt, log)
		if (result ne 1) then goto, error
		outputname = basename + "_" + strtrim(fileindex) + ".idl"
		logit, log, "Saving data in MULTIFIT format into " + outputname
		result = savedata(outputname)
		if (result ne 1) then goto, error
		percent = 100.*(i-startImageInt)/(endImageInt-startImageInt)
		progressBar->Update, percent
    endfor
	progressBar->Destroy
	Obj_Destroy, progressBar
    logit, log, "Save multiple sets of chi data files in MULTIFIT format: success"
    WIDGET_CONTROL, ev.TOP, /DESTROY
    return
endif else begin
    logit, log, "Read series of text files: canceled"
    WIDGET_CONTROL, ev.TOP, /DESTROY
endelse
error: tmp = DIALOG_MESSAGE(result, /ERROR)
progressBar->Destroy
Obj_Destroy, progressBar
logit, log, "Failed"
END

PRO convertmultiplechi, base, log
common files, extension, directory, outputdirectory
common inputinfo, string
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
nameBase =  WIDGET_BASE(basedialog,COLUMN=2, /GRID_LAYOUT, FRAME=1)
nameLa = WIDGET_LABEL(nameBase, VALUE='Base for name of images files', /ALIGN_LEFT)
startImLa = WIDGET_LABEL(nameBase, VALUE='First image file number', /ALIGN_LEFT)
endImLa = WIDGET_LABEL(nameBase, VALUE='Last image file number', /ALIGN_LEFT)
digitsImLa = WIDGET_LABEL(nameBase, VALUE='Number of digits for image files', /ALIGN_LEFT)
startiLa = WIDGET_LABEL(nameBase, VALUE='First azimuth angle', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Last azimuth angle', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Interval for azimuths', /ALIGN_LEFT)
nameText = WIDGET_TEXT(nameBase, XSIZE=10, /EDITABLE)
startImText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
endImText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
digitsImText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
startiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
endiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
intervalText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
extButtons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(extButtons, VALUE='Ok', UVALUE='OK', xsize=80)
cancel = WIDGET_BUTTON(extButtons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
stash = {log:log, nameText:nameText, startImText:startImText, endImText:endImText, ndigits:digitsImText, intervalText:intervalText, startiText:startiText, endiText:endiText}
WIDGET_CONTROL, basedialog, SET_UVALUE=stash
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'convertmultiplechi', basedialog
END

;PRO createmultiplefitfromdat, base, log
;common files, extension, directory, outputdirectory
;common inputinfo, string
;basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
;nameBase =  WIDGET_BASE(basedialog,COLUMN=2, /GRID_LAYOUT, FRAME=1)
;nSpLabel = WIDGET_LABEL(nameBase, VALUE='number of subpatterns', /ALIGN_LEFT)
;nSpText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
;extButtons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
;ok = WIDGET_BUTTON(extButtons, VALUE='Ok', UVALUE='OK', xsize=80)
;cancel = WIDGET_BUTTON(extButtons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
;stash = {log:log, nSpText:nSpText}
;WIDGET_CONTROL, basedialog, SET_UVALUE=stash
;WIDGET_CONTROL, basedialog, /REALIZE
;XMANAGER, 'createmultiplefitfromdat', basedialog
;END



; ****************************************** SETUP INPUT (DATA) FILES **************

pro oneInputFile, widget, log
common files, extension, directory, outputdirectory
common inputfiles, inputfiles, activeset
result=dialog_pickfile(title='Select input file', path=directory, DIALOG_PARENT=base, DEFAULT_EXTENSION='.idl')
if (result ne '') then begin
	FDECOMP, result, disk, dir, name, qual, version
	filename = directory + name + "." + Qual
	filenameshort = name + "." + Qual
	if (FILE_TEST(filename)) then begin
		inputText = strarr(1)
		inputText(0) = filenameshort
		WIDGET_CONTROL, widget, SET_VALUE=inputText
		inputfiles = inputText
		logit, log, "Set one dataset: " + filename
		changeActiveSet, log, widget, 0
	endif else begin
		tmp = DIALOG_MESSAGE(filenameshort + " is not in your data directory.", /ERROR)
		logit, log, "Set one dataset: failed"
	endelse
endif else begin
    logit, log, "Read file in MULTIFIT format: canceled"
endelse
END

pro inpufilesFromList, log, list_widget, list
common inputfiles, inputfiles, activeset
common files, extension, directory, outputdirectory
n = n_elements(list)
inputText = strarr(n)
for j=0,n-1 do begin
	filenameshort = list[j]
	filename = directory + filenameshort
	if (FILE_TEST(filename) ne 1) then begin
		tmp = DIALOG_MESSAGE(filenameshort + " is not in your data directory.", /ERROR)
		logit, log, "Failed setting input files: failed on " + filenameshort
		return
	endif
	inputText[j] = filenameshort
endfor
WIDGET_CONTROL, list_widget, SET_VALUE=inputText
inputfiles = inputText
logit, log, "Set multipe datasets from parameter file."
changeActiveSet, log, list_widget, 0
end

PRO multipleInputFiles_event, ev
common inputfiles, inputfiles, activeset
common files, extension, directory, outputdirectory
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
log=stash.log
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.inputFilesBaseSt, GET_VALUE=base
    WIDGET_CONTROL, stash.inputFilesFirstSt, GET_VALUE=firstS
    WIDGET_CONTROL, stash.inputFilesLastSt, GET_VALUE=lastS
    WIDGET_CONTROL, stash.inputFilesDigitsSt, GET_VALUE=digitsS
    first =  FIX(FLOAT(firstS[0]))
    last = FIX(FLOAT(lastS[0]))
    digits = FIX(FLOAT(digitsS[0]))
    inputText = strarr(last-first+1)
    n = 0
    for j=first,last do begin
	fileindex = intformat(j,digits);
	filenameshort = strtrim(base) + "_" + fileindex + ".idl"
	filename = directory + filenameshort
	if (FILE_TEST(filename) ne 1) then begin
		tmp = DIALOG_MESSAGE(filenameshort + " is not in your data directory.", /ERROR)
		logit, log, "Set multiple datasets: failed on " + filenameshort
		return
	endif
	inputText(n) = filenameshort
	n = n + 1
    endfor
    WIDGET_CONTROL, stash.widget, SET_VALUE=inputText
    inputfiles = inputText
    logit, log, "Set multipe datasets: " + strtrim(base) + "_" + intformat(first,digits) + ".idl to " +  strtrim(base) + "_" + intformat(last,digits) + ".idl"
    changeActiveSet, log, stash.widget, 0
    WIDGET_CONTROL, ev.TOP, /DESTROY
endif else begin
    logit, log, "Setting multipe input files... Cancel."
    WIDGET_CONTROL, ev.TOP, /DESTROY
endelse
END

PRO multipleInputFiles, base, widget, log
common files, extension, directory, outputdirectory
common fonts, titlefont, boldfont, mainfont, avFontHeight
input = WIDGET_BASE(/COLUMN, Title='Set multipe input files', /MODAL, GROUP_LEADER=base)
inputMacLa = WIDGET_LABEL(input, VALUE='Set multipe input files', /ALIGN_CENTER, font=titlefont)
inputFiles = WIDGET_BASE(input, COLUMN=2, /GRID_LAYOUT, FRAME=1)
inputFilesBaseLa = WIDGET_LABEL(inputFiles, VALUE='Root name of MULTIFIT files', /ALIGN_LEFT)
inputFilesFirstLa = WIDGET_LABEL(inputFiles, VALUE='First number', /ALIGN_LEFT)
inputFilesLastLa = WIDGET_LABEL(inputFiles, VALUE='Last number', /ALIGN_LEFT)
inputFilesDigitsLa = WIDGET_LABEL(inputFiles, VALUE='Number of digits for file numbers', /ALIGN_LEFT)
inputFilesBaseSt = WIDGET_TEXT(inputFiles, VALUE='', XSIZE=20, /EDITABLE)
inputFilesFirstSt = WIDGET_TEXT(inputFiles, VALUE='', XSIZE=10, /EDITABLE)
inputFilesLastSt = WIDGET_TEXT(inputFiles, VALUE='', XSIZE=10, /EDITABLE)
inputFilesDigitsSt = WIDGET_TEXT(inputFiles, VALUE='', XSIZE=10, /EDITABLE)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK', xsize=80)
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
; Finishing up
stash = {log:log, base:base, inputFilesBaseSt: inputFilesBaseSt,  inputFilesFirstSt: inputFilesFirstSt, inputFilesLastSt:inputFilesLastSt, inputFilesDigitsSt: inputFilesDigitsSt, widget: widget}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'multipleInputFiles', input
END

pro changeActiveSet, log, list, index
common inputfiles, inputfiles, activeset
common files, extension, directory, outputdirectory
file = directory + inputfiles(index)
res = readfile(file)
if (res eq 1) then begin
    activeset = index
    logit, log, "Read active set in MULTIFIT format from " + file + ": success"
    WIDGET_CONTROL, list, set_list_select=index
endif else begin
    tmp = DIALOG_MESSAGE(res, /ERROR)
    logit, log, "Read active set in MULTIFIT format from " + file + ": failed"
endelse
END

pro mapplotActiveSet, base, log, list, index
common rawdata, nalpha, ntheta, alpha, twotheta, data
logit, log, "Mapplot of active dataset "
mindata = min(data)
maxdata = max(data)
mintheta = min(twotheta)
maxtheta = max(twotheta)
; uses a function from compareFitWindow!!
plotDataContour, base, mindata, maxdata, mintheta, maxtheta
END

; ****************************************** REMOVE SLICE **************


PRO removeSlice_event, ev
common inputfiles, inputfiles, activeset
common files, extension, directory, outputdirectory
common datainfo, filenames, alphastart, alphaend, intervalle, date
common rawdata, nalpha, ntheta, alpha, twotheta, data
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
log=stash.log
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.sliceInfoFile, GET_VALUE=file
    WIDGET_CONTROL, stash.sliceInfoMin, GET_VALUE=minTheta
    WIDGET_CONTROL, stash.sliceInfoMax, GET_VALUE=maxTheta
    min =  (FLOAT(minTheta[0]))
    max = (FLOAT(maxTheta[0]))
	FDECOMP, file, disk, dir, name, qual, version
	if (qual ne 'idl') then file = file + ".idl"
	filename = directory + file
	if (FILE_TEST(filename) eq 1) then begin
		tmp = DIALOG_MESSAGE("Can not overwrite " + filename, /ERROR)
		return
	endif
	; Backup the present data
	databk = data
	; Slice data
	slicedata, min, max
	; Save into a file
	test = savedata(file)
	; Restore data
	data = databk
	logit, log, "Sliced dataset between: " + strtrim(string(min),2) + " and " + strtrim(string(max),2) + " and saved it in " +  strtrim(filename)
    WIDGET_CONTROL, ev.TOP, /DESTROY
endif else begin
    logit, log, "Removing slice of Data... Cancel."
    WIDGET_CONTROL, ev.TOP, /DESTROY
endelse
END

pro removeSlice, base, log
logit, log, "Removing slice from active dataset..."
common files, extension, directory, outputdirectory
common fonts, titlefont, boldfont, mainfont, avFontHeight
input = WIDGET_BASE(/COLUMN, Title='Remove Slice in Active Dataset', /MODAL, GROUP_LEADER=base)
title = WIDGET_LABEL(input, VALUE='Remove Slice in Active Dataset', /ALIGN_CENTER, font=titlefont)
sliceInfo = WIDGET_BASE(input, COLUMN=2, /GRID_LAYOUT, FRAME=1)
sliceInfoFLa = WIDGET_LABEL(sliceInfo, VALUE='Name of output file', /ALIGN_LEFT)
sliceInfoFirstLa = WIDGET_LABEL(sliceInfo, VALUE='2 theta min', /ALIGN_LEFT)
sliceInfoLastLa = WIDGET_LABEL(sliceInfo, VALUE='2 theta max', /ALIGN_LEFT)
sliceInfoFile = WIDGET_TEXT(sliceInfo, VALUE='', XSIZE=20, /EDITABLE)
sliceInfoMin = WIDGET_TEXT(sliceInfo, VALUE='', XSIZE=20, /EDITABLE)
sliceInfoMax = WIDGET_TEXT(sliceInfo, VALUE='', XSIZE=20, /EDITABLE)
; Buttons
buttons = WIDGET_BASE(input,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK', xsize=80)
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
; Finishing up
stash = {log:log, base:base, sliceInfoMin: sliceInfoMin, sliceInfoMax: sliceInfoMax, sliceInfoFile: sliceInfoFile}
WIDGET_CONTROL, input, SET_UVALUE=stash
WIDGET_CONTROL, input, /REALIZE
XMANAGER, 'removeSlice', input
END

; ****************************************** EXPORT FOR MAUD **************

FUNCTION saveesg, file
common experiment, wavelength, detectordistance
distance = detectordistance/10
openw, lun, file, /get_lun
printdataesgall, lun, distance
free_lun, lun
return, 1
END

pro maudExport, base, listSets, log
common files, extension, directory, outputdirectory
common inputfiles, inputfiles, activeset
if (FILE_TEST( outputdirectory, /DIRECTORY) ne 1) then begin
        tmp = DIALOG_MESSAGE("Error with directory: " + outputdirectory, /ERROR)
	logit, log, "Saving data in MAUD format in: " + outputdirectory + " not a valid directory"
	return
endif
nfiles = N_ELEMENTS(inputfiles)
for i=0, nfiles-1 do begin
	file = directory + inputfiles(i)
	logit, log, "Reading: " + file
	res = readfile(file)
	if (res ne 1) then begin
        	tmp = DIALOG_MESSAGE(res, /ERROR)
		logit, log, "Failed"
		return
	endif
	FDECOMP, file, disk, dir, name, qual, version
	file = outputdirectory + name + ".esg"
	logit, log, "Saving data in MAUD format in " + file
	if (saveesg(file) ne 1) then begin
        	tmp = DIALOG_MESSAGE("Saving to "+file+" failed", /ERROR)
		logit, log, "Failed"
		return
	endif
endfor
end

; *************************************** fit2dmac **************

FUNCTION fit2dinput, log, startAz, endAz, interval, directory, basename
macrofile = strtrim(strtrim(directory)+strtrim(basename)+'.mac')
if (endAz lt startAz) then return, "Start azimuth larger than end azimuth!"
ON_IOERROR, BADINPUT
GET_LUN, lun
OPENW, lun, macrofile
printf, lun, "%!*\\ MACRO FOR fit2d"
printf, lun, "%!*\\ generated by Multifit by Sebastien Merkel"
printf, lun, "%!*\\ output files: " + strtrim(directory) + strtrim(basename) + "_***.chi"
printf, lun, "%!*\\ start azimuth: " + STRING(startAz,/PRINT)
printf, lun, "%!*\\ end azimuth: " + STRING(endAz,/PRINT)
printf, lun, "%!*\\ interval: " + STRING(interval,/PRINT)
printf, lun, "%!*\\"
printf, lun, "EXIT"
printf, lun, "POWDER DIFFRACTION (2-D)"
top = (endAz-startAz)/interval
if (top eq 1) then top = 0
for i=0,top do begin
    angle = startAz+i*interval
    anglelow = 1.0*angle-1.0*interval/2.
    anglehigh = 1.0*angle+1.0*interval/2.
    if (anglelow gt 180.) then anglelow = anglelow-360.
    if (anglehigh gt 180.) then anglehigh = anglehigh-360.
    filename = strtrim(directory)+strtrim(basename) + "_" + STRTRIM(STRING(angle,/PRINT),2) + ".chi"
    if ((i eq 0) or (i eq top)) then $
    logit, log, "Integration for delta between " +  STRTRIM(STRING(anglelow,/PRINT),2) + " and " +  STRTRIM(STRING(anglehigh,/PRINT),2) + " to be save in " + filename
    if (i eq 0) then logit, log, "   ..."
    printf, lun, "CAKE"
    printf, lun, "INTEGRATE"
    printf, lun, "O.K."
    printf, lun, "START AZIMUTH"
    printf, lun, STRTRIM(STRING(anglelow,/PRINT),2)
    printf, lun, "END AZIMUTH"
    printf, lun, STRTRIM(STRING(anglehigh,/PRINT),2)
    printf, lun, "AZIMUTH BIN"
    printf, lun, STRTRIM(STRING(1,/PRINT),2)
    printf, lun, "O.K."
    printf, lun, "EXIT"
	if (i eq 0) then begin
		printf, lun, "OPTIONS"
		printf, lun, "Z-AXIS LABEL"
		printf, lun, "Intensity"
		printf, lun, "EXIT"
	endif
    printf, lun, "OUTPUT"
    printf, lun, "CHIPLOT"
    printf, lun, "FILE NAME"
    printf, lun, filename
    printf, lun, "O.K."
    printf, lun, "EXCHANGE"
endfor
printf, lun, "EXIT"
printf, lun, "MACROS / LOG FILE"
printf, lun, "%!*\\ END OF EXPG_IO MACRO FILE"
free_lun, lun
logit, log, "Macro save in file: " + macrofile
return, 1
BADINPUT: return, !ERR_STRING
END

FUNCTION fit2dinputlong, log, inputTiff, inputMar  
common fit2dinputlong, inputdir, outputdir, base, ext, firstS, lastS, digitsS, startAzS, endAzS, intervalS

if (WIDGET_INFO(inputTiff, /BUTTON_SET) eq 1) then begin
    fileFormat = 0
endif else if (WIDGET_INFO(inputMar, /BUTTON_SET) eq 1) then begin
    fileFormat = 1
endif
first =  FIX(FLOAT(firstS[0]))
last = FIX(FLOAT(lastS[0]))
digits = FIX(FLOAT(digitsS[0]))
startAz = FIX(FLOAT(startAzS[0]))
endAz = FIX(FLOAT(endAzS[0]))
interval = FIX(FLOAT(intervalS[0]))
macrofile = strtrim(strtrim(outputdir)+strtrim(base)+'.mac')
logit, log, "Macro file: " + macrofile
if (endAz lt startAz) then return, "Start azimuth larger than end azimuth!"
ON_IOERROR, BADINPUT
GET_LUN, lun
OPENW, lun, macrofile
printf, lun, "%!*\\ MACRO FOR fit2d"
printf, lun, "%!*\\ generated by Multifit by Sebastien Merkel"
fileindex = intformat(first,digits);
filename = strtrim(inputdir) + strtrim(base) + "_" + strtrim(fileindex) + strtrim(ext)
logit, log, "First input file: " + filename
printf, lun, "%!*\\ First input file: " + filename
fileindex = intformat(last,digits);
filename = strtrim(inputdir) + strtrim(base) + "_" + strtrim(fileindex) + strtrim(ext)
logit, log, "Last input file: " + filename
printf, lun, "%!*\\ Last input file: \n    " + filename
fileindex = intformat(first,digits);
filename1 = strtrim(outputdir) + strtrim(base) + "_" + strtrim(fileindex) + "_" + strtrim(string(startAz),2) + ".chi"
filename2 = strtrim(outputdir) + strtrim(base) + "_" + strtrim(fileindex) + "_" + strtrim(string(endAz),2) + ".chi"
printf, lun, "%!*\\ output files:"
printf, lun, "%!*\\   " + filename1 + " ---- " + filename2
printf, lun, "%!*\\    ------ "
fileindex = intformat(last,digits);
filename1 = strtrim(outputdir) + strtrim(base) + "_" + strtrim(fileindex) + "_" + strtrim(string(startAz),2) + ".chi"
filename2 = strtrim(outputdir) + strtrim(base) + "_" + strtrim(fileindex) + "_" + strtrim(string(endAz),2) + ".chi"
printf, lun, "%!*\\   " + filename1 + " ---- " + filename2
printf, lun, "%!*\\ start azimuth: " + STRING(startAz,/PRINT)
printf, lun, "%!*\\ end azimuth: " + STRING(endAz,/PRINT)
printf, lun, "%!*\\ interval: " + STRING(interval,/PRINT)
printf, lun, "%!*\\"
printf, lun, "EXIT"
printf, lun, "POWDER DIFFRACTION (2-D)"

topAz  = (endAz-startAz)/interval
if (topAz eq 1) then topAz = 0
topInputs = (last-first)+1
nChi = ((last-first)+1)*topAz
logit, log, "Number of chi files to be created by fit2d: " + strtrim(STRING(nChi,/PRINT),2) 
for j=first,last do begin
	printf, lun, "INPUT"
	fileindex = intformat(j,digits);
	filename = strtrim(inputdir) + strtrim(base) + "_" + strtrim(fileindex) + strtrim(ext)
	printf, lun, filename
	printf, lun, "O.K."
	; If Tiff file, we need a second OK
	if (fileFormat eq 0) then printf, lun, "O.K."
	basechi = strtrim(outputdir) + strtrim(base) + "_" + strtrim(fileindex) + "_"
	logit, log, "Data in " + strtrim(STRING(topAz,/PRINT),2) + " files: " + basechi + strtrim(STRING(startAz,/PRINT),2) + ".chi --- " + basechi + strtrim(STRING(endAz,/PRINT),2) + ".chi"
	for i=0,topAz do begin
		angle = startAz+i*interval
		anglelow = 1.0*angle-1.0*interval/2.
		anglehigh = 1.0*angle+1.0*interval/2.
		if (anglelow gt 180.) then anglelow = anglelow-360.
		if (anglehigh gt 180.) then anglehigh = anglehigh-360.
		filename = strtrim(basechi) + STRTRIM(STRING(angle,/PRINT),2) + ".chi"
		printf, lun, "CAKE"
		printf, lun, "INTEGRATE"
		printf, lun, "O.K."
		printf, lun, "START AZIMUTH"
		printf, lun, STRTRIM(STRING(anglelow,/PRINT),2)
		printf, lun, "END AZIMUTH"
		printf, lun, STRTRIM(STRING(anglehigh,/PRINT),2)
		printf, lun, "AZIMUTH BIN"
		printf, lun, STRTRIM(STRING(1,/PRINT),2)
		printf, lun, "O.K."
		printf, lun, "EXIT"
		if ((i eq 0) and (j eq first)) then begin
			printf, lun, "OPTIONS"
			printf, lun, "Z-AXIS LABEL"
			printf, lun, "Intensity"
			printf, lun, "EXIT"
		endif
		printf, lun, "OUTPUT"
		printf, lun, "CHIPLOT"
		printf, lun, "FILE NAME"
		printf, lun, filename
		printf, lun, "O.K."
		printf, lun, "EXCHANGE"
	endfor
endfor
printf, lun, "EXIT"
printf, lun, "MACROS / LOG FILE"
printf, lun, "%!*\\ END OF EXPG_IO MACRO FILE"
free_lun, lun
logit, log, "Macro save in file: " + macrofile
return, 1
BADINPUT: return, !ERR_STRING
END

; subroutine with GUI to create macro for a single image
PRO fit2dmac_event, ev
common files, extension, directory, outputdirectory
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
log=stash.log
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.fit2dMacAzimuthSt, GET_VALUE=startAz
    WIDGET_CONTROL, stash.fit2dMacAzimuthEnd, GET_VALUE=endAz
    WIDGET_CONTROL, stash.fit2dMacSliceText, GET_VALUE=interval
    WIDGET_CONTROL, stash.fit2dMacDirText, GET_VALUE=directory
    WIDGET_CONTROL, stash.fit2dMacBaseText, GET_VALUE=basename
    logit, log, "Creating macro for fit2d..."
    logit, log, "Fit2d will create files starting with " + basename + "_ with angles from " + startAz + " to " + endAz + " with interval " +  interval
    result = fit2dinput(log, FIX(FLOAT(startAz[0])), FIX(FLOAT(endAz[0])), FIX(FLOAT(interval[0])), directory, basename)
    if (FIX(result) ne 1) then begin
        logit, log, "Macro creation failed"
        tmp = DIALOG_MESSAGE(result, /ERROR)
    endif else begin
        WIDGET_CONTROL, ev.TOP, /DESTROY
    endelse
endif else begin
    logit, log, "Creating macro for fit2d... Cancel."
    WIDGET_CONTROL, ev.TOP, /DESTROY
endelse
END
PRO fit2dmac, base, log
common files, extension, directory, outputdirectory
common fonts, titlefont, boldfont, mainfont, avFontHeight
fit2d = WIDGET_BASE(/COLUMN, Title='Create macro for fit2d (single images)', /MODAL, GROUP_LEADER=base)
fit2dMacLa = WIDGET_LABEL(fit2d, VALUE='Create macro for fit2d (single images)', /ALIGN_center, font=titlefont)
fit2dMacDir = WIDGET_BASE(fit2d, /ROW)
fit2dMacDirLa = WIDGET_LABEL(fit2dMacDir, VALUE='Directory for CHI files export', /ALIGN_LEFT)
fit2dMacDirText = WIDGET_TEXT(fit2dMacDir, VALUE=directory, XSIZE=60)
fit2dMacBase = WIDGET_BASE(fit2d, /ROW)
fit2dMacBaseLa = WIDGET_LABEL(fit2dMacBase, VALUE='Base for filenames', /ALIGN_LEFT)
fit2dMacBaseText = WIDGET_TEXT(fit2dMacBase, VALUE='filename', XSIZE=60, /EDITABLE)
fit2dMacAzimuth = WIDGET_BASE(fit2d, /ROW)
fit2dMacAzimuthLa = WIDGET_LABEL(fit2dMacAzimuth, VALUE='Range for azimuth angles', /ALIGN_LEFT)
fit2dMacAzimuthSt = WIDGET_TEXT(fit2dMacAzimuth, VALUE='0',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
fit2dMacAzimuthLa2 = WIDGET_LABEL(fit2dMacAzimuth, VALUE='to', /ALIGN_LEFT)
fit2dMacAzimuthEnd = WIDGET_TEXT(fit2dMacAzimuth, VALUE='360',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
fit2dMacSlice = WIDGET_BASE(fit2d, /ROW)
fit2dMacSliceLa = WIDGET_LABEL(fit2dMacSlice, VALUE='Sliced size (degrees)', /ALIGN_LEFT)
fit2dMacSliceText = WIDGET_TEXT(fit2dMacSlice, VALUE='5',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
buttons = WIDGET_BASE(fit2d,/ROW, /ALIGN_CENTER)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK', xsize=80)
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
stash = {log:log, fit2dMacDirText:fit2dMacDirText, fit2dMacBaseText:fit2dMacBaseText, $
         fit2dMacAzimuthSt:fit2dMacAzimuthSt, fit2dMacAzimuthEnd:fit2dMacAzimuthEnd, $
         fit2dMacSliceText:fit2dMacSliceText}
WIDGET_CONTROL, fit2d, SET_UVALUE=stash
WIDGET_CONTROL, fit2d, /REALIZE
XMANAGER, 'fit2dmac', fit2d
END


; subroutine with GUI to create macro for a multiple images

PRO chgDiffDir, base, widget
common files, extension, directory, outputdirectory
result=dialog_pickfile(/DIRECTORY,title='Select input directory', path=directory, DIALOG_PARENT=base)
if (result ne '') then begin
    WIDGET_CONTROL, widget, SET_VALUE=result
endif
END

PRO fit2dmaclong_event, ev
common fit2dinputlong, inputdir, outputdir, base, ext, first, last, digits, startAz, endAz, interval
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
log=stash.log
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.fit2dInputText, GET_VALUE=inputdir
    WIDGET_CONTROL, stash.fit2dInputFilesBaseSt, GET_VALUE=base
    WIDGET_CONTROL, stash.fit2dInputFilesFirstSt, GET_VALUE=first
    WIDGET_CONTROL, stash.fit2dInputFilesLastSt, GET_VALUE=last
    WIDGET_CONTROL, stash.fit2dInputFilesDigitsSt, GET_VALUE=digits
    WIDGET_CONTROL, stash.fit2dInputFilesExtSt, GET_VALUE=ext
    WIDGET_CONTROL, stash.fit2dOutputDirText, GET_VALUE=outputdir
    WIDGET_CONTROL, stash.fit2dMacAzStVa, GET_VALUE=startAz
    WIDGET_CONTROL, stash.fit2dMacAzLtVa, GET_VALUE=endAz
    WIDGET_CONTROL, stash.fit2dMacAzInVa, GET_VALUE=interval
    logit, log, "Creating macro for fit2d..."
	; test if the diffraction images can be found
	firstInt =  FIX(FLOAT(first[0]))
	lastInt = FIX(FLOAT(last[0]))
	digitsInt = FIX(FLOAT(digits[0]))
	for index = firstInt, lastInt do begin
		fileindex = intformat(index,digits);
		filename = strtrim(inputdir) + strtrim(base) + "_" + strtrim(fileindex) + strtrim(ext)
		if (FILE_TEST(filename) ne 1) then begin
			messages = strarr(3)
			messages[0] = filename + " can not be found."
			messages[1] = "Fit2d will not find it either."
			messages[2] = "Keep going?"
			tmp = DIALOG_MESSAGE( messages , /QUESTION)
			if (tmp eq 'No') then return
		endif
	endfor
	; 
    result = fit2dinputlong(log, stash.inputTiff, stash.inputMar)
    if (FIX(result) ne 1) then begin
        logit, log, "Macro creation failed"
        tmp = DIALOG_MESSAGE(result, /ERROR)
    endif else begin
        WIDGET_CONTROL, ev.TOP, /DESTROY
    endelse
endif else if (uval eq 'DIFFDIR') then begin
	chgDiffDir, stash.base, stash.fit2dInputText
endif else if (uval eq 'CANCEL') then begin
    logit, log, "Creating macro for fit2d... Cancel."
    WIDGET_CONTROL, ev.TOP, /DESTROY
endif
END

PRO fit2dmaclong, base, log
common files, extension, directory, outputdirectory
common fonts, titlefont, boldfont, mainfont, avFontHeight
fit2d = WIDGET_BASE(/COLUMN, Title='Create macro for fit2d (multiple images)', /MODAL, GROUP_LEADER=base)
fit2dMacLa = WIDGET_LABEL(fit2d, VALUE='Create macro for fit2d (multiple images)', /ALIGN_CENTER, font=titlefont)
; Input files: diffraction images
fit2dInput = WIDGET_BASE(fit2d, /COLUMN, FRAME=1)
fit2dInputDir = WIDGET_BASE(fit2dInput, COLUMN=3)
fit2dInputDirLa = WIDGET_LABEL(fit2dInputDir, VALUE='Directory with diffraction images', /ALIGN_LEFT)
fit2dInputText = WIDGET_TEXT(fit2dInputDir, VALUE='', XSIZE=60)
fit2dInputBu = WIDGET_BUTTON(fit2dInputDir, VALUE='Change', UVALUE='DIFFDIR')
fit2dInputFiles = WIDGET_BASE(fit2dInput, COLUMN=2, /GRID_LAYOUT)
fit2dInputFilesBaseLa = WIDGET_LABEL(fit2dInputFiles, VALUE='Root name of image files', /ALIGN_LEFT)
fit2dInputFilesFirstLa = WIDGET_LABEL(fit2dInputFiles, VALUE='First number', /ALIGN_LEFT)
fit2dInputFilesLastLa = WIDGET_LABEL(fit2dInputFiles, VALUE='Last number', /ALIGN_LEFT)
fit2dInputFilesDigitsLa = WIDGET_LABEL(fit2dInputFiles, VALUE='Number of digits for file numbers', /ALIGN_LEFT)
fit2dInputFilesExtLa = WIDGET_LABEL(fit2dInputFiles, VALUE='Extension', /ALIGN_LEFT)
fit2dInputFilesBaseSt = WIDGET_TEXT(fit2dInputFiles, VALUE='', XSIZE=20, /EDITABLE)
fit2dInputFilesFirstSt = WIDGET_TEXT(fit2dInputFiles, VALUE='', XSIZE=10, /EDITABLE)
fit2dInputFilesLastSt = WIDGET_TEXT(fit2dInputFiles, VALUE='', XSIZE=10, /EDITABLE)
fit2dInputFilesDigitsSt = WIDGET_TEXT(fit2dInputFiles, VALUE='', XSIZE=10, /EDITABLE)
fit2dInputFilesExtSt = WIDGET_TEXT(fit2dInputFiles, VALUE='.tiff', XSIZE=10, /EDITABLE)
fit2dInputFormat = WIDGET_BASE(fit2dInput, COLUMN=2)
fit2dInputFormatLa = WIDGET_LABEL(fit2dInputFormat, VALUE='File format', /ALIGN_LEFT)
format = Widget_Base(fit2dInputFormat, Column=1, /Exclusive)
inputTiff = Widget_Button(format, Value='Tiff', UVALUE='FORMAT')
inputMar = Widget_Button(format, Value='Mar3450', UVALUE='FORMAT')
; Output files: chi files
fit2dOutput = WIDGET_BASE(fit2d, /COLUMN, FRAME=1)
fit2dOutputDir = WIDGET_BASE(fit2dOutput, /ROW)
fit2dOutputDirLa = WIDGET_LABEL(fit2dOutputDir, VALUE='Directory for CHI files export ', /ALIGN_LEFT)
fit2dOutputDirText = WIDGET_TEXT(fit2dOutputDir, VALUE=directory, XSIZE=60)
fit2dOutputFiles = WIDGET_BASE(fit2dOutput, COLUMN=2, /GRID_LAYOUT)
fit2dMacAzStLa = WIDGET_LABEL(fit2dOutputFiles, VALUE='First azimuth angle', /ALIGN_LEFT)
fit2dMacAzLtLa = WIDGET_LABEL(fit2dOutputFiles, VALUE='Last azimuth angle', /ALIGN_LEFT)
fit2dMacAzInLa = WIDGET_LABEL(fit2dOutputFiles, VALUE='Interval', /ALIGN_LEFT)
fit2dMacAzStVa = WIDGET_TEXT(fit2dOutputFiles, VALUE='0',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
fit2dMacAzLtVa = WIDGET_TEXT(fit2dOutputFiles, VALUE='360',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
fit2dMacAzInVa = WIDGET_TEXT(fit2dOutputFiles, VALUE='5',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
; Buttons
buttons = WIDGET_BASE(fit2d,/ROW, /ALIGN_CENTER, /GRID_LAYOUT)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK', xsize=80)
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
; Finishing up
stash = {log:log, base:fit2d, fit2dInputText:fit2dInputText, fit2dInputFilesBaseSt: fit2dInputFilesBaseSt,  fit2dInputFilesFirstSt: fit2dInputFilesFirstSt, fit2dInputFilesLastSt:fit2dInputFilesLastSt, fit2dInputFilesDigitsSt: fit2dInputFilesDigitsSt, fit2dInputFilesExtSt: fit2dInputFilesExtSt, fit2dOutputDirText:fit2dOutputDirText, fit2dMacAzStVa:fit2dMacAzStVa, fit2dMacAzLtVa:fit2dMacAzLtVa, fit2dMacAzInVa:fit2dMacAzInVa, inputTiff: inputTiff, inputMar: inputMar}
WIDGET_CONTROL, fit2d, SET_UVALUE=stash
WIDGET_CONTROL, fit2d, /REALIZE
XMANAGER, 'fit2dmaclong', fit2d
END



; *************************************** MAIN GUI FOR LOADING FILES AND FIT PEAKS **************

PRO resizebase, base, stash
	widget_control, base, TLB_GET_SIZE=size
	; resizing top part
	widget_control, stash.defaultBase, xsize=size[0], UPDATE=0  
	;tmpsize = widget_info(stash.defaultBase, /GEOMETRY)
	;xsize = (tmpsize.SCR_XSIZE - (2*tmpsize.MARGIN))
	;tmp = widget_info(stash.pathBase, /GEOMETRY)
	;print, tmp
	;widget_control, stash.pathBase, SCR_xsize=xsize, xsize=xsize
	;tmp = widget_info(stash.pathBase, /GEOMETRY)
	;print, tmp
	;tmpsize = widget_info(stash.inputDirLa, /GEOMETRY)
	;xsize -= (tmpsize.SCR_XSIZE + (2*tmpsize.MARGIN))
	;tmpsize = widget_info(stash.inputDirBu, /GEOMETRY)
	;xsize -= (tmpsize.SCR_XSIZE + (2*tmpsize.MARGIN))
	;widget_control, stash.inputDirText, SCR_XSIZE=xsize
	;widget_control, stash.outputDirText, SCR_XSIZE=xsize
	;widget_control, stash.pathBase, update=1
	;widget_control, stash.inputDirBu, /update, SCR_XSIZE=80
	;widget_control, stash.outputDirBu, /update, SCR_XSIZE=80
	;widget_control, stash.pathBase, /update
	; resizing bottom part
	widget_control, stash.bottom, xsize=size[0], UPDATE=0  
	tmpsize = widget_info(stash.listBase, /GEOMETRY)
	tmpsize2 = widget_info(stash.defaultBase, /GEOMETRY)
	xsize = size[0]-(tmpsize.SCR_XSIZE + (2*tmpsize.MARGIN))
	ysize = size[1]-(tmpsize2.SCR_YSIZE + (2*tmpsize2.MARGIN))
	widget_control, stash.log, SCR_XSIZE=xsize, SCR_YSIZE=ysize, UPDATE=0  
	widget_control, stash.listBase, SCR_YSIZE=ysize, UPDATE=0  
	tmpsize = widget_info(stash.listLa, /GEOMETRY)
	ysize -= (tmpsize.SCR_YSIZE + (2*tmpsize.MARGIN))
	tmpsize = widget_info(stash.mapplot, /GEOMETRY)
	ysize -= (tmpsize.SCR_YSIZE + (2*tmpsize.MARGIN))
	tmpsize = widget_info(stash.plotactive, /GEOMETRY)
	ysize -= (tmpsize.SCR_YSIZE + (2*tmpsize.MARGIN))
	ysize -= (tmpsize.SCR_YSIZE + (2*tmpsize.MARGIN))
	widget_control, stash.listSets, SCR_YSIZE=ysize, /UPDATE  
END

PRO exitit, widget
    WIDGET_CONTROL, widget, /DESTROY
end

PRO gui_event, ev
                                ; Get the 'stash' structure.
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
active = WIDGET_INFO(stash.listSets, /LIST_SELECT)
CASE ev.id OF
	stash.base: resizebase, stash.base, stash
	else: begin
		CASE uval OF
		'INPUTDIR': chgInputDir, stash.base, stash.log, stash.inputDirText
		'OUTPUTDIR': chgOutputDir, stash.base, stash.log, stash.outputDirText
		'SAVEPARAM': saveparams, stash.base, stash.log, stash.inputDirText, stash.outputDirText, stash.waveText, stash.ipDistanceText
		'READPARAM': readparams, stash.base, stash.log, stash.inputDirText, stash.outputDirText, stash.waveText, stash.ipDistanceText, stash.listSets
		'REMOVESLICE': removeSlice, stash.base
		'WAVE': chgWavelength, stash.base, stash.log, stash.waveText
		'DETECTORDISTANCE': chgDetectorDistance, stash.base, stash.log, stash.ipDistanceText
		'ONEIDL': oneInputFile, stash.listSets, stash.log
		'MULTIPLEIDL': multipleInputFiles, stash.base, stash.listSets, stash.log
		'CONVERTONECHI': convertonechi, stash.base, stash.log
		'CONVERTMULTIPLECHI': convertmultiplechi, stash.base, stash.log
		'CONVERTFILESERIES': convertfileseries, stash.base, stash.log
		'FIT2DMAC': fit2dmac, stash.base, stash.log
		'FIT2DMACLONG': fit2dmaclong, stash.base, stash.log
		'MAUDEXPORT': maudExport, stash.base, stash.listSets, stash.log
		'LISTSETS': changeActiveSet, stash.log, stash.listSets, active[0]
		'MAPPLOT': mapplotActiveSet, stash.base, stash.log, stash.listSets, active[0]
		'PLOTONESET': plotWindow, stash.base
		'FITONESET': fitWindow, stash.base
		'FITONESETAUTO': fitAutoWindow, stash.base, 1
		'FITMULTIPLESETAUTO': fitAutoWindow, stash.base, 2
		'FITONESETJCPDS': fitJCPDSWindow, stash.base
		'DATFILESTOFITFILE': createFitfromdatWindow, stash.base
		'COMPAREFIT': compareFitWindow, stash.base
		'COMPAREFIT1D': compareFit1DWindow, stash.base
		'PLOTFIT': plotResultsWindow, stash.base
		'MODELONEIMAGE': createModelWindow, stash.base
		'ABOUT': aboutWindow, stash.base
		'NOTAVAILABLE': tmp = DIALOG_MESSAGE("This function is not implemented yet!", /ERROR)
		'FORBIDDEN': tmp = DIALOG_MESSAGE("You need to register", /ERROR)
		'EXIT': exitit, ev.top
		else:
		ENDCASE
	endcase
endcase
END

PRO gui
common files, extension, directory, outputdirectory
common experiment, wavelength, detectordistance
common fonts, titlefont, boldfont, mainfont, avFontHeight
; default values
load_defaults_startup
; base GUI
base = WIDGET_BASE(Title='Multipeak superfit!',/COLUMN, MBAR=bar, /TLB_SIZE_EVENTS)
; File menu
file_menu = WIDGET_BUTTON(bar, VALUE='File', /MENU)
file_bttn3 = WIDGET_BUTTON(file_menu, VALUE='Save parameters', UVALUE='SAVEPARAM')
file_bttn4 = WIDGET_BUTTON(file_menu, VALUE='Read parameters', UVALUE='READPARAM')
file_bttn5 = WIDGET_BUTTON(file_menu, VALUE='Exit', UVALUE='EXIT', /SEPARATOR)
; Input file menu
input_menu = WIDGET_BUTTON(bar, VALUE='Input files', /MENU) 
file_bttn1 = WIDGET_BUTTON(input_menu, VALUE='Single input file', UVALUE='ONEIDL')
file_bttn2 = WIDGET_BUTTON(input_menu, VALUE='Multiple input files', UVALUE='MULTIPLEIDL')
; Data menu
data_menu = WIDGET_BUTTON(bar, VALUE='Data handling', /MENU) 
fit2d_bttn1 = WIDGET_BUTTON(data_menu, VALUE='Create Fit2d macro for one file', UVALUE='FIT2DMAC')
fit2d_bttn2 = WIDGET_BUTTON(data_menu, VALUE='Create Fit2d macro for multiple files', UVALUE='FIT2DMACLONG')
fit2d_bttn3 = WIDGET_BUTTON(data_menu, VALUE='Convert CHI to IDL: 1 set', UVALUE='CONVERTONECHI')
fit2d_bttn4 = WIDGET_BUTTON(data_menu, VALUE='Convert CHI to IDL: multiple sets', UVALUE='CONVERTMULTIPLECHI')
fit2d_bttn4 = WIDGET_BUTTON(data_menu, VALUE='Convert CHI to IDL: file series', UVALUE='CONVERTFILESERIES')
maud_bttn1 = WIDGET_BUTTON(data_menu, VALUE='Export active datasets for Maud', UVALUE='MAUDEXPORT', /SEPARATOR)
data_bttn3 = WIDGET_BUTTON(data_menu, VALUE='Remove slice from active dataset', UVALUE='REMOVESLICE', /SEPARATOR)
; Image fit menu
fit_menu = WIDGET_BUTTON(bar, VALUE='Image fit', /MENU) 
fit_bttn1 = WIDGET_BUTTON(fit_menu, VALUE='Fit active dataset (manual)', UVALUE='FITONESET')
fit_bttn3 = WIDGET_BUTTON(fit_menu, VALUE='Create a fit model', UVALUE='MODELONEIMAGE', /SEPARATOR)
fit_bttn5 = WIDGET_BUTTON(fit_menu, VALUE='Fit one image: automatic', UVALUE='FITONESETAUTO', /SEPARATOR)
fit_bttn6 = WIDGET_BUTTON(fit_menu, VALUE='Fit multiple images: automatic', UVALUE='FITMULTIPLESETAUTO')
fit_bttn7 = WIDGET_BUTTON(fit_menu, VALUE='Fit one image: from JCPDS', UVALUE='FITONESETJCPDS', /SEPARATOR)
fit_bttn8 = WIDGET_BUTTON(fit_menu, VALUE='Write .dats into .fit file(s)', UVALUE='DATFILESTOFITFILE', /SEPARATOR)
fitres_bttn1 = WIDGET_BUTTON(fit_menu, VALUE='Compare results with active dataset: mapplot', UVALUE='COMPAREFIT', /SEPARATOR)
fitres_bttn2 = WIDGET_BUTTON(fit_menu, VALUE='Compare results with active dataset: 2-D plots', UVALUE='COMPAREFIT1D')
fitres_bttn3 = WIDGET_BUTTON(fit_menu, VALUE='Plot Results', UVALUE='PLOTFIT', /SEPARATOR)
; About menu
about_menu = WIDGET_BUTTON(bar, VALUE='About...', /MENU, /ALIGN_RIGHT) 
about_bttn1 = WIDGET_BUTTON(about_menu, VALUE='About Multifit', UVALUE='ABOUT')
; top container
top = WIDGET_BASE(base,/ROW)
; display default parameters
ysizeparams=fix(2.*avFontHeight)
defaultBase =  WIDGET_BASE(top,/COLUMN, FRAME=1, /ALIGN_CENTER)
pathBase =  WIDGET_BASE(defaultBase,COLUMN=3)
inputDirLa = WIDGET_LABEL(pathBase, VALUE='Directory with CHI or MULTIFIT data files: ', /ALIGN_LEFT, ysize=ysizeparams)
outputDirLa = WIDGET_LABEL(pathBase, VALUE='Directory to save fits: ', /ALIGN_LEFT, ysize=ysizeparams)
inputDirText = WIDGET_TEXT(pathBase, VALUE=directory, XSIZE=40, SCR_YSIZE=ysizeparams)
outputDirText = WIDGET_TEXT(pathBase, VALUE=outputdirectory, XSIZE=40, SCR_YSIZE=ysizeparams)
inputDirBu = WIDGET_BUTTON(pathBase, VALUE='Change', UVALUE='INPUTDIR', ysize=ysizeparams, xsize=80)
outputDirBu = WIDGET_BUTTON(pathBase, VALUE='Change', UVALUE='OUTPUTDIR', ysize=ysizeparams, xsize=80)
waveBase =  WIDGET_BASE(defaultBase,COLUMN=3)
waveLa = WIDGET_LABEL(waveBase, VALUE='Wavelength (in angstroms)', /ALIGN_LEFT, ysize=ysizeparams)
ipDistanceLa = WIDGET_LABEL(waveBase, VALUE='Sample-Detector distance (in mm)', /ALIGN_LEFT, ysize=ysizeparams)
waveText = WIDGET_TEXT(waveBase, VALUE=STRTRIM(STRING(wavelength,/PRINT),2), XSIZE=10, SCR_ysize=ysizeparams)
ipDistanceText = WIDGET_TEXT(waveBase, VALUE=STRTRIM(STRING(detectordistance,/PRINT),2), XSIZE=10, SCR_ysize=ysizeparams)
waveBu = WIDGET_BUTTON(waveBase, VALUE='Change', UVALUE='WAVE', ysize=ysizeparams, xsize=80)
ipDistanceBu = WIDGET_BUTTON(waveBase, VALUE='Change', UVALUE='DETECTORDISTANCE', ysize=ysizeparams, xsize=80)
; List datasets and logwindow
bottom =  WIDGET_BASE(base,/ROW)
listBase =  WIDGET_BASE(bottom,/COLUMN, FRAME=1)
listLa = WIDGET_LABEL(listBase, VALUE='Active datasets', /ALIGN_CENTER)
listSets = Widget_List(listBase, VALUE='', UVALUE='LISTSETS',YSIZE=15, XSIZE=15)
mapplot = WIDGET_BUTTON(listBase, VALUE='Mapplot', UVALUE='MAPPLOT')
plotactive = WIDGET_BUTTON(listBase, VALUE='Plot', UVALUE='PLOTONESET')
log = WIDGET_TEXT(bottom, XSIZE=60, YSIZE=22, /ALIGN_CENTER, /EDITABLE, /WRAP, /SCROLL)

stash = {base: base, outputDirLa: outputDirLa,  inputDirLa: inputDirLa, defaultBase: defaultBase, bottom:bottom, listBase: listBase, listLa:listLa, log:log, inputDirText:inputDirText, outputDirText:outputDirText, waveText:waveText, ipDistanceText: ipDistanceText, listSets: listSets, mapplot:mapplot, plotactive:plotactive, inputDirBu:inputDirBu, outputDirBu:outputDirBu, pathBase: pathBase}
WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
resizebase, base, stash
XMANAGER, 'gui', base
END
