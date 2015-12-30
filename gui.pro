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


; ******************************************************************
; Saves and reads basic information about multifit in hidden file for future
; uses
;

pro readdefault
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common experiment, wavelength, detectordistance, experimenttype
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
defaultdir = GETENV('HOME')
; set default values
datadirectory = defaultdir
outputdirectory = defaultdir
defaultdirectory = defaultdir
jcpdsdirectory = defaultdir
id6directory = defaultdir
extension = '.chi'
wavelength=0.4000
detectordistance=200.
ID6_psize = 200.
ID6_center = 1500.
ID6_etamin = 0.
ID6_etamax = 180.
ID6_dark = ""
experimenttype = "General" 

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
          "WORK_DIRECTORY:": defaultdirectory = word[1]
          "JCPDS_DIRECTORY:": jcpdsdirectory = word[1]
          "DATA_DIRECTORY:": datadirectory = word[1]
          "OUTPUT_DIRECTORY:": outputdirectory = word[1]
          "ID6_DIRECTORY:": id6directory = word[1]
          "EXTENSTION:": extension = word[1]
          "WAVELENGTH:": wavelength = float(word[1])
          "DETECTORDISTANCE:": detectordistance = float(word[1])
          "ID6_PIXELSIZE:": ID6_psize = float(word[1])
          "ID6_CENTER:": ID6_center = float(word[1])
          "ID6_ETAMIN:": ID6_etamin = float(word[1])
          "ID6_ETAMAX:": ID6_etamax = float(word[1])
          "ID6_DARK:": ID6_dark = word[1]
          "EXPERIMENTTYPE:": experimenttype = word[1]
          else:
        endcase
      endif
    endwhile
    free_lun, lun
  endif
endif
end


pro savedefaults
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common experiment, wavelength, detectordistance, experimenttype
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark

if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)

str = "# Multifit default parameters" + newline
str += "WORK_DIRECTORY: " + defaultdirectory + newline
str += "JCPDS_DIRECTORY: " + jcpdsdirectory + newline
str += "DATA_DIRECTORY: " + datadirectory + newline
str += "OUTPUT_DIRECTORY: " + outputdirectory + newline
str += "ID6_DIRECTORY: " + id6directory + newline
str += "EXTENSTION: " + extension + newline
str += "WAVELENGTH: " + string(wavelength) + newline
str += "DETECTORDISTANCE: " + string(detectordistance) + newline
str += "ID6_PIXELSIZE: " + string(ID6_psize) + newline
str += "ID6_CENTER: " + string(ID6_center) + newline
str += "ID6_ETAMIN: " + string(ID6_etamin) + newline
str += "ID6_ETAMAX: " + string(ID6_etamax) + newline
str += "ID6_DARK: " + ID6_dark + newline
str += "EXPERIMENTTYPE: " +  experimenttype + newline

if (STRUPCASE(!VERSION.OS_FAMILY) eq 'UNIX') then begin
  default = GETENV('HOME') + "/" + ".multifit"
  ; print, "Trying to write " + str
  openw, lun, default, /get_lun
  printf, lun, str
  free_lun, lun
endif

end


; ********************************* LOAD_DEFAULTS *********************************

PRO load_defaults_startup
common fonts, titlefont, boldfont, mainfont, avFontHeight
; default color palette and font for plots
OS   = strupcase(!VERSION.OS)
OS   = strmid(OS,0,3)
if (OS ne 'WIN') then device, true_color=24
device, decomposed=0
loadct, 15, /SILENT
DEVICE, SET_FONT='Helvetica Bold', /TT_FONT, SET_CHARACTER_SIZE=[8,10]
; default for plots
!P.MULTI = 0
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
  mainfont = 'helvetica*14'
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
; Read other things from a default file
readdefault
end


; ****************************************** about window **************

PRO aboutWindow_event, ev
WIDGET_CONTROL, ev.TOP, /DESTROY
END

pro aboutWindow, base
common fonts, titlefont, boldfont, mainfont, avFontHeight
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base, Title='About Multifit')
infobase =  WIDGET_BASE(basedialog,/COLUMN)
la = WIDGET_TEXT(infobase, XSIZE=80, YSIZE=20, /ALIGN_LEFT, /EDITABLE, /WRAP)
ccr = STRING(13B) ; Carriage Return
clf = STRING([10B]) ; line feed
text = "Multifit" + ccr + clf
text += "Homepage: http://merkel.zoneo.net/Multifit-Polydefix" + ccr + clf + ccr + clf
text += "Multifit/Polydefix is an open-source IDL software package for an efficient processing of diffraction data obtained in deformation apparatuses at synchrotron beamlines. It is a compound of three different packages that can be run independently. Multifit is dedicated to the fitting of two-dimensional (2-D) diffraction data. It will extract d-spacings, intensities, and half-widths for peaks of a given material, for multiple azimuthal slices and over multiple diffraction images." + ccr + clf + ccr + clf
text += "Copyright S. Merkel, N. Hilairet Universite Lille 1, France" + ccr + clf
text += "Multifit is open source software, licensed under the GPL Version  2." + ccr + clf
text += "" + ccr + clf
text += "If you use results of Multifit/Polydefix in scientific publications, please refer to the following paper"  + ccr + clf  + ccr + clf
text += "S. Merkel and N. Hilairet, Multifit/Polydefix: a Framework for the Analysis of Polycrystal Deformation using X-Rays, Journal of Applied Crystallography, 48 (2015) [doi: 10.1107/S1600576715010390]."
WIDGET_CONTROL, la, SET_VALUE=text, /APPEND
WIDGET_CONTROL, la, SET_TEXT_TOP_LINE=0
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
PRO saveparams, base, log
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common experiment, wavelength, detectordistance, experimenttype
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
common inputfiles, inputfiles, activeset
filename=dialog_pickfile(title='Save parameters into...', path=defaultdirectory, DIALOG_PARENT=base, DEFAULT_EXTENSION='.par', FILTER=['*.par'], /WRITE, /OVERWRITE_PROMPT)
if (filename ne '') then begin
	filename = addextension(filename, 'par')
	openw, lun, filename, /get_lun
	printf, lun, '# Parameter file for multfit'
	printf, lun, '# Directory with chi and idl files'
	printf, lun, 'directory|'+datadirectory
	printf, lun, '# Directory with image fits'
	printf, lun, 'outputdirectory|'+outputdirectory
	printf, lun, '# Directory with ID6 diffraction data'
	printf, lun, 'id6directory|'+id6directory
	printf, lun, '# Wavelength (Angstroms)'
	printf, lun, 'wavelength|'+ STRING(wavelength, /PRINT)
	printf, lun, '# Detector distance (mm)'
	printf, lun, 'detectordistance|' + STRING(detectordistance, /PRINT)
	printf, lun, '# Experiment type ("General" or "ESRFID06")'
	printf, lun, 'experimenttype|' + experimenttype
	printf, lun, '# ID6 beam center (pixels)'
	printf, lun, 'ID6_center|' + STRING(ID6_center, /PRINT)
	printf, lun, '# ID6 Pixel size (microns)'
	printf, lun, 'ID6_psize|' + STRING(ID6_psize, /PRINT)
	printf, lun, '# ID6 eta min (degrees)'
	printf, lun, 'ID6_etamin|' + STRING(ID6_etamin, /PRINT)
	printf, lun, '# ID6 eta max (degrees)'
	printf, lun, 'ID6_etamax|' + STRING(ID6_etamax, /PRINT)
	printf, lun, '# ID6 dark file'
	printf, lun, 'ID6_dark|' + ID6_dark
	if (size(inputfiles, /TYPE) ne 0) then begin
		input = STRJOIN( inputfiles, ';' )
		printf, lun, '# Input files'
		printf, lun, 'inputfiles|'+input
	endif
	free_lun, lun
    logit, log, "Parameters save in " + filename
	FDECOMP, filename, disk, dir, name, qual, version
	defaultdirectory = disk+dir
endif
END

PRO readparams, base, log, listSets, stash
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
common experiment, wavelength, detectordistance, experimenttype
filename=dialog_pickfile(title='Read parameters from...', path=defaultdirectory, DIALOG_PARENT=base, FILTER=['*.par','*.*'], /must_exist)
if (filename ne '') then begin
  ; print, "Want to open ", filename
	openr, lun, filename, /get_lun
	while ~ EOF(lun) do begin
		row = ""
		readf, lun, row
		words = strsplit(row, '|', /EXTRACT)
		case words[0] of
			'directory': datadirectory = words[1]
			'outputdirectory': outputdirectory = words[1]
			'wavelength': wavelength = float(words[1])
			'detectordistance': detectordistance = float(words[1])
			'id6directory': id6directory = words[1]
			'inputfiles': inpufilesFromList, log, listSets, strsplit(words[1], ';', /EXTRACT)
			'experimenttype': experimenttype = words[1]
			'ID6_psize': ID6_psize = float(words[1])
			'ID6_center': ID6_center = float(words[1])
			'ID6_etamin': ID6_etamin = float(words[1])
			'ID6_etamax': ID6_etamax = float(words[1])
			'ID6_dark': ID6_dark = words[1]
			else:
		endcase
	endwhile 
	close, lun
  logit, log, "Parameters read from " + filename
	FDECOMP, filename, disk, dir, name, qual, version
	defaultdirectory = disk+dir
	chgExperimentType, stash
endif
end

; ****************************************** CHGINPUTDIR *********************************
PRO chgInputDir, base, log, widget
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
result=dialog_pickfile(/DIRECTORY,title='Select input directory', path=datadirectory, DIALOG_PARENT=base)
if (result ne '') then begin
    datadirectory = result
    inputDirLabel = 'Input directory: ' + datadirectory
    WIDGET_CONTROL, widget, SET_VALUE=datadirectory
    logit, log, inputDirLabel
endif
END


; ****************************************** CHGOUTPUTDIR *************************************
PRO chgOutputDir, base, log, widget
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
result=dialog_pickfile(/DIRECTORY,title='Select output directory', path=outputdirectory, DIALOG_PARENT=base)
if (result ne '') then begin
    outputdirectory=result
    outputDirLabel = 'Output directory: ' + outputdirectory
    WIDGET_CONTROL, widget, SET_VALUE=outputdirectory
    logit, log, outputDirLabel
endif
END

; ****************************************** CHGWAVELENGTH *************************************

PRO chgWavelength_event, ev
common experiment, wavelength, detectordistance, experimenttype
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
common experiment, wavelength, detectordistance, experimenttype
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

PRO chgDetectorDistance_event, ev
common experiment, wavelength, detectordistance, experimenttype
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
common experiment, wavelength, detectordistance, experimenttype
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

; ****************************************** ID6 Parameter changes *************************************

PRO chgID6PixelSize_event, ev
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.text, GET_VALUE=tt
    ID6_psize = float(tt)
    label = 'ID6 pixel size: ' + STRING(ID6_psize, /PRINT) + ' microns'
    WIDGET_CONTROL, stash.widget, SET_VALUE=STRTRIM(STRING(ID6_psize, /PRINT),2)
    logit, stash.log, label
  endif
  WIDGET_CONTROL, ev.TOP, /DESTROY
END

PRO chgID6PixelSize, base, log, widget
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
  base =  WIDGET_BASE(basedialog,/ROW)
  label = WIDGET_LABEL(base, VALUE='ID6 Pixel Size (in microns)', /ALIGN_LEFT)
  text = WIDGET_TEXT(base, XSIZE=10, VALUE=STRING(ID6_psize,/PRINT), /EDITABLE)
  buttons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
  ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
  cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
  stash = {widget:widget, text:text, log:log}
  WIDGET_CONTROL, basedialog, SET_UVALUE=stash
  WIDGET_CONTROL, basedialog, /REALIZE
  XMANAGER, 'chgID6PixelSize', basedialog
END

PRO chgID6Center_event, ev
  common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.text, GET_VALUE=tt
    ID6_center = float(tt)
    label = 'ID6 beam center: ' + STRING(ID6_center, /PRINT) + ' pixels'
    WIDGET_CONTROL, stash.widget, SET_VALUE=STRTRIM(STRING(ID6_center, /PRINT),2)
    logit, stash.log, label
  endif
  WIDGET_CONTROL, ev.TOP, /DESTROY
END

PRO chgID6Center, base, log, widget
  common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
  base =  WIDGET_BASE(basedialog,/ROW)
  label = WIDGET_LABEL(base, VALUE='ID6 beam center (in pixels)', /ALIGN_LEFT)
  text = WIDGET_TEXT(base, XSIZE=10, VALUE=STRING(ID6_center,/PRINT), /EDITABLE)
  buttons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
  ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
  cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
  stash = {widget:widget, text:text, log:log}
  WIDGET_CONTROL, basedialog, SET_UVALUE=stash
  WIDGET_CONTROL, basedialog, /REALIZE
  XMANAGER, 'chgID6Center', basedialog
END


PRO chgID6EtaMin_event, ev
  common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.text, GET_VALUE=tt
    ID6_etamin = float(tt)
    label = 'ID6 minimum eta: ' + STRING(ID6_etamin, /PRINT) + ' degrees'
    WIDGET_CONTROL, stash.widget, SET_VALUE=STRTRIM(STRING(ID6_etamin, /PRINT),2)
    logit, stash.log, label
  endif
  WIDGET_CONTROL, ev.TOP, /DESTROY
END

PRO chgID6EtaMin, base, log, widget
  common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
  base =  WIDGET_BASE(basedialog,/ROW)
  label = WIDGET_LABEL(base, VALUE='ID6 minimum eta (in degrees)', /ALIGN_LEFT)
  text = WIDGET_TEXT(base, XSIZE=10, VALUE=STRING(ID6_etamin,/PRINT), /EDITABLE)
  buttons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
  ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
  cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
  stash = {widget:widget, text:text, log:log}
  WIDGET_CONTROL, basedialog, SET_UVALUE=stash
  WIDGET_CONTROL, basedialog, /REALIZE
  XMANAGER, 'chgID6EtaMin', basedialog
END


PRO chgID6EtaMax_event, ev
  common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
  if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.text, GET_VALUE=tt
    ID6_etamax = float(tt)
    label = 'ID6 maximum eta: ' + STRING(ID6_etamax, /PRINT) + ' degrees'
    WIDGET_CONTROL, stash.widget, SET_VALUE=STRTRIM(STRING(ID6_etamax, /PRINT),2)
    logit, stash.log, label
  endif
  WIDGET_CONTROL, ev.TOP, /DESTROY
END

PRO chgID6EtaMax, base, log, widget
  common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
  base =  WIDGET_BASE(basedialog,/ROW)
  label = WIDGET_LABEL(base, VALUE='ID6 maximum eta (in degrees)', /ALIGN_LEFT)
  text = WIDGET_TEXT(base, XSIZE=10, VALUE=STRING(ID6_etamax,/PRINT), /EDITABLE)
  buttons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
  ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
  cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
  stash = {widget:widget, text:text, log:log}
  WIDGET_CONTROL, basedialog, SET_UVALUE=stash
  WIDGET_CONTROL, basedialog, /REALIZE
  XMANAGER, 'chgID6EtaMax', basedialog
END

PRO chgID6Dark, base, log, widget
  common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
  common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  filters = ['*.tif', '*.tiff', '*.*']
  result=dialog_pickfile(title='Select ID6 dark dataset', path=id6directory, DIALOG_PARENT=base, filter=filters, /must_exist)
  if (result ne '') then begin
    ID6_dark = result
    message = 'New ID6 dark file: ' + ID6_dark
    WIDGET_CONTROL, widget, SET_VALUE=ID6_dark
    logit, log, message
    FDECOMP, result, disk, dir, name, qual, version
    id6directory = disk+dir
  endif
END

; ****************************************** READ AND CONVERT CHI FILES **************

; Convert one series of chi files created by Fit2d multi-chi output
PRO convertonemultichi_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
log=stash.log
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.startiText, GET_VALUE=starti
    WIDGET_CONTROL, stash.endiText, GET_VALUE=endi
    WIDGET_CONTROL, stash.slicesText, GET_VALUE=slices
    WIDGET_CONTROL, stash.nameText, GET_VALUE=basename
    logit, log, "Read series of text files: " + basename + " with angles from " + starti + " to " + endi + " with " +  slices + " slices"
    startiFlt = FLOAT(starti)
    endiFlt = FLOAT(endi)
    slicesInt = FIX(FLOAT(slices))
    ; print, slicesInt, typename(slicesInt), n_elements(slicesInt)
    result = read_multichi(basename, startiFlt, endiFlt, slicesInt[0], log)
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


; Convert one series of chi files created by Fit2d multi-chi output
PRO convertonemultichi, base, log
common inputinfo, string
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
nameBase =  WIDGET_BASE(basedialog,COLUMN=2, /GRID_LAYOUT, FRAME=1)
nameLa = WIDGET_LABEL(nameBase, VALUE='Base for name of chi files', /ALIGN_LEFT)
startiLa = WIDGET_LABEL(nameBase, VALUE='First azimuth angle', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Last azimuth angle', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Number of slices', /ALIGN_LEFT)
nameText = WIDGET_TEXT(nameBase, XSIZE=10, /EDITABLE)
startiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
endiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
slicesText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
extButtons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(extButtons, VALUE='Ok', UVALUE='OK', xsize=80)
cancel = WIDGET_BUTTON(extButtons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
stash = {log:log, nameText:nameText, slicesText:slicesText, startiText:startiText, endiText:endiText}
WIDGET_CONTROL, basedialog, SET_UVALUE=stash
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'convertonemultichi', basedialog
END


; Convert series of chi files created by Fit2d multi-chi output for a whole load of images
PRO converfileseriesmultichi_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
log=stash.log
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.startiText, GET_VALUE=starti
    WIDGET_CONTROL, stash.endiText, GET_VALUE=endi
    WIDGET_CONTROL, stash.fimageText, GET_VALUE=firstimage
    WIDGET_CONTROL, stash.limageText, GET_VALUE=lastimage
    WIDGET_CONTROL, stash.slicesText, GET_VALUE=slices
    WIDGET_CONTROL, stash.nameText, GET_VALUE=basename
    logit, log, "Read series of images created with multi-chi ouput: " + basename + ", image " + firstimage + " to " + lastimage + " with angles from " + starti + " to " + endi + " with " +  slices + " slices"
    startiFlt = FLOAT(starti)
    endiFlt = FLOAT(endi)
    slicesInt = FIX(FLOAT(slices))
    fimage = FIX(FLOAT(firstimage[0]))
    limage = FIX(FLOAT(lastimage[0]))
    ; print, slicesInt, typename(slicesInt), n_elements(slicesInt)
    noclose = 0
    for  i = fimage,limage do begin
		file = basename + "_" + intformat(i,3)
		result = read_multichi(file, startiFlt, endiFlt, slicesInt[0], log)
		if (result eq 1) then begin
			logit, log, "Read chi files data for " + file
			outputname = file + ".idl"
			logit, log, "Saving data in MULTIFIT format into " + outputname
			res = savedata(outputname)
			if (res eq 1) then begin
				logit, log, "Save data in MULTIFIT format to " + outputname + ": success"
			endif else begin
				tmp = DIALOG_MESSAGE(res, /ERROR)
				logit, log, "Save data in MULTIFIT format: failed"
				noclose = 1
			endelse
		endif else begin
			tmp = DIALOG_MESSAGE(result, /ERROR)
			logit, log, "Failed reading chi files for " + file
			noclose = 1
		endelse
	endfor
	if (noclose eq 0) then WIDGET_CONTROL, ev.TOP, /DESTROY
endif else begin
    logit, log, "Read series of text files: canceled"
    WIDGET_CONTROL, ev.TOP, /DESTROY
endelse
END


; Convert series of chi files created by Fit2d multi-chi output for a whole load of images
PRO convertfileseriesmultichi, base, log
common inputinfo, string
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base)
nameBase =  WIDGET_BASE(basedialog,COLUMN=2, /GRID_LAYOUT, FRAME=1)
nameLa = WIDGET_LABEL(nameBase, VALUE='Base for name of file name', /ALIGN_LEFT)
nameLa = WIDGET_LABEL(nameBase, VALUE='First image number', /ALIGN_LEFT)
nameLa = WIDGET_LABEL(nameBase, VALUE='Last image number', /ALIGN_LEFT)
startiLa = WIDGET_LABEL(nameBase, VALUE='First azimuth angle', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Last azimuth angle', /ALIGN_LEFT)
intervalLa = WIDGET_LABEL(nameBase, VALUE='Number of slices', /ALIGN_LEFT)
nameText = WIDGET_TEXT(nameBase, XSIZE=10, /EDITABLE)
fimageText = WIDGET_TEXT(nameBase, XSIZE=10, /EDITABLE)
limageText = WIDGET_TEXT(nameBase, XSIZE=10, /EDITABLE)
startiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
endiText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
slicesText = WIDGET_TEXT(nameBase, XSIZE=5, /EDITABLE)
extButtons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(extButtons, VALUE='Ok', UVALUE='OK', xsize=80)
cancel = WIDGET_BUTTON(extButtons, VALUE='Cancel', UVALUE='CANCEL', xsize=80)
stash = {log:log, nameText:nameText, slicesText:slicesText, startiText:startiText, endiText:endiText, fimageText: fimageText, limageText:limageText}
WIDGET_CONTROL, basedialog, SET_UVALUE=stash
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'converfileseriesmultichi', basedialog
END



PRO convertonechi_event, ev
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
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common inputfiles, inputfiles, activeset
result=dialog_pickfile(title='Input data from...', path=datadirectory, DIALOG_PARENT=base, FILTER=['*.idl','*.*'], /must_exist)
if (result ne '') then begin
	FDECOMP, result, disk, dir, name, qual, version
	filename = datadirectory + name + "." + Qual
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
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
n = n_elements(list)
inputText = strarr(n)
for j=0,n-1 do begin
	filenameshort = list[j]
	filename = datadirectory + filenameshort
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
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
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
	filename = datadirectory + filenameshort
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

; Move to next set in list in gui and load the corresponding data
; send log and list widget ids
; created 12/2015
pro advanceActiveSet, log, list
active = (WIDGET_INFO(list, /LIST_SELECT))[0]
num = WIDGET_INFO(list, /LIST_NUMBER)
if (active lt (num-1)) then begin
	active = active + 1
	changeActiveSet, log, list, active
endif
end

; Move to previous set in in list in gui and load the corresponding data
; send log and list widget ids
; created 12/2015
pro movebackActiveSet, log, list
active = (WIDGET_INFO(list, /LIST_SELECT))[0]
if (active gt 0) then begin
	active = active - 1
	changeActiveSet, log, list, active
endif
end

; change active dataset (load the data and update list in main gui)
pro changeActiveSet, log, list, index
common inputfiles, inputfiles, activeset
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
file = datadirectory + inputfiles(index)
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
; uses a function from compareFitWindow!!
compareFitWindow, base, log, list, /nofit
END

; ****************************************** REMOVE SLICE **************


PRO removeSlice_event, ev
common inputfiles, inputfiles, activeset
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
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
	filename = datadirectory + file
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
common experiment, wavelength, detectordistance, experimenttype
distance = detectordistance/10
openw, lun, file, /get_lun
printdataesgall, lun, distance
free_lun, lun
return, 1
END

pro maudExport, base, listSets, log
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common inputfiles, inputfiles, activeset
if (FILE_TEST( outputdirectory, /DIRECTORY) ne 1) then begin
        tmp = DIALOG_MESSAGE("Error with directory: " + outputdirectory, /ERROR)
	logit, log, "Saving data in MAUD format in: " + outputdirectory + " not a valid directory"
	return
endif
nfiles = N_ELEMENTS(inputfiles)
for i=0, nfiles-1 do begin
	file = datadirectory + inputfiles(i)
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
common fonts, titlefont, boldfont, mainfont, avFontHeight
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
fit2d = WIDGET_BASE(/COLUMN, Title='Create macro for fit2d (single images)', /MODAL, GROUP_LEADER=base)
fit2dMacLa = WIDGET_LABEL(fit2d, VALUE='Create macro for fit2d (single images)', /ALIGN_center, font=titlefont)
fit2dMacDir = WIDGET_BASE(fit2d, /ROW)
fit2dMacDirLa = WIDGET_LABEL(fit2dMacDir, VALUE='Directory for CHI files export', /ALIGN_LEFT)
fit2dMacDirText = WIDGET_TEXT(fit2dMacDir, VALUE=datadirectory, XSIZE=60)
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
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
result=dialog_pickfile(/DIRECTORY,title='Directory with diffraction data...', path=defaultdirectory, DIALOG_PARENT=base)
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
common fonts, titlefont, boldfont, mainfont, avFontHeight
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
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
fit2dOutputDirText = WIDGET_TEXT(fit2dOutputDir, VALUE=datadirectory, XSIZE=60)
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
    savedefaults
    WIDGET_CONTROL, widget, /DESTROY
end

; This function is not used
; it was meant to be used for ESRD ID6 data but fit2d was adapted for ID6
; no need to multiply the work
; I also do not want to keep track of changes on id6 detectors
pro chgExperimentType, stash
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common experiment, wavelength, detectordistance, experimenttype
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
common fonts, titlefont, boldfont, mainfont, avFontHeight

ysizeparams=fix(1.6*avFontHeight)
baseoptions = stash.baseoptions
dropListExp = stash.dropListExp
log = stash.log
widget_control, dropListExp, get_value=exptypes
select = widget_info(dropListExp, /DROPLIST_SELECT)
if (select eq 0) then experimenttype = "General" else if (select eq 1) then experimenttype = "ESRFID06"
logit, log, "Switching experiment type to: " + exptypes[select]
newbaseoptions =  WIDGET_BASE(stash.defaultBase,ROW=9)

newbaseoptionsrow1 = WIDGET_BASE(newbaseoptions,COLUMN=2);
newbaseoptionsrow2 = WIDGET_BASE(newbaseoptions,COLUMN=2);
newbaseoptionsrow3 = WIDGET_BASE(newbaseoptions,COLUMN=2);
newbaseoptionsrow4 = WIDGET_BASE(newbaseoptions,COLUMN=2);
newbaseoptionsrow5 = WIDGET_BASE(newbaseoptions,COLUMN=2);
newbaseoptionsrow6 = WIDGET_BASE(newbaseoptions,COLUMN=2);
newbaseoptionsrow7 = WIDGET_BASE(newbaseoptions,COLUMN=2);
newbaseoptionsrow8 = WIDGET_BASE(newbaseoptions,COLUMN=2);
newbaseoptionsrow9 = WIDGET_BASE(newbaseoptions,COLUMN=2);

; Note: SCR_YSIZE=ysizeparams or ysize = ysizeparams: this screws up the displays on Windows systems...
label = WIDGET_LABEL(newbaseoptionsrow1, VALUE='Directory with CHI or MULTIFIT data files: ', /ALIGN_LEFT, XSIZE=250)
label = WIDGET_LABEL(newbaseoptionsrow2, VALUE='Directory to save fits: ', /ALIGN_LEFT, XSIZE=250)
label = WIDGET_LABEL(newbaseoptionsrow3, VALUE='Wavelength (angstroms)', /ALIGN_LEFT, XSIZE=250)
label = WIDGET_LABEL(newbaseoptionsrow4, VALUE='Sample-Detector distance (mm)', /ALIGN_LEFT, XSIZE=250)
if (experimenttype eq "ESRFID06") then begin
  label =  WIDGET_LABEL(newbaseoptionsrow5, VALUE='ID6 pixel size (microns)', /ALIGN_LEFT, XSIZE=250)
  label =  WIDGET_LABEL(newbaseoptionsrow6, VALUE='ID6 center (pixels)', /ALIGN_LEFT, XSIZE=250)
  label =  WIDGET_LABEL(newbaseoptionsrow7, VALUE='ID6 eta min (degrees)', /ALIGN_LEFT, XSIZE=250)
  label =  WIDGET_LABEL(newbaseoptionsrow8, VALUE='ID6 eta max (degrees)', /ALIGN_LEFT, XSIZE=250)
  label =  WIDGET_LABEL(newbaseoptionsrow9, VALUE='ID6 dark file', /ALIGN_LEFT, XSIZE=250)
endif
inputDirText = WIDGET_BUTTON(newbaseoptionsrow1, /ALIGN_LEFT, VALUE=datadirectory, XSIZE=400,  UVALUE='INPUTDIR')
outputDirText = WIDGET_BUTTON(newbaseoptionsrow2, /ALIGN_LEFT, VALUE=outputdirectory, XSIZE=400,  UVALUE='OUTPUTDIR')
waveText = WIDGET_BUTTON(newbaseoptionsrow3, /ALIGN_LEFT, VALUE=STRTRIM(STRING(wavelength,/PRINT),2), XSIZE=80,  UVALUE='WAVE')
ipDistanceText = WIDGET_BUTTON(newbaseoptionsrow4, /ALIGN_LEFT, VALUE=STRTRIM(STRING(detectordistance,/PRINT),2), XSIZE=80, UVALUE='DETECTORDISTANCE')
if (experimenttype eq "ESRFID06") then begin
  id6PixelSizeText = WIDGET_BUTTON(newbaseoptionsrow5, /ALIGN_LEFT, VALUE=STRTRIM(STRING(ID6_psize,/PRINT),2), XSIZE=80,  UVALUE='ID6PIXELSIZE')
  id6CenterText = WIDGET_BUTTON(newbaseoptionsrow6, /ALIGN_LEFT, VALUE=STRTRIM(STRING(ID6_center,/PRINT),2), XSIZE=80,  UVALUE='ID6CENTER')
  id6EtaMinText = WIDGET_BUTTON(newbaseoptionsrow7, /ALIGN_LEFT, VALUE=STRTRIM(STRING(ID6_etamin,/PRINT),2), XSIZE=80,  UVALUE='ID6ETAMIN')
  id6EtaMaxText = WIDGET_BUTTON(newbaseoptionsrow8, /ALIGN_LEFT, VALUE=STRTRIM(STRING(ID6_etamax,/PRINT),2), XSIZE=80,  UVALUE='ID6ETAMAX')
  id6DarkText = WIDGET_BUTTON(newbaseoptionsrow9, /ALIGN_LEFT, VALUE=ID6_dark, XSIZE=400, SCR_YSIZE=ysizeparams, UVALUE='ID6DARK')
endif else begin
  id6PixelSizeText = 0
  id6CenterText = 0
  id6EtaMinText = 0
  id6EtaMaxText = 0
  id6DarkText = 0
endelse
widget_control, baseoptions, /DESTROY
stash.baseoptions = newbaseoptions
stash.inputDirText = inputDirText
stash.outputDirText = outputDirText
stash.waveText = waveText
stash.ipDistanceText = ipDistanceText
stash.id6PixelSizeText = id6PixelSizeText
stash.id6CenterText = id6CenterText
stash.id6EtaMinText = id6EtaMinText
stash.id6EtaMaxText = id6EtaMaxText
stash.id6DarkText = id6DarkText
WIDGET_CONTROL, stash.base, SET_UVALUE=stash
WIDGET_CONTROL, stash.base, /REALIZE
resizebase, stash.base, stash
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
		'SAVEPARAM': saveparams, stash.base, stash.log
		'READPARAM': readparams, stash.base, stash.log, stash.listSets, stash
		'REMOVESLICE': removeSlice, stash.base
		'WAVE': chgWavelength, stash.base, stash.log, stash.waveText
		'DETECTORDISTANCE': chgDetectorDistance, stash.base, stash.log, stash.ipDistanceText
;		'ID6PIXELSIZE': chgID6PixelSize, stash.base, stash.log, stash.id6PixelSizeText
;		'ID6CENTER': chgID6Center, stash.base, stash.log, stash.id6CenterText
;		'ID6ETAMIN': chgID6EtaMin, stash.base, stash.log, stash.id6EtaMinText
;		'ID6ETAMAX': chgID6EtaMax, stash.base, stash.log, stash.id6EtaMaxText
;		'ID6DARK': chgID6Dark, stash.base, stash.log, stash.id6DarkText
		'ONEIDL': oneInputFile, stash.listSets, stash.log
		'MULTIPLEIDL': multipleInputFiles, stash.base, stash.listSets, stash.log
		'CONVERTONECHI': convertonechi, stash.base, stash.log
		'CONVERTMULTIPLECHI': convertmultiplechi, stash.base, stash.log
		'CONVERTFILESERIES': convertfileseries, stash.base, stash.log
		'CONVERTONEIMAGEMULTICHI': convertonemultichi, stash.base, stash.log
		'CONVERTFILESERIESMULTICHI': convertfileseriesmultichi, stash.base, stash.log
		'FIT2DMAC': fit2dmac, stash.base, stash.log
		'FIT2DMACLONG': fit2dmaclong, stash.base, stash.log
		'ID6CALIB': performID6Calibration, stash.base, stash.log, stash.ipDistanceText, stash.id6CenterText
		'ID6CONVERT': doID6SaveData, stash.base, stash.log
		'ID6UNCAKE':doID6Uncake, stash.base, stash.log
		'MAUDEXPORT': maudExport, stash.base, stash.listSets, stash.log
		'LISTSETS': changeActiveSet, stash.log, stash.listSets, active[0]
		'MAPPLOT': mapplotActiveSet, stash.base, stash.log, stash.listSets, active[0]
		'PLOTONESET': plotWindow, stash.base
		'FITONESET': fitWindow, stash.base
		'FITONESETAUTO': fitAutoWindow, stash.base, 1
		'FITMULTIPLESETAUTO': fitAutoWindow, stash.base, 2
		'FITONESETJCPDS': fitJCPDSWindow, stash.base
		'DATFILESTOFITFILE': createFitfromdatWindow, stash.base
		'COMPAREFIT': compareFitWindow, stash.base, stash.log, stash.listSets
		'COMPAREFIT1D': compareFit1DWindow, stash.base
		'PLOTFIT': plotResultsWindow, stash.base
		'MODELONEIMAGE': createModelWindow, stash.base
		'ABOUT': aboutWindow, stash.base
		'CHANGES': changesWindow, stash.base
		'NOTAVAILABLE': tmp = DIALOG_MESSAGE("This function is not implemented yet!", /ERROR)
		'FORBIDDEN': tmp = DIALOG_MESSAGE("You need to register", /ERROR)
;		'CHANGEEXPTYPE': chgExperimentType, stash
		'EXIT': exitit, ev.top
		else:
		ENDCASE
	endcase
endcase
END

PRO gui
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
common experiment, wavelength, detectordistance, experimenttype
; common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
common fonts, titlefont, boldfont, mainfont, avFontHeight
; default values
load_defaults_startup
; base GUI
base = WIDGET_BASE(Title='Multipeak superfit!',/COLUMN, MBAR=bar, /TLB_SIZE_EVENTS)
; File menu
file_menu = WIDGET_BUTTON(bar, VALUE='File', /MENU)
file_bttn3 = WIDGET_BUTTON(file_menu, VALUE='Save parameters', UVALUE='SAVEPARAM')
file_bttn4 = WIDGET_BUTTON(file_menu, VALUE='Read parameters', UVALUE='READPARAM')
file_bttn1 = WIDGET_BUTTON(file_menu, VALUE='Single input file', UVALUE='ONEIDL', /SEPARATOR)
file_bttn2 = WIDGET_BUTTON(file_menu, VALUE='Multiple input files', UVALUE='MULTIPLEIDL')
file_bttn5 = WIDGET_BUTTON(file_menu, VALUE='Exit', UVALUE='EXIT', /SEPARATOR)
; Fit2d interaction menu
data_menu = WIDGET_BUTTON(bar, VALUE='Fit2d', /MENU) 
fit2d_bttn1 = WIDGET_BUTTON(data_menu, VALUE='Create Multifit Fit2d macro for one file', UVALUE='FIT2DMAC')
fit2d_bttn2 = WIDGET_BUTTON(data_menu, VALUE='Create Multifit Fit2d macro for multiple files', UVALUE='FIT2DMACLONG')
fit2d_bttn3 = WIDGET_BUTTON(data_menu, VALUE='Convert CHI created by Multifit macro to IDL: 1 set', UVALUE='CONVERTONECHI')
fit2d_bttn4 = WIDGET_BUTTON(data_menu, VALUE='Convert CHI created by Multifit macro to IDL: multiple sets', UVALUE='CONVERTMULTIPLECHI')
fit2d_bttn4 = WIDGET_BUTTON(data_menu, VALUE='Convert CHI created by Multifit macro to IDL: file series', UVALUE='CONVERTFILESERIES')
fit2d_bttn5 = WIDGET_BUTTON(data_menu, VALUE='Convert CHI from Multi-CHI: one image', UVALUE='CONVERTONEIMAGEMULTICHI', /SEPARATOR)
fit2d_bttn6 = WIDGET_BUTTON(data_menu, VALUE='Convert CHI from Multi-CHI: file series', UVALUE='CONVERTFILESERIESMULTICHI')
; ID 06 menu, removed. We rely on fit2d
; id6_menu = WIDGET_BUTTON(bar, VALUE='ESRF ID06', /MENU)
; bttn1 = WIDGET_BUTTON(id6_menu, VALUE='Calibration from tif', UVALUE='ID6CALIB')
; bttn2 = WIDGET_BUTTON(id6_menu, VALUE='Convert tif to multifit', UVALUE='ID6CONVERT')
; bttn3 = WIDGET_BUTTON(id6_menu, VALUE='Uncake tif to tif', UVALUE='ID6UNCAKE')
; Other menu
dataset_menu = WIDGET_BUTTON(bar, VALUE='Current dataset', /MENU)
plotactive = WIDGET_BUTTON(dataset_menu, VALUE='Plot 2D', UVALUE='PLOTONESET')
mapplot = WIDGET_BUTTON(dataset_menu, VALUE='Mapplot', UVALUE='MAPPLOT')
maud_bttn1 = WIDGET_BUTTON(dataset_menu, VALUE='Export active datasets for Maud', UVALUE='MAUDEXPORT', /SEPARATOR)
data_bttn3 = WIDGET_BUTTON(dataset_menu, VALUE='Remove slice from active dataset', UVALUE='REMOVESLICE', /SEPARATOR)
; Image fit menu
fit_menu = WIDGET_BUTTON(bar, VALUE='Peak fitting', /MENU) 
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
about_bttn1 = WIDGET_BUTTON(about_menu, VALUE='Recent changes', UVALUE='CHANGES')
; top container
top = WIDGET_BASE(base,/ROW)
; display default parameters
ysizeparams=fix(1.6*avFontHeight)
defaultBase =  WIDGET_BASE(top,/ROW, FRAME=1, /ALIGN_CENTER)
thisbase =  WIDGET_BASE(defaultBase,/COLUMN, /ALIGN_CENTER)
; label = WIDGET_LABEL(thisbase, VALUE='Experiment type', /ALIGN_CENTER)
; listexp = ['General', 'ESRF ID06']
; dropListExp = WIDGET_DROPLIST(thisbase, VALUE=listexp, UVALUE='CHANGEEXPTYPE')
; if (experimenttype eq "General") then select=0 else if (experimenttype eq "ESRFID06") then select=1 
; widget_control, dropListExp,  SET_DROPLIST_SELECT = select
baseoptions =  WIDGET_BASE(defaultBase, row=9)
baseoptionsrow1 = WIDGET_BASE(baseoptions,COLUMN=2);
baseoptionsrow2 = WIDGET_BASE(baseoptions,COLUMN=2);
baseoptionsrow3 = WIDGET_BASE(baseoptions,COLUMN=2);
baseoptionsrow4 = WIDGET_BASE(baseoptions,COLUMN=2);
baseoptionsrow5 = WIDGET_BASE(baseoptions,COLUMN=2);
baseoptionsrow6 = WIDGET_BASE(baseoptions,COLUMN=2);
baseoptionsrow7 = WIDGET_BASE(baseoptions,COLUMN=2);
baseoptionsrow8 = WIDGET_BASE(baseoptions,COLUMN=2);
baseoptionsrow9 = WIDGET_BASE(baseoptions,COLUMN=2);
; Note: SCR_YSIZE=ysizeparams or ysize = ysizeparams: this screws up the displays on Windows systems...
label1 = WIDGET_LABEL(baseoptionsrow1 , VALUE='Directory with CHI or MULTIFIT data files: ', /ALIGN_LEFT, XSIZE=250 )
label2 = WIDGET_LABEL(baseoptionsrow2 , VALUE='Directory to save fits: ', /ALIGN_LEFT, XSIZE=250)
label3 = WIDGET_LABEL(baseoptionsrow3 , VALUE='Wavelength (angstroms)', /ALIGN_LEFT, XSIZE=250 )
label4 = WIDGET_LABEL(baseoptionsrow4 , VALUE='Sample-Detector distance (mm)', /ALIGN_LEFT, XSIZE=250 )
; if (experimenttype eq "ESRFID06") then begin
;  label =  WIDGET_LABEL(baseoptionsrow5 , VALUE='ID6 pixel size (microns)', /ALIGN_LEFT, XSIZE=250 )
;  label =  WIDGET_LABEL(baseoptionsrow6 , VALUE='ID6 center (pixels)', /ALIGN_LEFT, XSIZE=250 )
;  label =  WIDGET_LABEL(baseoptionsrow7 , VALUE='ID6 eta min (degrees)', /ALIGN_LEFT, XSIZE=250 )
;  label =  WIDGET_LABEL(baseoptionsrow8 , VALUE='ID6 eta max (degrees)', /ALIGN_LEFT, XSIZE=250 )
;  label =  WIDGET_LABEL(baseoptionsrow9 , VALUE='ID6 dark file', /ALIGN_LEFT, XSIZE=250 )
;endif
inputDirText = WIDGET_BUTTON(baseoptionsrow1, /ALIGN_LEFT, VALUE=datadirectory, XSIZE=400, UVALUE='INPUTDIR') 
outputDirText = WIDGET_BUTTON(baseoptionsrow2, /ALIGN_LEFT, VALUE=outputdirectory, XSIZE=400,  UVALUE='OUTPUTDIR') 
waveText = WIDGET_BUTTON(baseoptionsrow3, /ALIGN_LEFT, VALUE=STRTRIM(STRING(wavelength,/PRINT),2), XSIZE=80,  UVALUE='WAVE') 
ipDistanceText = WIDGET_BUTTON(baseoptionsrow4, /ALIGN_LEFT, VALUE=STRTRIM(STRING(detectordistance,/PRINT),2), XSIZE=80,  UVALUE='DETECTORDISTANCE') 
; if (experimenttype eq "ESRFID06") then begin
;  id6PixelSizeText = WIDGET_BUTTON(baseoptionsrow5, /ALIGN_LEFT, VALUE=STRTRIM(STRING(ID6_psize,/PRINT),2), XSIZE=80,  UVALUE='ID6PIXELSIZE')
;  id6CenterText = WIDGET_BUTTON(baseoptionsrow6, /ALIGN_LEFT, VALUE=STRTRIM(STRING(ID6_center,/PRINT),2), XSIZE=80, UVALUE='ID6CENTER')
;  id6EtaMinText = WIDGET_BUTTON(baseoptionsrow7, /ALIGN_LEFT, VALUE=STRTRIM(STRING(ID6_etamin,/PRINT),2), XSIZE=80,  UVALUE='ID6ETAMIN')
;  id6EtaMaxText = WIDGET_BUTTON(baseoptionsrow8, /ALIGN_LEFT, VALUE=STRTRIM(STRING(ID6_etamax,/PRINT),2), XSIZE=80, UVALUE='ID6ETAMAX') 
;  id6DarkText = WIDGET_BUTTON(baseoptionsrow9, /ALIGN_LEFT, VALUE=ID6_dark, XSIZE=400, UVALUE='ID6DARK') 
; endif else begin
;  id6PixelSizeText = 0
;  id6CenterText = 0 
;  id6EtaMinText = 0
;  id6EtaMaxText = 0 
;  id6DarkText = 0
; endelse
; List datasets and logwindow
bottom =  WIDGET_BASE(base,/ROW)
listBase =  WIDGET_BASE(bottom,/COLUMN, FRAME=1)
listLa = WIDGET_LABEL(listBase, VALUE='Active datasets', /ALIGN_CENTER)
listSets = Widget_List(listBase, VALUE='', UVALUE='LISTSETS',YSIZE=15, XSIZE=15)
mapplot = WIDGET_BUTTON(listBase, VALUE='Mapplot', UVALUE='MAPPLOT')
plotactive = WIDGET_BUTTON(listBase, VALUE='Plot', UVALUE='PLOTONESET')
log = WIDGET_TEXT(bottom, XSIZE=60, YSIZE=22, /ALIGN_CENTER, /EDITABLE, /WRAP, /SCROLL)

; stash = {base: base, log:log, baseoptions: baseoptions, $
;  dropListExp: dropListExp, inputDirText:inputDirText, outputDirText:outputDirText, waveText:waveText, ipDistanceText: ipDistanceText, $
;  id6PixelSizeText: id6PixelSizeText, id6CenterText:id6CenterText, id6EtaMinText:id6EtaMinText, id6EtaMaxText:id6EtaMaxText, id6DarkText:id6DarkText, $
;  listSets: listSets, defaultBase:defaultBase, bottom:bottom, listBase:listBase, listLa:listLa, mapplot:mapplot, plotactive:plotactive }

stash = {base: base, log:log, baseoptions: baseoptions, $
  inputDirText:inputDirText, outputDirText:outputDirText, waveText:waveText, ipDistanceText: ipDistanceText, $
  listSets: listSets, defaultBase:defaultBase, bottom:bottom, listBase:listBase, listLa:listLa, mapplot:mapplot, plotactive:plotactive }

WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
resizebase, base, stash
aboutWindow, base
XMANAGER, 'gui', base
END
