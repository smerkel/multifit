
; ********************************* PLOTITFUNCTION ***************

PRO fitit_event, ev
	WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
	; resize event?
	if (ev.id eq ev.top) then begin
		; resize the plot
		widget_control, stash.draw, draw_xsize=ev.x, draw_ysize=ev.y
	endif
END

PRO fitit, stash
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fitit, def, base, draw
common fitoptions, basescale, smallDetection, nLoop, startSmall, endSmall 
; Get list of spectra to fit
tofit = WIDGET_INFO(stash.listID, /LIST_SELECT)
if (tofit(0) eq -1) then begin
    tmp = DIALOG_MESSAGE('Select at least one spectrum!', /ERROR)
    return
endif
; Parsing fit options
WIDGET_CONTROL, stash.npeaksVa, GET_VALUE=npeaks
ntofit = fix(npeaks[0])
WIDGET_CONTROL, stash.restrictregionVa, GET_VALUE=restricregion
basescale = float(restricregion[0])
WIDGET_CONTROL, stash.smallPeakVa, GET_VALUE=smallpeak
smallDetection = float(smallpeak[0])
WIDGET_CONTROL, stash.loopsVa, GET_VALUE=nnloops
nLoop = fix(nnloops[0])
WIDGET_CONTROL, stash.loopsSStartVa, GET_VALUE=nnloops
startSmall = fix(nnloops[0])
WIDGET_CONTROL, stash.loopsSEndVa, GET_VALUE=nnloops
endSmall  = fix(nnloops[0])
if (WIDGET_INFO(stash.gauss, /BUTTON_SET) eq 1) then begin
    profile = 0
endif else if (WIDGET_INFO(stash.voigt, /BUTTON_SET) eq 1) then begin
    profile = 1
endif else begin
    profile = 2
endelse
if (WIDGET_INFO(stash.halfwidthBut, /BUTTON_SET) eq 1) then halfwidth = 1 else halfwidth = 0
; Parsing interrupt options
if (WIDGET_INFO(stash.negIntensity, /BUTTON_SET) eq 1) then stopNegIntensity = 1 else stopNegIntensity = 0
if (WIDGET_INFO(stash.tooWide, /BUTTON_SET) eq 1) then stopTooWide = 1 else stopTooWide = 0
if (WIDGET_INFO(stash.intensityChge, /BUTTON_SET) eq 1) then stopIntensityChge = 1 else stopIntensityChge = 0
; create plot window if necessary 
if (def ne 1) then begin
    base = WIDGET_BASE(Title='Fits',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
    draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=700) 
    stashnew = {base:base, draw:draw, stash:stash} 
    WIDGET_CONTROL, base, SET_UVALUE=stashnew
    WIDGET_CONTROL, base, /REALIZE
    XMANAGER, 'fitit', base
    def = 1
endif
if (WIDGET_INFO(draw, /VALID_ID) ne 1) then begin
    base = WIDGET_BASE(Title='Fits',/TLB_SIZE_EVENTS, GROUP_LEADER=stash.base) 
    draw = WIDGET_DRAW(base, RETAIN=2, XSIZE=600, YSIZE=700) 
    stashnew = {base:base, draw:draw, stash:stash} 
    WIDGET_CONTROL, base, SET_UVALUE=stashnew
    WIDGET_CONTROL, base, /REALIZE
    XMANAGER, 'fitit', base
    def = 1
endif 
WIDGET_CONTROL, draw, GET_VALUE = index 
WSET, index
; Call fitting routine
fitnpeaks, ntofit, tofit, profile, halfwidth, stopNegIntensity, stopTooWide, stopIntensityChge, /AUTOMATIC, /AUTOCHECK, /SIDEBG
END


; ****************************************** MAIN FIT WINDOW GUI ***************

; Resize fit window, 
; in fact, only resize the bottom-left part of the option window, the rest is done automatically
; just need to force the size of the list at the end
PRO resizefitwindow, base, stash
widget_control, base, TLB_GET_SIZE=size
; resizing top in x
; widget_control, stash.titleLa, xsize=size[0]
; Resizing middle of the window in y, leave space for top and bottom
tmpsize = widget_info(stash.titleLa, /GEOMETRY)
tmpsize2 = widget_info(stash.butBase, /GEOMETRY)
ysize = size[1]-(tmpsize2.SCR_YSIZE + (2*tmpsize2.MARGIN))-(tmpsize.SCR_YSIZE + (2*tmpsize.MARGIN))
; print, "Y size for middlde container becomes", ysize
; widget_control, stash.listBase, ysize=ysize
; Resizing the right part of middle container in x
tmpsize = widget_info(stash.listBase, /GEOMETRY)
xsize = size[0]-(tmpsize.SCR_XSIZE + (2*tmpsize.MARGIN))
tmpsize = widget_info(stash.dataBase, /GEOMETRY)
xsize -= (tmpsize.SCR_XSIZE + (2*tmpsize.MARGIN))
; widget_control, stash.allOptions, xsize=xsize, ysize=ysize
tmpsize = widget_info(stash.optTricks, /GEOMETRY)
ysize2 = ysize-(tmpsize.SCR_YSIZE + (2*tmpsize.MARGIN))
tmpsize = widget_info(stash.optBase, /GEOMETRY)
ysize2 -= (tmpsize.SCR_YSIZE + (2*tmpsize.MARGIN))
widget_control, stash.interrupt, xsize=xsize, ysize=ysize2, UPDATE=0  
; resizing button bar in x
; widget_control, stash.butBase, xsize=size[0]
; Resizing list height
tmpsize = widget_info(stash.listBase, /GEOMETRY)
tmpsize2 = widget_info(stash.listID, /GEOMETRY)
ysize = ysize-2*tmpsize2.MARGIN-2*tmpsize.YPAD
widget_control, stash.listID, SCR_YSIZE=ysize, /UPDATE  
end

; Process events generated in the window
PRO fitWindow_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
CASE ev.id OF
	stash.base: resizefitwindow, stash.base, stash
	else: begin
		CASE uval OF
			'FIT': fitit, stash
			'EXIT': WIDGET_CONTROL, ev.TOP, /DESTROY
			else:
			ENDCASE
		ENDCASE
	endcase
END

; Create fit window
PRO fitWindow, parent 
common datainfo, filenames, alphastart, alphaend, intervalle, date
common rawdata, nalpha, ntheta, alpha, twotheta, data
common fitit, def, base, draw
common fonts, titlefont, boldfont, mainfont, avFontHeight

                                ; force creation of a new plot window
def = 0
                                ; base GUI
base = WIDGET_BASE(Title='Multipeak fit window',/COLUMN, GROUP_LEADER=parent, /TLB_SIZE_EVENTS)
titleLa = WIDGET_LABEL(base, VALUE='Multipeak fitting functions', font=titlefont)
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
listID = Widget_List(listBase, VALUE=alphaText, UVALUE='LISTDELTA', /MULTIPLE)
; *************** Fit options
allOptions = WIDGET_BASE(top,/COLUMN,XPAD=0,YPAD=0)

; *************** Options de base
optBase = WIDGET_BASE(allOptions,/COLUMN, FRAME=1)
; number of peaks
npeaksBase = WIDGET_BASE(optBase,/ROW)
npeaksCh = Widget_Base(npeaksBase, /NonExclusive)
npeaksLa = Widget_Label(npeaksBase, Value='Number of peaks')
npeaksVa = WIDGET_TEXT(npeaksBase, VALUE='1',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
                                ; peak profile
profileLa = WIDGET_LABEL(optBase, VALUE='Peak profile', /ALIGN_LEFT)
peakprofile = Widget_Base(optBase, Column=1, /Exclusive)
gauss = Widget_Button(peakprofile, Value='Gauss', UVALUE='PROFILE')
voigt = Widget_Button(peakprofile, Value='Pseudo-Voigt', UVALUE='PROFILE')
lorentz = Widget_Button(peakprofile, Value='Lorentz', UVALUE='PROFILE')
Widget_Control, voigt, Set_Button=1
; Save half-width
halfwidthBase = Widget_Base(optBase, /NonExclusive)
halfwidthBut = Widget_Button(halfwidthBase, Value='Save half-width', UVALUE='H-WIDTH')
Widget_Control, halfwidthBut, Set_Button=1

; *************** optimisation
; Ad-hoc parameters
optTricks = WIDGET_BASE(allOptions,/COLUMN, FRAME=1)
adhocLa = Widget_Label(optTricks, Value='Options that control the quality of the fit')
; scaling for base
restrictregionBase = WIDGET_BASE(optTricks,/ROW)
restrictregionLa = Widget_Label(restrictregionBase, Value='Base restriction factor')
restrictregionVa = WIDGET_TEXT(restrictregionBase, VALUE='5.0',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
; what is a small peak?
smallPeakBase = WIDGET_BASE(optTricks,/ROW)
smallPeakLa = Widget_Label(smallPeakBase, Value='Small peak detection factor')
smallPeakVa = WIDGET_TEXT(smallPeakBase, VALUE='10.0',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
; loops
loopsBase = WIDGET_BASE(optTricks,/ROW)
loopsLa = Widget_Label(loopsBase , Value='Number of loops')
loopsVa = WIDGET_TEXT(loopsBase , VALUE='10',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
loopsSmallBase = WIDGET_BASE(optTricks,/ROW)
loopsSmallLa = Widget_Label(loopsSmallBase, Value='Fit small peaks between loop')
loopsSStartVa = WIDGET_TEXT(loopsSmallBase, VALUE='3',/ALIGN_LEFT, XSIZE=10, /EDITABLE)
loopsSmallLa2 = Widget_Label(loopsSmallBase, Value='and')
loopsSEndVa = WIDGET_TEXT(loopsSmallBase, VALUE='7',/ALIGN_LEFT, XSIZE=10, /EDITABLE)

; *************** Stop
interrupt = WIDGET_BASE(allOptions,/COLUMN, FRAME=1)
adhocLa = Widget_Label(interrupt, Value='Interrupt automatic fitting when')
interruptBase = Widget_Base(interrupt, /NonExclusive)
negIntensity = Widget_Button(interruptBase, Value='Negative intensity peak', UVALUE='')
Widget_Control, negIntensity, Set_Button=1
tooWide = Widget_Button(interruptBase, Value='Large change in half-width', UVALUE='')
Widget_Control, tooWide, Set_Button=1
intensityChge = Widget_Button(interruptBase, Value='Large change in intensity', UVALUE='')
Widget_Control, intensityChge, Set_Button=1

; *************** ACTION BUTTONS
butBase = WIDGET_BASE(base,/ROW, /ALIGN_CENTER)
plotBut = WIDGET_BUTTON(butBase, VALUE='Fit', UVALUE='FIT',xsize=80)
closeBut = WIDGET_BUTTON(butBase, VALUE='Close', UVALUE='EXIT',xsize=80)
; Create an anonymous structure to hold widget IDs
stash = {base:base, titleLa:titleLa, top:top, butBase:butBase, dataBase: dataBase, listBase:listBase, allOptions:allOptions, optBase:optBase, optTricks: optTricks, interrupt:interrupt, $
         listID: listID, npeaksVa:npeaksVa, $
         gauss:gauss, voigt:voigt, lorentz:lorentz, halfwidthBut:halfwidthBut,$
         restrictregionVa: restrictregionVa, smallPeakVa:smallPeakVa, loopsVa:loopsVa, $
         loopsSStartVa:loopsSStartVa, loopsSEndVa:loopsSEndVa, $
         negIntensity:negIntensity, tooWide:tooWide, intensityChge:intensityChge} 

WIDGET_CONTROL, base, SET_UVALUE=stash
WIDGET_CONTROL, base, /REALIZE
; forcing a resize event
resizefitwindow, base, stash
XMANAGER, 'fitWindow', base
END
