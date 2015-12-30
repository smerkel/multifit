

; ****************************************** changes window **************
; opens a window with a list of recent changes
; easier to track changes in the software for the use
; introduced in July 2015


PRO changesWindow_event, ev
WIDGET_CONTROL, ev.TOP, /DESTROY
END

pro changesWindow, base
common fonts, titlefont, boldfont, mainfont, avFontHeight
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=base, Title='Recent changes')
infobase =  WIDGET_BASE(basedialog,/COLUMN)
la = WIDGET_TEXT(infobase, XSIZE=80, YSIZE=30, /ALIGN_LEFT, /EDITABLE, /WRAP)
ccr = STRING(13B) ; Carriage Return
clf = STRING([10B]) ; line feed
text = "Multifit" + clf
text += "Compiled Dec 28 2015" + clf + clf
text += "Recent changes" + clf
text += "- Dec 2015: removed requirement for azimuth to be integer in automated fitting" + clf
text += "- Dec 2015: improvements to the mapplot interface (set range in azimuth, move from one image to the next)" + clf
text += "- Dec 2015: new functions to import data created by fit2d multi-chi export functions" + clf
text += "- July 2, 2015: added reference to multifit publication in the about window" + clf
text += "- July 2, 2015: open the about window at startup" + clf
text += "- July 2, 2015: started to record changes in this interface" + clf
WIDGET_CONTROL, la, SET_VALUE=text, /APPEND
WIDGET_CONTROL, la, SET_TEXT_TOP_LINE=0
buttons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK', xsize=80)
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'changesWindow', basedialog
end
