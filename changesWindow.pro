

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
text += "Compiled Mar 22 2022" + clf + clf
text += "Recent changes" + clf
text += "- Mar 2022: new function to read cake files exported from dioptas (used txt export in dioptas)." + clf
text += "- Jan 2017: new features in 1D plot: navigate between orientations in 1D plots." + clf
text += "- Jan 2016: fixed export into esg format for Maud. There was an issue with 2theta conversions." + clf
text += "- Jan 2016: fixed plot of fit results for non integer azimuth values." + clf
text += "- Jan 2016: many improvements to the UI. You can now load IDL data file series with gaps." + clf
text += "- Jan 2016: updated fit2d macro for regular 2D data. It should be much faster than previous version at fit2d will calculate one cake only per diffraction image. It uses the new multichi export in fit2d" + clf
text += "- Jan 2016: new fit2d macro for data at ESRF ID06 beamline" + clf
text += "- Jan 2016: removed specific functions to change experiment type (not used anymore)" + clf
text += "- Jan 2016: fixed MAUD export, export current dataset only" + clf
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
