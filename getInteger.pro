PRO getInteger_event, ev
WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
WIDGET_CONTROL, ev.ID, GET_UVALUE=uval
if (uval eq 'OK') then begin
    WIDGET_CONTROL, stash.intText, GET_VALUE=value
    value = fix(value)
    *stash.valuePtr = value
endif
WIDGET_CONTROL, ev.TOP, /DESTROY
END

Function getInteger, title, message, widgetBase, value=value, extramessages = extramessages
if (not keyword_set(value)) then begin
	value = ''
endif
basedialog = WIDGET_BASE(/COLUMN, /MODAL, GROUP_LEADER=widgetBase)
intBase =  WIDGET_BASE(basedialog,/COLUMN)
if (keyword_set(extramessages)) then begin
	for i=0,n_elements(extramessages)-1 do begin
		intLa = WIDGET_LABEL(intBase, VALUE=extramessages[i], /ALIGN_LEFT)
	endfor
endif
intText = cw_field(intBase, TITLE=message, /INTEGER, UVALUE=pReturnValue, VALUE=value)
; intText = WIDGET_TEXT(intBase, XSIZE=10, VALUE=STRING(value,/PRINT), /EDITABLE)
buttons = WIDGET_BASE(basedialog,/ROW, /GRID_LAYOUT, /ALIGN_CENTER)
ok = WIDGET_BUTTON(buttons, VALUE='Ok', UVALUE='OK')
cancel = WIDGET_BUTTON(buttons, VALUE='Cancel', UVALUE='CANCEL')
pReturnValue = ptr_new(/ALLOCATE_HEAP)
stash = {intText:intText, valuePtr:pReturnValue}
WIDGET_CONTROL, basedialog, SET_UVALUE=stash
WIDGET_CONTROL, basedialog, /REALIZE
XMANAGER, 'getInteger', basedialog
; After return
newvalue = *pReturnValue
ptr_free, pReturnValue
return, newvalue
END
