; ***************************************************************************
; Routines to create a 1D interactive plot (y vs. index)
;
; AUTHOR:
;   S. Merkel, CNRS - Universite Lille 1, http://merkel.ZoneO.net/
;
; CALLING SEQUENCE:
;   calplotinteractive1D, base, xdata, ydata, title = title, xlabel = xlabel, ylabel = ylabel, legend = legend
;
; INPUTS:
;   base: parent frame
;   xdata: 1-D array with x-data
;   ydata: array of numbers, data to plot
;   title: optional parameter to set a plot title
;   xlabel: optional parameter to set a label for the x-axis
;   ylabel: optional parameter to set a label for the y-axis
;   legend: optional parameter to set a legend label for the datasets
;
; If you want to plot multiple datasets, set ydata with an array of numbers as ydata(i,j)
; where i is the set number
; Legends should be an array of string with legend[i] = legend for dataset i
; HISTORY
;   11/2006: Creation of the framework
; ***************************************************************************


; ***************************************************************************
; export and print
; ***************************************************************************

; export to GIF function
pro plotinteractive1D_exportgif, event
common default, defaultdir
  widget_control, event.top, get_uvalue=pstate
  ; fix the extension
  filters = [['*.gif'], ['GIF']]
  ; pick a filename
  filename = DIALOG_PICKFILE(dialog_parent = (*pstate).tlb, filter=filters, /write , TITLE='Save graphics as...', path=defaultdir, get_path = newdefaultdir);
  ; if OK was pressed
  if (filename ne '') then begin
    ; export the content of the active window to gif
    write_gif, filename, TVRD()
    ; set the nez default path
    defaultdir = newdefaultdir
  endif
end

; export to JPEG unction
pro plotinteractive1D_exportjpg, event
  common default, defaultdir
  widget_control, event.top, get_uvalue=pstate
  ; fix the extension
  filters = [['*.jpg;*.jpeg'], ['JPEG']]
  ; pick a filename
  filename = DIALOG_PICKFILE(dialog_parent = (*pstate).tlb, filter=filters, /write , TITLE='Save graphics as...', path=defaultdir, get_path = newdefaultdir);
  ; if OK was pressed
  if (filename ne '') then begin
    ; export the content of the active window to jpeg
    write_jpeg, filename, TVRD()
    ; set the new default path
    defaultdir = newdefaultdir
  endif
end

; export to postscript
pro plotinteractive1D_exportps, event
  common default, defaultdir
  widget_control, event.top, get_uvalue=pstate
  ; get the data in the window
  widget_control, event.top, get_uvalue=pstate
  ; fix the extension
  filters = [['*.ps'], ['PS']]
  ; pick a filename
  filename = DIALOG_PICKFILE(dialog_parent = (*pstate).tlb, filter=filters, /write , TITLE='Save graphics as...', path=defaultdir, get_path = newdefaultdir);
  ; if OK was pressed
  if (filename ne '') then begin
    ; save current devide, set the device to postscript
    mydevice = !D.NAME
    set_plot, 'PS'
    device, filename = filename, /PORTRAIT, xsize = 15, ysize = 10, xoffset = 2.5, yoffset = 10, /color, bits_per_pixel=24
    ; replot the data in the postscript device
    plotinteractive1D_doplot, pstate, postscript=1
    ; close postscript devide, return to the old one
    device, /CLOSE
    set_plot, mydevice
    ; set the new default path
    defaultdir = newdefaultdir
  endif
end

; export to in chiplot unction
pro plotinteractive1D_exportchi, event
  common default, defaultdir
  widget_control, event.top, get_uvalue=pstate
  ; state = {xdata: xdata, ydata:ydata, ncolumns: ncolumns, tlb: tlb, w_id:w_id, draw:draw, status:status, xlabel:xlabel, ylabel:ylabel, title: title, xmin:xmin, xmax:xmax, ymin:ymin, ymax:ymax, sc_xmin:0.0, sc_xmax:0.0, sc_ymin:0.0, sc_ymax:0.0, scaling:0, plotlegend: plotlegend, legend: legend}
  if ((*pstate).ncolumns gt 1) then Result = DIALOG_MESSAGE( "You have several datasets. Only the first will be saved.", DIALOG_PARENT=(*pstate).tlb, /INFORMATION, TITLE="Multifit warning")
  ; fix the extension
  filters = [['*.chi'], ['CHI']]
  ; pick a filename
  filename = DIALOG_PICKFILE(dialog_parent = (*pstate).tlb, filter=filters, /write , TITLE='Save dataset as...', path=defaultdir, get_path = newdefaultdir);
  ; if OK was pressed
  if (filename ne '') then begin
    ; export the content of the active window to chiplot
    openw, lun, filename, /get_lun
    printf, lun, "Chi file created by Multifit"
    printf, lun, (*pstate).xlabel
    printf, lun, (*pstate).ylabel
    printf, lun, n_elements((*pstate).xdata)
    for i=0,n_elements((*pstate).xdata)-1 do printf, lun, (*pstate).xdata[i], (*pstate).ydata[0,i]
    free_lun, lun
    ; set the new default path
    defaultdir = newdefaultdir
  endif
end


; ***************************************************************************
; handling of mouse events
; ***************************************************************************

; handle mouse events in the plot window
pro plotinteractive1D_draw, event
  widget_control, event.top, get_uvalue=pstate
  if (event.type eq 0) then begin ; mouse is pressed
    if (event.clicks eq 2) then begin
      ; double-click: we autoscale the data
      (*pstate).scaling = 0
      (*pstate).xmin = min((*pstate).xdata)
      (*pstate).xmax = max((*pstate).xdata)
      (*pstate).ymin = min((*pstate).ydata)
      (*pstate).ymax = max((*pstate).ydata)
      plotinteractive1D_doplot, pstate
    endif else begin
      ; single click, scaling according to mouse, setting starting value and activating rectagle draw
      datac = convert_coord(event.x, event.y, /device, /to_data, /double)
      (*pstate).sc_xmin = datac[0]
      (*pstate).sc_ymin = datac[1]
      (*pstate).scaling = 1
    endelse
  endif else if (event.type eq 1) then begin ; mouse is released
    ; if we were scaling, end of scaling according to mouse
    if ((*pstate).scaling eq 1) then begin
      datac = convert_coord(event.x, event.y, /device, /to_data, /double)
      (*pstate).xmin = (*pstate).sc_xmin
      (*pstate).ymin = (*pstate).sc_ymin
      (*pstate).xmax = datac[0]
      (*pstate).ymax = datac[1]
      (*pstate).scaling = 0
      plotinteractive1D_doplot, pstate
    endif
  endif else if (event.type eq 2) then begin ; mouse is moved
    if ((*pstate).scaling eq 1) then begin
      ; if scaling according to mouse, drawing the rectangle
      datac = convert_coord(event.x, event.y, /device, /to_data, /double)
      (*pstate).sc_xmax = datac[0]
      (*pstate).sc_ymax = datac[1]
      plotinteractive1D_doplot, pstate
    endif else begin
      ; otherwise convert the data to a string and set it in the status variable of the widget
      datac = convert_coord(event.x, event.y, /device, /to_data, /double)
      statusstr = strtrim(datac[0],2) + ',' +  strtrim(datac[1],2)
      widget_control, (*pstate).status, set_value=statusstr
    endelse
  endif
end

; ***************************************************************************
; replot the data with or without scaling rectangle
; ***************************************************************************

; plot the data, send 'pstate', postscript = 1 if you are saving to a postscript file
pro plotinteractive1D_doplot, pstate, postscript=postscript
  IF N_Elements(postscript) EQ 0 THEN postscript = 0
  ; find and fix plotting range
  xmin = min([(*pstate).xmin,(*pstate).xmax])
  xmax = max([(*pstate).xmin,(*pstate).xmax])
  ymin = min([(*pstate).ymin,(*pstate).ymax])
  ymax = max([(*pstate).ymin,(*pstate).ymax])
  if (postscript eq 0) then begin ; plotting to screen
    ; ensure that data are being plotted in the draw window
    wset, (*pstate).w_id
    ; plot data
    plot,  (*pstate).xdata, (*pstate).ydata[0,*], xrange = [xmin,xmax], yrange=[ymin,ymax], background=255, color=0, xtitle=(*pstate).xlabel, ytitle=(*pstate).ylabel, title=(*pstate).title, ystyle = 1, xstyle=1
    if ((*pstate).ncolumns gt 1) then for i=1, (*pstate).ncolumns-1 do oplot, (*pstate).xdata, (*pstate).ydata[i,*], color=10*i
    ; if we are scaling, plot a red rectangle
    if ((*pstate).scaling eq 1) then begin
      oplot, [(*pstate).sc_xmin,(*pstate).sc_xmax,(*pstate).sc_xmax,(*pstate).sc_xmin,(*pstate).sc_xmin], [(*pstate).sc_ymin,(*pstate).sc_ymin,(*pstate).sc_ymax,(*pstate).sc_ymax,(*pstate).sc_ymin], color=10
    endif
    ; add the legend, if necessary
    if ((*pstate).plotlegend) then begin
      x1 = xmax-0.2*(xmax-xmin)
      x2 = xmax-0.15*(xmax-xmin)
      x3 = xmax-0.13*(xmax-xmin)
      y1 = ymax-0.05*(ymax-ymin)
      oplot, [x1,x2], [y1,y1], color=0
      xyouts, x3, y1, (*pstate).legend[0], color=0
      if ((*pstate).ncolumns gt 1) then begin
        for i=1, (*pstate).ncolumns-1 do begin
          y1 = y1 - 0.05*(ymax-ymin)
          oplot, [x1,x2], [y1,y1], color=10*i
          xyouts, x3, y1, (*pstate).legend[i],  color=10*i
        endfor
      endif
    endif
  endif else begin ; postscript, simply plot the data
    plot,  (*pstate).xdata,  (*pstate).ydata[0,*], xrange = [xmin,xmax], yrange=[ymin,ymax], background=255, color=0, xtitle=(*pstate).xlabel, ytitle=(*pstate).ylabel, title=(*pstate).title, xthick=2, ythick=2, thick=2, ystyle = 1, xstyle=1
    if ((*pstate).ncolumns gt 1) then for i=1, (*pstate).ncolumns-1 do oplot, (*pstate).xdata,  (*pstate).ydata[i,*], color=10*i
    ; add the legend, if necessary
    if ((*pstate).plotlegend) then begin
      x1 = xmax-0.2*(xmax-xmin)
      x2 = xmax-0.15*(xmax-xmin)
      x3 = xmax-0.13*(xmax-xmin)
      y1 = ymax-0.05*(ymax-ymin)
      oplot, [x1,x2], [y1,y1], color=0
      xyouts, x3, y1, (*pstate).legend[0], color=0
      if ((*pstate).ncolumns gt 1) then for i=1, (*pstate).ncolumns-1 do begin
        y1 = y1 - 0.05*(ymax-ymin)
        oplot, [x1,x2], [y1,y1], color=10*i
        xyouts, x3, y1, (*pstate).legend[i],  color=10*i
      endfor
    endif
  endelse
end

; ***************************************************************************
; resize event
; ***************************************************************************

; handle resize of window events (resize the plot)
pro plotinteractive1D_resize, event
  ; get the pstate pointer
  widget_control, event.top, get_uvalue=pstate
  ; getting size available for the plot
  statusg = widget_info((*pstate).status, /geometry)
  tlbg = widget_info(event.top, /geometry)
  newx = event.x - 2*tlbg.xpad
  newy = event.y - statusg.scr_ysize - 2*tlbg.ypad - 2*tlbg.space
  ; setting a new size
  widget_control, (*pstate).draw, xsize=newx, ysize=newy
  ; replot
  plotinteractive1D_doplot, pstate
end

; ***************************************************************************
; cleaning up
; ***************************************************************************

; if we come from an event (from the menu)
pro plotinteractive1D_cleanupmenu,event
  widget_control, event.top, get_uvalue=pstate
  IF Widget_Info((*pstate).tlb, /Valid_ID) THEN Widget_Control, (*pstate).tlb, /Destroy
  ptr_free, pstate
end

; if we come from a window (window has been shut, by the user or the application)
pro plotinteractive1D_cleanup, tlb
  widget_control, tlb, get_uvalue = pstate
  IF Widget_Info((*pstate).tlb, /Valid_ID) THEN Widget_Control, (*pstate).tlb, /Destroy
  ptr_free, pstate
end

; ***************************************************************************
; setting up
; ***************************************************************************

pro plotinteractive1D, base, xdata, sendydata, title = title, xlabel = xlabel, ylabel = ylabel, legend = legend
  IF N_Elements(title) EQ 0 THEN title = "Plot"
  IF N_Elements(xlabel) EQ 0 THEN xlabel = ""
  IF N_Elements(ylabel) EQ 0 THEN ylabel = ""
  IF N_Elements(legend) EQ 0 THEN BEGIN
    plotlegend = 0
    legend = ''
  endif else plotlegend = 1
  ; converting ydata to 2D array if it is not already
  if (N_ELEMENTS(SIZE(sendydata, /DIMENSION)) eq 1) then begin
    ydata = fltarr(1,N_ELEMENTS(sendydata))
    ydata[0,*] = sendydata
    ncolumns = 1
  endif else begin
    ydata = sendydata
    ncolumns = (SIZE(ydata, /DIMENSION))[0]
  endelse
  ; main window
  tlb = widget_base(title = title, /column, /tlb_size_events, MBAR=bar, GROUP_LEADER=base)
  ; menu bar
  file_menu = WIDGET_BUTTON(bar, VALUE='File', /MENU)
  file_bttn1 = WIDGET_BUTTON(file_menu, VALUE='Export plot to GIF', event_pro = 'plotinteractive1D_exportgif' )
  file_bttn2 = WIDGET_BUTTON(file_menu, VALUE='Export plot to JPEG', event_pro = 'plotinteractive1D_exportjpg' )
  file_bttn3 = WIDGET_BUTTON(file_menu, VALUE='Export plot to PS', event_pro = 'plotinteractive1D_exportps' )
  file_bttn4 = WIDGET_BUTTON(file_menu, VALUE='Export data in chi format', event_pro ='plotinteractive1D_exportchi', /SEPARATOR)
  file_bttn4 = WIDGET_BUTTON(file_menu, VALUE='Close window', event_pro ='plotinteractive1D_cleanupmenu', /SEPARATOR)
  ; other
  status = widget_label(tlb, value=' ', /dynamic_resize)
  draw = widget_draw(tlb, xsize=500, ysize=300, /motion_events, /button_events, event_pro='plotinteractive1D_draw')
  ; build the UI
  Widget_Control, tlb, /Realize
  ; get important information to communicate in the application
  Widget_Control, draw, get_value=w_id
  xmin = min(xdata)
  xmax = max(xdata)
  ymin = min(ydata,/NAN)
  ymax = max(ydata,/NAN)
  state = {xdata: xdata, ydata:ydata, ncolumns: ncolumns, tlb: tlb, w_id:w_id, draw:draw, status:status, xlabel:xlabel, ylabel:ylabel, title: title, xmin:xmin, xmax:xmax, ymin:ymin, ymax:ymax, sc_xmin:0.0, sc_xmax:0.0, sc_ymin:0.0, sc_ymax:0.0, scaling:0, plotlegend: plotlegend, legend: legend}
  ; create a pointer to the state structure and put that pointer
  ; into the user value of the top-level base
  pstate = ptr_new(state,/no_copy)
  widget_control, tlb, set_uvalue=pstate
  widget_control, draw, set_uvalue=pstate
  widget_control, file_bttn1, set_uvalue=pstate
  widget_control, file_bttn2, set_uvalue=pstate
  widget_control, file_bttn3, set_uvalue=pstate
  widget_control, file_bttn4, set_uvalue=pstate
  ; plot the data
  plotinteractive1D_doplot, pstate
  ; Register with XMANAGER so you can receive events.
  Widget_Control, tlb, Kill_Notify='plotinteractive1D_cleanup'
  xmanager, 'plotinteractive1D', tlb, event_handler='plotinteractive1D_resize', cleanup='plotinteractive1D_cleanup'
end
