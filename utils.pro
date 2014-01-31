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

; ****************************************** exist ******************
; check if a variable exist
;

function exist, var
on_error,2                      ;Return to caller if an error occurs
siz=size(var)
if siz[1] eq 0 then exfl=0 else exfl=1
return,exfl
end

; ****************************************** add extension **********
; add an extension to a file, replace the existing one if needed
;

function addextension, file, extension
FDECOMP,  file, disk, dir, name, qual, version
len = strlen(name)
last = STRMID(name, len-1)
if (last eq '.') then name2 = STRMID(name,0,len-1) else name2 = name
newfile = disk + dir + name2 + '.' + extension
return, newfile
end

; ****************************************** save file dialog ******
; dialog to save file, force an extension, force a directory, 
; and make sure we're not overwriting
; for instance, to save a file ***.dat in /usr/data/, call
;	pickfile_dir_ext, parent,'/usr/data/', 'dat'
; parent is the parent window
; whatevere the user choses, it'll return a file in '/usr/data/', with
; a 'dat' extension, and make sure it is OK to overwrite
function pickfile_dir_ext, dir, extension, parent=parent, title=title
modal = 1
IF N_Elements(title) EQ 0 THEN title = "Select output file"
IF N_Elements(parent) EQ 0 THEN modal = 0
fileloopout = 0
saveit = 1
filter = '*.'+extension
def_ext = "."+extension
while (fileloopout ne 1) do begin
	if (modal eq 1) then $
		result=dialog_pickfile(title=title, path=dir, DIALOG_PARENT=parent, DEFAULT_EXTENSION=def_ext, FILTER=[filter], /write) $
		else result=dialog_pickfile(title=title, path=dir, DEFAULT_EXTENSION=def_ext, FILTER=[filter], /write)
	if (result eq '') then return, '' ; it was a cancel
	result = addextension(result,extension)
	FDECOMP, result, disk, olddir, name, qual, version
	filename = dir + name + "." + qual
	if (FILE_TEST(filename) eq 1) then begin
		tmp = DIALOG_MESSAGE(filename + " exists. Overwrite?", /QUESTION)
		if (tmp eq 'Yes') then return, filename
	endif else return, filename
endwhile
return, ''
end

; ****************************************** readascii **************
; readascii: 
; reads a line out of an ascii file, but skips lines that start with a
; given comment character

function readascii, lun, com=com
IF N_Elements(com) EQ 0 THEN com = ''
test = 1
while (test) do begin
	if (EOF(lun)) then return, 'EOF'
	row = STRARR(1)
	READF, lun, row
	if (strmid(row,0,1) ne com) then return, row
endwhile
end


; ****************************************** array_pop **************
; pops an element out of an array

function array_pop, array, index
   first = 0
   last = N_Elements(array)-1
   CASE index OF
      first: array = array[1:*]
      last: array = array[first:last-1]
      ELSE: array = [ array[first:index-1], array[index+1:last] ]
   ENDCASE
return, array
end


; ****************************************** TEXTBOX **************

; Got this function off the web: http://www.dfanning.com/widget_tips/popup.html


PRO TextBox_Event, event
   ; This event handler responds to all events. Widget
   ; is always destoyed. The text is recorded if ACCEPT
   ; button is selected or user hits CR in text widget.
Widget_Control, event.top, Get_UValue=info
CASE event.ID OF
   info.cancelID: Widget_Control, event.top, /Destroy
   ELSE: BEGIN
         ; Get the text and store it in the pointer location.
      Widget_Control, info.textID, Get_Value=theText
      (*info.ptr).text = theText[0]
      (*info.ptr).cancel = 0 ; The user hit ACCEPT.
      Widget_Control, event.top, /Destroy
      ENDCASE
ENDCASE
END

FUNCTION TextBox, Title=title, Label=label, Cancel=cancel, $
   Group_Leader=groupleader, XSize=xsize, Value=value
   ; Return to caller if there is an error. Set the cancel
   ; flag and destroy the group leader if it was created.
Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Dialog_Message(!Error_State.Msg)
   IF destoy_groupleader THEN Widget_Control, groupleader, /Destroy
   cancel = 1
   RETURN, ""
ENDIF
   ; Check parameters and keywords.
IF N_Elements(title) EQ 0 THEN title = 'Provide Input:'
IF N_Elements(label) EQ 0 THEN label = ""
IF N_Elements(value) EQ 0 THEN value = ""
IF N_Elements(xsize) EQ 0 THEN xsize = 200
IF N_Elements(groupleader) EQ 0 THEN BEGIN
   groupleader = Widget_Base(Map=0)
   Widget_Control, groupleader, /Realize
   destroy_groupleader = 1
ENDIF ELSE destroy_groupleader = 0
   ; Create modal base widget.
tlb = Widget_Base(Title=title, Column=1, /Modal, $
   /Base_Align_Center, Group_Leader=groupleader)
labelbase = Widget_Base(tlb, Row=1)
IF label NE "" THEN label = Widget_Label(labelbase, Value=label)
textID = Widget_Text(labelbase, /Editable, Scr_XSize=xsize, Value=value)
buttonBase = Widget_Base(tlb, Row=1)
acceptID = Widget_Button(buttonBase, Value='Accept')
cancelID = Widget_Button(buttonBase, Value='Cancel')
   ; Center the widgets on display.
Device, Get_Screen_Size=screenSize
IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2
geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2
Widget_Control, tlb, XOffset = xCenter-xHalfSize, YOffset = yCenter-yHalfSize
   ; Realize the widget hierarchy.
Widget_Control, tlb, /Realize
ptr = Ptr_New({text:"", cancel:1})
info = {ptr:ptr, textID:textID, cancelID:cancelID}
Widget_Control, tlb, Set_UValue=info, /No_Copy 
XManager, 'textbox', tlb
theText = (*ptr).text
cancel = (*ptr).cancel
Ptr_Free, ptr
IF destroy_groupleader THEN Widget_Control, groupleader, /Destroy
RETURN, theText
END

; ****************************************** again dialog **************

PRO againDialog_Event, event
   ; This event handler responds to all events. Widget
   ; is always destoyed. The text is recorded if ACCEPT
   ; button is selected or user hits CR in text widget.
Widget_Control, event.top, Get_UValue=info
CASE event.ID OF
   info.noID: BEGIN    
       (*info.ptr).answer = 0
       Widget_Control, event.top, /Destroy
   ENDCASE
   info.yesID: BEGIN    
       (*info.ptr).answer = 1
       Widget_Control, event.top, /Destroy
   ENDCASE
   info.stopID: BEGIN    
       (*info.ptr).answer = -1
       Widget_Control, event.top, /Destroy
   ENDCASE
ENDCASE
END

FUNCTION againDialog, Title=title, Label=label, Group_Leader=groupleader
Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Dialog_Message(!Error_State.Msg)
   IF destoy_groupleader THEN Widget_Control, groupleader, /Destroy
   cancel = 1
   RETURN, ""
ENDIF
   ; Check parameters and keywords.
IF N_Elements(title) EQ 0 THEN title = 'Question'
IF N_Elements(label) EQ 0 THEN label = ""
IF N_Elements(groupleader) EQ 0 THEN BEGIN
   groupleader = Widget_Base(Map=0)
   Widget_Control, groupleader, /Realize
   destroy_groupleader = 1
ENDIF ELSE destroy_groupleader = 0
   ; Create modal base widget.
tlb = Widget_Base(Title=title, Column=1, /Modal, /Base_Align_Center, Group_Leader=groupleader)
labelbase = Widget_Base(tlb, Row=1)
IF label NE "" THEN label = Widget_Label(labelbase, Value=label)
buttonBase = Widget_Base(tlb, Row=1)
yesID = Widget_Button(buttonBase, Value='Yes')
noID = Widget_Button(buttonBase, Value='No')
stopID = Widget_Button(buttonBase, Value='Stop all fits')
   ; Center the widgets on display.
Device, Get_Screen_Size=screenSize
IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
Widget_Control, tlb, /Realize
ptr = Ptr_New({answer:0})
info = {ptr:ptr, yesID: yesID, noID: noID, stopID:stopID}
Widget_Control, tlb, Set_UValue=info, /No_Copy 
XManager, 'againDialog', tlb
answer = (*ptr).answer
IF destroy_groupleader THEN Widget_Control, groupleader, /Destroy
RETURN, answer
END
