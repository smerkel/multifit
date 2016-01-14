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


; *******************************************************************
;
; Plotting routines...
;
; *******************************************************************


pro startPS, outputfile
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory
common devices, mydevice
mydevice = !D.NAME
set_plot, 'PS'
output = outputdirectory + outputfile
file = STRCOMPRESS(output, /REMOVE_ALL)
device, filename = file, /PORTRAIT, xsize = 19, $
        ysize = 25, xoffset = 1, yoffset = 2
end

pro endPS
common devices, mydevice
device, /CLOSE
set_plot, mydevice
end

pro plotdata, include, stretchit, stretch, thetait, thetaMin, thetaMax, legendeskipit, legendeskip, $
              FILENAME = outputfile, TITRE = titre, OFFSET = offset, $
              YPLOTMAX = yplotmax, YPLOTMin = yplotmin,CHARSIZE = charsize, CHARTHICK=charthick
common rawdata, nalpha, ntheta, alpha, twotheta, data
common datainfo, filenames, alphastart, alphaend, intervalle, date
common plotproperties, plotXmin, plotXmax, plotYmin, plotYmax
if (not keyword_set(titre)) then titre=filenames
if (not keyword_set(offset)) then offset = 0.0
if (not keyword_set(yplotmax)) then yplotmax = -100000
if (not keyword_set(yplotmin)) then yplotmin = 0
if (not keyword_set(charsize)) then charsize = 1
if (not keyword_set(charthick)) then charthick = 1
if(keyword_set(outputfile)) then begin
    mydevice = !D.NAME
    set_plot, 'PS'
    device, filename = outputfile, /PORTRAIT, xsize = 19, ysize = 19, xoffset = 1, yoffset = 2
endif
if (stretchit eq 0) then begin
    sepfactor = float(1)
endif else begin
    sepfactor = 1.0*float(stretch[0])
endelse
if (legendeskipit eq 0) then begin
    legendesep = 1
endif else begin
    legendesep = fix(legendeskip)
endelse
if (thetait eq 0) then begin
    thetamin = min(twotheta)
    thetamax = max(twotheta)
endif else begin
    thetamin = float(thetamin)
    thetamax = float(thetamax)
endelse

xStart = getIndex(thetamin,  twotheta, ntheta)
xEnd = getIndex(thetamax,  twotheta, ntheta)
index = 0
nplots = 0
; If X data is in reverse order, switch the beginning and the end indices
if (xStart gt xEnd) then begin
    tt = xEnd
    xEnd = xStart
    xStart = tt
endif
toplot = fltarr(nalpha, xEnd-xStart+1)

                                ; retrieving data and storing in new array
                                ; remove minumum value
while (index lt nalpha) do begin
    toplot(nplots,*) = data(index,xStart:xEnd)-min(data(index,xStart:xEnd))
    index = index + 1
    nplots = nplots + 1
endwhile

                                ; use INCLUDE to select datasets to actually plot
                                ; define legende for each plot
                                ; add an offset in Y so all plots do not overlapp
dataplot = fltarr(nplots, xEnd-xStart+1)
ntoplot = 0
leg = intarr(nplots)
for i=0, (nplots-1) do begin
    test = where(include eq i)
    if (test(0) ge 0) then begin
        if (ntoplot gt 0) then offset = offset + 1.0*max(dataplot(ntoplot-1,*)-offset)/sepfactor
        dataplot(ntoplot,*) = toplot(i,*) + offset
        leg(ntoplot) = alpha(i)
        ntoplot = ntoplot + 1
    endif
endfor

                                ; basic plot characteritics (scale...)
max = max(dataplot(*,*))
plotYmax = max
if (yplotmax gt 0) then plotYmax = yplotmax
plotXmin = thetamin
plotXmax = thetamax+0.1*(1.+.5*charsize)*(thetamax-thetamin)
texte = ' ' + strcompress(''+leg(0),/REMOVE_ALL)

                                ; Plot datasets
plot, twotheta(xStart:xEnd), dataplot(0,*), background=255, color=0, yrange = [yplotmin,plotYmax], $
    xrange = [plotXmin,plotXmax], xtitle = '2 theta', ytitle='intensity', title = titre, ystyle=1, $
    xstyle=1, charsize=charsize, ytickname=[" ", " ", " ", " ", " ", " ", " ", " ", " "], $
    charthick=charthick
plotYmin = 0
xyouts, thetamax, dataplot(0,xEnd-xStart), texte, color=0, charsize=charsize, charthick=charthick
index = 0
legendeindex = 0
for i=1, (ntoplot-1) do begin
    index = index + 1
    texte = ' ' + strcompress(''+leg(i),/REMOVE_ALL)
    oplot, twotheta(xStart:xEnd), dataplot(i,*) , color=0
    legendeindex = legendeindex + 1
    if (legendeindex eq legendesep) then begin
        xyouts, thetamax, dataplot(i,xEnd-xStart), ''+texte, color=0, charsize=charsize, charthick=charthick
        legendeindex = 0
    endif
endfor
plotXmax = thetamax
if(keyword_set(outputfile)) then begin
    device, /CLOSE
    set_plot, mydevice
endif
end
