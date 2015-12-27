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


;
; Generate the eta scale, based on number of pixiels in image height and eta ranges
; image: 2D image data
; This function is not used. We rely on fit2d
; 
function ID6_make_eta_scale, image
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
ny = (size(image))[2]
y = fltarr(ny)
for i=0,ny-1 do y[i] = 1.*ID6_etamin+1.0*(ID6_etamax-ID6_etamin)*i/(ny-1.)
return, y
end

;
; Generate the two theta scale, based on number of pixels in image length and calibration parameters
; image: 2D image data
; This function is not used. We rely on fit2d
;
function ID6_make_twotheta_scale, image
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  common experiment, wavelength, detectordistance, experimenttype
nx = (size(image))[1]
x = fltarr(nx)
for i=0,(nx-1) do x[i] = 180*atan(ID6_psize/1000*(ID6_center-i)/detectordistance)/acos(-1.)
return, x
end

;
; Returns data from a tiff file, with the dark removed
; file: tiff file
; This function is not used. We rely on fit2d
;
function ID6_load, file
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
image = long(READ_TIFF(file))
darkdata = long(READ_TIFF(ID6_dark))
data = image-darkdata
return, data
end

;
; Routine to fit a clibration image
; Assumes LaB6 as a calibrant with a = 4.1549
; Looks for peaks 100, 110, 111, 200, 210, 211
; Assumes a pixel size of 0.2 mm
; First guess
;   - detector distance : 1040 mm
;   - center at pixel 1500
; This function is not used. We rely on fit2d
;
pro ID6_fitcalibrant, image, log
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
common experiment, wavelength, detectordistance, experimenttype
  
if (!D.NAME eq 'WIN') then newline = string([13B, 10B]) else newline = string(10B)
; Create a first x-scale based on first guesses

hwidth=40
nx = (size(image))[1]

; Collapse all y-bins into one set of x-y data
rebin = rebin(image,nx,1)
miny=min(rebin)
maxy=max(rebin)

logit, log, "ID 6 calibration" + newline + "Starting values" + newline + "- distance = " + string(detectordistance) + newline + "- center = " + string(ID6_center)

h = [1, 1, 1, 2, 2, 2]
k = [0, 1, 1, 0, 1, 1]
l = [0, 0, 1, 0, 0, 1]
a = 4.1549
d = fltarr(n_elements(h))
twotheta = fltarr(n_elements(h))
d = a/sqrt(h*h+k*k+l*l)
twotheta = 2.*asin(wavelength/(2.*d))
print, d
print, twotheta
  
; Loop to optimize detector distance and beam center based on calibrant peak positions
; We do it three times, it's plenty
pixel = intarr(n_elements(h))
for loop=0,2 do begin
    ; Estimate of LaB6 peak positions
    x = ID6_make_twotheta_scale(image)
    for i=0, n_elements(h)-1 do pixel[i] = fix(ID6_center-tan(twotheta[i])*detectordistance/(ID6_psize/1000.))
    ; print, twotheta, pixel
    ; Look for all LaB6 peaks based on the first estimate
    for i=0,(n_elements(h)-1) do begin
      test = pixel[i]
      
      ; test = getIndex(twotheta[i],x,n_elements(x))
      ; print, twotheta[i], test, n_elements(x)
      ;plot, rebin[test-hwidth:test+hwidth], color=0, background=255
      ;oplot, [hwidth,hwidth], [miny,maxy], color=100
      xx = intarr(2*hwidth+1)
      for j=0,2*hwidth do xx[j]=j
      yfit = GAUSSFIT(xx,rebin[test-hwidth:test+hwidth], coeff, NTERMS=4)
      ;oplot, yfit, color=200
      center = test-hwidth+coeff[1]
      pixel[i] = center
      ; print, "Expected at ", test, " found at ", center
      ;wait, 2.
    endfor
    ; Optimizing detector distance and center based on actual peak positions
    ; pp = ID6_center-tan(twotheta)*ID6_distance/ID6_psize should be equal to pixel avec optimizing ID6_distance and ID6_center
    A = fltarr(2,n_elements(twotheta))
    for i=0,n_elements(twotheta)-1 do begin
      A[0,i] = 1.
      A[1,i] = -tan(twotheta[i])/(ID6_psize/1000.)
    endfor
    ; print, A
    ; print, pixel
    res = LA_LEAST_SQUARES(a, pixel)
    detectordistance = res[1]
    ID6_center = res[0]
    logit, log, "Loop" + string (loop+1) + newline + "- distance = " + string(detectordistance) + newline + "- center = " + string(ID6_center) 
  endfor
  
  ; Recreating x-scale based on new detector distance and center b
  x = ID6_make_twotheta_scale(image)
  plot, x, rebin, color=0, background=255, xtitle='2 theta', ytitle='Intensity', xstyle=1, charsize=2
  for i=0,(n_elements(h)-1) do begin
    tt = 180.*twotheta[i]/acos(-1.)
    oplot, [tt,tt], [miny,maxy], color=100
  endfor
  xx1 = min(x)+0.60*(max(x)-min(x))
  xx2 = min(x)+0.65*(max(x)-min(x))
  xx3 = min(x)+0.68*(max(x)-min(x))
  yy = miny+0.9*(maxy-miny)
  oplot, [xx1,xx2], [yy,yy], color=0
  xyouts, xx3, yy, 'Data', color=0, charsize=2
  yy = miny+0.85*(maxy-miny)
  oplot, [xx1,xx2], [yy,yy], color=100
  xyouts, xx3, yy, 'Calibrant peaks', color=0, charsize=2
end

; 
; Function to convert ID6 data into regular diffraction rings
; Send data and name of outputfile
; Will be saved in tiff
; This function is not used. We rely on fit2d
;
pro ID6_uncake, image, outputfile
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
pi = acos(-1.)
x = ID6_make_twotheta_scale(image)
eta = ID6_make_eta_scale(image)
delta_eta = eta[1]-eta[0]
newdata = lonarr(2*ID6_center+1,2*ID6_center+1)
nx = (size(image))[1]
xx = fltarr(nx)
yy = fltarr(nx)
pixel = intarr(nx)
for i=0,nx-1 do pixel[i]=i+1
; Calculating number of pixels for 360 degrees at the edge of the image
twopipixels = ID6_center*2.*pi
; Number of pixels for each eta slice at the edge of the image
nx_eta = fix(twopipixels*delta_eta/(360.))+1
nx_etaB = fix(nx_eta/2.)+1
; for each eta value, we will the arc between eta-delta_eta and eta+delta_eta
; we do that in nx_eta steps
for i=0,n_elements(eta)-1 do begin
  ; print, i, eta[i]
  for j=-nx_etaB,nx_etaB do begin
    thiseta = eta[i]+j*(delta_eta/(2.*nx_etaB))
    ; print, i, j, thiseta
    coseta = cos(pi*thiseta/180.+pi) ; +pi to match setting of detector at ID6 (0 degrees to the left looking down beam)
    sineta = sin(pi*thiseta/180.+pi) ; +pi to match setting of detector at ID6 (0 degrees to the left looking down beam)
    xx = ID6_center+(pixel-ID6_center)*coseta
    yy = ID6_center+(pixel-ID6_center)*sineta
    for k=0,nx-1 do newdata[fix(xx[k]),fix(yy[k])] = image[k,i]
  endfor
endfor
; contour, newdata, ystyle=1, xstyle=1, background=255, color = 0, levels = contours, /FILL
write_tiff, outputfile, newdata, /LONG
end

; This function is not used. We rely on fit2d
function ID6_savedata, data, file
  common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
  ON_IOERROR, IOERROR
  twotheta = ID6_make_twotheta_scale(data)
  eta = ID6_make_eta_scale(data)
  date = systime(0)
  filenames = "ESRF ID6 diffraction data"
  openw, /XDR, 1, file
  ; basic info
  writeu, 1, strlen(filenames)
  writeu, 1, filenames
  writeu, 1, min(eta)
  writeu, 1, max(eta)
  writeu, 1, (max(eta)-min(eta))/n_elements(eta)
  writeu, 1, strlen(date)
  writeu, 1, date
  ; data
  writeu, 1, n_elements(eta)
  writeu, 1, n_elements(twotheta)
  writeu, 1, eta
  writeu, 1, twotheta
  newdata = 1.0*float(data)
  writeu, 1, transpose(newdata)
  close, 1
  return, 1
  IOERROR:close, 1
  return, !ERR_STRING
end

;
; Looks for calibration file, performs calibration, and save parameters in the user interface
; Send
; - base: parent widget
; - log: log window
; - ipDistanceText: field to update with detector distance
; - id6CenterText: field to update with beam center
; This function is not used. We rely on fit2d
;
pro performID6Calibration, base, log, ipDistanceText, id6CenterText
common esrfid6, ID6_psize, ID6_center, ID6_etamin, ID6_etamax, ID6_dark
common experiment, wavelength, detectordistance, experimenttype
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
filters = ['*.tif', '*.tiff', '*.*']
result=dialog_pickfile(title='Select calibration dataset', path=id6directory, DIALOG_PARENT=base, filter=filters)
if (result ne '') then begin
  datafile = result
  FDECOMP, result, disk, dir, name, qual, version
  id6directory = disk+dir
  data = ID6_load(datafile)
  logit, log, "Running ID 6 calibration on " + datafile
  ID6_fitcalibrant, data, log
  logit, log, "Done"
  WIDGET_CONTROL, ipDistanceText, SET_VALUE=STRTRIM(STRING(detectordistance, /PRINT),2)
  WIDGET_CONTROL, id6CenterText, SET_VALUE=STRTRIM(STRING(ID6_center, /PRINT),2)
endif
end

;
; Run uncaking on an image, will open an interface to look for the image to uncake and the new file name
; Send
; - base: parent window
; - log: log window
; This function is not used. We rely on fit2d
; 
pro doID6Uncake, base, log
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
filters = ['*.tif', '*.tiff', '*.*']
result=dialog_pickfile(title='Select dataset to uncake', path=id6directory, DIALOG_PARENT=base, filter=filters)
if (result ne '') then begin
  ; print, "Result is ", result
  datafile = result
  FDECOMP, result, disk, dir, name, qual, version
  id6directory = disk+dir
  data = ID6_load(datafile)
  logit, log, "Read ID 6 data from " + datafile
  filters = ['*.tif', '*.tiff', '*.*']
  result=dialog_pickfile(/write, title='Select name of output file (Tiff format)', path=id6directory, DIALOG_PARENT=base, $
     filter=filters, default_extension='.tif', /OVERWRITE_PROMPT)
  if (result ne '') then begin
    ; print, "Result is ",  result
    output = result
    FDECOMP, result, disk, dir, name, qual, version
    id6directory = disk+dir
    logit, log, "Preparing un-cake version. This may take a while..."
    ID6_uncake, data, output
    logit, log, "Saved uncake image to " + output
  endif
  logit, log, "Done"
endif
end

;
; Function to convert ID6 tif data to multifit format. Will open a dialog to select the name of the image to convert and
; a name for the file to be created
; Send
; - base: parent window
; - log: log window
; This function is not used. We rely on fit2d
;
pro doID6SaveData, base, log
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory, id6directory
  filters = ['*.tif', '*.tiff', '*.*']
  result=dialog_pickfile(title='Select dataset to convert to multifit', path=id6directory, DIALOG_PARENT=base,  filter=filters)
  if (result ne '') then begin
  datafile = result
  FDECOMP, result, disk, dir, name, qual, version
  id6directory = disk+dir
  data = ID6_load(datafile)
  logit, log, "Read ID 6 data from " + datafile
  filters = ['*.idl', '*.*']
  newname = name+'.idl'
  result=dialog_pickfile(/write, file=newname, title='Select name of output file', path=id6directory, DIALOG_PARENT=base, $
    filter=filters, default_extension='.tif', /OVERWRITE_PROMPT)
  if (result ne '') then begin
    ; print, "Result is ",  result
    output = result
    FDECOMP, result, disk, dir, name, qual, version
    id6directory = disk+dir
    logit, log, "Saving data in multifit format"
    test = ID6_savedata(data, output)
    if (test eq 1) then logit, log, "Saved data to " + output else logit, log, "Error: " + test
  endif
endif
end
