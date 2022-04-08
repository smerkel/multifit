function read_dioptas_txt_cake, filename, widgetBase, widgetlog
common rawdata, nalpha, ntheta, alpha, twotheta, data
common datainfo, filenames, alphastart, alphaend, intervalle, date
common files, extension, datadirectory, outputdirectory, defaultdirectory, jcpdsdirectory
common diotasfilesparameter, rebinFactor, setRebinFactor
ON_IOERROR, BADINPUT

tmp = read_ascii(filename, DATA_START=1, HEADER=tthethaInfo)
datafile = tmp.(0)
data_size=size(datafile)

; Two thetas are on the first line, except that there is one more twotheta than there are intensites
; We assume that the first twotheta is the left of the pixel and the last twotheta is the right of the pixel
; Does not change much because there are many many points
; print, tthethaInfo
twothetaFirst = strsplit(tthethaInfo, COUNT=nthetafirst,/EXTRACT)
ntheta = nthetafirst-1
twotheta = fltarr(ntheta)
for i = 0,ntheta-1  do begin
	twotheta[i] = (float(twothetaFirst[i])+float(twothetaFirst[i+1]))/2.
endfor
; print, twotheta
; print, twothetaFirst


; then for each line, azimuth is first element and intensity data
nalpha = data_size(2)
alpha = fltarr(nalpha)
data = fltarr(nalpha,ntheta)
for i = 0,nalpha-1  do begin
	alpha[i] = datafile[0,i]
	data[i,*] = datafile[1:*,i]
endfor


; Rebinning data to improve resolution (1 file every degree or more is not great)
needSetRebin = 0
if (~ isa(setRebinFactor)) then begin
	needSetRebin = 1
endif else begin
	if (setRebinFactor eq 1) then needSetRebin = 1
endelse
if (needSetRebin eq 1) then begin
	guess = fix(5. / ((max(alpha)-min(alpha))/nalpha)) ; guess try to get 1 file every 5 degrees
	extramessages = strarr(3)
	extramessages[0] = " - Current number of azimuths: " + STRTRIM(string(nalpha, /print),2) + ", range from " + STRTRIM(string(min(alpha), /print),2) + " to " + STRTRIM(string(max(alpha), /print),2) + " degrees"
	extramessages[1] = " - Set 1 to keep the same number of azimuths, 2 to divide it by 2 and improve signal to noise ration, etc."
	extramessages[2] = " - 1 spectrum every 5 degrees is often good for stress and strain analysis"
	rebinFactor = getInteger('Rebin data in azimuth', 'Rebin data in azimuth ?', widgetBase, value=guess, extramessages=extramessages)
	test = 0 ; the new number of bins needs to be an integer multiple of the old one
	while (test eq 0) do begin
		if( fix(nalpha / rebinFactor) ne (1.0 * nalpha / rebinFactor)) then begin
			rebinFactor = rebinFactor + 1
		endif else begin
			test = 1
		endelse
	endwhile
endif
logit, widgetlog, "Original data had " + string(nalpha, /print) + " azimuth values. Rebinning to " + string(nalpha/rebinFactor, /print) + "."
; 
alpha = REBIN(alpha, fix(nalpha / rebinFactor))
data = REBIN(data, fix(nalpha / rebinFactor), ntheta)
nalpha = fix(nalpha / rebinFactor)

; saving basic informations about the data
filenames = filename
alphastart = alpha[0]
alphaend = alpha[-1]
intervalle = alpha[1]-alpha[0]
date = systime(0)

logit, widgetlog, "Read cake image from " + filename + "."
logit, widgetlog, "  - number of azimuth (after rebinning): " + STRTRIM(string(nalpha, /print),2)
logit, widgetlog, "  - number of 2theta: " +  STRTRIM(string(ntheta, /print),2)
return, 1
BADINPUT: return, !ERR_STRING
end
