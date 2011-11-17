; *******************************************************************
; Multfit efficient processing of 2D diffraction images
; Copyright (C) 2000-2011 S. Merkel, Universite Lille 1
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

PRO JCPDSObject__DEFINE 
	struct = { JCPDSObject, set: 0, name:'', comment:'', npeaks: 0, symmetry: 0, h: PTR_NEW(), k: PTR_NEW(), l: PTR_NEW(), d0: PTR_NEW(), I0: PTR_NEW(), K0: 0.0, dK0: 0.0, dK0dT: 0.0, dK0dTdP: 0.0, alphaT:0.0, dAlphaT:0.0, a0: 0.0, b0: 0.0, c0: 0.0, alpha0: 0.0, beta0:0.0, gamma0: 0.0, v0: 0.0}
END
; ****************************************************************
; Basic init method
function JCPDSObject::Init
return, 1
end

; ****************************************************************
; Cleanup method
pro JCPDSObject::Cleanup
end

; ****************************************************************
; Read from file
function JCPDSObject::fromFile, lun, filename
on_ioerror, bad
self.name = FILE_BASENAME(filename)
row = ""
readf, lun, row
word = strsplit(row, COUNT=nn, /EXTRACT)
case nn of
	1: begin
		if (word[0] EQ 3) then return, "Can only read JCPDS card version 4"
		return, "Wrong format at line 1"
	end
	2: begin
		if (word[1] EQ 4) then begin
			test = self->fromFileV4(lun)
			return, test
		endif
	end
	else: return, "Wrong format at line 1"
endcase
return, "Wrong format at line 1"
bad: return, !ERR_STRING
end

; Read file, JCPDS V4
function JCPDSObject::fromFileV4, lun
on_ioerror, bad
nline = 1
npeaksread = 0
hread = intarr(50)
kread = intarr(50)
lread = intarr(50)
dread = fltarr(50)
iread = fltarr(50)
self.comment = ""
while ~ EOF(lun) do begin
	row = ""
	readf, lun, row
	; print, "Looking at " + row
	nline += 1
	word = strsplit(row, COUNT=nn, /EXTRACT)
	; print, "Number of words: ", nn
	; print, "First one: ", word[0]
	if (nn gt 0) then begin
		label = STRUPCASE(word[0])
		CASE label OF
			"COMMENT:": begin
				for i=1, nn-1 do self.comment = self.comment + " " + word[i]
			end
			"K0:": self.K0 = float(word[1])
			"K0P:": self.dK0 = float(word[1])
			"DK0DT:": self.dK0dT = float(word[1])
			"DK0PDT:": self.DK0PDT = float(word[1])
			"SYMMETRY:": begin
				label2 = STRUPCASE(word[1])
				case label2 of
					"CUBIC": self.symmetry = 1
					"TETRAGONAL": self.symmetry = 3
					"HEXAGONAL": self.symmetry = 2
					"ORTHORHOMBIC": self.symmetry = 4
					"MONOCLINIC": self.symmetry = 5
					"TRICLINIC": self.symmetry = 6
					"RHOMBOHEDRAL": self.symmetry = 7
					else: return, "Symmetry " + label2 + " is not supported"
				endcase
			end
			"A:": self.a0 = float(word[1])
			"B:": self.b0 = float(word[1])
			"C:": self.c0 = float(word[1])
			"ALPHA:": self.alpha0 = float(word[1])
			"BETA:": self.beta0 = float(word[1])
			"GAMMA:": self.gamma0 = float(word[1])
			"VOLUME:": self.v0 = float(word[1])
			"ALPHAT:": self.alphaT = float(word[1])
			"DALPHAT:": self.dalphaT = float(word[1])
			"DIHKL:": begin
				dread[npeaksread] = float(word[1])
				iread[npeaksread] = float(word[2])
				hread[npeaksread] = fix(word[3])
				kread[npeaksread] = fix(word[4])
				lread[npeaksread] = fix(word[5])
				npeaksread += 1
			end
			else: return, "Don't know what to do with " + label
		endcase
		; print, "Done with " + row
	endif
endwhile
; Fixing up symmetries
case self.symmetry of
	1: begin  ; Cubic
		self.b0 = self.a0
		self.c0 = self.a0
		self.alpha0 = 90.
		self.beta0 = 90.
		self.gamma0 = 90.
	end
	2: begin ; Hexagonal
		self.b0 = self.a0
		self.alpha0 = 90.
		self.beta0 = 90.
		self.gamma0 = 130.
	end
	 3: begin ; Tetragonal
		self.b0 = self.a0
		self.alpha0 = 90.
		self.beta0 = 90.
		self.gamma0 = 90.
	end
	4: begin ; Orthorhombic
		self.alpha0 = 90.;
		self.beta0 = 90.;
		self.gamma0 = 90.;
	end
	5: begin ; Monoclinic
		self.alpha0 = 90.
		self.gamma0 = 90.
	end
	6: ; Triclinic
	7: begin ; Rhombohedral
		self.b0 = a0;
		self.c0 = a0;
	end
	else: return, "Illegal symmetry code"
endcase
self.v0 = self.a0 * self.b0* self.c0
if (self.symmetry eq 2) then self.v0 = self.v0*sqrt(3.)/2.;
if (self.symmetry eq 5) then self.v0 = self.v0*sin(self.beta0*!PI/180.);
if ((self.symmetry eq 6) or (self.symmetry eq 7)) then self.v0 = self.v0* sqrt(1.-(cos(self.alpha0*!PI/180.))^2 $
				 - (cos(self.beta0*!PI/180.))^2 - (cos(self.gamma0*!PI/180.))^2 + $
				 2.*cos(self.alpha0*!PI/180.)*cos(self.beta0*!PI/180.)*cos(self.gamma0*!PI/180.))
; Fixing peak list
; print, "Fixing peak list"
self.npeaks = npeaksread
self.h = PTR_NEW(intarr(self.npeaks))
self.k = PTR_NEW(intarr(self.npeaks))
self.l = PTR_NEW(intarr(self.npeaks))
self.d0 = PTR_NEW(fltarr(self.npeaks))
self.I0 = PTR_NEW(fltarr(self.npeaks))
for i=0, self.npeaks-1 do begin
	; print, "Working on peak ", i
	(*self.d0)[i] = dread[i]
	(*self.I0)[i] = iread[i]
	(*self.h)[i] = hread[i]
	(*self.k)[i] = kread[i]
	(*self.l)[i] = lread[i]
endfor
; print, "Done fixing peak list"
self.set = 1
return, self.set
bad: return, !ERR_STRING
end

; ****************************************************************
; Return name of symmetry according to symmetry code
function JCPDSObject::symmetry
case self.symmetry of
	1: return, "cubic";
	2: return, "hexagonal";
	3: return, "tetragonal";
	4: return, "orthorombic";
	5: return, "monoclinic (angle doesn't change with pressure)";
	6: return, "triclinic (not supported!)";
	7: return, "rhombohedral (not supported!)";
	else: return, "unknown (trouble ahead!)";
endcase
return, "unknown (trouble ahead!)";
end

; ****************************************************************
; Prepares a summary frame with the info about the JCPDS card
;
function JCPDSObject::summaryFrame, container
; Name
name = WIDGET_BASE(container,COLUMN=2)
nameL1 = WIDGET_LABEL(name, value="Name: ", /ALIGN_LEFT)
nameL2 = WIDGET_LABEL(name, value="Comment: ", /ALIGN_LEFT)
nameL3 = WIDGET_LABEL(name, value="Symmetry: ", /ALIGN_LEFT)
nameE1 = WIDGET_LABEL(name, value=self.name, /ALIGN_LEFT)
if (strlen(self.comment) gt 45) then comment = STRMID(self.comment, 0, 45) + "..." else comment = self.comment
nameE2 = WIDGET_LABEL(name, value=comment, /ALIGN_LEFT)
nameE3 = WIDGET_LABEL(name, value=self->symmetry(), /ALIGN_LEFT)
; Cell parameters and equation of state
eos = WIDGET_BASE(container,COLUMN=4, /GRID_LAYOUT)
cellparL1 = WIDGET_LABEL(eos, value="a: ", /ALIGN_LEFT)
cellparL1 = WIDGET_LABEL(eos, value="b: ", /ALIGN_LEFT)
cellparL1 = WIDGET_LABEL(eos, value="c: ", /ALIGN_LEFT)
eosL1 = WIDGET_LABEL(eos, value="K0: ", /ALIGN_LEFT)
eosL2 = WIDGET_LABEL(eos, value="dK0/dT: ", /ALIGN_LEFT)
eosL3 = WIDGET_LABEL(eos, value="alphaT: ", /ALIGN_LEFT)
cellparE1 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.a0,/PRINT),2), /ALIGN_LEFT)
cellparE1 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.b0,/PRINT),2), /ALIGN_LEFT)
cellparE1 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.c0,/PRINT),2), /ALIGN_LEFT)
eosE1 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.K0,/PRINT),2), /ALIGN_LEFT)
eosE2 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.dK0dT,/PRINT),2), /ALIGN_LEFT)
eosE3 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.alphaT,/PRINT),2), /ALIGN_LEFT)
cellparL2 = WIDGET_LABEL(eos, value="alpha: ", /ALIGN_LEFT)
cellparL2 = WIDGET_LABEL(eos, value="beta: ", /ALIGN_LEFT)
cellparL2 = WIDGET_LABEL(eos, value="gamma: ", /ALIGN_LEFT)
eosL4 = WIDGET_LABEL(eos, value="K'0: ", /ALIGN_LEFT)
eosL5 = WIDGET_LABEL(eos, value="d2K0/dTdP: ", /ALIGN_LEFT)
eosL6 = WIDGET_LABEL(eos, value="dalphaT/dT: ", /ALIGN_LEFT)
cellparE2 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.alpha0,/PRINT),2), /ALIGN_LEFT)
cellparE2 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.beta0,/PRINT),2), /ALIGN_LEFT)
cellparE2 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.gamma0,/PRINT),2), /ALIGN_LEFT)
eosE4 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.dK0,/PRINT),2), /ALIGN_LEFT)
eosE5 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.dK0dTdP,/PRINT),2), /ALIGN_LEFT)
eosE6 = WIDGET_LABEL(eos, value=STRTRIM(STRING(self.dAlphaT,/PRINT),2), /ALIGN_LEFT)
; Peaklist, under unix, we add a scroll function; Does not work under the piece of shit of windows
usearray = intarr(self.npeaks)
OS   = strupcase(!VERSION.OS)
OS   = strmid(OS,0,3)
if (OS ne 'WIN') then begin
	peaklist2 = WIDGET_BASE(container, /ALIGN_center, /scroll, X_SCROLL_SIZE=400, xsize=400, y_scroll_size=250, ysize=1000)
	peaklist = WIDGET_BASE(peaklist2, COLUMN=5, /ALIGN_left,  /GRID_LAYOUT)
endif else begin
	peaklist = WIDGET_BASE(container, COLUMN=5, /ALIGN_left,  /GRID_LAYOUT)
endelse
peaki = WIDGET_LABEL(peaklist, value="h", /ALIGN_center, xsize=70)
for i=0, self.npeaks-1 do begin
	peaki = WIDGET_LABEL(peaklist, value=STRTRIM(STRING((*self.h)[i],/PRINT),2), /ALIGN_center, xsize=70)
endfor
peaki = WIDGET_LABEL(peaklist, value="k", /ALIGN_center, xsize=70)
for i=0, self.npeaks-1 do  begin
	peaki = WIDGET_LABEL(peaklist, value=STRTRIM(STRING((*self.k)[i],/PRINT),2), /ALIGN_center, ysize=30, xsize=70)
endfor
peaki = WIDGET_LABEL(peaklist, value="l", /ALIGN_center)
for i=0, self.npeaks-1 do  begin
	peaki = WIDGET_LABEL(peaklist, value=STRTRIM(STRING((*self.l)[i],/PRINT),2), /ALIGN_center, xsize=70)
endfor
peaki = WIDGET_LABEL(peaklist, value="d0", /ALIGN_center, xsize=70)
for i=0, self.npeaks-1 do  begin
	peaki = WIDGET_LABEL(peaklist, value=STRTRIM(STRING((*self.d0)[i],/PRINT),2), /ALIGN_center,xsize=70)
endfor
peaki = WIDGET_LABEL(peaklist, value="Use", /ALIGN_center, xsize=70)
for i=0, self.npeaks-1 do begin
	uvalue = "use" + STRTRIM(STRING(i,/PRINT),2)
	choicelist = widget_Base(peaklist, /NonExclusive,ypad=0)
	usearray[i] = Widget_Button(choicelist, value="use", UVALUE=uvalue, xsize=70, SCR_YSIZE=30)
	Widget_Control, usearray[i], Set_Button=1
endfor
return, usearray
end

; ****************************************************************
; Finds V/Vo for a pressure using the  3d order Birch-Murngham EOS
;
function JCPDSObject::vBirch3, P
vmin = .1
vmax = 1.
prec = 0.001
v = (vmin+vmax)/2.
while ((vmax-vmin) gt prec) do begin
	v = (vmin+vmax)/2.
	pp = self->birch3(v)
	if (pp gt P) then vmin = v else vmax = v
endwhile
return, v
end

; ****************************************************************
; Calculates the pressure for a given V/Vo  using the 
; 3d order Birch-Murnagham EOS
;
function JCPDSObject::birch3, v
f = .5*(v^(-2./3.)-1.)
p0 = self.K0
p1 = 1.5*self.K0*(self.dK0-4.)
Fp = p0+p1*f
p = Fp*3.*f*(1.+2.*f)^(2.5)
return, p
end

; ****************************************************************
; Returns a list of peaks according to the buttons set in the form
; summaryFrame
; Send pressure and wavelength...
;
function JCPDSObject::peaklist, useit, pressure, wavelength
nuse = 0
use = intarr(50)
for i=0, self.npeaks-1 do begin
	thisone = WIDGET_INFO(useit[i], /BUTTON_SET)
	if (thisone eq 1) then begin
		use[nuse] = i
		nuse += 1
	endif
endfor
return, self->somePeaksAtP(nuse, use, pressure, wavelength)
end

; ****************************************************************
; Returns a list of peaks according to a list of peak indices
; Send the indices of the peaks you want to use, pressure and wavelength...
;
function JCPDSObject::somePeaksAtP, nuse, use, pressure, wavelength
label = strarr(nuse)
d = fltarr(nuse)
toreturn = strarr(nuse,2)
v = self.v0*self->vBirch3(pressure)
f = v^(1./3.)
; Calculating cell parameters at this pressure
case self.symmetry of
	1: a = f ; cubic
	2: begin ; hexagonal
		a = (2.*v/(sqrt(3.0)*self.c0/self.a0))^(1./3.)
		c = a*self.c0/self.a0
	end
	3: begin ; tetragonal
		a = (v/(self.c0/self.a0))^(1./3.)
		c = self.c0/self.a0 * a;
		b = a;
	end
	4: begin ; orthorombic
		a = (v/(self.b0/self.a0*self.c0/self.a0))^(1./3.)
		c = self.c0/self.a0 * a;
		b = self.b0/self.a0 * a;
	end
	5: begin ; monoclinic (assume that angle doesn't change with pressure)
		betarad = self.beta0*!PI/180.
		a = a0*(v/(self.a0*self.b0*self.c0*sin(betarad)))^(1./3.)
		c = self.c0/self.a0 * a;
		b = self.b0/self.a0 * a;
	end
endcase
; Calculating d-spacings and 2theta
for i=0, nuse-1 do begin
	h = (*self.h)[use[i]]
	k = (*self.k)[use[i]]
	l = (*self.l)[use[i]]
	toreturn[i,0] = strtrim(string(h,/print),2) + " " + strtrim(string(k,/print),2) + " " +  strtrim(string(l,/print),2)
	case self.symmetry of
		1: d = 1./sqrt((h*h+k*k+l*l)/(a*a)); cubic
		2: d = 1./sqrt( (4./3.)*(h*h + h*k + k*k)/(a*a) + l*l/(c*c) ) ; hexagonal
		3: d = 1./sqrt( ((h*h)+(k*k))/(a*a) + (l*l)/(c*c)); tetragonal
		4: d = 1./sqrt((h*h)/(a*a)+(k*k)/(b*b)+(l*l)/(c*c)); orthorombic
		5: d = 1./sqrt(1./(h*h/(a*a*(sin(betarad))^2) $
				+ k*k/b^2 + l*l/(c*c* (sin(betarad))^2) $
				    - 2.*h*l*cos(betarad)/(a*c*(sin(betarad))^2))); monoclinic
		else: d = 100.;
	endcase
	twotheta = 360.*asin(wavelength/(2.*d))/!PI
	toreturn[i,1] = strtrim(string(twotheta,/print),2)
endfor
return, toreturn
end