
PRO FitPatternModel__DEFINE 
	struct = { FitPatternModel, nsubpat:0, peakprofile: 0, subpatterns: PTR_NEW() } 
END

; Init method
function FITPATTERNModel::Init
return, 1
end
 
; Cleanup method
pro FitPatternModel::Cleanup
end

; 
FUNCTION FitPatternModel::setNSubPat, n
	if (self.nsubpat ne 0) then return, 'Sorry, this fitPattern object is used already'
	self.nsubpat = n
	; self.subpatterns = PTR_NEW(fltarr(n))
	self.subpatterns = PTR_NEW(OBJARR(n))
RETURN, 1
END

FUNCTION FitPatternModel::setSubPat, log, i, filename
	(*(self.subpatterns))(i) = OBJ_NEW('SubPatternModel')
	test = (*self.subpatterns)(i)->fromFile(log,filename,self.peakprofile)
RETURN, test
END

FUNCTION FitPatternModel::setPeakProfile, n ; 0: gauss, 1: lorentz, 2: pseudo-voigt 
	self.peakprofile = n
RETURN, 1
END

FUNCTION FitPatternModel::peakProfile
RETURN, self.peakprofile
END

FUNCTION FitPatternModel::nPat
RETURN, self.nsubpat
END

FUNCTION FitPatternModel::subPattern, i
	return, (*(self.subpatterns))(i)
end

FUNCTION FitPatternModel::readFromAscii, lun
	on_ioerror, bad
	row = readascii(lun,com='#')
	if (fix(row) ne 1) then return, 'Sorry, we can only read file format 1 at this time'
	row = readascii(lun,com='#')
	self.peakprofile = fix(row)
	row = readascii(lun,com='#')
	self.nsubpat = fix(row)
	self.subpatterns = PTR_NEW(OBJARR(self.nsubpat))
	for i=0, self.nsubpat-1 do begin
		(*(self.subpatterns))(i) = OBJ_NEW('SubPatternModel')
		noerror = (*self.subpatterns)(i)->readFromAscii(lun)
		if (noerror ne 1) then return, noerror
	endfor
RETURN, 1
bad: return, !ERR_STRING
END

FUNCTION FitPatternModel::saveToAscii, lun
	printf, lun, '# Fit pattern model file'
	printf, lun, '# File version'
	printf, lun, '1'
	printf, lun, '# Peak profile (0: gauss, 1: pseudo-voigt, 2: lorentz)'
	printf, lun, STRING(self.peakprofile, /PRINT)
	printf, lun, '# Number of sub-patterns'
	printf, lun, STRING(self.nsubpat, /PRINT)
	for i=0, self.nsubpat-1 do begin
		printf, lun, '# Sub-patterns ' + STRING(i, /PRINT)
		noerror = (*self.subpatterns)(i)->saveToAscii(lun)
		if (noerror ne 1) then return, noerror
	endfor
RETURN, 1
END