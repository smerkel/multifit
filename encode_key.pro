
pro encode_key, email, duration
email = strtrim(strlowcase(email),2)
expir = strtrim(string(long(systime(/julian)+duration*366)),2)
expirb = byte(expir)
nc = N_ELEMENTS(expirb)
print, expirb
key = ''
for i=0,nc-1 do begin
	digit = fix(long(expirb[i])-48); digit
	tmp = (digit+3) mod 10
	key += strtrim(tmp,2)
endfor
print, 'Expiration time: ', expir
print, 'Email: ', email
print, 'Key: ', key
str = key + email
strb = byte(str)
nc = N_ELEMENTS(strb)
testcode = ''
for i=0,nc-1 do begin
	; enc += string(long(strb[i]),FORMAT='(I03)')
	testcode += strtrim(string(fix(long(strb[i])*3 mod 10)),2)
endfor
print, 'Code: ', testcode
; Trying to decode key
keyb = byte(key)
nc = N_ELEMENTS(keyb)
decode = ''
for i=0,nc-1 do begin
	digit = fix(long(keyb[i])-48); digit
	tmp = digit-3
	if (tmp lt 0) then tmp=10+tmp
	decode += strtrim(tmp,2)
endfor
print, 'Decoded: ', decode
CALDAT, decode, Month1, Day1, Year1
print, 'Decoded and readable: ', Month1, Day1, Year1
end
