; *******************************************************************
;
; function GET_INDEX
;   to know the index of a given two theta value in the twotheta table
; send the two theta value you are looking for, the twotheta table and
;   the number of two theta values
; returns the index
;
; *******************************************************************

function getIndex, x, twotheta, ntheta
index = 0
for i = 0, (ntheta-1) do begin
    if (x gt twotheta(i)) then begin
        index = i
    endif
endfor
return, index
end
