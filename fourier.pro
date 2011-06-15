function fourier, X, A
;        0 - a(0)
;        1 - a(1)    -> cos(x)
;        2 - a(2)    -> sin(x)
;        3 - a(3)    -> cos(2x)
;        4 - a(4)    -> sin(2x)
; 
nel = N_ELEMENTS(A-1)/2
f = replicate(A(0), N_ELEMENTS(X))
for i=1, nel do begin
	j = 2*(i-1)+1
	k = 2*(i-1)+2
	f = f + A(j) * cos(i*x) + A(k) * sin(i*x)
endfor
return, f
end
