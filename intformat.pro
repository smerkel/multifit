function intformat, n, ndigits
case ndigits of
	'1': s = string(n,format='(I1.1)')
	'2': s = string(n,format='(I2.2)')
	'3': s = string(n,format='(I3.3)')
	'4': s = string(n,format='(I4.4)')
	'5': s = string(n,format='(I5.5)')
	'6': s = string(n,format='(I6.6)')
	'7': s = string(n,format='(I7.7)')
	'8': s = string(n,format='(I8.8)')
	'9': s = string(n,format='(I9.9)')
	'10': s = string(n,format='(I10.10)')
endcase
return, s
end
