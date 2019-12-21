roots 0 0 0 = error "Nothing to calculate"
roots 0 0 _ = error "No Solutions"
roots a b c 	| a == 0 = (x, x)
		| d == 0 = (x, x)
		| a /= 0 = if d<0 then error "No Solutions" else (x1, x2)
										where
											x = -b / (2*a)
											x1 = (-b + sqrt d ) / (2*a)
											x2 = (-b - sqrt d ) / (2*a)
											d = b*b - 4*a*c
