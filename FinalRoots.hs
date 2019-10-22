roots a b c =  if a == 0 && c == 0 && b == 0 then  error "a=0, b=0, c=0, So...What to do?"
					else if a == 0 && b == 0 then c
						else if a == 0 then ((-1)*c) / b 
							else 
								if d>=0 then (x1, x2) where
											d = b*b - 4 * a * c
											x1 = (-b + sqrt d) / 2
											x2 = (-b - sqrt d) / 2
							    else  error "Diskrimonant < 0"
