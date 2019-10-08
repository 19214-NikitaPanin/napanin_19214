roots a b c  | a == 0 = case b of 0 -> error "Ento ne to chto kvadratnoe, dazhe ne lineinoe uravnenie"
                                  _ -> ((-1)*c) / b
             | d >= 0 = (x1, x2)
             |otherwise = error "Diskrimonant < 0"			 where
                                     d = b*b - 4 * a * c
                                     x1 = (-b + sqrt d) / 2
                                     x2 = (-b - sqrt d) / 2