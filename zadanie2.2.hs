roots :: Float -> Float -> Float -> (Float, Float)
roots a b c  | (b*b - 4 * a * c) < 0 = error "Смэээээрть"
             | (b*b - 4 * a * c) > 0 = (x1, x2) where
                                                d = b*b - 4 * a * c
                                                x1 = (-b + sqrt d) / 2
                                                x2 = (-b - sqrt d) / 2
             | (b*b - 4 * a * c) == 0 = -b /	(2 * a)						