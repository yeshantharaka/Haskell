module Test 
where
squre x = x * x

roots a b c =
    let discr = sqrt (b*b - 4*a*c)
        twice_a = 2*a
    in  ((-b + discr) / twice_a,
        (-b - discr) / twice_a)

factorial 1 = 1
factorial n = n * factorial(n-1)

expo a 0 = 1
expo a b = a * expo a (b-1)

my_length [] = 0
my_length (x:xs) = 1 + my_length xs

my_filter p [] = []
my_filter p (x:xs) =
    if p /= x
        then x : my_filter p xs
    else my_filter p xs
