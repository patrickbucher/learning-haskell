uncurriedAddition nums =
  let
    a = fst nums
    b = snd nums
  in a + b

myCurry f = \x -> \y -> f (x, y)

myUncurry f = \nums -> f (fst nums) (snd nums)
