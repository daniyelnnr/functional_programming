-- fibonacci single recursion
fib num
  | num <= 1 = num
  | otherwise = fib_acc num 1 0

fib_acc num prev acc
  | num <= 0 = acc
  | otherwise = fib_acc (num-1) acc (prev+acc)

-- isPrime number
isPrime num = null [ x | x <- [2..num-1], num `mod` x == 0 ]

-- factorial
fat n = product [1..n]

-- factorial variation
