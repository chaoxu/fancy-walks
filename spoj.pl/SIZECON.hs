z (n:b) = take n b
main = interact $ show.sum.z.map(max 0.read).lines
