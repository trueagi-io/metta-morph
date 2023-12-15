!(hyperpose (1 2))

!(hyperpose (1 (If (== 1 2) 1)))
!(hyperpose ((If (== 1 2) 1) 1))

!(hyperpose (1 2 3))

!(hyperpose ((If (== 1 2) 1) 2 3))
!(hyperpose (1 (If (== 1 2) 1) 3))
!(hyperpose (1 2 (If (== 1 2) 1)))
!(hyperpose (1 (If (== 1 2) 1) (If (== 1 2) 1)))

!(hyperpose (1 2 3 4))

!(hyperpose ((1 2) (3 4)))
