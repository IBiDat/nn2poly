# combinations with repetition algorithm works

    Code
      combinations_with_repetition(5, 3)
    Output
            [,1] [,2] [,3]
       [1,]    1    1    1
       [2,]    1    1    2
       [3,]    1    1    3
       [4,]    1    1    4
       [5,]    1    1    5
       [6,]    1    2    2
       [7,]    1    2    3
       [8,]    1    2    4
       [9,]    1    2    5
      [10,]    1    3    3
      [11,]    1    3    4
      [12,]    1    3    5
      [13,]    1    4    4
      [14,]    1    4    5
      [15,]    1    5    5
      [16,]    2    2    2
      [17,]    2    2    3
      [18,]    2    2    4
      [19,]    2    2    5
      [20,]    2    3    3
      [21,]    2    3    4
      [22,]    2    3    5
      [23,]    2    4    4
      [24,]    2    4    5
      [25,]    2    5    5
      [26,]    3    3    3
      [27,]    3    3    4
      [28,]    3    3    5
      [29,]    3    4    4
      [30,]    3    4    5
      [31,]    3    5    5
      [32,]    4    4    4
      [33,]    4    4    5
      [34,]    4    5    5
      [35,]    5    5    5

---

    Code
      combinations_with_repetition(3, 5)
    Output
            [,1] [,2] [,3] [,4] [,5]
       [1,]    1    1    1    1    1
       [2,]    1    1    1    1    2
       [3,]    1    1    1    1    3
       [4,]    1    1    1    2    2
       [5,]    1    1    1    2    3
       [6,]    1    1    1    3    3
       [7,]    1    1    2    2    2
       [8,]    1    1    2    2    3
       [9,]    1    1    2    3    3
      [10,]    1    1    3    3    3
      [11,]    1    2    2    2    2
      [12,]    1    2    2    2    3
      [13,]    1    2    2    3    3
      [14,]    1    2    3    3    3
      [15,]    1    3    3    3    3
      [16,]    2    2    2    2    2
      [17,]    2    2    2    2    3
      [18,]    2    2    2    3    3
      [19,]    2    2    3    3    3
      [20,]    2    3    3    3    3
      [21,]    3    3    3    3    3

# multiset partitions are correctly generated

    Code
      generate_partitions(5, 3)
    Output
      [[1]]
      [[1]][[1]]
      [[1]][[1]][[1]]
      [1] 1
      
      
      
      [[2]]
      [[2]][[1]]
      [[2]][[1]][[1]]
      [1] 1 1
      
      
      [[2]][[2]]
      [[2]][[2]][[1]]
      [1] 1
      
      [[2]][[2]][[2]]
      [1] 1
      
      
      
      [[3]]
      [[3]][[1]]
      [[3]][[1]][[1]]
      [1] 1 2
      
      
      [[3]][[2]]
      [[3]][[2]][[1]]
      [1] 1
      
      [[3]][[2]][[2]]
      [1] 2
      
      
      
      [[4]]
      [[4]][[1]]
      [[4]][[1]][[1]]
      [1] 1 1 1
      
      
      [[4]][[2]]
      [[4]][[2]][[1]]
      [1] 1 1
      
      [[4]][[2]][[2]]
      [1] 1
      
      
      [[4]][[3]]
      [[4]][[3]][[1]]
      [1] 1
      
      [[4]][[3]][[2]]
      [1] 1
      
      [[4]][[3]][[3]]
      [1] 1
      
      
      
      [[5]]
      [[5]][[1]]
      [[5]][[1]][[1]]
      [1] 1 1 2
      
      
      [[5]][[2]]
      [[5]][[2]][[1]]
      [1] 1 1
      
      [[5]][[2]][[2]]
      [1] 2
      
      
      [[5]][[3]]
      [[5]][[3]][[1]]
      [1] 1 2
      
      [[5]][[3]][[2]]
      [1] 1
      
      
      [[5]][[4]]
      [[5]][[4]][[1]]
      [1] 1
      
      [[5]][[4]][[2]]
      [1] 1
      
      [[5]][[4]][[3]]
      [1] 2
      
      
      
      [[6]]
      [[6]][[1]]
      [[6]][[1]][[1]]
      [1] 1 2 3
      
      
      [[6]][[2]]
      [[6]][[2]][[1]]
      [1] 1 2
      
      [[6]][[2]][[2]]
      [1] 3
      
      
      [[6]][[3]]
      [[6]][[3]][[1]]
      [1] 1 3
      
      [[6]][[3]][[2]]
      [1] 2
      
      
      [[6]][[4]]
      [[6]][[4]][[1]]
      [1] 1
      
      [[6]][[4]][[2]]
      [1] 2 3
      
      
      [[6]][[5]]
      [[6]][[5]][[1]]
      [1] 1
      
      [[6]][[5]][[2]]
      [1] 2
      
      [[6]][[5]][[3]]
      [1] 3
      
      
      

---

    Code
      generate_partitions(3, 5)
    Output
      [[1]]
      [[1]][[1]]
      [[1]][[1]][[1]]
      [1] 1
      
      
      
      [[2]]
      [[2]][[1]]
      [[2]][[1]][[1]]
      [1] 1 1
      
      
      [[2]][[2]]
      [[2]][[2]][[1]]
      [1] 1
      
      [[2]][[2]][[2]]
      [1] 1
      
      
      
      [[3]]
      [[3]][[1]]
      [[3]][[1]][[1]]
      [1] 1 2
      
      
      [[3]][[2]]
      [[3]][[2]][[1]]
      [1] 1
      
      [[3]][[2]][[2]]
      [1] 2
      
      
      
      [[4]]
      [[4]][[1]]
      [[4]][[1]][[1]]
      [1] 1 1 1
      
      
      [[4]][[2]]
      [[4]][[2]][[1]]
      [1] 1 1
      
      [[4]][[2]][[2]]
      [1] 1
      
      
      [[4]][[3]]
      [[4]][[3]][[1]]
      [1] 1
      
      [[4]][[3]][[2]]
      [1] 1
      
      [[4]][[3]][[3]]
      [1] 1
      
      
      
      [[5]]
      [[5]][[1]]
      [[5]][[1]][[1]]
      [1] 1 1 2
      
      
      [[5]][[2]]
      [[5]][[2]][[1]]
      [1] 1 1
      
      [[5]][[2]][[2]]
      [1] 2
      
      
      [[5]][[3]]
      [[5]][[3]][[1]]
      [1] 1 2
      
      [[5]][[3]][[2]]
      [1] 1
      
      
      [[5]][[4]]
      [[5]][[4]][[1]]
      [1] 1
      
      [[5]][[4]][[2]]
      [1] 1
      
      [[5]][[4]][[3]]
      [1] 2
      
      
      
      [[6]]
      [[6]][[1]]
      [[6]][[1]][[1]]
      [1] 1 2 3
      
      
      [[6]][[2]]
      [[6]][[2]][[1]]
      [1] 1 2
      
      [[6]][[2]][[2]]
      [1] 3
      
      
      [[6]][[3]]
      [[6]][[3]][[1]]
      [1] 1 3
      
      [[6]][[3]][[2]]
      [1] 2
      
      
      [[6]][[4]]
      [[6]][[4]][[1]]
      [1] 1
      
      [[6]][[4]][[2]]
      [1] 2 3
      
      
      [[6]][[5]]
      [[6]][[5]][[1]]
      [1] 1
      
      [[6]][[5]][[2]]
      [1] 2
      
      [[6]][[5]][[3]]
      [1] 3
      
      
      
      [[7]]
      [[7]][[1]]
      [[7]][[1]][[1]]
      [1] 1 1 1 1
      
      
      [[7]][[2]]
      [[7]][[2]][[1]]
      [1] 1 1 1
      
      [[7]][[2]][[2]]
      [1] 1
      
      
      [[7]][[3]]
      [[7]][[3]][[1]]
      [1] 1 1
      
      [[7]][[3]][[2]]
      [1] 1 1
      
      
      [[7]][[4]]
      [[7]][[4]][[1]]
      [1] 1 1
      
      [[7]][[4]][[2]]
      [1] 1
      
      [[7]][[4]][[3]]
      [1] 1
      
      
      [[7]][[5]]
      [[7]][[5]][[1]]
      [1] 1
      
      [[7]][[5]][[2]]
      [1] 1
      
      [[7]][[5]][[3]]
      [1] 1
      
      [[7]][[5]][[4]]
      [1] 1
      
      
      
      [[8]]
      [[8]][[1]]
      [[8]][[1]][[1]]
      [1] 1 1 1 2
      
      
      [[8]][[2]]
      [[8]][[2]][[1]]
      [1] 1 1 1
      
      [[8]][[2]][[2]]
      [1] 2
      
      
      [[8]][[3]]
      [[8]][[3]][[1]]
      [1] 1 1 2
      
      [[8]][[3]][[2]]
      [1] 1
      
      
      [[8]][[4]]
      [[8]][[4]][[1]]
      [1] 1 1
      
      [[8]][[4]][[2]]
      [1] 1 2
      
      
      [[8]][[5]]
      [[8]][[5]][[1]]
      [1] 1 1
      
      [[8]][[5]][[2]]
      [1] 1
      
      [[8]][[5]][[3]]
      [1] 2
      
      
      [[8]][[6]]
      [[8]][[6]][[1]]
      [1] 1 2
      
      [[8]][[6]][[2]]
      [1] 1
      
      [[8]][[6]][[3]]
      [1] 1
      
      
      [[8]][[7]]
      [[8]][[7]][[1]]
      [1] 1
      
      [[8]][[7]][[2]]
      [1] 1
      
      [[8]][[7]][[3]]
      [1] 1
      
      [[8]][[7]][[4]]
      [1] 2
      
      
      
      [[9]]
      [[9]][[1]]
      [[9]][[1]][[1]]
      [1] 1 1 2 2
      
      
      [[9]][[2]]
      [[9]][[2]][[1]]
      [1] 1 1 2
      
      [[9]][[2]][[2]]
      [1] 2
      
      
      [[9]][[3]]
      [[9]][[3]][[1]]
      [1] 1 1
      
      [[9]][[3]][[2]]
      [1] 2 2
      
      
      [[9]][[4]]
      [[9]][[4]][[1]]
      [1] 1 1
      
      [[9]][[4]][[2]]
      [1] 2
      
      [[9]][[4]][[3]]
      [1] 2
      
      
      [[9]][[5]]
      [[9]][[5]][[1]]
      [1] 1 2 2
      
      [[9]][[5]][[2]]
      [1] 1
      
      
      [[9]][[6]]
      [[9]][[6]][[1]]
      [1] 1 2
      
      [[9]][[6]][[2]]
      [1] 1 2
      
      
      [[9]][[7]]
      [[9]][[7]][[1]]
      [1] 1 2
      
      [[9]][[7]][[2]]
      [1] 1
      
      [[9]][[7]][[3]]
      [1] 2
      
      
      [[9]][[8]]
      [[9]][[8]][[1]]
      [1] 1
      
      [[9]][[8]][[2]]
      [1] 1
      
      [[9]][[8]][[3]]
      [1] 2 2
      
      
      [[9]][[9]]
      [[9]][[9]][[1]]
      [1] 1
      
      [[9]][[9]][[2]]
      [1] 1
      
      [[9]][[9]][[3]]
      [1] 2
      
      [[9]][[9]][[4]]
      [1] 2
      
      
      
      [[10]]
      [[10]][[1]]
      [[10]][[1]][[1]]
      [1] 1 1 2 3
      
      
      [[10]][[2]]
      [[10]][[2]][[1]]
      [1] 1 1 2
      
      [[10]][[2]][[2]]
      [1] 3
      
      
      [[10]][[3]]
      [[10]][[3]][[1]]
      [1] 1 1 3
      
      [[10]][[3]][[2]]
      [1] 2
      
      
      [[10]][[4]]
      [[10]][[4]][[1]]
      [1] 1 1
      
      [[10]][[4]][[2]]
      [1] 2 3
      
      
      [[10]][[5]]
      [[10]][[5]][[1]]
      [1] 1 1
      
      [[10]][[5]][[2]]
      [1] 2
      
      [[10]][[5]][[3]]
      [1] 3
      
      
      [[10]][[6]]
      [[10]][[6]][[1]]
      [1] 1 2 3
      
      [[10]][[6]][[2]]
      [1] 1
      
      
      [[10]][[7]]
      [[10]][[7]][[1]]
      [1] 1 2
      
      [[10]][[7]][[2]]
      [1] 1 3
      
      
      [[10]][[8]]
      [[10]][[8]][[1]]
      [1] 1 2
      
      [[10]][[8]][[2]]
      [1] 1
      
      [[10]][[8]][[3]]
      [1] 3
      
      
      [[10]][[9]]
      [[10]][[9]][[1]]
      [1] 1 3
      
      [[10]][[9]][[2]]
      [1] 1
      
      [[10]][[9]][[3]]
      [1] 2
      
      
      [[10]][[10]]
      [[10]][[10]][[1]]
      [1] 1
      
      [[10]][[10]][[2]]
      [1] 1
      
      [[10]][[10]][[3]]
      [1] 2 3
      
      
      [[10]][[11]]
      [[10]][[11]][[1]]
      [1] 1
      
      [[10]][[11]][[2]]
      [1] 1
      
      [[10]][[11]][[3]]
      [1] 2
      
      [[10]][[11]][[4]]
      [1] 3
      
      
      
      [[11]]
      [[11]][[1]]
      [[11]][[1]][[1]]
      [1] 1 1 1 1 1
      
      
      [[11]][[2]]
      [[11]][[2]][[1]]
      [1] 1 1 1 1
      
      [[11]][[2]][[2]]
      [1] 1
      
      
      [[11]][[3]]
      [[11]][[3]][[1]]
      [1] 1 1 1
      
      [[11]][[3]][[2]]
      [1] 1 1
      
      
      [[11]][[4]]
      [[11]][[4]][[1]]
      [1] 1 1 1
      
      [[11]][[4]][[2]]
      [1] 1
      
      [[11]][[4]][[3]]
      [1] 1
      
      
      [[11]][[5]]
      [[11]][[5]][[1]]
      [1] 1 1
      
      [[11]][[5]][[2]]
      [1] 1 1
      
      [[11]][[5]][[3]]
      [1] 1
      
      
      [[11]][[6]]
      [[11]][[6]][[1]]
      [1] 1 1
      
      [[11]][[6]][[2]]
      [1] 1
      
      [[11]][[6]][[3]]
      [1] 1
      
      [[11]][[6]][[4]]
      [1] 1
      
      
      [[11]][[7]]
      [[11]][[7]][[1]]
      [1] 1
      
      [[11]][[7]][[2]]
      [1] 1
      
      [[11]][[7]][[3]]
      [1] 1
      
      [[11]][[7]][[4]]
      [1] 1
      
      [[11]][[7]][[5]]
      [1] 1
      
      
      
      [[12]]
      [[12]][[1]]
      [[12]][[1]][[1]]
      [1] 1 1 1 1 2
      
      
      [[12]][[2]]
      [[12]][[2]][[1]]
      [1] 1 1 1 1
      
      [[12]][[2]][[2]]
      [1] 2
      
      
      [[12]][[3]]
      [[12]][[3]][[1]]
      [1] 1 1 1 2
      
      [[12]][[3]][[2]]
      [1] 1
      
      
      [[12]][[4]]
      [[12]][[4]][[1]]
      [1] 1 1 1
      
      [[12]][[4]][[2]]
      [1] 1 2
      
      
      [[12]][[5]]
      [[12]][[5]][[1]]
      [1] 1 1 1
      
      [[12]][[5]][[2]]
      [1] 1
      
      [[12]][[5]][[3]]
      [1] 2
      
      
      [[12]][[6]]
      [[12]][[6]][[1]]
      [1] 1 1 2
      
      [[12]][[6]][[2]]
      [1] 1 1
      
      
      [[12]][[7]]
      [[12]][[7]][[1]]
      [1] 1 1 2
      
      [[12]][[7]][[2]]
      [1] 1
      
      [[12]][[7]][[3]]
      [1] 1
      
      
      [[12]][[8]]
      [[12]][[8]][[1]]
      [1] 1 1
      
      [[12]][[8]][[2]]
      [1] 1 1
      
      [[12]][[8]][[3]]
      [1] 2
      
      
      [[12]][[9]]
      [[12]][[9]][[1]]
      [1] 1 1
      
      [[12]][[9]][[2]]
      [1] 1 2
      
      [[12]][[9]][[3]]
      [1] 1
      
      
      [[12]][[10]]
      [[12]][[10]][[1]]
      [1] 1 1
      
      [[12]][[10]][[2]]
      [1] 1
      
      [[12]][[10]][[3]]
      [1] 1
      
      [[12]][[10]][[4]]
      [1] 2
      
      
      [[12]][[11]]
      [[12]][[11]][[1]]
      [1] 1 2
      
      [[12]][[11]][[2]]
      [1] 1
      
      [[12]][[11]][[3]]
      [1] 1
      
      [[12]][[11]][[4]]
      [1] 1
      
      
      [[12]][[12]]
      [[12]][[12]][[1]]
      [1] 1
      
      [[12]][[12]][[2]]
      [1] 1
      
      [[12]][[12]][[3]]
      [1] 1
      
      [[12]][[12]][[4]]
      [1] 1
      
      [[12]][[12]][[5]]
      [1] 2
      
      
      
      [[13]]
      [[13]][[1]]
      [[13]][[1]][[1]]
      [1] 1 1 1 2 2
      
      
      [[13]][[2]]
      [[13]][[2]][[1]]
      [1] 1 1 1 2
      
      [[13]][[2]][[2]]
      [1] 2
      
      
      [[13]][[3]]
      [[13]][[3]][[1]]
      [1] 1 1 1
      
      [[13]][[3]][[2]]
      [1] 2 2
      
      
      [[13]][[4]]
      [[13]][[4]][[1]]
      [1] 1 1 1
      
      [[13]][[4]][[2]]
      [1] 2
      
      [[13]][[4]][[3]]
      [1] 2
      
      
      [[13]][[5]]
      [[13]][[5]][[1]]
      [1] 1 1 2 2
      
      [[13]][[5]][[2]]
      [1] 1
      
      
      [[13]][[6]]
      [[13]][[6]][[1]]
      [1] 1 1 2
      
      [[13]][[6]][[2]]
      [1] 1 2
      
      
      [[13]][[7]]
      [[13]][[7]][[1]]
      [1] 1 1 2
      
      [[13]][[7]][[2]]
      [1] 1
      
      [[13]][[7]][[3]]
      [1] 2
      
      
      [[13]][[8]]
      [[13]][[8]][[1]]
      [1] 1 1
      
      [[13]][[8]][[2]]
      [1] 1 2 2
      
      
      [[13]][[9]]
      [[13]][[9]][[1]]
      [1] 1 1
      
      [[13]][[9]][[2]]
      [1] 1 2
      
      [[13]][[9]][[3]]
      [1] 2
      
      
      [[13]][[10]]
      [[13]][[10]][[1]]
      [1] 1 1
      
      [[13]][[10]][[2]]
      [1] 1
      
      [[13]][[10]][[3]]
      [1] 2 2
      
      
      [[13]][[11]]
      [[13]][[11]][[1]]
      [1] 1 1
      
      [[13]][[11]][[2]]
      [1] 1
      
      [[13]][[11]][[3]]
      [1] 2
      
      [[13]][[11]][[4]]
      [1] 2
      
      
      [[13]][[12]]
      [[13]][[12]][[1]]
      [1] 1 2 2
      
      [[13]][[12]][[2]]
      [1] 1
      
      [[13]][[12]][[3]]
      [1] 1
      
      
      [[13]][[13]]
      [[13]][[13]][[1]]
      [1] 1 2
      
      [[13]][[13]][[2]]
      [1] 1 2
      
      [[13]][[13]][[3]]
      [1] 1
      
      
      [[13]][[14]]
      [[13]][[14]][[1]]
      [1] 1 2
      
      [[13]][[14]][[2]]
      [1] 1
      
      [[13]][[14]][[3]]
      [1] 1
      
      [[13]][[14]][[4]]
      [1] 2
      
      
      [[13]][[15]]
      [[13]][[15]][[1]]
      [1] 1
      
      [[13]][[15]][[2]]
      [1] 1
      
      [[13]][[15]][[3]]
      [1] 1
      
      [[13]][[15]][[4]]
      [1] 2 2
      
      
      [[13]][[16]]
      [[13]][[16]][[1]]
      [1] 1
      
      [[13]][[16]][[2]]
      [1] 1
      
      [[13]][[16]][[3]]
      [1] 1
      
      [[13]][[16]][[4]]
      [1] 2
      
      [[13]][[16]][[5]]
      [1] 2
      
      
      
      [[14]]
      [[14]][[1]]
      [[14]][[1]][[1]]
      [1] 1 1 1 2 3
      
      
      [[14]][[2]]
      [[14]][[2]][[1]]
      [1] 1 1 1 2
      
      [[14]][[2]][[2]]
      [1] 3
      
      
      [[14]][[3]]
      [[14]][[3]][[1]]
      [1] 1 1 1 3
      
      [[14]][[3]][[2]]
      [1] 2
      
      
      [[14]][[4]]
      [[14]][[4]][[1]]
      [1] 1 1 1
      
      [[14]][[4]][[2]]
      [1] 2 3
      
      
      [[14]][[5]]
      [[14]][[5]][[1]]
      [1] 1 1 1
      
      [[14]][[5]][[2]]
      [1] 2
      
      [[14]][[5]][[3]]
      [1] 3
      
      
      [[14]][[6]]
      [[14]][[6]][[1]]
      [1] 1 1 2 3
      
      [[14]][[6]][[2]]
      [1] 1
      
      
      [[14]][[7]]
      [[14]][[7]][[1]]
      [1] 1 1 2
      
      [[14]][[7]][[2]]
      [1] 1 3
      
      
      [[14]][[8]]
      [[14]][[8]][[1]]
      [1] 1 1 2
      
      [[14]][[8]][[2]]
      [1] 1
      
      [[14]][[8]][[3]]
      [1] 3
      
      
      [[14]][[9]]
      [[14]][[9]][[1]]
      [1] 1 1 3
      
      [[14]][[9]][[2]]
      [1] 1 2
      
      
      [[14]][[10]]
      [[14]][[10]][[1]]
      [1] 1 1 3
      
      [[14]][[10]][[2]]
      [1] 1
      
      [[14]][[10]][[3]]
      [1] 2
      
      
      [[14]][[11]]
      [[14]][[11]][[1]]
      [1] 1 1
      
      [[14]][[11]][[2]]
      [1] 1 2 3
      
      
      [[14]][[12]]
      [[14]][[12]][[1]]
      [1] 1 1
      
      [[14]][[12]][[2]]
      [1] 1 2
      
      [[14]][[12]][[3]]
      [1] 3
      
      
      [[14]][[13]]
      [[14]][[13]][[1]]
      [1] 1 1
      
      [[14]][[13]][[2]]
      [1] 1 3
      
      [[14]][[13]][[3]]
      [1] 2
      
      
      [[14]][[14]]
      [[14]][[14]][[1]]
      [1] 1 1
      
      [[14]][[14]][[2]]
      [1] 1
      
      [[14]][[14]][[3]]
      [1] 2 3
      
      
      [[14]][[15]]
      [[14]][[15]][[1]]
      [1] 1 1
      
      [[14]][[15]][[2]]
      [1] 1
      
      [[14]][[15]][[3]]
      [1] 2
      
      [[14]][[15]][[4]]
      [1] 3
      
      
      [[14]][[16]]
      [[14]][[16]][[1]]
      [1] 1 2 3
      
      [[14]][[16]][[2]]
      [1] 1
      
      [[14]][[16]][[3]]
      [1] 1
      
      
      [[14]][[17]]
      [[14]][[17]][[1]]
      [1] 1 2
      
      [[14]][[17]][[2]]
      [1] 1 3
      
      [[14]][[17]][[3]]
      [1] 1
      
      
      [[14]][[18]]
      [[14]][[18]][[1]]
      [1] 1 2
      
      [[14]][[18]][[2]]
      [1] 1
      
      [[14]][[18]][[3]]
      [1] 1
      
      [[14]][[18]][[4]]
      [1] 3
      
      
      [[14]][[19]]
      [[14]][[19]][[1]]
      [1] 1 3
      
      [[14]][[19]][[2]]
      [1] 1
      
      [[14]][[19]][[3]]
      [1] 1
      
      [[14]][[19]][[4]]
      [1] 2
      
      
      [[14]][[20]]
      [[14]][[20]][[1]]
      [1] 1
      
      [[14]][[20]][[2]]
      [1] 1
      
      [[14]][[20]][[3]]
      [1] 1
      
      [[14]][[20]][[4]]
      [1] 2 3
      
      
      [[14]][[21]]
      [[14]][[21]][[1]]
      [1] 1
      
      [[14]][[21]][[2]]
      [1] 1
      
      [[14]][[21]][[3]]
      [1] 1
      
      [[14]][[21]][[4]]
      [1] 2
      
      [[14]][[21]][[5]]
      [1] 3
      
      
      
      [[15]]
      [[15]][[1]]
      [[15]][[1]][[1]]
      [1] 1 1 2 2 3
      
      
      [[15]][[2]]
      [[15]][[2]][[1]]
      [1] 1 1 2 2
      
      [[15]][[2]][[2]]
      [1] 3
      
      
      [[15]][[3]]
      [[15]][[3]][[1]]
      [1] 1 1 2 3
      
      [[15]][[3]][[2]]
      [1] 2
      
      
      [[15]][[4]]
      [[15]][[4]][[1]]
      [1] 1 1 2
      
      [[15]][[4]][[2]]
      [1] 2 3
      
      
      [[15]][[5]]
      [[15]][[5]][[1]]
      [1] 1 1 2
      
      [[15]][[5]][[2]]
      [1] 2
      
      [[15]][[5]][[3]]
      [1] 3
      
      
      [[15]][[6]]
      [[15]][[6]][[1]]
      [1] 1 1 3
      
      [[15]][[6]][[2]]
      [1] 2 2
      
      
      [[15]][[7]]
      [[15]][[7]][[1]]
      [1] 1 1 3
      
      [[15]][[7]][[2]]
      [1] 2
      
      [[15]][[7]][[3]]
      [1] 2
      
      
      [[15]][[8]]
      [[15]][[8]][[1]]
      [1] 1 1
      
      [[15]][[8]][[2]]
      [1] 2 2 3
      
      
      [[15]][[9]]
      [[15]][[9]][[1]]
      [1] 1 1
      
      [[15]][[9]][[2]]
      [1] 2 2
      
      [[15]][[9]][[3]]
      [1] 3
      
      
      [[15]][[10]]
      [[15]][[10]][[1]]
      [1] 1 1
      
      [[15]][[10]][[2]]
      [1] 2 3
      
      [[15]][[10]][[3]]
      [1] 2
      
      
      [[15]][[11]]
      [[15]][[11]][[1]]
      [1] 1 1
      
      [[15]][[11]][[2]]
      [1] 2
      
      [[15]][[11]][[3]]
      [1] 2
      
      [[15]][[11]][[4]]
      [1] 3
      
      
      [[15]][[12]]
      [[15]][[12]][[1]]
      [1] 1 2 2 3
      
      [[15]][[12]][[2]]
      [1] 1
      
      
      [[15]][[13]]
      [[15]][[13]][[1]]
      [1] 1 2 2
      
      [[15]][[13]][[2]]
      [1] 1 3
      
      
      [[15]][[14]]
      [[15]][[14]][[1]]
      [1] 1 2 2
      
      [[15]][[14]][[2]]
      [1] 1
      
      [[15]][[14]][[3]]
      [1] 3
      
      
      [[15]][[15]]
      [[15]][[15]][[1]]
      [1] 1 2 3
      
      [[15]][[15]][[2]]
      [1] 1 2
      
      
      [[15]][[16]]
      [[15]][[16]][[1]]
      [1] 1 2 3
      
      [[15]][[16]][[2]]
      [1] 1
      
      [[15]][[16]][[3]]
      [1] 2
      
      
      [[15]][[17]]
      [[15]][[17]][[1]]
      [1] 1 2
      
      [[15]][[17]][[2]]
      [1] 1 2
      
      [[15]][[17]][[3]]
      [1] 3
      
      
      [[15]][[18]]
      [[15]][[18]][[1]]
      [1] 1 2
      
      [[15]][[18]][[2]]
      [1] 1 3
      
      [[15]][[18]][[3]]
      [1] 2
      
      
      [[15]][[19]]
      [[15]][[19]][[1]]
      [1] 1 2
      
      [[15]][[19]][[2]]
      [1] 1
      
      [[15]][[19]][[3]]
      [1] 2 3
      
      
      [[15]][[20]]
      [[15]][[20]][[1]]
      [1] 1 2
      
      [[15]][[20]][[2]]
      [1] 1
      
      [[15]][[20]][[3]]
      [1] 2
      
      [[15]][[20]][[4]]
      [1] 3
      
      
      [[15]][[21]]
      [[15]][[21]][[1]]
      [1] 1 3
      
      [[15]][[21]][[2]]
      [1] 1
      
      [[15]][[21]][[3]]
      [1] 2 2
      
      
      [[15]][[22]]
      [[15]][[22]][[1]]
      [1] 1 3
      
      [[15]][[22]][[2]]
      [1] 1
      
      [[15]][[22]][[3]]
      [1] 2
      
      [[15]][[22]][[4]]
      [1] 2
      
      
      [[15]][[23]]
      [[15]][[23]][[1]]
      [1] 1
      
      [[15]][[23]][[2]]
      [1] 1
      
      [[15]][[23]][[3]]
      [1] 2 2 3
      
      
      [[15]][[24]]
      [[15]][[24]][[1]]
      [1] 1
      
      [[15]][[24]][[2]]
      [1] 1
      
      [[15]][[24]][[3]]
      [1] 2 2
      
      [[15]][[24]][[4]]
      [1] 3
      
      
      [[15]][[25]]
      [[15]][[25]][[1]]
      [1] 1
      
      [[15]][[25]][[2]]
      [1] 1
      
      [[15]][[25]][[3]]
      [1] 2 3
      
      [[15]][[25]][[4]]
      [1] 2
      
      
      [[15]][[26]]
      [[15]][[26]][[1]]
      [1] 1
      
      [[15]][[26]][[2]]
      [1] 1
      
      [[15]][[26]][[3]]
      [1] 2
      
      [[15]][[26]][[4]]
      [1] 2
      
      [[15]][[26]][[5]]
      [1] 3
      
      
      

