context("calculate_pareto_front")

test_that("calculate_pareto_front", {

  # 2D example copied of ecr2 package
  data_matrix = matrix(
    c(# front 1
      1, 4,
      2, 3,
      4, 1,
      # front 2
      2.2, 3.2,
      4, 3,
      4.2, 1,
      # front 3
      3, 5,
      3.2, 4.7,
      6, 2,
      # front 4
      6, 6
    ), nrow = 2L
  )
  # maximize
  pareto_front = calculate_pareto_front(data_matrix)
  expect_equal(pareto_front, 10)
  # minimize
  pareto_front = calculate_pareto_front(data_matrix, maximize = FALSE)
  expect_equal(pareto_front, 1:3)

  # 2D example (slightly turned parallelogram)
  data_matrix = rbind(c(1, 0.25, 0.75, 0), c(0, 0.25, 0.75, 1))
  # maximize
  pareto_front = calculate_pareto_front(data_matrix)
  expect_equal(pareto_front, c(1, 3, 4))
  # minimize
  pareto_front = calculate_pareto_front(data_matrix, maximize = FALSE)
  expect_equal(pareto_front, c(1, 2, 4))

  # 4D example with redundant column 
  data_matrix = rbind(c(1,1,0,1), c(1,1,0,0), c(1,0,1,0), c(1,1,1,1))
  # maximize
  pareto_front = calculate_pareto_front(data_matrix)
  expect_equal(pareto_front, 1)
  # minimize
  pareto_front = calculate_pareto_front(data_matrix, maximize = FALSE)
  expect_equal(pareto_front, 3:4)


})
