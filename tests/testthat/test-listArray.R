test_that("listArray", {
  l <- listArray(1:5)
  expect_equal(l[3], 3)
  #
  v <- 1:5
  names(v) <- letters[1:5]
  l <- listArray(v)
  expect_true(hasKey(l,'c'))
  l <- listArray(v, use.names=FALSE)
  expect_false(hasKey(l,'c'))
  #
  l[3] <- "test"
  expect_equal(l[3], "test")
  #
  l<-listArray(1:5, ignore=4)
  expect_equal(length(l), 4)
  #
  expect_equal(length(keys(l)), 4)
  #
  l <- listArray(letters[1:5])
  expect_equal(length(l), 5)
  expect_equal(l[3], 'c')
  #
  l <- listArray(as.list(1:5))
  expect_equal(l[3], list(3))
  #
  m  <- matrix(1:9, 3, 3)
  ml <-listArray(m)
  expect_equal(length(ml), 9)  
  expect_true(hasKey(ml, 2, 2))
  #
  l <- listArray()
  l[NULL] <- "Null"
  expect_equal(l[NULL], 'Null')
  #
  l['iris'] <- iris
  expect_false(is.null(l['iris']))
  expect_equal(dim(l['iris']), c(150, 5))
})
