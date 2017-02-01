library(gradinfo)
library(dplyr)
context("Reading information about graduating classes from text files")

test_that("readBAyear reads in data correctly",{
  expect_that(length((filter(readStudentInfo("2000-01.txt", 2001), Latin.honor == "Summa"))), is_more_than(0))
  expect_that(length((filter(readStudentInfo("2000-01.txt", 2001), Latin.honor == "Summa"))), is_less_than(20))
  expect_that(length((filter(readStudentInfo("2002-03.txt", 2003), Latin.honor == "Summa"))), is_more_than(0))
  expect_that(length((filter(readStudentInfo("2003-04.txt", 2004), Latin.honor == "Summa"))), is_less_than(20))
})


test_that("readStudentInfo output a dataframe with columns of correct format",{
  expect_equal(is.character(readStudentInfo("2000-01.txt", 2001)$Name), TRUE)
  expect_equal(is.character(readStudentInfo("2000-01.txt", 2001)$Dept.honor), TRUE)
  expect_equal(is.character(readStudentInfo("2000-01.txt", 2001)$Dept.honor.lv), TRUE)
  expect_equal(is.logical(readStudentInfo("2000-01.txt", 2001)$sigma.xi), TRUE)
  expect_equal(is.logical(readStudentInfo("2000-01.txt", 2001)$PKB), TRUE)
  expect_equal(is.logical(readStudentInfo("2000-01.txt", 2001)$Clark.Fellow), TRUE)
  expect_equal(is.character(readStudentInfo("2000-01.txt", 2001)$Latin.honor), TRUE)
  expect_equal(is.character(readStudentInfo("2000-01.txt", 2001)$degree), TRUE)
  expect_equal(is.numeric(readStudentInfo("2000-01.txt", 2001)$Grad.Year), TRUE)
})
