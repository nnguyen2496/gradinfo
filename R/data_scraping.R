#' @title Read in data on all available graduating classes
#' @description Read in data on all available graduating classes. The information of interest
#'              includes student names, latin honors, department honors, Clark fellowship,
#'              Phi Kappa Beta membership and Sigma Xi membership.
#' @keywords  StudentInfo readStudent
#' @return A dataframe object containing information about all available graduating classes
#'
#' @usage
#' data_scraping()
#' @export
data_scraping <- function(){
  year0001 <- readStudentInfo("2000-01.txt", 2001)
  year0102 <- readStudentInfo("2001-02.txt", 2002)
  year0203 <- readStudentInfo("2002-03.txt", 2003)
  year0304 <- readStudentInfo("2003-04.txt", 2004)
  year0405 <- readStudentInfo("2004-05.txt", 2005)
  year0506 <- readStudentInfo("2005-06.txt", 2006)
  year0607 <- readStudentInfo("2006-07.txt", 2007)
  year0708 <- readStudentInfo("2007-08.txt", 2008)
  year0809 <- readStudentInfo("2008-09.txt", 2009)
  year0910 <- readStudentInfo("2009-10.txt", 2010)
  year1011 <- readStudentInfo("2010-11.txt", 2011)
  year1112 <- readStudentInfo("2011-12.txt", 2012)
  year1213 <- readStudentInfo("2012-13.txt", 2013)
  year1314 <- readStudentInfo("2013-14.txt", 2014)
  year1415 <- readStudentInfo("2014-15.txt", 2015)
  year1516 <- readStudentInfo("2015-16.txt", 2016)
  williams_grad <- rbind(year0001, year0102, year0203, year0304, year0405, year0506, year0607,
                         year0708, year0809, year0910, year1011, year1112, year1213, year1314, year1415,
                         year1516)
  williams_grad <- extract(williams_grad, Name, c("First.Name", "Last.and.Middle.Name"),"([^ ]+) (.*)")


  gender <- gender(williams_grad$First.Name, method = "kantrowitz")
  gender$gender[which(gender$gender == "either" | is.na(gender$gender))] <- sample(c("male","female"), 1)
  gender <- gender[c(-1066, -7002),]
  williams_grad <- cbind(williams_grad, Gender = gender$gender)
  devtools::use_data(williams_grad, pkg = "data", overwrite = TRUE)
}
