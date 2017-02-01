readMajors <- function(){
  x <- read.delim(system.file("extdata", "majors_breakdown.txt", package = "gradinfo"), sep = " ")
  colnames(x) <- c("Majors", 2007:2016)
  x <- melt(x, id = "Majors")
  colnames(x) <- c("Majors", "Year", "Number.of.Students")

  majors = data.frame()
  for (i in 2007:2016){
    y <- filter(x, Year == i)
    y$Percentage <- prop.table(y$Number.of.Students)
    majors <- rbind(tally, y)
    devtools::use_data(majors, pkg = "data")
  }
}

readNumberOfMajors <- function(){
  x <- read.delim(system.file("extdata", "majors.txt", package = "gradinfo"), sep = " ")
  colnames(x) <- 2007:2016
  sum_majors <- melt(x, variable.name = "Year", value.name = "Number.of.Majors")
  devtools::use_data(sum_majors, pkg = "data")
}

