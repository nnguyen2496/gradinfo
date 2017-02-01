#' @title Summary Statistics
#' @description Uses data from the williams_grad dataset to compute and display appropriate summary
#' statistics in either tabular or graphical format.
#' @keywords  analysis, data_analysis
#' @param type The subject of analysis. Possible options are \code{undergrad, grad, latin, department, gender}.
#' \itemize{
#'  \item \strong{undergrad}: generates summary statistics about undegraduate students
#'  \item \strong{grad}: generates summary statistics about graduate students
#'  \item \strong{latin}: generates summary statistics about the distribution of Latin honors at Williams
#'  \item \strong{department}: generates summary statistics about the distribution of department honors at
#'                             Williams
#'  \item \strong{gender}: generates summary statistics about the overall gender ratio and the gender ratios
#'                         of each department.
#' }
#'
#' @param format This argument has three values \code{("numeric", "timeseries", "distribution")}
#' \itemize{
#'  \item \strong{numeric}: returns a data frame containing relevant summary statistics
#'  \item \strong{timeseries}: generates graphical summary that displays changes of the interested statistics over time.
#'  \item \strong{distribution}: generates graphical summary that displays distribution of the interested subject.
#' }
#'
#' @return A dataframe containing summary statistics if \code{format == "numeric"}
#'
#' @usage
#' readStudentInfo(data, grad.year)
#'
#' @examples
#' statsummary("undergrad", "numeric") # Return a dataframe containing the numbers of undergraduate students by years
#' statsummary("latin", "timeseries")  # Return a bar chart displaying the percentage of students receiving Summa Cum Laude,
#'                                     # Magna Cum Laude, Cum Laude and no Latin honors over time.
#' statsummay("depertment", "distribution") # Return a bar chart showing top 5 most popular department (ranked by percentage of total students
#'                                            majoring in the department)
#'
#' @import ggplot2 reshape2 gridExtra
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @export
statsummary <- function(type = "undergrad", format = "timeseries"){
  # Load data
  data(williams_grad)

  if (type == "undergrad"){
    undergrad_handler(format)
  }
  else if (type == "grad"){
    graduate_handler(format)
  }
  else if (type == "latin"){
    latin_handler(format)
  }
  else if (type == "department"){
    dept_handler(format)
  }
  else if (type == "gender"){
    gender_handler(format)
  }
  else stop("Invalid type argument, type ?statsummary or help(statsummary) for more information")
}


# Count the number of students for a given year and type of degree (i.e: "undergrad"/"grad")
student_count <- function(deg, year){
  if (!is.numeric(year) || year > 2016 || year < 2001) stop("year has to be an integer between
                                                            2001 and 2016")
  if (deg != "BA" && deg != "MA" && deg != "MAED") stop("degree can only take one of these three values: \"MA\", \"MAED\" and \"BA\"")
  return(nrow(filter(williams_grad, degree == deg, Grad.Year == year)))
}

# Display numeric or graphical summary of important statistics on undergraduate students
undergrad_handler <- function(format){
  nstudent = integer()
  for (i in 2001:2016){
    nstudent <- append(nstudent, student_count(deg = "BA", year = i))
  }
  stat <- data.frame(Year = 2001:2016, number.of.undergraduate = nstudent)

  if (format == "numeric") return(stat)
  else if (format == "timeseries"){
    ggplot(data = stat, aes(x = Year, y = number.of.undergraduate)) +
      geom_bar(fill = "#00BFFF", stat = "identity") +
      labs(y = "Number of undergraduates", x = "Year") +
      scale_x_continuous(breaks = 2001:2016) +
      ggtitle("Number of undergraduates over time") +
      theme(plot.title = element_text(size = 12, hjust = 0.5), axis.text.x = element_text(size = 6.5, hjust = 0.5))

  }
  else if (format == "distribution"){
    ggplot(data = stat, aes(x = number.of.undergraduate)) +
    geom_histogram(aes(y=..density..), binwidth = 2, fill = "orange", alpha = 0.7, col = "black") +
    geom_density(alpha=0.4, fill="#FF6666")+labs(y = "Density", x = "Number of Students",
    title = ("Distribution of number of students graduating from Williams")) +
    xlim(480, 570) +
    theme(plot.title = element_text(hjust = 0.5))
  }
  else stop("Invalid format argument!")
}

# Display numeric or graphical summary of important statistics on graduate students
graduate_handler <- function(format){
  nstudent = integer()
  for (i in 2001:2016){
    nstudent <- append(nstudent, student_count(deg = "MA", year = i))
    nstudent <- append(nstudent, student_count(deg = "MAED", year = i))
  }
  stat <- data.frame(Year = rep(2001:2016, each = 2), number.of.graduate = nstudent, degree = rep(c("MA", "MAED"), 16))

  if (format == "numeric") return(stat)
  else if (format == "timeseries"){
    ggplot(data = stat) + geom_bar(mapping = aes(x = Year, y = number.of.graduate, fill = degree), stat = "identity", position = "dodge") +
    labs(y = "Number of graduates", x = "Year",
    title = "Number of graduate students over time") +
    scale_x_continuous(breaks = 2001:2016) +
    theme(axis.text.x = element_text(size = 7, hjust = 0.5), plot.title = element_text(hjust = 0.5))
  }
  else if (format == "distribution"){
    g1 <- ggplot(data = filter(stat, degree == "MA"), aes(x = number.of.graduate)) +
    geom_histogram(aes(y=..density..), fill = "orange", alpha = 0.7, col = "black", binwidth = 2) +
    geom_density(alpha=0.4, fill="pink") +
    labs(y = "Density", x = "Number of Students", title = "MA graduates") +
    xlim(0, 22) + theme(plot.title = element_text(hjust = 0.5))

    g2 <- ggplot(data = filter(stat, degree == "MAED"), aes(x = number.of.graduate)) +
    geom_histogram(aes(y=..density..), fill = "orange", alpha = 0.7, col = "black", binwidth = 2) +
    geom_density(alpha=0.4, fill="pink") +
    labs(y = "Density", x = "Number of Students", title = "MAED graduates") +
    xlim(0, 35) + theme(plot.title = element_text(hjust = 0.5))

    grid.arrange(g1, g2, ncol = 2)

  }
  else stop("Invalid format argument!")
}

# Display numeric or graphical summary on the distribution of Latin honors
latin_handler <- function(format){
  summary <- matrix(nrow = 16, ncol = 5)
  year <- 2001
  for (i in 1:16){
    summary[i,] <- count_honors(year)
    year <- year + 1
  }
  result <- data.frame(Grad.Year = 2001:2016, Summa = summary[,1], Magna = summary[,2], Cum.Laude =
                       summary[,3], no_honors = summary[,4], total = summary[,5])
  if (format == "numeric") return(result)
  else if (format == "timeseries"){
    input <- melt(result, id = c("Grad.Year", "total"), variable.name = "Legend")
    ggplot(data = input, aes(x = Grad.Year, y = value/total*100, fill = Legend)) +
    geom_bar(stat = "identity", position = "dodge", color = "white") +
    labs(x = "Year", y = "Percentage") +
    scale_x_continuous(breaks = 2001:2016) +
    coord_flip()
  }
  else if (format == "distribution"){
    input <- melt(result, id = c("Grad.Year", "total"))
    h1 <- ggplot(data = filter(input, variable == "Summa"), aes(x = value)) + geom_histogram(fill = "orange", color = "black", alpha = 0.7, binwidth = 1) +
          labs(x = "Number of Summa Cum Laude") + geom_density(alpha=0.4, fill="#FF6666") + xlim(8,15)
    h2 <- ggplot(data = filter(input, variable == "Magna"), aes(x = value)) + geom_histogram(fill = "orange", color = "black", alpha = 0.7, binwidth = 1) +
          labs(x = "Number of Managa Cum Laude") + geom_density(alpha=0.4, fill="#FF6666")
    h3 <- ggplot(data = filter(input, variable == "Cum.Laude"), aes(x = value)) + geom_histogram(fill = "orange", color = "black", alpha = 0.7, binwidth = 1.5) +
          labs(x = "Number of Cum Laude") + geom_density(alpha=0.4, fill="#FF6666")
    h4 <- ggplot(data = filter(input, variable == "no_honors"), aes(x = value)) + geom_histogram(fill = "orange", color = "black", alpha = 0.7, binwidth = 4) +
          labs(x = "Number of students with no Latin honors") + geom_density(alpha=0.4, fill="#FF6666")
    grid.arrange(h1, h2, h3, h4, ncol = 2, nrow = 2)
  }
  else stop("Invalid format argument")
}

# @parameteter: year: the graduating year of students receiving Latin honors
# @return: a data frame containing the numbers of students receiving Summa Cum Laude, Magna Cum Laude, Cum Laude and no Latin honors 
#          for each year between 2001 and 2016
count_honors <- function(year){
  result <- c(nrow(filter(williams_grad, Latin.honor == "summa", Grad.Year == year)),
              nrow(filter(williams_grad, Latin.honor == "magna", Grad.Year == year)),
              nrow(filter(williams_grad, Latin.honor == "cum laude", Grad.Year == year)),
              nrow(filter(williams_grad, is.na(Latin.honor), Grad.Year == year, degree == "BA")),
              nrow(filter(williams_grad, Grad.Year == year, degree == "BA")))
  return(result)
}

# Display numeric or graphical summary on the number of majors and the distribution of students by among departments over time
dept_handler <- function(format){
  if (format == "numeric") {
    return(majors)
  }
  else if (format == "timeseries"){
    ggplot(data = sum_majors, aes(x = Year, y = Number.of.Majors)) +
    geom_line(size = 2, color = "blue", alpha = 0.7) +
    geom_point(shape = 23, fill = "red", size = 3) +
    scale_x_continuous(breaks = 2007:2016) +
    scale_y_continuous(breaks = seq(650,780, 10)) + ylab("Number of majors") + theme_bw()
  }
  else if (format == "distribution"){
    top_five <- data.frame()
    for (i in 2007:2016){
      stat <- filter(majors, Year == i)
      stat <- head(stat[order(stat$Percentage, decreasing = TRUE),],5)
      top_five <- rbind(top_five, stat)
    }
    ggplot(data = top_five) + geom_bar(aes(x = Year, y = Percentage, fill = Majors), color = "black", stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#FF0033", "blue", "#99FF66", "#FFFF66","purple","orange", "#CCCCCC", "pink")) +
    scale_y_continuous(breaks = seq(0, 0.20, 0.05)) + labs(title = "Top 5 most popular majors over time", hjust = 0.5) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  }
}

# Display numeric or graphical summary of the gender ratio by year or by department
gender_handler <- function(format){
  male <- numeric()
  female <- numeric()
  department <- character()
  Year <- numeric()

  Dept <- filter(williams_grad, !is.na(Dept.honor))
  Dept <- unique(Dept$Dept.honor)
  Dept <- sapply(Dept, FUN = function(z) ifelse(!grepl("Gender", z) && !grepl("Methodology", z),
                                                unlist(strsplit(as.character(z)," and ")), z))
  Dept <- unique(as.vector(Dept))


  for (i in Dept){
    # Create a data frame that contains numbers of males/females by department and the corresponding gender ratios
    department <- append(department, rep(i, 16))
    for (j in 2001:2016){
      nmale <- nrow(filter(williams_grad, Grad.Year == j, Gender == "male", grepl(i, Dept.honor)))
      nfemale <- nrow(filter(williams_grad, Grad.Year == j, Gender == "female", grepl(i, Dept.honor)))
      male <- append(male, nmale)
      female <- append(female, nfemale)
      Year <- append(Year, j)
    }
  }
  summary <- data.frame(Department = department, Grad.Year = Year, Male = male, Female = female)
  summary <- mutate(summary, Ratio = signif(ifelse(pmin(Male, Female) != 0,
                                                   pmax(Male, Female)/pmin(Male, Female),
                                                   pmax(Male, Female) + 0.5), 4))

  if (format == "numeric"){
    return(summary)
  } else if (format == "timeseries"){
    # Create a data frame that contains the overall gender ratio for each year
    males <- numeric()
    females <- numeric()
    for (i in 2001:2016){
      males <- append(males, nrow(filter(williams_grad, Grad.Year == i, Gender == "male")))
      females <- append(females, nrow(filter(williams_grad, Grad.Year == i, Gender == "female")))
    }
    sex <- data.frame(Year = 2001:2016, Male = males, Female = females)
    sex <- mutate(sex, Ratio = signif(pmax(Male, Female)/pmin(Male, Female), 4))
    sex <- melt(sex, id = c("Year", "Ratio"), variable.name = "Gender", value.name = "Count")

    g1 <- ggplot(data = sex) + geom_bar(aes(x = Year, y = Count, fill = Gender), stat = "identity", position = "dodge") +
          scale_x_continuous(breaks = 2001:2016) + theme(axis.text.x = element_text(size = 7, hjust = 0.5))
    g2 <- ggplot(data = sex, aes(x = Year, y = Ratio)) + geom_line(color = "blue") + geom_point(shape = 23, size = 3, fill = "yellow") +
          scale_x_continuous(breaks = 2001:2016)
    grid.arrange(g1, g2)

  } else if (format == "distribution"){
    top_one <- data.frame()
    for (i in 2001:2016){
      stat <- filter(summary, Year == i)
      stat <- head(stat[order(stat$Ratio, decreasing = TRUE),],1)
      top_one <- rbind(top_one, stat)
    }
    ggplot(data = top_one) +
    geom_bar(aes(x = Grad.Year, y = Ratio, fill = Department), color = "black", stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = 2001:2016) + scale_y_continuous(breaks = seq(0, 12, 1)) +
    scale_fill_manual(values = c("#FF0033", "blue", "#99FF66", "#FFFF66","purple","orange", "#CCCCCC", "pink", "#003319", "#CCE5FF")) +
    coord_flip() + theme_bw()
  }
}

