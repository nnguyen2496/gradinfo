#' @title Read in data on a specified graduating class
#' @description Read in information about the graduating class specified by the \code{grad.year}
#'              parameter. The information of interest includes student names, latin honors,
#'              department honors, Clark fellowship, Phi Kappa Beta membership and Sigma Xi
#'              membership
#' @keywords  StudentInfo readStudent
#' @param data Name of the source text file containing information about the interested
#'        graduating class. The argument should follow this format "yyyy-yy.txt".
#'        For instance,if the interested graduating year is 2001, the name of the source
#'        text file would be "2000-01.txt". Accepted graduating year ranges from 2001 to
#'        2016
#' @param grad.year Sets the year in which the given data was collected. The argument should
#'        be a 4 digit integer between 2001 and 2016 (inclusive)
#' @return A dataframe object containing information about the specified graduating class
#'
#' @usage
#' readStudentInfo(data, grad.year)
#'
#' @examples
#' readStudentInfo("2000-01.txt", 2001)
#' readStudentInfo("2001-02.txt", 2002)
#'
#' @export
readStudentInfo <- function(data, grad.year){
  if (!is.numeric(grad.year) || grad.year < 2001 || grad.year > 2016){
    warning("grad.year must be an integer between 2001 and 2016. Enter ?readStudentInfo
         or help(readStudentInfo) for more information")
  }
  input <- readLines(system.file("extdata", data, package = "gradinfo"), warn = FALSE)
  input <- as.data.frame(input)
  colnames(input) <- "raw_content"
  input$raw_content <- as.character(input$raw_content)

  # list of indices of break lines that divide the list of students into different categories
  # based on their degrees and latin honors
  brks <- which(input$raw_content == "")

  # Subsetting data about grad students from Art school
  art_grad <- input[2: (brks[1] - 1),]
  art_grad <- process_data(art_grad, level = "grad")
  degree <- rep("MA", nrow(art_grad))
  Latin.honor <- rep(NA, nrow(art_grad))
  art_grad <- cbind(art_grad, Latin.honor, degree)

  # Subsetting data about grad students from School of Economics Development
  ec_grad <- input[(brks[1] + 3):(brks[2] - 1),]
  ec_grad <- process_data(ec_grad, level = "grad")
  degree <- rep("MAED", nrow(ec_grad))
  Latin.honor <- rep(NA, nrow(ec_grad))
  ec_grad <- cbind(ec_grad, Latin.honor, degree)

  # Subsetting data of summa cum laude undergrads
  summa <- input[(brks[2] + 2):(brks[3] - 1),]
  summa <- process_data(summa, level = "undergrad")
  degree <- rep("BA", nrow(summa))
  Latin.honor <- rep("summa", nrow(summa))
  summa <- cbind(summa, Latin.honor, degree)

  # Subsetting data of magna cum laude undergrads
  magna <- input[(brks[3] + 2):(brks[4] - 1),]
  magna <- process_data(magna, level = "undergrad")
  degree <- rep("BA", nrow(magna))
  Latin.honor <- rep("magna", nrow(magna))
  magna <- cbind(magna, Latin.honor, degree)

  # Subsetting data of cum laude undergrads
  claude <- input[(brks[4] + 2):(brks[5] - 1),]
  claude <- process_data(claude, level = "undergrad")
  degree <- rep("BA", nrow(claude))
  Latin.honor <- rep("cum laude", nrow(claude))
  claude <- cbind(claude, Latin.honor, degree)

  # Subsetting data of undergrads without latin honors
  no_latin <- input[(brks[5] + 2): nrow(input),]
  no_latin <- process_data(no_latin, level = "undergrad")
  degree <- rep("BA", nrow(no_latin))
  Latin.honor <- rep(NA, nrow(no_latin))
  no_latin <- cbind(no_latin, Latin.honor, degree)

  result <- rbind(art_grad, ec_grad, summa, magna, claude, no_latin)
  result <- cbind(result, rep(grad.year, nrow(result)))
  colnames(result)[9] <- "Grad.Year"
  result$degree <- as.character(result$degree)
  return(as.data.frame(result))
}

# @title Modify the dataframe object given by one of the parameters and return
#        a new dataframe object that can display interested information in a desirable
#        format
# @param dataset A dataframe object containing raw data extracted from a source text file
# @param level The level of degree awarded to students whose names appear in the dataset.
#        This parameter only takes in two string values "undergrad" and "grad"
# @return A new dataframe containing information about academic honors, sigma xi membership,
#          Phi Kappa Beta membership and Clark fellowship of the students.

process_data <- function(dataset, level){
  dataset <- as.data.frame(dataset)
  colnames(dataset) <- "content"
  dataset$content <- as.character(dataset$content)

  academic_honor <- rep(NA, nrow(dataset))
  honor_level <- rep(NA, nrow(dataset))
  sigma_xi <- rep(FALSE, nrow(dataset))
  pkb <- rep(FALSE, nrow(dataset))
  clark.fellow <- rep(FALSE, nrow(dataset))

  for (i in 1: nrow(dataset)){

    temp <- dataset[i,]
    if (temp == "") next
    # extract name
    name <- gsub("[+*]", "", temp)
    name <- gsub(",.*$", "", name)
    dataset[i,] <- name

    # check if the student is enlisted in Phi Kappa Beta or
    # Sigma Xi
    if (grepl("[+]", temp)) sigma_xi <- replace(sigma_xi, i, TRUE)
    if (grepl("[*]", temp)) {
      if (level == "undergrad") pkb <- replace(pkb, i, TRUE)
      if (level == "grad") clark.fellow <- replace(clark.fellow, i, TRUE)
    }

    if (grepl(" with", temp)){

        if (grepl("highest", temp)){
          honor_level <- replace(honor_level, i, "Highest")
        } else honor_level <- replace(honor_level, i, "Normal")

        # Read in Department honors
        if (grepl(" in ", temp) && (grepl(" ", dataset[i + 1,]) || i == nrow(dataset))) {
            temp <- gsub("[\r\n]", "", temp) # remove end of line character (if there is any)
            subject <- gsub("^.*\\ in ","", temp) # read the subject
            academic_honor <- replace(academic_honor, i, subject) # add subject (if there is any)
                                                                  #to honor list
        } else {
          # read the subject
          subject <- paste(temp, dataset[i + 1,], sep = " ")
          subject <- gsub("^.*\\ in ","", subject)

          # update academic_honor column
          academic_honor <- replace(academic_honor, i, subject)
          if(i < nrow(dataset)) dataset[i + 1,] <- ""
        }
    }
  }
  result <- as.data.frame(cbind(dataset, academic_honor, honor_level, sigma_xi, pkb, clark.fellow))
  # Remove rows without student names
  colnames(result) <- c("Name", "Dept.honor", "Dept.honor.lv", "sigma.xi", "PKB", "Clark.Fellow")
  result <- dplyr::filter(result, Name != "")
  result$Dept.honor <- as.character(result$Dept.honor)
  result$Dept.honor.lv <- as.character(result$Dept.honor.lv)
  return(result)
}

