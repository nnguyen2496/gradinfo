#' Data on Williams College's graduating classes from 2001 to 2016
#'
#' This dataframe contains information about Williams College's graduating classes from 2001
#' to 2016.
#'
#' @format A data frame with 9 variables
#' \itemize{
#'   \item \strong{First.Name}: First names of students
#'   \item \strong{Middle.and.Last.Name}: Middle names and last names of students
#'   \item \strong{Dept.Honor}: Name of the department that awarded academic honor to the student (NA if
#'                      there is none)
#'   \item \strong{Dept.Honor.lv}: The degree of department honor awared to the student, either "highest"
#'                         or "normal" (NA if the student is not awarded an honor)
#'   \item \strong{sigma.xi}: Status of student's sigma xi membership (TRUE if student is a member of sigma xi
#'                    FALSE if student isn't)
#'   \item \strong{PKB}: Status of student's Phi Kappa Beta membership (TRUE if student is a member of Phi
#'               Kappa Beta, FALSE if student isn't)
#'   \item \strong{Clark.Fellow}: Status of student's clark fellowship (TRUE if student is a fellow, FALSE
#'                        if student isn't)
#'   \item \strong{Latin.honor}: Title of Latin honor that student received: "summa", "magna", "cum laude".
#'                       NA if student did not receive any title
#'   \item \strong{degree}: degree that student received upon graduation. There are three possible values:
#'                  \itemize{
#'                    \item MA: Master Degree of Arts
#'                    \item MAED: Master Degree of Economics Development
#'                    \item BA: Bachelor of Art
#'                  }
#'   \item \strong{Grad.Year} = Graduation year of student
#'   \item \strong{Gender} = Gender of student
#'   }
#' @docType data
#' @name williams_grad
#'
#' @keywords data, williams, grad, graduate, BA, MA, MAED, Dept.Honor, sigma.xi, Latin.honor, degree
NULL
