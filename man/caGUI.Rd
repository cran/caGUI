\name{caGUI}
\Rdversion{1.1}
\alias{caGUI}
\docType{package}
\title{A Tcl/Tk GUI for the functions in the ca package}
\description{
A Tcl/Tk Graphical User Interface (GUI) for computation and visualization of simple, multiple and joint correspondence analysis
with the functions of the ca package.
}
\usage{caGUI()}
\details{
The main dialog window contains two tabs, one for Simple and another for Multiple and Joint Correspondence Analysis.
The options for Simple, as well as for Multiple and Joint CA include supplementary and subset rows and/or columns, the dimensionality of the solution 
and the graphical options provided by the ca package. 

Data sets can be loaded with the "Select" button, or typed directly in the entry field. The "Select" button displays a listbox with a list of available objects in the global environment. 
This list is restricted to objects with an adequate type (dataframe, matrix, factor, etc). Entries can also be filled directly by typing the desired value.
In this case, R expressions can also be entered, for example wg93[,c(1:4)], data$kol[1:20,1:5], bad$plan$dat , or 1:20.

Once the user has finished interacting with the dialogs, the corresponding ca function is executed by pressing the "Run" button. The "Reset" button resets all dialogs to their default values.
}
\author{
Angelos Markos \email{amarkos@uom.gr} 
}
\references{Nenadic, O. and Greenacre, M. J. (2007). Correspondence analysis in R, with two- and three-dimensional
graphics: The ca package. Journal of Statistical Software, 20 (1).
}
\examples{
\dontrun{caGUI()}
}
\keyword{multivariate}