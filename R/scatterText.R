#' Character-based Scatterplot 
#'
#' @description
#' \code{scatterText} produces a character-based scatter plot, in which the points
#' are represented by characters. This function supports various visual styles to
#' render the plot.
#'
#' @param x the x coordinates of points in the plot.
#' @param y the y coordinates of points in the plot,
#' @param char.dim size of the plot in characters. Either a single number that defines the width and height or a vector of size two describing x and y size
#'
#' @examples
#' x <- rnorm(10)
#' y <- rnorm(10)
#' scatterText(x,y, char.dim=c(10,5))
#'
#' @author Andreas M. Brandmaier 
#'
#' @export
scatterText <- function(x, y, char.dim=5, style=1)
{

  
  # middle dot U+00B7
  dot <- '\u00B7'
  
  # rescaling
  xmin <- min(x)
  xmax <- max(x)
  ymin <- min(y)
  ymax <- max(y)
  
  x.norm <- (x-xmin) / (xmax-xmin)
  y.norm <- (y-ymin) / (ymax-ymin)
  
  if (length(char.dim)==1) {
  char.dim.x <- char.dim*2
  char.dim.y <- char.dim
  } else if (length(char.dim)==2) {
    char.dim.x <- char.dim[1]
    char.dim.y <- char.dim[2]  
  }
  
  x.disc <- round(x.norm*(char.dim.x-1))
  y.disc <- round(y.norm*(char.dim.y-1))
  
  #plot(x.norm, y)
  #points(x.disc, y.disc, pch=2)
  
  output <- ""
  for (jy in 1:char.dim.y) {
  
    output <- paste(output,"|" , sep="")
      for (ix in 1:char.dim.x) {

      dot.here <- sum(x.disc==(ix-1) & (char.dim.y-1-y.disc)==(jy-1))
      
      if (style==1) {
      char <- ifelse(dot.here, "\u00B7"," ")
      } else if (style==2) {
      
      # or
      if (dot.here == 0) {
        char <- " "
     } else if (dot.here == 1) {
        char <- "\u00B7" # Middle Dot
        #char <- "•"
      } else if (dot.here == 2) {
        char <- ":"
      } else if (dot.here == 3) {
        char <- "\u2234"
      } else {
        char <- "※"
      }
        
      } else {
        stop("Unknown style.")
      }
      
     # char <- ifelse(dot.here, "\u00B7"," ")
      
      output <- paste(output,char,sep="")
      
      
    }
    output <- paste( output,"\n",sep="")
  }
  
  output <- paste(output,paste("+",paste(rep("-",char.dim.x),collapse="",sep=""),sep=""),sep="")
  
  cat(output)
  
  return(invisible(output))
  
}

