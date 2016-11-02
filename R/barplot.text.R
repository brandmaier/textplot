# good-working chars are X, |, \u2591-\u2593
bartext.multiline<-function(x, num.lines=5, char="\u2588", padding=2, sep=" ", ...)
{
  frac_seq <- (1:num.lines)/num.lines
  rel.x <- x / (max(x))
  #cat(rel.x)
  find.index <- function(x){min(which(x <= frac_seq))}
  mapping <- sapply(rel.x, FUN=find.index)
  
  #pad <- paste(rep(" ",padding),collapse="",sep="")
  empty.pad <- paste(rep(" ",padding),collapse="",sep="")
  
    
  between.sep <- sep
  
  strs<- t(sapply(num.lines:1, function(x) {paste(ifelse(mapping>=x, char," "),sep="",collapse = between.sep)} ))
  result <- (paste(empty.pad,strs,empty.pad,collapse="\n"))

  xaxis <- paste(rep("-", padding*2+nchar(sep)*(length(x)+1)+length(x)),collapse="",sep="")
  result <- paste(result, xaxis,sep="\n")
  
  cat(result)  
}


bartext.singleline<-function(x, padding=2, ...)
{
  char_seq <- c("▁","▂","▃","▅","▆","▇","█","▆");
  frac_seq <- c(1.0/8, 2.0/8.0, 3.0/8.0, 4.0/8.0, 5.0/8, 6.0/8, 7.0/8, 1)
  rel.x <- x / (max(x))
  #cat(rel.x)
  find.index <- function(x){min(which(x <= frac_seq))}
  mapping <- sapply(rel.x, FUN=find.index)
  #cat(paste(mapping))
  pad <- paste(rep("_",padding),collapse="",sep="")
  return(
    paste(pad,
    paste(char_seq[mapping],sep="",collapse=""),
    pad,collapse="",sep=""));
}

#' Character-based Barplot 
#' @description
#' \code{bartext} replaces the barplot function for character environments.
#' 
#' @param x A vector containing values for the bars
#' @param num.lines The number of lines to use. Single-line plots will be rendered with different characters within the single line.
# 
#' @export
bartext <- function(x, num.lines=1, ...)
{
	if (num.lines==1)
  return(bartext.singleline(x, ...))
	else
  return(bartext.multiline(x, num.lines, ...));
}
