require("Unicode")

text.multiline.barplot.twitter<-function(x, num.lines=5)
{
  char <- "X"
  frac_seq <- (1:num_lines)/num_lines
  rel.x <- x / (max(x))
  #cat(rel.x)
  find.index <- function(x){min(which(x <= frac_seq))}
  mapping <- sapply(rel.x, FUN=find.index)
  
  strs<- t(sapply(num_lines:1, function(x) {paste(ifelse(mapping>=x, char," "),sep="",collapse = "")} ))
  cat(paste(strs,collapse="\n"))
}


text.barplot.twitter<-function(x, padding=2, ...)
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
  
text.barplot <- function(x, style="twitter", ...)
{
  return(text.barplot.twitter(x, ...));
}

x <- c(9,6,3,12,25,16)

text.barplot(x)
