#' @title A arrow plot using R
#' @description A arrow plot using R
#' @import grid
#' @param color the color of arrow (character)
#' @param lty the type of arrow lines (integer)
#' @return a plot of a arrow
#' @examples
#' \dontrun{
#' myarrow <- arrow("red",2)
#' myheart
#' }
#' @export
arrow<-function(color,lty){
  grid.lines(x=c(c(0,1.5)/10,(1:10)/10),y=c((5:6)/10,rep(0.5,9),0.6),gp=gpar(col=color,lty=lty,lwd=3))
  grid.lines(x=c(0.8,0.9),y=c(0.5,0.6),gp=gpar(col=color,lty=lty,lwd=3))
  grid.lines(x=c(0.85,0.95),y=c(0.5,0.6),gp=gpar(col=color,lty=lty,lwd=3))
  grid.lines(x=c(c(0,1.5)/10,(1:10)/10),y=c((5:4)/10,rep(0.5,9),0.4),gp=gpar(col=color,lty=lty,lwd=3))
  grid.lines(x=c(0.8,0.9),y=c(0.5,0.4),gp=gpar(col=color,lty=lty,lwd=3))
  grid.lines(x=c(0.85,0.95),y=c(0.5,0.4),gp=gpar(col=color,lty=lty,lwd=3))
}

