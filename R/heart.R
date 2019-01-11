#' @title A heart plot using R
#' @description A heart plot using R
#' @import grid
#' @param color the color of heart (character)
#' @param lty the type of heart lines (integer)
#' @return a plot of a heart
#' @examples
#' \dontrun{
#' myheart <- heart("red",2)
#' myheart
#' }
#' @export
heart<-function(color,lty){
  t=seq(0, 2*pi, by=0.1)
  x=16*sin(t)^3
  y=13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
  a=(x-min(x))/(max(x)-min(x))
  b=(y-min(y))/(max(y)-min(y))
  grid.lines(a,b,gp=gpar(col=color,lty = lty,lwd = 3))
}

