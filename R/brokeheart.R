#' @title A broken heart plot using R
#' @description A  broken heart plot using R
#' @import ggplot2
#' @import stats
#' @param color the color of heart (character)
#' @param words the greeting (character)
#' @return a plot of a broken heart
#' @examples
#' \dontrun{
#' myheart <- brokenheart("red","I am an orphan")
#' myheart
#' }
#' @export
brokenheart<-function(color,words){
  t=seq(0, 2*pi, by=0.1)
  x=16*sin(t)^3
  y=13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
  a=(x-min(x))/(max(x)-min(x))
  b=(y-min(y))/(max(y)-min(y))
  a1<-c(0.5,0.55,0.51,0.62,a[2:60],0.40,0.52,0.44,0.52,0.5)
  b1<-c(0.2,0.3,0.45,0.63,b[2:60],0.74,0.63,0.45,0.3,0.2)
  ggplot(data=NULL, aes(x=a1, y=b1)) +
    geom_line(aes(color=I('white'))) +
    geom_polygon(aes(fill=color,alpha=0.5), show.legend = F) +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) +
    annotate('text', x=median(a), y=median(b),
             label=words,
             size=10)
}

