#' @title The overall composition using R
#' @description The overall composition using R
#' @import grid
#' @import ggplot2
#' @param style the style of plot (integer)
#' @param words the greeting (character)
#' @param lty the type of line (integer)
#' @return a plot of some hearts
#' @examples
#' \dontrun{
#' myplot <- design(1,"Merry Christmas !",2)
#' myplot
#' }
#' @export
design<-function(style,words,lty){
  if(style==1){
    grid.newpage()
    grid.text(words,x=.5,y =.15,gp = gpar(fontsize=20,col="grey"))
    vp1 <- viewport(.4, .5, width=.5, height=.5,angle=15)
    pushViewport(vp1)
    heart("red",lty)
    vp2 <- viewport(0.9, .27, width=.7, height=.7,angle=-30)
    pushViewport(vp2)
    heart("hotpink",lty)
  }
  else if(style==2){
    grid.newpage()
    grid.text(words,x=.5,y =.15,gp = gpar(fontsize=20,col="grey"))
    vp1 <- viewport(.5, .5, width=1, height=1,angle=20)
    pushViewport(vp1)
    arrow("tomato3",lty)
    upViewport()
    vp2 <- viewport(.45, .5, width=.5, height=.5,angle=15)
    pushViewport(vp2)
    heart("tomato3",lty)
    vp3 <- viewport(.65, .5, width=1, height=1,angle=0)
    pushViewport(vp3)
    heart("tomato3",lty)
  }
  else if(style==3){
    grid.newpage()
    grid.text(words,x=.5,y =.15,gp = gpar(fontsize=20,col="grey"))
    for(j in 1:30){
      vp<-viewport(0.5,0.5,width=0.9,height=0.9,angle=15)
      pushViewport(vp)
      heart(sample(seq(1:657),1),lty)
    }
  }
  else{
    brokenheart("red",words)
  }
}
