#' Tumor growth curves
#'
#' Plot tumor growth curves
#'
#' @param alldata list DataStructure() output
#' @return plot growth curves
#' @examples
#' @import ggplot2
#' @export

CurvePlot <- function(dataset)
{ 
  library(ggplot2)
  dataplot <- dataset$mydata[,-2]
  dataset$mydata$Groups <- as.factor(dataset$mydata$Groups)
  windows()
  p<- ggplot(data=dataplot, aes(x=Time, y=Vol, group=Groups)) +
  geom_line(aes(color=Groups))+
  geom_point(aes(color=Groups)) +
  labs(title="Tumor growth curves",x="Time", y = "Volume") 
  p + scale_colour_manual(values = dataset$PalColour) 
 # geom_tile(aes(fill=factor(value)), colour="white")+
 # scale_fill_manual(name = "Values", values=setNames(colors, 1:8))
  print(p)
  Sys.sleep(3)
  UserChoice <- SavePlot()
  if (UserChoice=="Yes")
  {
  dev.copy2pdf(device = postscript, file = "D:/ITH_pacchetto/GrowthCurve.pdf",paper="a4r",width=11)
  dev.off()
  }
   }
#  if (UserChoice=="Yes") ggsave("D:/ITH_pacchetto/GrowthCurve.pdf",paper="a4r",width=11)
  
# deve restituire plot e aggiornare la lista
