#' Exploratory analysis plots for binary response variable.
#'
#' Create plots for exploratory analysis for binary response vs predictor. Also creates frequency distribution of predictor.
#' @param data_table A data frame containing target_var and cat_var.
#' @param target_var Name of the target variable (binary variable) field in the dataframe data_table
#' @param cat_var Name of the predictor variable field in the dataframe data_table. If the cat_var is numeric and
#' has more than 10 levels it will be broken down into 10 buckets.
#' @param y_value It is the value of target_var (binary response variable) that we are interested in.
#' @return 2x1 grid plot. Top plot is propensity of target var == y_value against cat_var.
#' The bottom plot is frequency distribution of cat_var in the dataset.
#' @export




# devtools::use_package('ggplot2')
# devtools::use_package('gridExtra')
# library(ggmap)
# library(maps)
# library(mapdata)
# library(plyr)
# library(MASS)
# library(devtools)
# library(broom)
# library(dplyr)
# library(doBy)


propensity.univariate<-function(data_table,target_var, cat_var,y_value)
{
  if(is.numeric(data_table[,cat_var]) & (length(unique(data_table[,cat_var])))>10)
  {
    define_cuts <- seq(floor(min(data_table[,cat_var])), ceiling(max(data_table[,cat_var])), length.out = 10)
    # define_cuts <- seq(floor(min(data_table[,cat_var])), 14, length.out = 14)
    data_table[,cat_var]<-cut(data_table[,cat_var],breaks = define_cuts,include.lowest=TRUE)
  }


  data_count<-table(data_table[,cat_var],data_table[,target_var])

  data_prop <- prop.table(data_count,1)

  data_count <- as.data.frame(data_count)
  data_prop <- as.data.frame(data_prop)

  colnames(data_count)<-c(cat_var,target_var,"Count")
  colnames(data_prop)<-c(cat_var,target_var,"Prop")

  data_count<-data_count[data_count[,target_var]==y_value,]
  data_prop<-data_prop[data_prop[,target_var]==y_value,]

  assign("yvar1","Count")
  assign("yvar2","Prop")

  # pdf(paste("plots/bivariate_",cat_var,"plots.pdf"), width=6, height=9)


  # plot1<-ggplot(data=data_count, aes_string(x=cat_var, y=yvar1, group=1)) +
  #   geom_line(color="red")+
  #   geom_point()+xlab(cat_var)+ylab(paste("Count of ",target_var,"=",y_value))
  ggplot2::theme_set(ggplot2::theme_gray(base_size = 18))
  plot1<-ggplot2::ggplot(data=data_prop, ggplot2::aes_string(x=cat_var, y=yvar2, group=1)) +
    ggplot2::geom_line(color="red")+
    ggplot2::geom_point()+ggplot2::xlab(cat_var)+ggplot2::ylab(paste("Proportion"))+
    # +ylab(paste("Prop of ",target_var,"=",y_value))
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))



  x<-data_table[,cat_var]
  xtext<-as.data.frame(table(x))

  # bp<-barplot((table(x)),
  #                ylab = "Count of cases",
  #                xlab = cat_var,
  #                main = paste("Barplot of ",cat_var ,"(count of cases)"),
  #                border = "yellow")
  # text(bp, xtext$Freq, labels = xtext$Freq, pos = 1)


  plot2<-ggplot2::ggplot(data=xtext, ggplot2::aes(x=xtext$x, y=xtext$Freq)) +
    ggplot2::geom_bar(stat="identity", color="black", fill="yellow")+
    ggplot2::xlab(cat_var)+ggplot2::ylab("Count of cases")+ggplot2::geom_text(ggplot2::aes(label=xtext$Freq), vjust=-0.5,
                                                   color="black", size=3.5)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  gridExtra::grid.arrange(plot1, plot2, nrow=2)
 # dev.off()

}



