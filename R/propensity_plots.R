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
  plot1<-ggplot2::ggplot(data=data_prop, aes_string(x=cat_var, y=yvar2, group=1)) +
    ggplot2::geom_line(color="red")+
    ggplot2::geom_point()+ggplot2::xlab(cat_var)+ggplot2::ylab(paste("Proportion"))+
    # +ylab(paste("Prop of ",target_var,"=",y_value))
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))



  x<-data_table[,cat_var]
  xtext<-as.data.frame(table(x))

  # bp<-barplot((table(x)),
  #                ylab = "Count of cases",
  #                xlab = cat_var,
  #                main = paste("Barplot of ",cat_var ,"(count of cases)"),
  #                border = "yellow")
  # text(bp, xtext$Freq, labels = xtext$Freq, pos = 1)


  plot2<-ggplot2::ggplot(data=xtext, aes(x=xtext$x, y=xtext$Freq)) +
    ggplot2::geom_bar(stat="identity", color="black", fill="yellow")+
    ggplot2::xlab(cat_var)+ggplot2::ylab("Count of cases")+ggplot2::geom_text(aes(label=xtext$Freq), vjust=-0.5,
                                                   color="black", size=3.5)+
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))
  gridExtra::grid.arrange(plot1, plot2, nrow=2)
 # dev.off()

}


# binaryResponse_exploratory_three_var_plots<-function(data_table,target_var, cat_var1,cat_var2,y_value)
# {
# #cat_var2 is clustering variable
#   if(is.numeric(data_table[,cat_var1]) && (length(unique(data_table[,cat_var1])>7)))
#   {
#     data_table[,cat_var1]<-cut(data_table[,cat_var1],breaks = 6)
#   }
#
#   if(is.numeric(data_table[,cat_var2]) && (length(unique(data_table[,cat_var2])>7)))
#   {
#     data_table[,cat_var2]<-cut(data_table[,cat_var2],breaks = 6)
#   }
#
#
#   data_count<-table(data_table[,cat_var2],data_table[,cat_var1],data_table[,target_var])
#
#   data_prop <- prop.table(data_count,c(2,1))
#
#   data_count <- as.data.frame(data_count)
#   data_prop <- as.data.frame(data_prop)
#
#   colnames(data_count)<-c(cat_var2,cat_var1,target_var,"Count")
#   colnames(data_prop)<-c(cat_var2,cat_var1,target_var,"Prop")
#
#   data_count<-data_count[data_count[,target_var]==y_value,]
#   data_prop<-data_prop[data_prop[,target_var]==y_value,]
#
#   assign("yvar1","Count")
#   assign("yvar2","Prop")
#   assign("legend_var2",cat_var2)
#
#   pdf(paste("plots/multivariate_",cat_var1,cat_var2,"plots.pdf"), width=10, height=9)
#
#   plot1<-ggplot(data=data_count, aes_string(x=cat_var1, y=yvar1, group=cat_var2,color = (legend_var2))) +
#   geom_line()+
#   geom_point()+xlab(cat_var1)+ylab(paste("Count of ",target_var,"=",y_value))
#
#   plot2<-ggplot(data=data_prop, aes_string(x=cat_var1, y=yvar2, group=cat_var2,color =(legend_var2))) +
#   geom_line()+
#   geom_point()+xlab(cat_var1)+ylab(paste("Proportion of ",target_var,"=",y_value))
#
#   grid.arrange(plot1, plot2, nrow=2)
#
#   dev.off()
# }
#
#
#
# binaryResponse_exploratory_continuous_var_plots<-function(data_table,target_var, cat_var,y_value)
# {
#   #analyze continuous and target variable. This function will draw histogram polygon of continuous variable
#   #for all values of target variable.
#
#
#   assign("yvar",target_var)
#   pdf(paste("plots/numerical_",cat_var,"plots.pdf"), width=6, height=9)
#   plot1<-ggplot(data = data_table, mapping = aes_string(x = cat_var, fill = yvar)) +
#     geom_density(alpha=.25)
#   plot2<-ggplot(data = data_table, mapping = aes_string(x = cat_var)) +
#     geom_histogram(alpha=.25)
#   grid.arrange(plot1, plot2, nrow=2)
#   dev.off()
#
#
# }
#
# binaryResponse_map<-function(data_table,target_var, cat_var,y_value)
# {
#   #this function will plot heat map of count of a value of target variable of US map
#
#   states <- map_data("state")
#
#
#
#   state_name<-(state.name[match(data_table[,cat_var],state.abb)])
#   data_count<-table(state_name,data_table[,target_var])
#
#   data_prop <- prop.table(data_count,1)
#
#   data_count <- as.data.frame(data_count)
#   data_prop <- as.data.frame(data_prop)
#   colnames(data_count)<-c("region",target_var,"Count")
#   colnames(data_prop)<-c("region",target_var,"Prop")
#   data_count$region<-tolower(as.character(data_count$region))
#   data_prop$region<-tolower(as.character(data_prop$region))
#   data_count<-data_count[data_count[,target_var]==y_value,c("region","Count")]
#   data_prop<-data_prop[data_prop[,target_var]==y_value,c("region","Prop")]
#
#   # map_count<-merge(states,data_count,by="region",all.x  = T)
#   map_count<-states
#   map_count$Count<-0
#   map_count$Prop<-0
#   for(k in 1:length(map_count$long))
#   {
#     if(sum(data_count$region==map_count$region[k])!=0)
#     {
#       map_count$Count[k] = data_count$Count[data_count$region==map_count$region[k]]
#       map_count$Prop[k] = data_prop$Prop[data_prop$region==map_count$region[k]]
#
#     }
#     else
#     {
#       map_count$Count[k]=NA
#       map_count$Prop[k]=NA
#     }
#
#   }
#
#
#   txtval<- summaryBy(long+lat+group~region,data = map_count,FUN = mean,keep.names = T)
#   txtval$region<-state.abb[match(txtval$region,tolower(state.name))]
#   txtval$region[8]="DC"
#
#   pdf(paste("plots/map_",cat_var,"plots.pdf"), width=6, height=9)
#   map1 = ggplot(data = map_count,aes(x=long, y = lat, group = group)) + geom_polygon(data = map_count,aes(fill=Count)) +
#     coord_fixed(1.3)+geom_path(data=states,colour="black")+ theme_bw() +
#     scale_fill_gradient(low = "#ece2f0", high = "#1c9099")+
#     labs(title = paste("Count of ",target_var,"=",y_value,"based on ",cat_var))
#     # geom_text(aes(x=long,y = lat,label=region),data = txtval, col="black", cex=3)
#
#   map2 = ggplot(data = map_count,aes(x=long, y = lat, group = group)) + geom_polygon(data = map_count,aes(fill=Prop)) +
#     coord_fixed(1.3)+geom_path(data=states,colour="black")+ theme_bw() +
#     scale_fill_gradient(low = "#ece2f0", high = "#1c9099")+
#     labs(title = paste("Proportion of ",target_var,"=",y_value,"based on ",cat_var))
#     # geom_text(aes(x=long,y = lat,label=region),data = txtval, col="black", cex=3)
#   grid.arrange(map1, map2, nrow=2)
#   dev.off()
#
# }

# binaryResponse_exploratory_two_var_plots(propensity_data_rm_dup,"LITIGATION_IND","COUNT_CLAIMANT_ORG","Y")

# binaryResponse_exploratory_two_var_plots(train_data_1yr,"LITIGATION_IND","prior_damage_ind","Y")
# binaryResponse_map(data_raw_propensity,"LITIGATION_IND","POLICY_STATE_ABBREV","Y")

# binaryResponse_exploratory_two_var_plots(data_raw_propensity,"LITIGATION_IND","LOSS_YEAR","Y")

# binaryResponse_exploratory_three_var_plots(data_raw_propensity,"LITIGATION_IND","Reported_year","FILE_FORMAT_CD","Y")

# binaryResponse_exploratory_continuous_var_plots(data_raw_propensity,"LITIGATION_IND","LOSS_YEAR","Y")
