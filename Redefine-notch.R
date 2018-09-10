
require(magrittr)
require(dplyr)
require(ggplot)

df_tidy_example <- read.csv("Data_tidy_example.csv", na.strings = "")


df_tidy_selected <- df_tidy_example %>% select(Condition = conditions , Value = Activity) %>% filter(!is.na(Value))

df_summary <- df_tidy_selected %>%
     group_by(Condition) %>% 
     summarise(n = n(),
               mean = mean(Value),
               SD = sd(Value),
               median = median(Value),
               MAD = mad(Value, constant=1),
               IQR = IQR(Value))




p <- ggplot(data=df_tidy_selected, aes(x= Condition, y= Value))
p <-  p + geom_boxplot(notch=TRUE)
p <- p+geom_point(data=df_tidy_selected)

gg <-  ggplot_build(p)


#This gives a list of the values of the lower notches
gg$data[[1]]$notchlower

#Change the lower notch to 1st quartile
gg$data[[1]]$notchlower <- tapply(df_tidy_selected$Value, df_tidy_selected$Condition, quantile, probs=0.25)
gg$data[[1]]$notchupper <- tapply(df_tidy_selected$Value, df_tidy_selected$Condition, quantile, probs=0.75)


#Plot
p <- plot(ggplot_gtable(gg))




p
