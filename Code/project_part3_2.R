library(xlsx)
library(dplyr)
library(ggplot2)
df<- read.csv('E:/ѧϰ/ѧϰ/IE 6600/project/police_2017.csv', header = TRUE)
c <- c('gun and vehicle','vehicle and gun','gun and car')
df<-df %>% mutate(Weapon = case_when( armed %in% c('gun and vehicle','vehicle and gun','gun and car')~'gun and vehicle',
                                  !(armed %in% c('gun and vehicle','vehicle and gun','vehicle and car')) ~ armed))

plot1<- ggplot(df) +
  geom_bar(aes(x = threat_level, fill = signs_of_mental_illness))

plot1

df %>% group_by(threat_level)%>%count(signs_of_mental_illness)

plot<- ggplot(df) +
  geom_bar(aes(x = threat_level, fill = Weapon))

plot

plot2<- ggplot(df) +
  geom_bar(aes(x = threat_level, fill = armed))

plot2



dfp<- df%>%group_by(signs_of_mental_illness)%>%count()%>%ungroup()%>%mutate(perc = n/sum(n))%>% arrange(perc) %>%
  mutate(labels = scales::percent(perc))



plot3<- ggplot(dfp,aes( x = "", y = perc, fill = signs_of_mental_illness)) +
  geom_col()+
  geom_text(aes(label = labels),position = position_stack(vjust = 0.5),
            show.legend = FALSE)+
  coord_polar(theta = "y")
plot3  

write.csv(df,"E:/ѧϰ/ѧϰ/IE 6600/project/newpolice.csv", row.names = FALSE)
