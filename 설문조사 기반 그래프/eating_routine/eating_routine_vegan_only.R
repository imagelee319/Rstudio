library(readxl)
vegan_raw <- read_excel("C:/Rstudy/mini_team1/data/only_vegan.xlsx")
str(vegan_raw)


#### 21 23routine, routine2 원하는 행 추출 ####
library(dplyr)
change_routine1 <- vegan_raw %>% select(routine)


change_routine2 <- vegan_raw %>% select(routine2)


#### 21 23 결측치 제거 ####
table(is.na(change_routine1$routine))
table(is.na(change_routine2$routine2))

change_routine1$routine <- ifelse(is.na(change_routine1$routine), "skip", change_routine1$routine)
change_routine2$routine2 <- ifelse(is.na(change_routine2$routine2), "skip", change_routine2$routine2)

table(is.na(change_routine1$routine))
table(is.na(change_routine2$routine2))


#### 21 23 그룹화 및 빈도수 측정 ####
change_routine_g1 <- change_routine1 %>% group_by(routine) %>% summarise(n=n())

change_routine_g2 <- change_routine2 %>% group_by(routine2) %>% summarise(n2=n())


#### 21 23 rbind를 위한 컬럼명 통일 시키기 #####
change_routine_g2 <- dplyr::rename(change_routine_g2, routine = routine2)
change_routine_g2 <- dplyr::rename(change_routine_g2, n = n2)


#### 21 23 행 합치기 ####
change_routine_g <- rbind(change_routine_g1, change_routine_g2)



#### 21 23 그래프 그룹화를 위한 열 추가 ####
change_routine_g$group<-c("routine1","routine1","routine1","routine1","routine1","routine1","routine1",
                          "routine2","routine2","routine2","routine2","routine2","routine2","routine2")



change_routine_g <- change_routine_g %>% filter(routine != "skip")


change_routine_g$routine <- ifelse(change_routine_g$routine =="Did not eat red meat, chicken or fish", "X meat,chicken or fish",
                                   ifelse(change_routine_g$routine =="Did not eat red meat, but ate everything else", "X meat",
                                          ifelse(change_routine_g$routine =="Did not eat red meat or chicken, but ate fish, eggs and dairy", "X meat or chicken" , ifelse(change_routine_g$routine =="Did not eat any animal products", "X" , ifelse(change_routine_g$routine =="Ate all types of animal products but was now eating less of them", "eating less" ,ifelse(change_routine_g$routine =="Ate all types of animal products", "all types" ,
                                                                                                                                                                                                                                                                                                                                                                          change_routine_g$routine
                                          ))))))  

#### 21 23 그래프 시각화####
library(ggplot2)
b <- ggplot(change_routine_g, aes(routine, n, group = group, fill = group)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  xlab('대답') + ylab('응답자 수') +
  coord_flip() +
  geom_text(aes(label=n), position = position_dodge(width=0.8), hjust=-.25, size=3.5) +
  theme_bw() + 
  ggtitle("식습관 변화", subtitle = "vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))

ggplotly(b)
