library(readxl)
vegan_raw <- read_excel("C:/Rstudy/mini_team1/data/vegan_Raw_data_c.xls")

vegan_raw1 <- vegan_raw %>% filter(!(eat_beef == "Never" & eat_chicken == "Never" &eat_dairy =="Never"& 
                                       eat_eggs =="Never" & eat_fish_seafood =="Never"& eat_pork =="Never")) 
  





#### routine, routine2 원하는 행 추출####
library(dplyr)
change_routine3 <- vegan_raw1 %>% select(routine)


change_routine4 <- vegan_raw1 %>% select(routine2)

#### 결측치 대체 skip으로 ####

change_routine3$routine <- ifelse(is.na(change_routine3$routine), "skip", change_routine3$routine)
change_routine4$routine2 <- ifelse(is.na(change_routine4$routine2), "skip", change_routine4$routine2)

table(is.na(change_routine3$routine))
table(is.na(change_routine4$routine2))


####그룹화 및 빈도수 측정####
change_routine_g3 <- change_routine3 %>% filter(routine != "skip") %>% group_by(routine) %>% summarise(n3=n())

change_routine_g4 <- change_routine4 %>% filter(routine2 != "skip") %>% group_by(routine2) %>% summarise(n4=n())



#### rbind를 위한 컬럼명 통일 시키기 #####
change_routine_g4 <- dplyr::rename(change_routine_g4, routine = routine2)
change_routine_g4 <- dplyr::rename(change_routine_g4, n3 = n4)


#### rbind로 합치기 ####
change_routine_g0 <- rbind(change_routine_g3, change_routine_g4)
table(change_routine_g4)
table(change_routine_g3)


#### 그래프 그룹화를 위한 열 추가 ####
change_routine_g0$group<-c("routine1","routine1","routine1","routine1","routine1","routine1",
                           "routine2","routine2","routine2","routine2","routine2", "routine2")


change_routine_g0$routine <- ifelse(change_routine_g0$routine =="Did not eat red meat, chicken or fish", "X meat,chicken or fish",
                                    ifelse(change_routine_g0$routine =="Did not eat red meat, but ate everything else", "X meat",
                                           ifelse(change_routine_g0$routine =="Did not eat red meat or chicken, but ate fish, eggs and dairy", "X meat or chicken" , ifelse(change_routine_g0$routine =="Did not eat any animal products", "X" , ifelse(change_routine_g0$routine =="Ate all types of animal products but was now eating less of them", "eating less" ,ifelse(change_routine_g0$routine =="Ate all types of animal products", "all types" ,
                                                                                                                                                                                                                                                                                                                                                                              change_routine_g0$routine
                                           ))))))  
#### 그래프 시각화 ####
library(ggplot2)
ggplot(change_routine_g0, aes(routine, n3, group = group, fill = group)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  xlab('대답') + ylab('응답자 수') +
  coord_flip() +
  geom_text(aes(label=n3), position = position_dodge(width=0.8), hjust=-.25, size=3.5) +
  theme_bw() + 
  ggtitle("식습관 변화", subtitle = "non-vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))
