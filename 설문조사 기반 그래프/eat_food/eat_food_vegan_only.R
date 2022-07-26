library(readxl)
vegan_raw2 <- read_excel("C:/Rstudy/mini_team1/data/only_vegan.xlsx")
#install.packages("ggpubr")
#install.packages("gridExtra")
library(dplyr)

#### 2 비건인 사람들 중 섭취 비건 음식 택 ####
non_eat_food <- vegan_raw2 %>% select(eat_non_dairy, eat_seitan, eat_tempeh, eat_tofu, eat_vegetarian_meats)


#### 2 결측치 대체 및 빈도수 ####
table(is.na(non_eat_food))

non_dairy1 <-ifelse(is.na(non_eat_food$eat_non_dairy), "skip", non_eat_food$eat_non_dairy)
non_dairy <- table(non_dairy1)

table(is.na(non_eat_food$eat_non_dairy))

table(is.na(non_eat_food$eat_seitan))


seitan1 <-ifelse(is.na(non_eat_food$eat_seitan), "skip", non_eat_food$eat_seitan)
seitan <- table(seitan1)

table(is.na(non_eat_food$eat_tempeh))

tempeh1 <- ifelse(is.na(non_eat_food$eat_tempeh), "skip", non_eat_food$eat_tempeh)
tempeh <- table(tempeh1)


table(is.na(non_eat_food$eat_tofu))

tofu1 <- ifelse(is.na(non_eat_food$eat_tofu), "skip", non_eat_food$eat_tofu)
tofu <- table(tofu1)

table(is.na(non_eat_food$eat_vegetarian_meats))

vegetaian_meats1 <- ifelse(is.na(non_eat_food$eat_vegetarian_meats), "skip", non_eat_food$eat_vegetarian_meats)
vegetaian_meats <- table(vegetaian_meats1)


#### 2 빈도수 합치기 및 데이터프레임화 ###$
s_non_eat_food <-cbind(non_dairy, seitan, tempeh, tofu, vegetaian_meats)

str(s_non_eat_food)
s_non_eat_food_g <- data.frame(s_non_eat_food)


#### 2 인덱스 추출 및 이름 변경 #####
s_non_eat_food_g <- dplyr::add_rownames(s_non_eat_food_g, var="answer")
head(s_non_eat_food_g)

s_non_eat_food_g$answer <- ifelse(s_non_eat_food_g$answer =="Never", 0,
                                  ifelse(s_non_eat_food_g$answer =="About 1 day per month", 1,
                                         ifelse(s_non_eat_food_g$answer =="About 1 day per week", 2 ,
                                                ifelse(s_non_eat_food_g$answer =="About every other day", 3, 
                                                       ifelse(s_non_eat_food_g$answer =="Almost every day", 4,s_non_eat_food_g$answer 
                                                       )))))

library(tidyverse)

s_non_eat_food_g %>%
  pivot_longer(-answer) %>%
  ggplot(aes(x = answer, y = value, fill = name)) +
  geom_col(position = position_stack(), colour = "black") + 
  geom_text(aes(label = value), size = 2, position = position_stack(0.5))+
  scale_fill_brewer(name = '식품', palette = 'Paired') + 
  theme_bw() + xlab("섭취 빈도") + ylab("빈도수") + 
  ggtitle("식품별 섭취항목 빈도수", subtitle = "vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30, color = "darkblue"),
        plot.subtitle = element_text(size = 20))



#position = position_dodge(width=0.8),
#position = "dodge"


########## only vegan이기 때문에 무의미 ######
#eat_food <- vegan_raw %>% select(eat_beef ,eat_chicken, eat_dairy, eat_eggs, eat_fish_seafood, eat_pork)
# #### 결측치 확인 ####
# table(is.na(eat_food))
# 
# table(is.na(eat_food$eat_beef))
# table(is.na(eat_food$eat_chicken))
# table(is.na(eat_food$eat_dairy))
# table(is.na(eat_food$eat_eggs))
# table(is.na(eat_food$eat_fish_seafood))
# table(is.na(eat_food$eat_pork))
# head(table(eat_food))
# 

# 
# #### 결측치 대체 ####
# beef1 <-ifelse(is.na(eat_food$eat_beef), "skip", eat_food$eat_beef)
# 
# str(beef)
# beef <- table(beef1)
# chicken1 <-ifelse(is.na(eat_food$eat_chicken), "skip", eat_food$eat_chicken)
# chicken <- table(chicken1)
# dairy1 <-ifelse(is.na(eat_food$eat_dairy), "skip", eat_food$eat_dairy)
# dairy <- table(dairy1)
# eggs1 <-ifelse(is.na(eat_food$eat_eggs), "skip", eat_food$eat_eggs)
# eggs <- table(eggs1)
# fish_seafood1 <-ifelse(is.na(eat_food$eat_fish_seafood), "skip", eat_food$eat_fish_seafood)
# fish_seafood <- table(fish_seafood1)
# pork1 <-ifelse(is.na(eat_food$eat_pork), "skip", eat_food$eat_pork)
# pork <- table(pork1)
# 
# head(beef1)
# 
# 
# 
# #### 컬럼 합치기 ####
# s_eat_food <-cbind(beef, chicken, dairy, eggs, fish_seafood, pork)
# 
# str(s_eat_food)
# s_eat_food <- data.frame(s_eat_food)
# 
# 
# #### 인덱스로 들어간 항목을 꺼내서 컬럼명 넣어주기 ####
# s_eat_food_g <- dplyr::add_rownames(s_eat_food, var="answer")
# head(s_eat_food_g)
# 
# 
# 
# #### 그래프 그리기 ####
# library(tidyverse)
# library(reshape)
# library(ggplot2)
# 
# s_eat_food_g %>%
#   pivot_longer(-answer) %>%
#   ggplot(aes(x = answer, y = value, fill = name)) +
#   geom_col(position = "dodge") + 
#   geom_text(aes(label=value), position = position_dodge(width=0.8), hjust=-.25, size=3.5) + coord_flip()

