library(readxl)

library(dplyr)
vegan_raw <- read_excel("C:/Rstudy/mini_team1/data/vegan_Raw_data_c.xls")

vegan_raw1 <- vegan_raw %>% filter(!(eat_beef == "Never" & eat_chicken == "Never" & eat_dairy =="Never" & 
                                       eat_eggs =="Never" & eat_fish_seafood =="Never"& eat_pork =="Never")) 


eat_food1 <- vegan_raw1 %>% select(eat_beef ,eat_chicken, eat_dairy, eat_eggs, eat_fish_seafood, eat_pork)
non_eat_food1 <- vegan_raw1 %>% select(eat_non_dairy, eat_seitan, eat_tempeh, eat_tofu, eat_vegetarian_meats)



#### 1 결측치 확인 ####
table(is.na(eat_food1))

table(is.na(eat_food1$eat_beef))
table(is.na(eat_food1$eat_chicken))
table(is.na(eat_food1$eat_dairy))
table(is.na(eat_food1$eat_eggs))
table(is.na(eat_food1$eat_fish_seafood))
table(is.na(eat_food1$eat_pork))
head(table(eat_food1))



#### 1 결측치 대체 ####
beef2 <-ifelse(is.na(eat_food1$eat_beef), "skip", eat_food1$eat_beef)
beef3 <- table(beef2)
chicken2 <-ifelse(is.na(eat_food1$eat_chicken), "skip", eat_food1$eat_chicken)
chicken3 <- table(chicken2)
dairy2 <-ifelse(is.na(eat_food1$eat_dairy), "skip", eat_food1$eat_dairy)
dairy3 <- table(dairy2)
eggs2 <-ifelse(is.na(eat_food1$eat_eggs), "skip", eat_food1$eat_eggs)
eggs3 <- table(eggs2)
fish_seafood2 <-ifelse(is.na(eat_food1$eat_fish_seafood), "skip", eat_food1$eat_fish_seafood)
fish_seafood3 <- table(fish_seafood2)
pork2 <-ifelse(is.na(eat_food1$eat_pork), "skip", eat_food1$eat_pork)
pork3 <- table(pork2)


#### 1 컬럼 합치기 ####
s_eat_food1 <-cbind(beef3, chicken3, dairy3, eggs3, fish_seafood3, pork3)

str(s_eat_food1)
s_eat_food1 <- data.frame(s_eat_food1)


#### 1 인덱스로 들어간 항목을 꺼내서 컬럼명 넣어주기 ####
s_eat_food_g1 <- dplyr::add_rownames(s_eat_food1, var="answer")
head(s_eat_food_g1)


#### 1 skip 지우기 ####
s_eat_food_g1 <- s_eat_food_g1 %>% filter(answer != "skip")

#### 1 컬럼명 변경 ####
s_eat_food_g1 <- dplyr::rename(s_eat_food_g1, "소고기" = "beef3", 
                                   "치킨" = "chicken3", "유제품" = "dairy3", "계란" = "eggs3", 
                                   "생선 및 해산물" = "fish_seafood3", "돼지고기" = "pork3")


s_eat_food_g1$answer <- ifelse(s_eat_food_g1$answer =="Never", 0,
                        ifelse(s_eat_food_g1$answer =="About 1 day per month", 1,
                               ifelse(s_eat_food_g1$answer =="About 1 day per week", 2 ,
                                      ifelse(s_eat_food_g1$answer =="About every other day", 3, 
                                             ifelse(s_eat_food_g1$answer =="Almost every day", 4, s_eat_food_g1$answer 
                                                    )))))
                               

library(tidyverse)
library(reshape)
library(ggplot2)

s_eat_food_g1%>%
  pivot_longer(-answer) %>%
  ggplot(aes(x = answer, y = value, fill = name)) +
  geom_col(position = position_stack(), colour = "black") +
  geom_text(aes(label = value), size = 2, position = position_stack(0.5)) +
  scale_fill_brewer(name = '식품', palette = 'Paired') + 
  theme_bw() + xlab("섭취 빈도") + ylab("빈도수") + 
  ggtitle("식품별 섭취항목 빈도수", subtitle = "non-vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30, color = "darkblue"),
        plot.subtitle = element_text(size = 20))



###############################################
###############################################
#### 2 두부 등 비건 음식 2번 ####
table(is.na(non_eat_food1))


#### 2 결측치 대체 및 빈도수 ####
non_dairy2 <-ifelse(is.na(non_eat_food1$eat_non_dairy), "skip", non_eat_food1$eat_non_dairy)
non_dairy3 <- table(non_dairy2)
table(is.na(non_eat_food1$eat_non_dairy))
table(is.na(non_eat_food1$eat_seitan))

seitan2 <-ifelse(is.na(non_eat_food1$eat_seitan), "skip", non_eat_food1$eat_seitan)
seitan3 <- table(seitan2)

table(is.na(non_eat_food1$eat_tempeh))

tempeh2 <- ifelse(is.na(non_eat_food1$eat_tempeh), "skip", non_eat_food1$eat_tempeh)
tempeh3 <- table(tempeh2)


table(is.na(non_eat_food1$eat_tofu))

tofu2 <- ifelse(is.na(non_eat_food1$eat_tofu), "skip", non_eat_food1$eat_tofu)
tofu3 <- table(tofu2)

table(is.na(non_eat_food1$eat_vegetarian_meats))

vegetaian_meats2 <- ifelse(is.na(non_eat_food1$eat_vegetarian_meats), "skip", non_eat_food1$eat_vegetarian_meats)
vegetaian_meats3 <- table(vegetaian_meats2)


#### 2 합치기 및 데이터프레임화 ####
s_non_eat_food1 <-cbind(non_dairy3, seitan3, tempeh3, tofu3, vegetaian_meats3)

str(s_non_eat_food1)
s_non_eat_food_g1 <- data.frame(s_non_eat_food1)


#### 2 인덱스 빼고 이름 넣기 ####
s_non_eat_food_g1 <- dplyr::add_rownames(s_non_eat_food_g1, var="answer")
head(s_non_eat_food_g1)


#### 2 skip항목 제외 추출 ####
s_non_eat_food_g1 <- s_non_eat_food_g1 %>% filter(answer != "skip")

#### 2 rename 항목 이름 변경 ####
s_non_eat_food_g1 <- dplyr::rename(s_non_eat_food_g1, "유제품이 아닌" = "non_dairy3", 
                            "세이탄" = "seitan3", "템페" = "tempeh3", "두부" = "tofu3", 
                            "대체육" = "vegetaian_meats3")



s_non_eat_food_g1$answer <- ifelse(s_non_eat_food_g1$answer =="Never", 0,
                               ifelse(s_non_eat_food_g1$answer =="About 1 day per month", 1,
                                      ifelse(s_non_eat_food_g1$answer =="About 1 day per week", 2 ,
                                             ifelse(s_non_eat_food_g1$answer =="About every other day", 3, 
                                                    ifelse(s_non_eat_food_g1$answer =="Almost every day", 4, s_non_eat_food_g1$answer 
                                                    )))))


#### 2 그래프 시각화 ####

a <- s_non_eat_food_g1 %>%
  pivot_longer(-answer) %>%
  ggplot(aes(x = answer, y = value, fill = name)) +
  geom_col(position = position_stack(), colour = "black") + 
  geom_text(aes(label = value),size = 2, position = position_stack(0.5)) +
  scale_fill_brewer(name = '식품', palette = 'Paired') + 
  theme_bw() + xlab("섭취 빈도") + ylab("빈도수") + 
  ggtitle("식품별 섭취항목 빈도수", subtitle = "non-vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30, color = "darkblue"),
        plot.subtitle = element_text(size = 20))


