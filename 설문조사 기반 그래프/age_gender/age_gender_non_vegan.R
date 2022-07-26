library(readxl)
library(dplyr)
vegan_raw <- read_excel("C:/Rstudy/mini_team1/data/vegan_Raw_data_c.xls")

vegan_raw1 <- vegan_raw %>% filter(!(eat_beef == "Never" & eat_chicken == "Never" &eat_dairy =="Never"& 
                                       eat_eggs =="Never" & eat_fish_seafood =="Never"& eat_pork =="Never"))


#데이터 학인
str(vegan_raw1$gender)
str(vegan_raw1$age)

vegan_raw1$gender <- ifelse(is.na(vegan_raw1$gender), "skip", vegan_raw1$gender)
vegan_raw1$age <- ifelse(is.na(vegan_raw1$age), "skip", vegan_raw1$age)
# class2라는 변수에 각항목 응답수 확인

table(vegan_raw1$age)


#### 연령대별이 필요하기 때문에 정확할 필요는 없음 -> 40대냐 50대냐가 중요 ####
vegan_raw1$age <- gsub(" years old", "", vegan_raw1$age)
vegan_raw1$age <- gsub("yrs", "", vegan_raw1$age)
vegan_raw1$age <- gsub(" years", "", vegan_raw1$age)
vegan_raw1$age <- gsub("Sixty four", 64, vegan_raw1$age)
vegan_raw1$age <- gsub(" yo", "", vegan_raw1$age)
vegan_raw1$age <- gsub("sixty-four", 64, vegan_raw1$age)
vegan_raw1$age <- gsub("20 and a half", 20, vegan_raw1$age)
vegan_raw1$age <- gsub("mid-50s", 55, vegan_raw1$age)


vegan_raw1$age <- gsub(" ", "", vegan_raw1$age)



vegan_raw1$age <- ifelse(vegan_raw1$age %in% c(1:120), vegan_raw1$age, "skip")

table(vegan_raw1$age)

str(vegan_raw1$age)
str(vegan_raw1$gender)
#### 나이와 성별 추출 단, skip 제외 ####
non_vegan_age_gender <- vegan_raw1 %>% select(age, gender) %>% filter(age != "skip" & gender != "skip" & gender != "Non-gendered")
str(non_vegan_age_gender)

non_vegan_ageg_gender <- non_vegan_age_gender %>% mutate(ageg = ifelse(
  age < 20, "10대", ifelse(age < 30, "20대", ifelse(age < 40, "30대", ifelse(age < 50, "40대",
                                                                          ifelse(age<60, "50대",
                                                                                 ifelse(age<70, "60대",
                                                                                        ifelse(age<80, "70대"
                                                                                        ))))))))


table(non_vegan_ageg_gender)


non_vegan_ageg_gender_g <- non_vegan_ageg_gender %>% group_by(ageg, gender) %>% dplyr::summarise(n6 =n())
# vegan_ageg_gender_g <- vegan_ageg_gender_g %>% add_row(ageg = "10대", gender = "Male", n6 = 0)

non_vegan_ageg_gender_g$n6[non_vegan_ageg_gender_g$gender == "Female"] = -non_vegan_ageg_gender_g$n6



#install.packages("plotly")

library(ggplot2)
ggplot(non_vegan_ageg_gender_g, aes(x = ageg, y = n6, fill = gender)) + geom_bar(stat = "identity", width = .9) +
  coord_flip() + scale_y_continuous(labels = abs) + ylim(-500,500) +
  theme_bw() + 
  geom_text(aes(label=n6), hjust=-.25, size=3.5) +
  ggtitle("연령대별 남자 여자", subtitle = "non-vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15)) + xlab('연령대') + ylab('남자 여자 수')

