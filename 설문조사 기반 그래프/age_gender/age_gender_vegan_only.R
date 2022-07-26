library(readxl)
library(dplyr)
vegan_raw <- read_excel("C:/Rstudy/mini_team1/data/only_vegan.xlsx")


#데이터 학인
str(vegan_raw$gender)
str(vegan_raw$age)

vegan_raw$gender <- ifelse(is.na(vegan_raw$gender), "skip", vegan_raw$gender)
vegan_raw$age <- ifelse(is.na(vegan_raw$age), "skip", vegan_raw$age)
# class2라는 변수에 각항목 응답수 확인


class4<- margin.table(x= table(vegan_raw$gender), margin=1)
class4

table(vegan_raw$age)


#### 연령대별이 필요하기 때문에 정확할 필요는 없음 -> 40대냐 50대냐가 중요 ####
vegan_raw$age <- gsub(" years old", "", vegan_raw$age)
vegan_raw$age <- gsub("yrs", "", vegan_raw$age)
vegan_raw$age <- gsub(" yrs old", "", vegan_raw$age)
vegan_raw$age <- gsub(" year old", "", vegan_raw$age)
vegan_raw$age <- gsub("Twenty five.", 25, vegan_raw$age)
vegan_raw$age <- gsub("Sixty", 60, vegan_raw$age)
vegan_raw$age <- gsub(" old", "", vegan_raw$age)
vegan_raw$age <- gsub("I will be 46 next month.", 46, vegan_raw$age)
vegan_raw$age <- gsub("almost 54", 54, vegan_raw$age)
vegan_raw$age <- gsub("mid to upper 30's", 35, vegan_raw$age)
vegan_raw$age <- gsub("df", "", vegan_raw$age)
vegan_raw$age <- gsub("almost 54", 54, vegan_raw$age)
vegan_raw$age <- gsub("51 almost 52", 51, vegan_raw$age)
vegan_raw$age <- gsub("45-55", "skip", vegan_raw$age)
vegan_raw$age <- gsub("44.5", 44, vegan_raw$age)
vegan_raw$age <- gsub("40s", 40, vegan_raw$age)
vegan_raw$age <- gsub("25, also female is not a gender, it's a sex", 25, vegan_raw$age)
vegan_raw$age <- gsub(" ", "", vegan_raw$age)



vegan_raw$age <- ifelse(vegan_raw$age %in% c(1:120), vegan_raw$age, "skip")

table(vegan_raw$age)

str(vegan_raw$age)
str(vegan_raw$gender)
#### 나이와 성별 추출 단, skip 제외 ####
vegan_age_gender <- vegan_raw %>% select(age, gender) %>% filter(age != "skip" & gender != "skip" & gender != "Non-gendered")
str(vegan_age_gender)



vegan_ageg_gender <- vegan_age_gender %>% mutate(ageg = ifelse(
  age < 20, "10대", ifelse(age < 30, "20대", ifelse(age < 40, "30대", ifelse(age < 50, "40대",
                                                                             ifelse(age<60, "50대",
                                                                                    ifelse(age<70, "60대",
                                                                                           ifelse(age<80, "70대"
                                                                                                  ))))))))


table(vegan_ageg_gender)


vegan_ageg_gender_g <- vegan_ageg_gender %>% group_by(ageg, gender) %>% dplyr::summarise(n6 = n())
# vegan_ageg_gender_g <- vegan_ageg_gender_g %>% add_row(ageg = "10대", gender = "Male", n6 = 0)

vegan_ageg_gender_g$n6[vegan_ageg_gender_g$gender == "Female"] = -vegan_ageg_gender_g$n6



library(ggplot2)
ggplot(vegan_ageg_gender_g, aes(x = ageg, y = n6, fill = gender)) + geom_bar(stat = "identity", width = .9) +
  coord_flip() + ylim(-500,500) +  theme_bw() + 
  geom_text(aes(label=n6), hjust=-.25, size=3.5) +
  ggtitle("연령대별 남자 여자", subtitle = "vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15)) +
  xlab('연령대') + ylab('남자 여자 수')

  


