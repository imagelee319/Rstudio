library(readxl)
library(dplyr)
vegan_raw <- read_excel("C:/Rstudy/mini_team1/data/only_vegan.xlsx")
inspire_reducing_consumption <- vegan_raw %>% select(inspire_ani,inspire_env, inspire_ff, inspire_ht, inspire_hr,
                                                     inspire_nt, inspire_rg, inspire_sb, inspire_wh)

#### 10 결측치 대체 및 빈도수 ####
table(is.na(inspire_reducing_consumption))

inspire_ani1 <- ifelse(is.na(inspire_reducing_consumption$inspire_ani), "skip", inspire_reducing_consumption$inspire_ani)
inspire_ani <- table(inspire_ani1)


table(is.na(inspire_reducing_consumption$inspire_env))
inspire_env1 <- ifelse(is.na(inspire_reducing_consumption$inspire_env), "skip", inspire_reducing_consumption$inspire_env)
inspire_env <- table(inspire_env1)


table(is.na(inspire_reducing_consumption$inspire_ff))
inspire_ff1 <- ifelse(is.na(inspire_reducing_consumption$inspire_ff), "skip", inspire_reducing_consumption$inspire_ff)
inspire_ff <- table(inspire_ff1)


table(is.na(inspire_reducing_consumption$inspire_ht))
inspire_ht1 <- ifelse(is.na(inspire_reducing_consumption$inspire_ht), "skip", inspire_reducing_consumption$inspire_ht)
inspire_ht <- table(inspire_ht1)



table(is.na(inspire_reducing_consumption$inspire_hr))
inspire_hr1 <- ifelse(is.na(inspire_reducing_consumption$inspire_hr), "skip", inspire_reducing_consumption$inspire_hr)
inspire_hr <- table(inspire_hr1)


table(is.na(inspire_reducing_consumption$inspire_nt))
inspire_nt1 <- ifelse(is.na(inspire_reducing_consumption$inspire_nt), "skip", inspire_reducing_consumption$inspire_nt)
inspire_nt <- table(inspire_nt1)



table(is.na(inspire_reducing_consumption$inspire_rg))
inspire_rg1 <- ifelse(is.na(inspire_reducing_consumption$inspire_rg), "skip", inspire_reducing_consumption$inspire_rg)
inspire_rg <- table(inspire_rg1)


table(is.na(inspire_reducing_consumption$inspire_sb))
inspire_sb1 <- ifelse(is.na(inspire_reducing_consumption$inspire_sb), "skip", inspire_reducing_consumption$inspire_sb)
inspire_sb <- table(inspire_sb1)



table(is.na(inspire_reducing_consumption$inspire_wh))
inspire_wh1 <- ifelse(is.na(inspire_reducing_consumption$inspire_wh), "skip", inspire_reducing_consumption$inspire_wh)
inspire_wh <- table(inspire_wh1)





#### 10 cbind로 합치기 및 데이터프레임화 ####
inspire <-cbind(inspire_ani, inspire_env, inspire_ff, inspire_hr, inspire_ht, inspire_nt, inspire_rg, inspire_sb, inspire_wh)

inspire<- data.frame(inspire)



#### 10 인덱스 및 컬럼명 변경 ####
inspire_g <- dplyr::add_rownames(inspire, var="answer")
inspire_g <- dplyr::rename(inspire_g, "Animal welfare" = inspire_ani,
                           "Environment" = inspire_env, 
                           "Family Friend" = inspire_ff,
                           "Health" = inspire_ht,
                           "Human rights" = inspire_hr,
                           "Nutrition" = inspire_nt,
                           "Religion" = inspire_rg,
                           "Someone" = inspire_sb,
                           "World Hunger" = inspire_wh)

inspire_g$answer <- ifelse(inspire_g$answer =="Not at all", 0,
                           ifelse(inspire_g$answer =="To little extent", 1,
                                  ifelse(inspire_g$answer =="To some extent", 2 ,
                                         ifelse(inspire_g$answer =="To a moderate extent", 3, 
                                                ifelse(inspire_g$answer =="To a large extent", 4,inspire_g$answer)))))

#### 10 skip항목 제외 ####
inspire_g <- inspire_g %>% filter(answer != "skip")



#### 10 그래프 시각화 ####
library(ggplot2)
library(tidyr)
inspire_g %>%
  pivot_longer(-answer) %>%
  ggplot(aes(x = reorder(answer,-value), y = value, fill = name)) +
  geom_col(position = position_stack(), colour = "black") + 
  geom_text(aes(label = value), size = 2, position = position_stack(0.5))+
  scale_fill_brewer(name = '이유', palette = 'Paired') + 
  theme_bw() + xlab("실천정도") + ylab("영향도") + 
  ggtitle("동물제품 소비 줄이게 된 이유", subtitle = "vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))






#### 한번에 결측치 제거하고 데이터 프레임 만들면 컬럼명 이상하게 들어가,, ####
# inspire_reducing_consumption_s <- ifelse(is.na(inspire_reducing_consumption), "skip", inspire_reducing_consumption)
# head(str(inspire_reducing_consumption_s))
# inspire_reducing_consumption_s <- data.frame(inspire_reducing_consumption_s)
# head(inspire_reducing_consumption_s)


