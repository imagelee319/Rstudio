library(readxl)
vegan_raw <- read_excel("C:/Rstudy/mini_team1/data/vegan_Raw_data_c.xls")

vegan_raw1 <- vegan_raw %>% filter(!(eat_beef == "Never" & eat_chicken == "Never" &eat_dairy =="Never"& 
                                       eat_eggs =="Never" & eat_fish_seafood =="Never"& eat_pork =="Never")) 


library(dplyr)
#### 동물 제품 줄이기 영감 주는 이유 컬럼 추출 ####
non_inspire_reducing_consumption <- vegan_raw1 %>% select(inspire_ani,inspire_env, inspire_ff, inspire_ht, inspire_hr,
                                                     inspire_nt, inspire_rg, inspire_sb, inspire_wh)

#### 결측치 확인 ####
table(is.na(non_inspire_reducing_consumption))





###############################################################################
##########################################################################
################### 결측치 대체 및 빈도수 구하기 ####################
non_inspire_ani1 <- ifelse(is.na(non_inspire_reducing_consumption$inspire_ani), "skip", non_inspire_reducing_consumption$inspire_ani)
#### 동물복지 각 항목 빈도수 ####
non_inspire_ani <- table(non_inspire_ani1)


table(is.na(non_inspire_reducing_consumption$inspire_env))
non_inspire_env1 <- ifelse(is.na(non_inspire_reducing_consumption$inspire_env), "skip", non_inspire_reducing_consumption$inspire_env)
#### 환경 각 항목 빈도수 ####
non_inspire_env <- table(non_inspire_env1)


table(is.na(non_inspire_reducing_consumption$inspire_ff))
non_inspire_ff1 <- ifelse(is.na(non_inspire_reducing_consumption$inspire_ff), "skip", non_inspire_reducing_consumption$inspire_ff)
#### 가족영향 각 항목 빈도수 ####
non_inspire_ff <- table(non_inspire_ff1)


table(is.na(non_inspire_reducing_consumption$inspire_ht))
non_inspire_ht1 <- ifelse(is.na(non_inspire_reducing_consumption$inspire_ht), "skip", non_inspire_reducing_consumption$inspire_ht)
#### 건강 각 항목 빈도수 ####
non_inspire_ht <- table(non_inspire_ht1)



table(is.na(non_inspire_reducing_consumption$inspire_hr))
non_inspire_hr1 <- ifelse(is.na(non_inspire_reducing_consumption$inspire_hr), "skip", non_inspire_reducing_consumption$inspire_hr)
#### 인권 각 항목 빈도수 ####
non_inspire_hr <- table(non_inspire_hr1)


table(is.na(non_inspire_reducing_consumption$inspire_nt))
non_inspire_nt1 <- ifelse(is.na(non_inspire_reducing_consumption$inspire_nt), "skip", non_inspire_reducing_consumption$inspire_nt)
#### 영양 각 항목 빈도수 ####
non_inspire_nt <- table(non_inspire_nt1)



table(is.na(non_inspire_reducing_consumption$inspire_rg))
non_inspire_rg1 <- ifelse(is.na(non_inspire_reducing_consumption$inspire_rg), "skip", non_inspire_reducing_consumption$inspire_rg)
#### 종교 각 항목 빈도수 ####
non_inspire_rg <- table(non_inspire_rg1)


table(is.na(non_inspire_reducing_consumption$inspire_sb))
non_inspire_sb1 <- ifelse(is.na(non_inspire_reducing_consumption$inspire_sb), "skip", non_inspire_reducing_consumption$inspire_sb)
#### 누군가에의해서 각 항목 빈도수 ####
non_inspire_sb <- table(non_inspire_sb1)



table(is.na(non_inspire_reducing_consumption$inspire_wh))
non_inspire_wh1 <- ifelse(is.na(non_inspire_reducing_consumption$inspire_wh), "skip", non_inspire_reducing_consumption$inspire_wh)
#### 기아 각 항목 빈도수 ####
non_inspire_wh <- table(non_inspire_wh1)




#### 각 컬럼별 항목 빈도수 추출한것들 cbind로 묶어주기고 데이터프레임화 시키기 ####
non_inspire <-cbind(non_inspire_ani, non_inspire_env, non_inspire_ff, non_inspire_hr, non_inspire_ht, non_inspire_nt, non_inspire_rg, non_inspire_sb, non_inspire_wh)

non_inspire<- data.frame(non_inspire)


#### 인덱스로 들어간 항목을 answer이라는 컬럼명으로 빼주기 ####
non_inspire_g <- dplyr::add_rownames(non_inspire, var="answer")



#### 각 컬럼명 실제 항목으로 대체하기 ####
non_inspire_g <- dplyr::rename(non_inspire_g, "Animal welfare" = non_inspire_ani,
                               "Environment" = non_inspire_env, 
                               "Family Friend" = non_inspire_ff,
                               "Health" = non_inspire_ht,
                               "Human rights" = non_inspire_hr,
                               "Nutrition" = non_inspire_nt,
                               "Religion" = non_inspire_rg,
                               "Someone" = non_inspire_sb,
                               "World Hunger" = non_inspire_wh)

non_inspire_g$answer <- ifelse(non_inspire_g$answer =="Not at all", 0,
                               ifelse(non_inspire_g$answer =="To little extent", 1,
                                      ifelse(non_inspire_g$answer =="To some extent", 2 ,
                                             ifelse(non_inspire_g$answer =="To a moderate extent", 3, 
                                                    ifelse(non_inspire_g$answer =="To a large extent", 4,non_inspire_g$answer 
                                                    )))))
non_inspire_g <- non_inspire_g %>% filter(answer != "skip")


#### 그래프 시각화 ####
library(ggplot2)
library(tidyr)
non_inspire_g %>%
  pivot_longer(-answer) %>%
  ggplot(aes(x = answer, y = value, fill = name)) +
  geom_col(position = position_stack(), colour = "black") + 
  geom_text(aes(label = value), size = 2, position = position_stack(0.5))+
  scale_fill_brewer(name = '이유', palette = 'Paired') + 
  theme_bw() + xlab("실천정도") + ylab("영향도") + 
  ggtitle("동물제품 소비 줄이게 된 이유", subtitle = "non-vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))


