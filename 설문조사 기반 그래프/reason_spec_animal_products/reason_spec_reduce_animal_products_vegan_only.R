
#### 11 동물성 제품 줄이는 가장 큰 이유 ####
library(readxl)
library(dplyr)
vegan_raw <- read_excel("C:/Rstudy/mini_team1/data/only_vegan.xlsx")

reducing_products_one <- vegan_raw$reducing_ap



#### 11 결측치 대체 및 데이터프레임화 ####
reducing_products_one_s <- ifelse(is.na(reducing_products_one), "skip", reducing_products_one)
reducing_products_one1 <- data.frame(reducing_products_one_s)


#### 11 skip항목 제외 및 빈도수 ####
reducing_products_one2 <-reducing_products_one1 %>% filter(reducing_products_one_s != "skip")%>% group_by(reducing_products_one_s) %>% 
  summarise(n=n())

reducing_products_one2$reducing_products_one_s <- ifelse(reducing_products_one2$reducing_products_one_s =="Animal welfare concerns", "Animal welfare",
                                                         ifelse(reducing_products_one2$reducing_products_one_s =="Environmental concerns", "Environment",
                                                                ifelse(reducing_products_one2$reducing_products_one_s =="Family or friend doing it", "Family Friend" ,
                                                                       ifelse(reducing_products_one2$reducing_products_one_s =="Someone made the transition with me", "Someone", reducing_products_one2$reducing_products_one_s
                                                                       ))))  
#### 11 그래프 시각화 ####
ggplot(reducing_products_one2, aes(x=reorder(reducing_products_one_s, n), y= n)) +
  geom_col() + coord_flip() +
  scale_y_continuous(limits = c(0, 1200)) +
  geom_text(aes(label=n), hjust=-.25, size=3.5) +
  theme_bw() + xlab("이유") + ylab("빈도수") + 
  ggtitle("동물성 제품 소비를 줄인 이유", subtitle = "vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))




#### 12 동물 복지 선택자들 이유? ####
reducing_aw <- vegan_raw$aw_reason
table(is.na(reducing_aw))


#### 12 결측치 대체 및 데이터프레임화 ####
reducing_aw_s <- ifelse(is.na(reducing_aw), "skip", reducing_aw) 
reducing_aw_s <- data.frame(reducing_aw_s)


#### 12 cbind로 묶고 skip항목,해당 항목 제외 추출 ####
reducing_pro_aw_inf <- cbind(reducing_products_one1, reducing_aw_s)

reducing_pro_aw_inf_f <- reducing_pro_aw_inf %>% filter(reducing_aw_s != "skip" & 
                                                          reducing_aw_s != "Not applicable" & 
                                                          reducing_products_one_s =="Animal welfare concerns")



#### 12 그룹화 및 빈도수 ####
reducing_pro_aw_inf_f <- reducing_pro_aw_inf_f %>% group_by(reducing_aw_s) %>% 
  summarise(n=n())

reducing_pro_aw_inf_f$reducing_aw_s <- ifelse(reducing_pro_aw_inf_f$reducing_aw_s =="I learned about how farm animals have intelligence and emotions", "animal-friendly",
                                              ifelse(reducing_pro_aw_inf_f$reducing_aw_s =="I do not like the fact that farm animals are killed", "don't like kill",
                                                     ifelse(reducing_pro_aw_inf_f$reducing_aw_s =="I do not like the fact that farm animals are raised in very cruel conditions", "hate bad environment" ,
                                                            reducing_pro_aw_inf_f$reducing_aw_s
                                                     )))  


#### 12 그래프 시각화 ####
library(ggplot2)
ggplot(reducing_pro_aw_inf_f, aes(x = reducing_aw_s, y =n)) +
  geom_col() + coord_flip() +
  scale_y_continuous(limits = c(0, 500)) +
  geom_text(aes(label=n), hjust=-.25, size=3.5) +
  theme_bw() + xlab("이유") + ylab("빈도수") + 
  ggtitle("동물 복지를 선택한 이유", subtitle = "vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))


# vegan_raw<-as.data.frame(vegan_raw)
#### 13 건강,영양 선택한 이유? ####
reducing_ht <- vegan_raw %>% 
  select(hn_animal_disease, hn_energy, hn_weight, hn_certain_diseases, hn_other)


#### 13 cbind로 합치기 및 필요 항목만 추출 ####
reducing_pro_ht <- cbind(reducing_products_one1, reducing_ht)
reducing_pro_ht_f <- reducing_pro_ht %>% filter(reducing_products_one_s =="Health" |
                                                  reducing_products_one_s =="Nutrition")



#### 13 각 항목 빈도수 및 데이터 프레임화 ####
animal_disease1 <- table(reducing_pro_ht_f$hn_animal_disease)
energy1 <- table(reducing_pro_ht_f$hn_energy)
weight1 <- table(reducing_pro_ht_f$hn_certain_diseases)
other1 <- table(reducing_pro_ht_f$hn_other)

reducing_pro_ht_f1 <- as.data.frame(c(animal_disease1, energy1, weight1, other1))



#### 13 인덱스 및 컬럼명 변경 ####
reducing_pro_ht_f1 <- dplyr::add_rownames(reducing_pro_ht_f1, var="answer")
reducing_pro_ht_f1 <- dplyr::rename(reducing_pro_ht_f1, n = "c(animal_disease1, energy1, weight1, other1)")


reducing_pro_ht_f1$answer <- ifelse(reducing_pro_ht_f1$answer =="Other (Please Specify)", "Other",
                                    ifelse(reducing_pro_ht_f1$answer =="I wanted more energy/well-being", "energy well-being",
                                           ifelse(reducing_pro_ht_f1$answer =="I’m concerned about antibiotics/hormones/diseases in animal products", "concern diseases from animal" , ifelse(reducing_pro_ht_f1$answer =="I wanted to reduce my risk of developing certain diseases", "reduce diseases" ,
                                                                                                                                                                                              reducing_pro_ht_f1$answer
                                           ))))  

#### 13 그래프 시각화 ####
library(ggplot2)
ggplot(reducing_pro_ht_f1, aes(x = reorder(answer, n), y=n)) + geom_col() + coord_flip() +
  scale_y_continuous(limits = c(0, 500)) +
  geom_text(aes(label=n), hjust=-.25, size=3.5) +
  theme_bw() + xlab("이유") + ylab("빈도수") + 
  ggtitle("건강 영양을 선택한 이유", subtitle = "vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))

