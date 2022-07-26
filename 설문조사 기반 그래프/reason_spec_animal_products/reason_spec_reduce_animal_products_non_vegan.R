library(readxl)
library(dplyr)
vegan_raw <- read_excel("C:/Rstudy/mini_team1/data/vegan_Raw_data_c.xls")
vegan_raw1 <- vegan_raw %>% filter(!(eat_beef == "Never" & eat_chicken == "Never" &eat_dairy =="Never"& 
                                       eat_eggs =="Never" & eat_fish_seafood =="Never"& eat_pork =="Never")) 

#### 11 동물성 제품 줄이는 가장 큰 이유 ####
non_reducing_products_one <- vegan_raw1$reducing_ap


#### 11 결측치 대체 및 데이터 프레임화 ####
non_reducing_products_one_s <- ifelse(is.na(non_reducing_products_one), "skip", non_reducing_products_one)
non_reducing_products_one1 <- data.frame(non_reducing_products_one_s)


#### 11 skip이 아닌 행만 추출하고 빈도수 측정 ####
non_reducing_products_one2 <-non_reducing_products_one1 %>% filter(non_reducing_products_one_s != "skip") %>%  group_by(non_reducing_products_one_s) %>% 
  summarise(n=n())

### 항목명 변경- non-vegan ### 
non_reducing_products_one2$non_reducing_products_one_s <- ifelse(non_reducing_products_one2$non_reducing_products_one_s =="Animal welfare concerns", "Animal welfare",
                                                                 ifelse(non_reducing_products_one2$non_reducing_products_one_s =="Environmental concerns", "Environment",
                                                                        ifelse(non_reducing_products_one2$non_reducing_products_one_s =="Family or friend doing it", "Family Friend" ,
                                                                               ifelse(non_reducing_products_one2$non_reducing_products_one_s =="Someone made the transition with me", "Someone", non_reducing_products_one2$non_reducing_products_one_s
                                                                               ))))  



#### 11 그래프 시각화 ####
ggplot(non_reducing_products_one2, aes(x=reorder(non_reducing_products_one_s, n), y= n)) +
  geom_col() + coord_flip() +
  scale_y_continuous(limits = c(0, 1200)) +
  geom_text(aes(label=n), hjust=-.25, size=3.5) +
  theme_bw() + xlab("이유") + ylab("빈도수") + 
  ggtitle("동물성 제품 소비를 줄인 이유", subtitle = "non-vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))




###################################################
###################################################
#### 12 동물 복지 선택자들 이유? ####
non_reducing_aw <- vegan_raw1$aw_reason
table(is.na(non_reducing_aw))


#### 12 결측치 대체 및 데이터프레임화 ####
non_reducing_aw_s <- ifelse(is.na(non_reducing_aw), "skip", non_reducing_aw) 
non_reducing_aw_s <- data.frame(non_reducing_aw_s)


#### 12 cbind로 합치기 여기서 one1은 11번에서 선택한 항목임 ####
non_reducing_pro_aw_inf <- cbind(non_reducing_products_one1, non_reducing_aw_s)



#### 12 skip항목 제외 및 동물 복지 선택 안한사람들 제외 -> 동물 복지 선택한 사람들만 추출 ####
non_reducing_pro_aw_inf_f <- non_reducing_pro_aw_inf %>% filter(non_reducing_aw_s != "skip" & 
                                                                  non_reducing_aw_s != "Not applicable" & 
                                                          non_reducing_products_one_s =="Animal welfare concerns")



#### 12 추출한 항목 빈도수 ####
non_reducing_pro_aw_inf_f <- non_reducing_pro_aw_inf_f %>% group_by(non_reducing_aw_s) %>% 
  summarise(n5=n())

non_reducing_pro_aw_inf_f$non_reducing_aw_s <- ifelse(non_reducing_pro_aw_inf_f$non_reducing_aw_s =="I learned about how farm animals have intelligence and emotions", "animal-friendly",
                                                      ifelse(non_reducing_pro_aw_inf_f$non_reducing_aw_s =="I do not like the fact that farm animals are killed", "don't like kill",
                                                             ifelse(non_reducing_pro_aw_inf_f$non_reducing_aw_s =="I do not like the fact that farm animals are raised in very cruel conditions", "hate bad environment" ,
                                                                    non_reducing_pro_aw_inf_f$non_reducing_aw_s
                                                             )))  


#### 12 시각화 ####
library(ggplot2)
ggplot(non_reducing_pro_aw_inf_f, aes(x = reorder(non_reducing_aw_s,n5), y =n5)) +
  geom_col() + coord_flip() +
  scale_y_continuous(limits = c(0, 500)) +
  geom_text(aes(label=n5), hjust=-.25, size=3.5) +
  theme_bw() + xlab("이유") + ylab("빈도수") + 
  ggtitle("동물 복지를 선택한 이유", subtitle = "non-vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))




# vegan_raw<-as.data.frame(vegan_raw)
#### 13 건강,영양 선택한 이유? ####
non_reducing_ht <- vegan_raw1 %>% 
  select(hn_animal_disease, hn_energy, hn_weight, hn_certain_diseases, hn_other)


#### 13 cbind로 11에서 선택 항목과 건강영양 선택 이유에 대한 컬럼들 합치기 ####
non_reducing_pro_ht <- cbind(non_reducing_products_one1, non_reducing_ht)



#### 13 helth또는 nutrition을 선택한 사람들 추출 ####
non_reducing_pro_ht_f <- non_reducing_pro_ht %>% filter(non_reducing_products_one_s =="Health" |
                                                          non_reducing_products_one_s =="Nutrition")


#### 13 각 컬럼들 빈도수, 4개의 컬럼 중 2개 선택이므로 결측치 제거하면 안됨 -> ex) 1번 고르고 3번 고르면 2,4번은 결측치임 ####
#### 각 컬럼의 항목들은 같은 데이터임 -> ex) animal_disease컬럼의 입력 항목들은 동물 죽는게 싫다임 ####
non_animal_disease1 <- table(non_reducing_pro_ht_f$hn_animal_disease)
non_energy1 <- table(non_reducing_pro_ht_f$hn_energy)
non_weight1 <- table(non_reducing_pro_ht_f$hn_certain_diseases)
non_other1 <- table(non_reducing_pro_ht_f$hn_other)



#### 하린님이 가르쳐주신 컬럼별 빈도수 구하기 matrix.table ####
# a <- c(reducing_pro_ht_f$hn_animal_disease, reducing_pro_ht_f$hn_energy,reducing_pro_ht_f$hn_weight, reducing_pro_ht_f$hn_certain_diseases, reducing_pro_ht_f$hn_other)
# b <- margin.table(x = table(a), margin=1)
# c <- as.data.frame(b)
# 
# library(ggplot2)
# ggplot(c, aes(x = reorder(a, -Freq), y=Freq)) + geom_col() + coord_flip() +
#   geom_text(aes(label=Freq), hjust=-.25, size=3.5)



#### 13 뽑은 빈도수 합치기 및 데이터프레임화 ####
non_reducing_pro_ht_f1 <- as.data.frame(c(non_animal_disease1, non_energy1, non_weight1, non_other1))



#### 13 인덱스명 및 컬럼명 변경 ####
non_reducing_pro_ht_f1 <- dplyr::add_rownames(non_reducing_pro_ht_f1, var="answer")
non_reducing_pro_ht_f1 <- dplyr::rename(non_reducing_pro_ht_f1, n = "c(non_animal_disease1, non_energy1, non_weight1, non_other1)")

non_reducing_pro_ht_f1$answer <- ifelse(non_reducing_pro_ht_f1$answer =="Other (Please Specify)", "Other",
                                        ifelse(non_reducing_pro_ht_f1$answer =="I wanted more energy/well-being", "energy well-being",
                                               ifelse(non_reducing_pro_ht_f1$answer =="I’m concerned about antibiotics/hormones/diseases in animal products", "concern diseases from animal" , ifelse(non_reducing_pro_ht_f1$answer =="I wanted to reduce my risk of developing certain diseases", "reduce diseases" ,
                                                                                                                                                                                                      non_reducing_pro_ht_f1$answer
                                               ))))  

#### 13 그래프 시각화 ####
library(ggplot2)
ggplot(non_reducing_pro_ht_f1, aes(x = reorder(answer, n), y=n)) + geom_col(colour = "black") + coord_flip() +
  scale_y_continuous(limits = c(0, 500)) +
  geom_text(aes(label=n), hjust=-.25, size=3.5) +
  theme_bw() + xlab("이유") + ylab("빈도수") + 
  ggtitle("건강 영양을 선택한 이유", subtitle = "non-vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))

