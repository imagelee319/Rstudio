library(readxl)
library(dplyr)
library(ggplot2)

#Page 13. "consumption of animal products" Non-Vegan
### 논비건 사람들의 동물성 제품 소비 예상

vegan_raw <- read_excel("C:/Rstudy/0506/newProj/rvd.xls")

vegan_raw1 <- vegan_raw %>% filter(!(eat_beef == "Never" & eat_chicken == "Never" &eat_dairy =="Never"& 
                                       eat_eggs =="Never" & eat_fish_seafood =="Never"& eat_pork =="Never")) 

consumption <- vegan_raw1 %>% select(consumption)
consumption

#how to count the number of non NA values in R
sum(!is.na(vegan_raw1$consumption))

#False and True
t_df_c <- table(is.na(vegan_raw1$consumption))
t_df_c

consump<- ifelse(vegan_raw1$consumption =="It will stay the same", "eat same", 
                 ifelse(vegan_raw1$consumption =="I will eat fewer animal products", "eat fewer",
                        ifelse(vegan_raw1$consumption =="I will eat more animal products", "eat more",
                               vegan_raw1$consumption)))
consump

consump <- ifelse(is.na(vegan_raw1$consumption), "skip", consump)
consump 

df_c <- data.frame(consump)
df_c
View(df_c)

unique(df_c[c("consump")])

df_c <- dplyr::add_rownames(df_c, var="Answers")
head(df_c)

df_c <- df_c %>% filter(consump != "skip")
View(df_c)

#Finding the unique values in column "consump" 
unique(df_c[c("consump")])

#Get Maximum value of the column by column position
df_c %>% summarise_if(is.character, max)


df_c1 <- df_c %>% group_by(consump) %>% summarise(n=n())
df_c1

library(reshape)
library(tidyverse)
library(plotly)

cons_p <- df_c1  %>% pivot_longer(-consump)

cons_graph<-ggplot(cons_p, aes(x = consump,y=value, fill =consump)) +
  geom_col(position = position_stack(), colour = "black") +
  geom_text(aes(label=value), size=3, position = position_stack(0.9)) +
  scale_y_continuous(limits = c(0, 1500)) +
  theme_bw() + xlab("예상") + ylab("응답수") +
  ggtitle("Consumption of the animal products", subtitle = "non-vegan") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))

cons_graph
ggplotly(cons_graph)




### 비건 사람들의 동물성 제품 소비 예상

library(readxl)
library(dplyr)
library(ggplot2)

### 비건 사람들의 동물성 제품 소비 예상
#Page 13. "consumption of animal products" Vegan

vegan_raw <- read_excel("C:/Rstudy/0506/newProj/rvd.xls")


consumption <- vegan_raw %>% select(consumption)
consumption

#how to count the number of non NA values in R
sum(!is.na(vegan_raw1$consumption))

#False and True
t_df_c <- table(is.na(vegan_raw$consumption))
t_df_c

consump<- ifelse(vegan_raw$consumption =="It will stay the same", "eat same", 
                 ifelse(vegan_raw$consumption =="I will eat fewer animal products", "eat fewer",
                        ifelse(vegan_raw$consumption =="I will eat more animal products", "eat more",
                               vegan_raw$consumption)))
consump


consump <- ifelse(is.na(vegan_raw$consumption), "skip", consump)
consump 



df_c <- data.frame(consump)
df_c
View(df_c)

unique(df_c[c("consump")])

df_c <- dplyr::add_rownames(df_c, var="Answers")
head(df_c)

df_c <- df_c %>% filter(consump != "skip")
View(df_c)


#Finding the unique values in column "consump" 
unique(df_c[c("consump")])

#Get Maximum value of the column by column position
df_c %>% summarise_if(is.character, max)


df_c1 <- df_c %>% group_by(consump) %>% summarise(n=n())
df_c1

library(reshape)
library(tidyverse)
library(plotly)


cons_p <- df_c1  %>% pivot_longer(-consump)

cons_graph<-ggplot(cons_p, aes(x = consump,y=value, fill =consump)) +
  geom_col(position = position_stack(), colour = "black") +
  geom_text(aes(label=value), size=3, position = position_stack(0.9)) +
  scale_y_continuous(limits = c(0, 1500)) +
  theme_bw() + xlab("예상") + ylab("응답수") +
  ggtitle("Consumption of the animal products", subtitle = "vegan") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))

cons_graph
ggplotly(cons_graph)




