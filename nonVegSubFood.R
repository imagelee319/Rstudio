#install.packages("readxl")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

vegan_raw <- read_excel("C:/Rstudy/teamR/rvd.xls") #non vegan

vegan_raw1 <- vegan_raw %>% filter(!(eat_beef == "Never" & eat_chicken == "Never" &eat_dairy =="Never"& 
                                       eat_eggs =="Never" & eat_fish_seafood =="Never"& eat_pork =="Never")) 

alf<- vegan_raw1 %>% select(sub_beans_lentils,	sub_extra_vegetables,
                               sub_grains,	sub_nuts,	sub_seitan,	sub_tempeh,	sub_tofu,	sub_vegetarian_meats)
View(alf)

head(
  select(alf, sub_beans_lentils,	sub_extra_vegetables,
         sub_grains,	sub_nuts,	sub_seitan,	sub_tempeh,	sub_tofu,	sub_vegetarian_meats)
)


#find unique value of column one by one
unique(alf[c("sub_beans_lentils")])
unique(alf[c("sub_extra_vegetables")])
unique(alf[c("sub_grains")])

#find unique value from columns without NA
sort(unique(alf$sub_beans_lentils), decreasing=TRUE)


# data.frame
df_alf <- data.frame(alf)
df_alf
View(df_alf)



#
t_alf <- table(is.na(alf))
t_alf


alf_beans <-ifelse(is.na(alf$sub_beans_lentils), "skip", alf$sub_beans_lentils)
alf_beans
beans <- table(alf_beans)
beans



alf_vegetables <-ifelse(is.na(alf$sub_extra_vegetables), "skip", alf$sub_extra_vegetables)
alf_vegetables
vegetables <- table(alf_vegetables)
vegetables



alf_grains <-ifelse(is.na(alf$sub_grains), "skip", alf$sub_grains)
alf_grains
grains <- table(alf_grains)
grains

alf_nuts <-ifelse(is.na(alf$sub_nuts), "skip", alf$sub_nuts)
alf_nuts 
nuts <- table(alf_nuts)
nuts

alf_seitan <-ifelse(is.na(alf$sub_seitan), "skip", alf$sub_seitan)
alf_seitan
seitan <- table(alf_seitan)
seitan

alf_tempeh <-ifelse(is.na(alf$sub_tempeh), "skip", alf$sub_tempeh)
alf_tempeh
tempeh <- table(alf_tempeh)
tempeh

alf_tofu <-ifelse(is.na(alf$sub_tofu), "skip", alf$sub_tofu)
alf_tofu
tofu <- table(alf_tofu)
tofu

alf_meats <-ifelse(is.na(alf$sub_vegetarian_meats), "skip", alf$sub_vegetarian_meats)
alf_meats
meats<-table(alf_meats)
meats


alf_group <- cbind (beans,	vegetables, grains,	nuts,	seitan,	tempeh,	tofu,	meats)
str(alf_group)

alf_group_df <- data.frame(alf_group)
alf_group_df

alf_group_df1 <- dplyr::add_rownames(alf_group_df, var="answers")
head(alf_group_df1)

alf_group_df <- alf_group_df1 %>% filter(answers != "skip")
alf_group_df 

alf_group_df$answers <- ifelse(alf_group_df$answers =="Not at all", "0",
                               ifelse(alf_group_df$answers =="To a large extent", "4",
                                      ifelse(alf_group_df$answers =="To a moderate extent", "3" ,
                                             ifelse(alf_group_df$answers =="To little extent", "2", 
                                                    ifelse(alf_group_df$answers =="To some extent", "1", alf_group_df$answers)))))

alf_group_df

library(reshape)
library(tidyverse)
library(plotly)


alf_group_df_p <- alf_group_df %>% pivot_longer(-answers) 

non_veg_alt<-ggplot(alf_group_df_p, aes(x = answers, y=value, fill = name)) + 
  geom_col(position = position_stack(), colour = "black") +
  geom_text(aes(label=value), size=3, position = position_stack(0.9)) +
  scale_y_continuous(limits = c(0, 3000)) +
  theme_bw() + xlab("실천정도") + ylab("영향도") + 
  ggtitle("Non-Vegan: Alternative food", subtitle = "non-vegan") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "darkblue"),
        plot.subtitle = element_text(size = 15))

non_veg_alt
ggplotly(non_veg_alt)

