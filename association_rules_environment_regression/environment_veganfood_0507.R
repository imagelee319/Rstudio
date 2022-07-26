library 
c02data <- read_excel("c02data.xls")
str(c02data)

#카테고리로 그룹화하여, 그룹별 평균 산출하고 내림차순 정렬
library(dplyr) 


c02data1 <- c02data %>% select(Category, GHGemissionperkg2013) %>% 
  group_by(Category) %>% 
  summarise(mean_GHG_2013 = round(mean(GHGemissionperkg2013),digits = 1)) %>% 
  arrange(desc(mean_GHG_2013)) 


library(psych) #패키지 로드
describe(c02data1) #기술통계량


#식품 카테고리 범주형 변환
c02data1$Category <- factor(c02data1$Category, 
                            levels = c("Meat", "Fish", "Dairy", "Coffe", 
                                       "Chocolate", "Vegetable Oil", "Grain", 
                                       "Soy Food", "Wine", "Vegetable", "Bean", "Fruit")) 

###########데이터 중간 저장#############
save(c02data1, file="barchartdata1.RData") 
load("barchartdata1.RData") 
########################################

#ggplot2 막대 그래프로 시각화 
library(ggplot2) 
ggplot(c02data1, aes(x=reorder(Category, mean_GHG_2013), y=mean_GHG_2013)) + geom_col(fill="darkgreen") + 
  theme_light() + labs(x= "식품 종류", y= "온실가스 배출량(kg당)", title= "식품별 온실가스 배출량") + coord_flip() 
#인사이트 : 고기 > 생선 > 유제품 > 커피 > 초콜릿 순으로 많은 온실가스 발생함. 


#식품별 온실가스 발생 비중 컬럼 추가, 컬러명 변경
data_prop <- c02data1 %>% 
  mutate(paste0(prop = round((mean_GHG_2013/sum(mean_GHG_2013)*100),2))) 
names(data_prop) <- c("Category", "GreenhouseGas", "Percent") 
data_prop 

#식품별 온실가스 발생 비중 원형 차트로 시각화
data_prop_numeric <- as.numeric(data_prop$Percent) #텍스트를 숫자로 변환 
data_prop2 <- data_prop 
data_prop2$Percent <- data_prop_numeric 


###########데이터 중간 저장#############
save(data_prop2, file="piechartdata1.RData") 
load("piechartdata1.RData") 
########################################


#반응형 파이차트 (R Markdown html용)
library(plotly) #반응형 그래프 패키지 로드
library(dplyr)
p <- plot_ly(data_prop2, labels = ~Category, values = ~Percent, textinfo='label+percent',type = 'pie',
             maker = list(colors = ~colors)) %>% 
  layout(title = "식품별 온실가스 배출 비중",
         xasis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
p

#일반 파이차트 (R Markdown Word용)
pie(data_prop2$Percent, labels = paste0(data_prop2$Percent, "%"),
    main = "식품별 온실가스 배출 비중", col=rainbow(12), cex.main=1.5, col.main="black", cex=0.5, radius=0.8, border=NA) 
legend(1.2, 1, data_prop2$Category,fill=rainbow(12), cex = 0.5)    

#인사이트 : 고기(40.2%), 생선(14.8%), 유제품(11.2%)을 먹지 않으면, 개인이 배출하는 온실가스 배출량의 약66%를 감소시키는 효과.
#-----------------------------------------------------------------------------------------------------

