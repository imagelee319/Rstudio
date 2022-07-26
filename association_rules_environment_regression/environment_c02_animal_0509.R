
#저장할 디렉토리 지정
setwd("C:/Rstudy/miniprj/rvdata") 

##연도별 글로벌 C02 증가 raw data 불러옴
c02inc <- read.csv("c02increase.csv") 
#연도 날짜 포맷으로 변경
c02inc$Year <- format(as.Date(c02inc$Year, "%Y"), "%Y") 
c02inc 

##연도별 멸종위기동물 raw data 불러옴
mammal <- read.csv("redlistindex.csv") 
mammal <- mammal[,1:2]  
mammal$Year <- as.character(mammal$Year) #연도 문자형 변환 
mammal 

##데이터 결합 및 변수명 변경 (이너조인)
library(dplyr) 
c02_red_mal <- inner_join(mammal, c02inc, by="Year") 
c02_red_mal 
c02_red_mal <- c02_red_mal[,1:3] 
names(c02_red_mal) <- c("Year", "threatened_animals", "c02_increase") 


###########데이터 중간 저장#############
save(c02_red_mal, file="pairbarchartdata1.RData")  
load("pairbarchartdata1.RData") 
########################################

##연도별 c02 증가 막대그래프
library(ggplot2)
p1 <- ggplot() + geom_col(aes(x=Year, y=c02_increase), data=c02_red_mal, fill="green") + 
  theme_light() +
  labs(x = "Year(연도)",
       y = "C02증가(PPM)",
       title = "연도별 C02 증가 분석(1990~2017)")

##연도별 멸종위기동물 수 막대그래프
p2 <- ggplot() + geom_col(aes(x=Year, y=threatened_animals), data=c02_red_mal, fill="#990033") + 
  theme_light() +
  labs(x = "Year(연도)",
       y = "멸종위기동물수(마리)",
       title = "연도별 멸종위기 동물 수 분석(1990~2017)")

 
##두 그래프 한페이지에 보기
library(gridExtra)
grid.arrange(p1, p2, nrow = 2) 

#------------------------------------------------------------------------------------------------------
##상관분석
cor.test(c02_red_mal$threatened_animals, c02_red_mal$c02_increase) 
#p-value = 0.03982 상관관계 있다********
#cor 0.4750949

##F검정(등분산검정)
var.test(c02_red_mal$threatened_animals, c02_red_mal$c02_increase) 
#p-value < 2.2e-16 등분산성 만족*******

#회귀분석
model_lm_5 <- lm(threatened_animals ~ c02_increase, data = c02_red_mal)
summary(model_lm_5)
# 귀무가설 : c02 증가와 멸종위기 포유류 수는 관계가 없다
# 대립가설 : c02 증가와 멸종위기 포유류 수는 관계가 있다
# p-value: 0.03982 으로 귀무가설 기각. 대립가설 채택.**************
# Multiple R-squared:  0.2257,	Adjusted R-squared:  0.1802
# threatened_animal(y) = 152.45 * c02_increase(x) + 777.53

# 산점도 그래프로 x,y 상관관계 확인
library(ggpubr) 
ggscatter(c02_red_mal, x = "c02_increase", y = "threatened_animals", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "C02 증가 (PPM)", ylab = "멸종위기 동물 수 (마리)", title = "C02 증가와 멸종위기 동물 수 상관관계") 


#---------------------------------------------------
