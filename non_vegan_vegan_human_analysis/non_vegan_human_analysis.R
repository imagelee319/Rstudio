###### 비건데이터 파일 가져오기 ######

library(readxl)

library(dplyr)
library(ggplot2)
library(descr)


vegan_raw3 <-read_excel("C:/Rstudy/miniprojectR/vegan_Raw_data_c.xls")
vegan_raw1 <- vegan_raw3 %>% filter(!(eat_beef == "Never" & eat_chicken == "Never" &eat_dairy =="Never"& 
                                        eat_eggs =="Never" & eat_fish_seafood =="Never"& eat_pork =="Never"))


head(vegan_raw1)
str(vegan_raw1)
sum(is.na(vegan_raw1)) # NA 값을 가진 데이터 프레임이 있는지 확인



# 결측치 "skip"으로 변경하기
vegan_raw1$student <- ifelse(is.na(vegan_raw1$student), "skip", vegan_raw1$student)

str(vegan_raw1$student) #확인해보기




##### 학생 분포 시각화 #####

###그래프 형태로 확인하여보기
#qplot(vegan_raw1$student)

#freq(vegan_raw1$student, plot = T, col ="chocolate", main =' 학생인가요 ? ')


####  파이차트 만들기
class <- margin.table(x= table(vegan_raw1$student), margin=1)
class



#### 응답수 + 비율 표시된 파이차트

### 응답수- 비율 구하기
class.pct <- round(class/sum(class)*100, 1)
class.pct

##그래프에 응답수와 비율 같이 표시하기
lbl <- paste( class, "명, ", class.pct, "%", sep= "")
lbl

##### 3d파이차트

library(plotrix)

pie3D(class, labels= lbl,explode= 0.03, labelcex= 1.2,  
      col =heat.colors(3), main = "직업 (학생인가요?)", cex.main=2.5, col.main= "chocolate", bg= "242" )

text(0, 0.3, "학생아님", cex=1.8, font=2) 
text(0.25, -0.006, "학생", cex=1.5, font=3)
text(-0.2, -0.006, "응답안함", cex=1.3, font=3)





###### 교육수준 시각화 #####


#그래프로 미리보기

#freq(vegan_raw1$education, plot = T, col ="chocolate", main ='최종 학력은?')



#결측치 skip으로 변경
vegan_raw1$education <- ifelse(is.na(vegan_raw1$education), "skip", vegan_raw1$education)

#데이터 학인
str(vegan_raw1$education)


#####  파이차트 만들기

### class1라는 변수에 각항목 응답수 확인

class1 <- margin.table(x= table(vegan_raw1$education), margin=1)
class1

###응답수 포함 파이차트
lbl1 <- paste(names(class1), ": ", class1, sep = "")


###비율 구하고 응답수와 비율 포함한 파이차트

class.pct1 <- round(class1/sum(class1)*100, 1)
class.pct1

lbl1 <- paste(names(class1), ": ", class1, "명, ", class.pct1, "%", sep= "")
lbl1


pie(class1, labels= lbl1, main ="최종학력", col =heat.colors(7), cex.main=2, col.main= "chocolate" )

text(0, 0.35, "학사", cex=2.2, font=2) 
text(0.5, 0.15, "준학사", cex=1.7, font=3)
text(0.47, -0.2, "응답안함", cex=1.3, font=3)
text(-0.04, -0.4, "석사", cex=1.7, font=3)
text(-0.45, -0.2, "고등학교졸업", cex=1.4, font=3)
text(-0.6, 0., "박사", cex=1.5, font=3)
text(-0.5, 0.15, "고등학교졸업X", cex=1, font=3)



#fanplot차트


lbl1_5 <- paste(c("준학사", "학사", "고등학교졸업X", "박사", "고등학교졸업", "석사","응답안함" ), ": ", class1, "명, ", class.pct1, "%", sep= "")
fan.plot(class1,labels= lbl1_5,col =heat.colors(7),  main="최종학력", cex.main=2.5, col.main= "chocolate", align='left',max.span=pi)





##### 성별 비율

#데이터 학인
str(vegan_raw1$gender)


vegan_raw1$gender <- ifelse(is.na(vegan_raw1$gender), "skip", vegan_raw1$gender)

# class2라는 변수에 각항목 응답수 확인


class2<- margin.table(x= table(vegan_raw1$gender), margin=1)
class2


class.pct2 <- round(class2/sum(class2)*100, 1)
class.pct2

lbl2 <- paste( class2, "명, ", class.pct2, "%", sep= "")
lbl2


##### 응답자수, 비율 포함 파이차트 

pie3D(class2, labels= lbl2, explode= 0.03, labelcex= 1.2,
      col =heat.colors(3), main = "성별", cex.main=2.5, col.main= "chocolate" )


##### 그래프 안에 텍스트 넣기

text(0, 0.25, "여자", cex=2.2, font=2)
text(0.63, -0.018, "응답안함", cex=1.5, font=3)
text(0.48, -0.1, "성별X", cex=1, font=3)
text(0.08, -0.1, "남자",  cex=2.2 , font=2)






###### 소득수준 구하기

str(vegan_raw1$income)


vegan_raw1$income <-ifelse(is.na(vegan_raw1$income), "skip", vegan_raw1$income)

income <- gsub("250000", "$250,000+", vegan_raw1$income) 


## class3라는 변수에 각항목 응답수 확인

#파이차트 그리기
class3<- margin.table(x= table(income), margin=1)
class3




###응답수 포함 파이차트

lbl3 <- paste(names(class3), ": ", class3, sep = "")


class.pct3 <- round(class3/sum(class)*100, 1)
class.pct3

lbl3 <- paste(names(class3), ": ", class3, "명, ", class.pct3, "%", sep= "")
lbl3

pie(class3, labels= lbl3, main = "소득수준", col =heat.colors(6), cex.main=2.5, col.main= "chocolate")

#fanplot차트

fan.plot(class3,labels= lbl3 ,col =terrain.colors(6),  main="소득수준", cex.main=2.5, col.main= "darkgreen", align='left',max.span=pi)






##### 반려동물 관련

pet= c(vegan_raw1$h_bird, vegan_raw1$h_cat, vegan_raw1$h_dog, vegan_raw1$h_reptile, vegan_raw1$h_small_m, vegan_raw1$h_none_of_the_above)
str(pet)


vegan_raw1 <-ifelse(is.na(pet), "skip", pet)



## class4 라는 변수에 각항목 응답수 확인

class4<- margin.table(x= table(pet), margin=1)
class4



####응답수 포함 파이차트

lbl4 <- paste(names(class4), ": ", class3, sep = "")



class.pct4 <- round(class4/sum(class4)*100, 1)
class.pct4

lbl4 <- paste(names(class4), ": ", class4, "명, ", class.pct4, "%", sep= "")
lbl4



pie(class4, labels= lbl4,col =terrain.colors(3), main = "반려동물들", cex.main=2.5, col.main= "darkgreen")



#fanplot차트

fan.plot(class4,labels= names(class4),col =terrain.colors(6),  main="반려동물들", cex.main=2.5, col.main= "darkgreen", align='left',max.span=pi)


lbl5 <- paste(c("파충류", "고양이", "강아지", "키우지 않음", "새", "작은 포유류"), ": ", class4, "명, ", class.pct4, "%", sep= "")
lbl5

fan.plot(class4,labels= lbl5 ,col =terrain.colors(6),  main="반려동물들", cex.main=2.5, col.main= "darkgreen", align='left',max.span=pi)









