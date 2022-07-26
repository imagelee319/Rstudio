
########################################
####트위터 추출한 raw data 전처리 (줄단위 데이터 구조 유지)

#트위터에서 추출한 raw data 

load("C:/Rstudy/miniprj/textmining/vegan_raw.RData") #데이터 로드 

vegan  
write.table(vegan) 
#getText() : 텍스트추출, sapply() : 함수 적용하여 벡터로 변환
vegan_word <- sapply(vegan, function(t) t$getText()) 

library(stringr)  
#str_replace_all() : 해당 하는 문자를 치환(특수문자를 공백으로 바꿈)
vegan_word <- str_replace_all(vegan_word, "\\W", " ") 

#불용어 제거를 위해 vegan_gsub.txt 파일에 있는 텍스트 읽어들임
txt <- readLines("vegan_gsub.txt", encoding="UTF-8") 
cnt_txt <- length(txt) #텍스트 라인 개수 확인
#i <- 1
for(i in 1:cnt_txt){
  vegan_word <- gsub((txt[i]), "", vegan_word) 
} 
# cnt_txt 개수만큼 반복문 실행. 
# gsub("바꾸고자하는 대상의 text 또는 패턴","대체할 text",text객체)
# vegan_word에서 txt를 ""으로 치환하여, 다시 vegan_word에 할당. 

vegan_word #불용어 제거된 줄 단위 텍스트.

########################################


# 줄 단위 단어 추출
library(KoNLP) 

lword <- Map(extractNoun, vegan_word) #Map(extractNoun,변수) : 변수에서 명사단위로 추출
length(lword) #length() 함수 : 데이터 개수 확인 / 출력값 : [1] 1000
lword <- unique(lword) #unique() 함수 : 빈 block 필터링
length(lword) # 중복제거 후 출력값 : [1] 441
str(lword) #List of 441
head(lword, 2) 

# 단어 필터링 함수 정의
# : 길이가 2개 이상 4개 이하 사이의 문자 길이로 구성된 단어
#   is.hangul() 함수 : 영어 단어 필터링
filter1 <- function(x){
  nchar(x) >= 2 && nchar(x) <=4 && is.hangul(x) 
} 
filter2 <- function(x){
  Filter(filter1,x)
}  

# 줄 단위로 추출된 단어 전처리
lword <- sapply(lword, filter2) # 단어 길이 1이하 또는 5이상인 단어 제거  
head(lword) # 단어길이 2~4 사이의 단어 출력 (1이하 또는 5이상 제거, 한글외 언어 제거)


########################################
##연관분석###

# 연관분석을 위해서는 추출된 단어를 대상으로 트랜잭션 형식으로 자료구조 변환.
library(arules) #arules 연관분석 패키지 로드

#트랜잭션 생성
wordtran <- as(lword, "transactions") #as(data, "transactions") : 트랜잭션으로 변환
wordtran  # 출력값 : 441 transactions (rows) and 1943 items (columns)

#교차표 작성
wordtable <- crossTable(wordtran) #crossTable() : 교차테이블 생성
wordtable #유사단어들이 함께 있는 형태로 출력

#단어간 연관 규칙 산출
transrules <- apriori(wordtran, 
                      parameter = list(support=0.015, conf=0.08)) # 출력값 : writing ... [27 rule(s)] done [0.00s]. (27개 규칙을 찾음)

################데이터 중간 저장 #############
save(transrules, file="tabledata1.RData") 
load("tabledata1.RData") 
##############################################

#apriori() : 연관분석 기능 적용하여 규칙성을 찾는 함수. (transaction 자료 구조에서 실행) 
#support : 지지도. 좋은 규칙(빈도가 많은, 구성비가 높은)을 찾거나 불필요한 연산 줄일 때 기준 사용.
#conf : 신뢰도. 신뢰도가 높을 수록 유용한 규칙일 가능성이 높다고 할수있음. 
#lift : 향상도. 향상도가 1보다 크거나 작으면 우연적 기회보다 우수함 의미.(list=1이면 서로 독립관계) 

#연관규칙 생성 결과 보기
inspect(transrules) 


########################################
##연관어 시각화##
#연관 단어 시각화 위해 자료 구조 변경
rules <- labels(transrules, ruleSep = " ") #연관규칙 레이블을 " "으로 분리
rules  

#문자열로 묶인 연관 단어를 행렬 구조 변경
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)   
rules 
class(rules) 

# 행 단위로 묶어서 matrix로 반환 (do.call)
rulemat <- do.call("rbind", rules) 
rulemat 
class(rulemat) 

# 연관어 시각화를 위한 igraph 패키지 설치 
library(igraph) 

# edgelist 보기 - 연관 단어를 정점(vertex) 형태의 목록 제공(matrix 형태의 자료형을 전달 받게 되어 있음)
relueg <- graph.edgelist(rulemat[c(3:27),], directed = F)  #[c(1:2)] - "{}"제외 
relueg 

################데이터 중간 저장 #############
save(relueg, file="graphdata1.RData") 
load("graphdata1.RData") 
##############################################

# edgelist 시각화 
# plot.igraph(relueg)   

# edgelist 시각화 2.
plot.igraph(relueg, vertex.label=V(relueg)$name, vertex.label.cex=1.1, vertex.label.color='black',
            vertex.size=20, vertex.color='green', vertex.frame.color='gray') 

# 거리가 가까울수록 높은 연관성을 가지고 있음을 보여준다.
