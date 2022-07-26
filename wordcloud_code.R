library(KoNLP)
library(stringr)
library(rtweet)
library(twitteR)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

library(devtools)
devtools::install_github("lchiffon/wordcloud2")

####사전 불러오기 및 단어 등록####
useNIADic()
mergeUserDic(data.frame(c("비건","비거니즘","논비건","미라클버거","요거트","풀무원","종달리","낫아워스","불매","레시피", "일기","롯데리아", "인증마크","참나물잣페스토","페스토"),"ncn"))

load("C:/Rstudy/mini/teamR/vegan_raw.RData")

####검색 결과에서 text만 추출하기 위해 DF로 변환#### 
vegan_df <- twListToDF(vegan)
head(vegan_df)

####text부분만 추출####
vegan_txt_list <- vegan_df$text
str(vegan_txt_list)

vegan_txt_list <- readLines("c:/Rstudy/mini/teamR/Emoji_rm.txt")

####명사만 추출####
vegan_noun <- sapply(vegan_txt_list, extractNoun,USE.NAMES=F)

head(unlist(vegan_noun),30)

vegan_noun_1 <- unlist(vegan_noun)
str(vegan_noun_1)

####특수문자 제거####
vegan_noun_1 <- str_replace_all(vegan_noun_1, "\\W", " ")

vegan_noun_1 <- gsub('[[:upper:]]',"",vegan_noun_1) #영어 대문자 제거                                 
vegan_noun_1 <- gsub('[[:lower:]]',"",vegan_noun_1) #영어 소문자 제거
vegan_noun_1 <- gsub('[[:punct:]]',"",vegan_noun_1) #특수문자 제거
vegan_noun_1 <- gsub('\\d',"",vegan_noun_1) #숫자 제거
vegan_noun_1 <- gsub('\\n',"",vegan_noun_1) #엔터 제거
vegan_noun_1 <- gsub('[ㄱ-ㅎ]',"",vegan_noun_1) #자음으로만 된 글자 제거
vegan_noun_1 <- gsub('[ㅏ-ㅣ]',"",vegan_noun_1) #모음으로만 된 글자 제거

####필요 없는 문자 제거####
txt <- readLines("c:/Rstudy/mini/teamR/vegan_gsub.txt", encoding="UTF-8")
cnt_txt <- length(txt)
#i <- 1
for(i in 1:cnt_txt){
  vegan_noun_1 <- gsub((txt[i]), "", vegan_noun_1)
}
str(vegan_noun_1)


write(unlist(vegan_noun_1),"after_gsub.txt")
vegan_noun_2 <- read.table("after_gsub.txt")
str(vegan_noun_2)

noun_two <- filter(vegan_noun_2, nchar(V1)>=2)

wordcount <- table(noun_two)
head(sort(wordcount, decreasing=T),20)
head(noun_two)
str(wordcount)

df_word <- as.data.frame(wordcount)
str(df_word)

#head(df_word) <- filter(df_word, nchar(noun_two)>=2)

top_50 <- df_word %>% 
  arrange(desc(Freq)) %>% 
  head(50)


palete <- brewer.pal(9, "Set1")
wordcloud(words=top_50$noun_two,
          freq=top_50$Freq,
          min.freq=2,
          max.words=200,
          random.order = F,
          rot.per=.1,
          scale=c(5,0.7),
          colors=palete)


wordcloud2(data=top_50,color = "random-light")
