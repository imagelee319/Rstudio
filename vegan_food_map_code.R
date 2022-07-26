library(leaflet)
library(plyr)
library(htmltools)
library(dplyr)
library(plotly)

#서울시 비건 음식점 정보 불러오기####
P <- read.csv('./seoul/seoul_food.csv', header=TRUE)

seoul_food <- rename(P,
            c('gu'='자치구.명',
              'name'='업소.명',
              'kind_code'='업태.코드',
              'kind_name'='업태.명',
              'lon'='지도.X좌표',
              'lat'='지도.Y좌표',
              'addr'='도로명주소'))

#서울시 자치구별 고유 id 불러오기####
P_id <- read.csv('./seoul/seoul_id.csv')

P_id <- rename(P_id,
               c('gu'='시군구명'))

#비건 음식점 정보에 자치구 고유 id 추가####
seoul_food <- left_join(seoul_food, P_id, by='gu')


seoul_food$kind_name <- gsub('한식','1',seoul_food$kind_name)
seoul_food$kind_name <- gsub('분식','1',seoul_food$kind_name)
seoul_food$kind_name <- gsub('중국식','2',seoul_food$kind_name)
seoul_food$kind_name <- gsub('일식','2',seoul_food$kind_name)
seoul_food$kind_name <- gsub('카페','3',seoul_food$kind_name)
seoul_food$kind_name <- gsub('까페','3',seoul_food$kind_name)
seoul_food$kind_name <- gsub('베이커리','3',seoul_food$kind_name)
seoul_food$kind_name <- gsub('양식','4',seoul_food$kind_name)
seoul_food$kind_name <- gsub('인도/중동','5',seoul_food$kind_name)
seoul_food$kind_name <- gsub('동남아','5',seoul_food$kind_name)
seoul_food$kind_name <- gsub('술집','6',seoul_food$kind_name) 

seoul_food$kind_name <- as.numeric(seoul_food$kind_name)

getColor <- function(seoul_food) {
  sapply(seoul_food$kind_name, function(kind_name) {
    if(kind_name == 1){
      "blue"
    } else if(kind_name == 2){
      "lightred"
    } else {
      if(kind_name==3){
        "beige"
      } else if(kind_name==4){
        "pink"
      } else{
        (ifelse(kind_name==5,"orange","green"))
      }
    }
  }
  )
}

icons <- awesomeIcons(
  markerColor = getColor(seoul_food),
  icon = 'cutlery')



#서울시 지도에 비건 음식점 표시하기####
leaflet() %>% 
  addTiles() %>% 
  setView(lng =126.98, lat=37.541, zoom=11.3) %>% 
  addAwesomeMarkers(data=seoul_food, ~lon, ~lat,icon=icons,
                    label= ~name,
                    popup = ~addr)


#한식/분식(1)=blue, 중국식/일식(2)=lightred
#카페/베이커리(3)=beige, 양식(4)=pink
#인도/동남아(5)=orange
#술집(6)=green

save(seoul_food, "seoul_food.RData")
save(getColor, "getColor.RData")
