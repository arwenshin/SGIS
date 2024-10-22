library(googledrive)

library(lubridate)
library(tidyverse)
library(sf)
library(leaflet)
library(readxl)
library(scales)

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(bslib)
library(bsicons)
library(shinyjs)
library(shinyalert)

library(httr)
library(xml2)
library(jsonlite)

library(survival)
library(randomForestSRC)

# function

load("bdata.RData")

# drive_auth_configure(api_key = "AIzaSyBd2g1dvvs4lCpKHh6T_9PRX4Kf4GbZvEY")
# drive_download(file = "modelShiny.RData", path = "modelShiny.RData", overwrite = TRUE)
load("modelShiny.RData")


A_full = data.frame(gu =predData$SIGNGU_CD_, dong = predData$ADSTRD_CD_,
                    sangkwon = predData$TRDAR_CD_N, sname = predData$상권_구분_코드_명,
                    apart = predData$아파트_단지_수,
                    movingp = predData$총_유동인구_수,livingp = predData$총_상주인구_수,
                    workp = predData$총_직장_인구_수, bighos = predData$종합병원_수,
                    genhos = predData$일반_병원_수,
                    pharmarcy = predData$약국_수, kindergarden = predData$유치원_수,
                    school = predData$학교_수, univ = predData$대학교_수,
                    department = predData$백화점_수, cinema = predData$극장_수,
                    airport = predData$공항_수,
                    busTerminal = predData$버스_터미널_수, subway = predData$지하철_역_수,
                    busStation = predData$버스_정거장_수,
                    change = predData$선행종합지수전년동월비_누적변동,
                    open3 = predData$개업업체수_개업_3개월, open6 = predData$개업업체수_개업_6개월,
                    open1 = predData$개업업체수_개업_1년, close3 = predData$폐업업체수_개업_3개월,
                    close6 = predData$폐업업체수_개업_6개월, close1 = predData$폐업업체수_개업_1년,
                    compete_kor = predData$경쟁업체수_한식, compete_jap =  predData$경쟁업체수_일식,
                    compete_chin =  predData$경쟁업체수_중식, compete_west =  predData$경쟁업체수_경양식)

# 선행종합지수
indexRate = read_xlsx("경기종합지수_2020100__10차__20240914212854.xlsx")
indexRate$시점 <- ym(indexRate$시점)
indexRate = indexRate[,c(1,3,6)]
colnames(indexRate) = c("시점", "선행종합지수전월비", "선행지수전년동월비")

# 검색용 행정동 코드
newcode <- read_xlsx( "adm_code.xlsx")
newcode <- newcode %>% filter(시도명칭 == "서울특별시")
predData <- left_join(predData, newcode %>% select(시군구명칭, 읍면동명칭, 행정구역코드),
                      by = c("ADSTRD_CD_" = "읍면동명칭", "SIGNGU_CD_" = "시군구명칭"))

# 유동인구
population <- read.csv("서울시 상권분석서비스(길단위인구-행정동).csv")
population <-population %>% filter(기준_년분기_코드 == 20242) %>%
  select(행정동_코드, 총_유동인구_수)


# ui
header <- dashboardHeader(title = "서울시상권추천시스템",
                          dropdownMenu( type = 'message',
                                        #customSentence = customSentence,
                                        messageItem(
                                          from = "Seowoo Jung",#'Feedback and suggestions',
                                          message =  "Send me email if there's any error",
                                          icon = icon("envelope"),
                                          href = "mailto:jsw1347@ewhain.net"
                                        ),
                                        messageItem(
                                          from = "Seonmin Sin",#'Feedback and suggestions',
                                          message =  "",
                                          icon = icon("envelope"),
                                          href = "mailto:arwenshin@ewhain.net"
                                        ), icon = icon("comment")))
#, icon = icon('comment')

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Input data", tabName = "상권 위치 추천",   
             selectInput("type_input", "업종", c("한식" = "한식",
                                               "일식" = "일식",
                                               "중국식" = "중국식",
                                               "경양식" = "경양식")),
             numericInput("initmoney_input",  tooltip(trigger = list("예상 월 임대료(원)",
                                                                     bsicons::bs_icon("info-circle")),
                                                      "Tooltip message"), value = 900000), 
             numericInput("size_input", tooltip(trigger = 
                                                  list(paste0("공간규모(m\u00B2)"),
                                                       bsicons::bs_icon("info-circle")),
                                                "예상 임대료를 입력하세요",  id = "tip"), value=20), ## 변경
             radioButtons("areatype_input", "건물규모", 
                          choices = list("소형" = 3, "중대형" = 2, "집합상가" = 4)),
             radioButtons("firstFloor_input", "건물 1층 여부", 
                          choices = list("1층" = 1, "지하, 2층 이상" = 0)),
             radioButtons("franchise_input", "프랜차이즈 여부", 
                          choices = list("프랜차이즈" = 1, "개인 사업" = 0)),
             actionButton("go", "Go", icon = icon("magnifying-glass"), width = "100px"),
             startExpanded = TRUE)
    
  )
)

body <- dashboardBody(
  tabItem(tabName = "상권추천시스템", 
          fluidPage(column(12,
                           box(title= tooltip(trigger = list("도움말",
                                                             bsicons::bs_icon("info-circle")),
                                              "Tooltip message"), 
                               status="danger",
                               collapsible = TRUE,
                               width=12, collapsed = TRUE,
                               fluidRow(
                                 column(12,
                                        h2("상권추천시스템 소개", 
                                           style = "text-align: center; font-weight: bold; color: #2E8B57;"),
                                        hr(),
                                        h3("- 서울시 상권 추천 시스템은 서울시 내 다양한 상권 정보를 활용하여, 
    사용자가 설정한 조건에 적합한 상권을 효과적으로 추천합니다. 
    예상 임대료 이하의 상권 중에서, 3년 이상 생존할 확률이 높은 지역을 손쉽게 조회할 수 있습니다.", 
                                           style = "line-height: 1.5;"),
                                        h3("- 본 서비스는 통계적 추정을 기반으로 생존 확률을 계산하여, 
    상권 분석에 있어 참고 자료로 활용될 수 있습니다.", 
                                           style = "line-height: 1.5;"),
                                        h2("상권추천시스템 이용법",
                                           style = "text-align: center; font-weight: bold; color: #2E8B57;"),
                                        hr(),
                                        h3("1. 왼쪽 Input data에 원하는 조건을 입력한 후 \'Go\'버튼을 누르세요."),
                                        h3("2. 해당 조건 중, 예상 임대료 내에서 3년이상 생존할 확률이 높은 상권을 조회할 수 있습니다."),
                                        h3("3. 오른쪽 list에서 행을 선택하면 원하는 상권의 정보를 조회하고 비교해볼 수 있습니다."),
                                        h3("4. 예상 임대료 이하의 식당이 존재하지 않은 경우 상권은 조회되지 않습니다."),
                                        h4("더 자세한 정보가 궁금하시면, 아래 링크로 들어가서 해당 시군구/동의 관련 정보를 조회해보실수 있습니다.",
                                           style = "line-height: 5;"),
                                        column(6,actionButton('lnk1', "More Info",onclick="location.href='https://sgis.kostat.go.kr/view/bizStats/bizStatsMap?biz=0';"))
                                        
                                 )))))),
  
  tags$head(
    # SGIS API 불러오기 (API 키 필요)
    tags$script(src = "https://sgisapi.kostat.go.kr/OpenAPI3/auth/javascriptAuth?consumer_key=da9ac618dac74b09ab8a")
  ),
  
  fluidRow(column(width = 12, box(htmlOutput("map"), width = 5),
                  tabBox(width = 7,
                         title = tagList(shiny::icon("check"), "3년 생존확률"),
                         tabPanel("상권별 생존확률", DTOutput("top3List")),
                         tabPanel("생존확률 분포", plotlyOutput("hazardFunction"))
                  ))),
  fluidRow(column(width = 12,
                  valueBoxOutput("diffmonth",width=4),
                  valueBoxOutput("diffyear",width=4),
                  valueBoxOutput("trend",width=4))),
  
  fluidRow(column(width=12,
                  tabBox(width = 6,
                         title = tagList(shiny::icon("arrow-trend-up"), "주변 상권 추세"),
                         #tabPanel("사업체 비율",plotlyOutput("company")),
                         tabPanel("개업 추세", plotlyOutput("open")),
                         # tabPanel("동일업종개수", plotlyOutput("compete")),
                         tabPanel("폐업 추세", plotlyOutput("close"))),
                  tabBox(width = 6,
                         title = tagList(shiny::icon("chart-simple"), "인구 정보"),
                         tabPanel("유동인구", plotlyOutput("moving")),
                         tabPanel("직장인구", plotlyOutput("job")),
                         tabPanel("거주인구", plotlyOutput("living")))
  ))
)

ui <- dashboardPage(header, sidebar, body, skin = "green")

# server

server <- function(input, output, session){
  # initial value
  input_money = NA; franchise = NA; firstFloor = NA; area = NA; areatype = NA
  observeEvent(input$go,{
    tryCatch({
      type = factor(input$type_input, level = levels(modelDataRF$type));  
      input_money = as.numeric(input$initmoney_input); 
      franchise = as.numeric(input$franchise_input);
      firstFloor = as.numeric(input$firstFloor_input);
      area = as.numeric(input$size_input);
      areatype = as.numeric(input$areatype_input);
      
      month = ifelse(month(Sys.Date()) %in% c(3,4,10,11), 1, 0)
      A_full <- A_full %>% mutate(franchise = franchise, firstFloor = firstFloor,
                                  month = month, area = area, type = type)
      A_full$money = predData[,areatype]
      
      if (type == "한식"){
        A_full <- A_full %>% mutate(compete = compete_kor) %>% select(-c(compete_kor:compete_west))
        
      } else if(type == "일식"){
        A_full <- A_full %>% mutate(compete = compete_jap) %>% select(-c(compete_kor:compete_west))
        
      } else if (type == "중식"){
        A_full <- A_full %>% mutate(compete = compete_chin) %>% select(-c(compete_kor:compete_west))
        
      } else{
        A_full <- A_full %>% mutate(compete = compete_west) %>% select(-c(compete_kor:compete_west))
        
      }
      
      A_full = A_full %>% mutate(dong = as.factor(dong), gu = as.factor(gu),
                                 sangkwon = as.factor(sangkwon), sname = as.factor(sname))
      
      A_full <- A_full %>% mutate(total = money*area) %>% 
        filter(total <= (input_money/1000)) %>% select(-total)
      
      if (nrow(A_full) == 0){
        shinyalert("오류!", "해당 조건에 맞는 예상임대료 이하 상권이 존재하지 않습니다.", type = "error")
      }else{
        preds_new <- predict(rfmodel, newdata = A_full)
        
        time_set = 3
        time_index = which(preds_new$time.interest > time_set)[1]
        top_index = order(preds_new$survival[,time_index], decreasing = TRUE)
        
        
        list_top3 = A_full[top_index[1:3],1:4]
        list_top3$rank = 1:3
        list_top3 = list_top3[,c(5,1:4)]
        list_top3$prob <- round(sort(preds_new$survival[,time_index], decreasing = TRUE)[1:3],3)
        
        # 데이터 전부 수집
        
        info <- left_join(A_full[top_index[1:3],], predData[,c(1,4:10, 124)], 
                          by = c("sangkwon" = "TRDAR_CD_N"))
        dongCode <- info$행정구역코드 # 행정구역코드
        
        # 모든 데이터
        list_all = A_full[top_index,1:4]
        list_all$rank = 1:nrow(A_full)
        list_all = list_all[,c(5,1:4)]
        list_all$prob <- round(sort(preds_new$survival[,time_index], decreasing = TRUE),3)
        
        # info_all <- left_join(A_full[top_index,], predData, by = c("sangkwon" = "TRDAR_CD_N"))
        # dongCode <- info$행정구역코드 # 행정구역코드
        
        # 시각화할 표
        list_top3 <- as.data.frame(list_top3)
        colnames(list_top3) <- c("순위", "구", "동", "상권", "상권 종류", "생존 확률")
        
        output$top3List <- renderDT({
          datatable(list_top3, rownames = FALSE, escape = FALSE, 
                    selection = "none")
        }) 
        
        
        # hazard function
        plotData <- data.frame(순위 = as.factor(rep(1:3, each = 150)), 
                               time = rep(preds_new$time.interest, 3), 
                               survfunction = c(t(preds_new$survival[top_index[1],]), 
                                                t(preds_new$survival[top_index[2],]), 
                                                t(preds_new$survival[top_index[3],])))
        
        output$hazardFunction <- renderPlotly({
          label <- list_top3 %>% mutate(inx = paste(list_top3$순위, paste(paste(list_top3$구, list_top3$동),
                                                                        list_top3$상권), sep = "-")) 
          label <- label$inx
          
          p <- ggplot(plotData, aes(time, survfunction, color = 순위, group = 순위))+ 
            geom_line(aes(
              text = paste("상권정보:",
                           ifelse(순위 == 1, label[1],
                                  ifelse(순위 == 2, label[2], label[3])),
                           "<br>시간:", round(time,3),
                           "<br>생존 확률:", round(survfunction, 4)))) + 
            geom_vline(xintercept = 3, color = "orange") + 
            ylab("생존 확률") + xlab("시간 (년)") +
            scale_color_manual(values = c("1" = "red", "2" = "green", "3" = "blue"),
                               labels = c("1" = label[1], "2" = label[2], "3" = label[3])) +
            theme_minimal()
          if (nrow(A_full) == 0){p} else{ggplotly(p, tooltip = "text")}
          
        })
        
        
        output$map <- renderUI({
          # map
          predData_sf <- st_as_sf(info %>% select(-상권_코드, -행정구역코드))  
          
          #지도에 시각화 할 수 있도록 위경도좌표계로 변환 
          predData_sf <- st_transform(predData_sf,crs = 5179)
          geojson_data <- geojson_json(predData_sf)
          
          tags$div(
            id = "map",
            style = "width:100%; height:400px;",
            tags$script(HTML(
              paste0("
              
          // 기존 지도 초기화
          if (window.map) {
            map.remove();
          }
          
          // 지도 객체 생성
          var map = sop.map('map');
          
          // 지도 중심 좌표 설정 (서울시)
          map.setView(sop.utmk(953820, 1951837), 5);
      

          // GeoJSON 데이터를 지도에 추가
          var geojsonData = ", geojson_data, ";
    
          // geojson 데이터를 지도에 추가
          var polygonLayer = sop.geoJson(geojsonData).addTo(map);
  "))))
          
        })
        
        
        # api : 직장인구, 유동인구, 상주인구
        
        response <- GET("https://sgisapi.kostat.go.kr/OpenAPI3/auth/authentication.json?consumer_key=a5402cc8d3744b3d874c&consumer_secret=83ec79e6727d4183bbdb")
        data_xml <- content(response, as = "text", encoding = "utf-8")
        parsed_data <- fromJSON(data_xml)
        accessToken <-  parsed_data$result$accessToken
        
        ## 거주인구(동)
        url_live <- paste0(
          "https://sgisapi.kostat.go.kr/OpenAPI3/startupbiz/Compareregiontotal.json?",
          "accessToken=", accessToken, "&",
          "first_adm_cd=", dongCode[1], "&",
          "second_adm_cd=", dongCode[2], "&",
          "third_adm_cd=", dongCode[3], "&",
          "ppl_type=", 1, "&",
          "ppl_val=", 2
        )
        data_xml <- content(GET(url_live), as = "text", encoding = "utf-8")
        parsed_data <- fromJSON(data_xml)
        info$resident <- parsed_data$result$staypeople$resultdata[[1]]$value[2:4]
        
        ## 직장인구(동)
        url_live <- paste0(
          "https://sgisapi.kostat.go.kr/OpenAPI3/startupbiz/Compareregiontotal.json?",
          "accessToken=", accessToken, "&",
          "first_adm_cd=", dongCode[1], "&",
          "second_adm_cd=", dongCode[2], "&",
          "third_adm_cd=", dongCode[3], "&",
          "ppl_type=", 2, "&",
          "ppl_val=", 2
        )
        data_xml <- content(GET(url_live), as = "text", encoding = "utf-8")
        parsed_data <- fromJSON(data_xml)
        info$job <- parsed_data$result$jobpeople$resultdata[[1]]$value[2:4]
        
        ## 유동인구 (동)
        population$행정동_코드 <- as.character(population$행정동_코드)
        colnames(population) <- c("행정동_코드", "popnum_Dong")
        info <- left_join(info,population, by = c("ADSTRD_CD"= "행정동_코드"))
        
        colnames(info)[1:4] <- c("시군구", "동", "상권", "상권이름")
        colnames(info)[43:45] <- c("거주인구수", "직장인구수", "유동인구수")
        
        output$moving <- renderPlotly({
          p <- info %>% mutate(동 = factor(동, levels = c(info$동))) %>% 
            ggplot(aes(동, 유동인구수, fill=동)) + 
            geom_bar(aes(text = paste("동:", 동,"<br>유동인구수:", label_number(big.mark = ",")(유동인구수), "명")), stat = "identity") +
            geom_text(aes(label = label_number(big.mark = ",")(유동인구수)), vjust = -0.7) +
            scale_fill_manual(values = c("red", "green", "blue")) +
            xlab("동") + ylab("유동인구 수")+
            labs(title = "동단위 유동인구 수 (명)", subtitle = "2023년 4분기 기준") +
            theme_minimal()
          if (nrow(A_full) == 0){NULL} else{ggplotly(p, tooltip = "text")}
        })
        
        output$living <- renderPlotly({
          p <- info %>% mutate(동 = factor(동, levels = c(info$동))) %>% 
            ggplot(aes(동, 거주인구수, fill=동)) + 
            geom_bar(aes(text = paste("동:", 동,"<br>거주인구수:", label_number(big.mark = ",")(거주인구수), "명")),stat = "identity") +
            scale_fill_manual(values = c("red", "green", "blue")) +
            geom_text(aes(label = label_number(big.mark = ",")(거주인구수)), vjust = -0.5)  +
            xlab("동") + ylab("거주인구 수")+
            labs(title = "동단위 거주인구 수 (명)", subtitle = "2022년 11월 기준") +
            theme_minimal()
          if (nrow(A_full) == 0){NULL} else{ggplotly(p, tooltip = "text")}
        })    
        
        output$job <- renderPlotly({
          p <- info %>% mutate(동 = factor(동, levels = c(info$동))) %>% 
            ggplot(aes(동, 직장인구수, fill=동)) + 
            geom_bar(aes(text = paste("동:", 동,"<br>직장인구수:", label_number(big.mark = ",")(직장인구수), "명")),stat = "identity") +
            scale_fill_manual(values = c("red", "green", "blue")) +
            geom_text(aes(label = label_number(big.mark = ",")(직장인구수)), vjust = -0.5) +
            xlab("동") + ylab("직장인구 수")+
            labs(title = "동단위 직장인구 수 (명)", subtitle = "2022년 11월 기준") +
            theme_minimal()
          if (nrow(A_full) == 0){NULL} else{ggplotly(p, tooltip = "text")}
          
        })
        
        ## 주변 상권 추세
        # 사업체 비율
        # propCompany <- data.frame(동 = NULL, 년도 = NULL, num = NULL)
        # for (j in 1:3){
        #   url_live <- paste0(
        #     "https://sgisapi.kostat.go.kr/OpenAPI3/startupbiz/corpindecrease.json?",
        #     "accessToken=", accessToken, "&",
        #     "adm_cd=", dongCode[j]
        #   )
        #   data_xml <- content(GET(url_live), as = "text", encoding = "utf-8")
        #   parsed_data <- fromJSON(data_xml)
        #   
        #   num <- NULL
        #   type1 <- ifelse(type == "경양식", "서양식", type)
        #   for (i in 1:16){
        #     A = parsed_data$result$theme_list[[i]]
        #     num <- c(num, A[A$theme_nm == type1,"corp_cnt"])
        #   }
        # 
        #   dong = c(propCompany$동 , rep(as.character(info$동[j]),16))
        #   yearList = c(propCompany$year , parsed_data$result$year[1:16])
        #   numList = c(propCompany$num , num)
        #   propCompany <- data.frame(동 = dong, 년도 = yearList, num =numList)
        #   
        # }
        # 
        # output$company <- renderPlotly({
        #   propCompany %>% mutate(동 = factor(동, levels = unique(propCompany$동))) %>% 
        #     ggplot(aes(년도, num, color=동)) + 
        #     geom_line(aes(group =  동)) +
        #     geom_point() +
        #     scale_fill_manual(values = c("red", "green", "blue")) +
        #     xlab("연도 (년)") + ylab("동일 사업체 수")+
        #     labs(title = "동단위 동일 사업체 수 (개)", subtitle = "2022년 11월 기준") +
        #     theme_minimal()
        # })
        
        # 개업업체 수
        openData <- info %>% select(상권, open3:open1)
        openData <- openData %>% pivot_longer(cols = c("open3", "open6","open1"),
                                              names_to = "시점", values_to = "value") %>%
          mutate(시점 = rep(c("3개월 전", "6개월 전", "1년전"),3))
        
        
        
        output$open <- renderPlotly({
          p <- openData %>% mutate(상권 = factor(상권, levels = unique(openData$상권)),
                                   시점 = factor(시점, levels = c("3개월 전", "6개월 전", "1년전"))) %>% 
            ggplot(aes(시점, value, color=상권)) + 
            geom_line(aes(group =  상권), show.legend = TRUE) +
            geom_point(show.legend = TRUE) +
            scale_fill_manual(values = c("red", "green", "blue"), 
                              labels = levels(openData$상권)) +
            xlab("시점 (년)") + ylab("개업업체 수")+
            labs(title = "상권단위 개업 업체 수 (개)", subtitle = "2024년 5월 기준") +
            theme_minimal() 
          if (nrow(A_full) == 0){NULL} else{p}
        })
        
        # 폐업업체수
        closeData <- info %>% select(상권, close3:close1)
        closeData <- closeData %>% pivot_longer(cols = c("close3", "close6","close1"),
                                                names_to = "시점", values_to = "value") %>%
          mutate(시점 = rep(c("3개월 전", "6개월 전", "1년전"),3))
        
        
        
        output$close <- renderPlotly({
          p <- closeData %>% mutate(상권 = factor(상권, levels = unique(openData$상권)),
                                    시점 = factor(시점, levels = c("3개월 전", "6개월 전", "1년전"))) %>% 
            ggplot(aes(시점, value, color=상권)) + 
            geom_line(aes(group =  상권)) +
            geom_point() +
            scale_fill_manual(values = c("red", "green", "blue")) +
            xlab("시점 (년)") + ylab("폐업업체 수")+
            labs(title = "상권단위 폐업 업체 수 (개)", subtitle = "2024년 5월 기준") +
            theme_minimal()
          if (nrow(A_full) == 0){NULL} else{p}
        })
        
        
        
        
        ## 평당 임대료
        
        # output$money <- renerPlot({
        #   info %>% mutate(dong = factor(dong, levels = c(info$dong)),
        #                   middle_rent = middle_rent*1000,
        #                   small_rent = small_rent *1000,
        #                   densed_rent = densed_rent*1000) %>% 
        #     select(dong, middle_rent:densed_rent) %>%
        #     pivot_longer(cols = c(middle_rent, small_rent,densed_rent), names_to = "size") %>%
        #     mutate(size = factor(size, levels = c("small_rent", "middle_rent", "densed_rent"))) %>%
        #     ggplot(aes(size, value, fill=dong)) + 
        #     geom_bar(stat = "identity", position="dodge") +
        #     geom_text(aes(label = round(value)), vjust = -0.5,position = position_dodge(width = 0.9)) +
        #     ggtitle("평당 임대료 (원)")
        # })
        
      }
      
      
      
      
    },error = function(e) {})})
  
  # 선행종합지수 - 경기추세
  output$diffmonth <- renderValueBox({
    valueBox(paste(indexRate[134,2], "(%)"), h4("경기선행지수(전월비)"), color = "purple")       
  })
  
  output$diffyear <- renderValueBox({
    valueBox(paste(indexRate[134,3], "(%)"), h4("경기선행지수(전년동월비)"), color = "orange")
  })
  output$trend <- renderValueBox({
    valueBox("상승세", h4("building"), color = "green", icon = icon("fa-regular fa-face-smile"))
  })
  
}

shinyApp(ui=ui, server = server)



