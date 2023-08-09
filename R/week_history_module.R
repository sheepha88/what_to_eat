# UI --------------------------------------------------------------------------------------------- #
week_his_UI <- function(id){
    ns <- NS(id)
    fluidPage(
        id = id,
        fluidRow(
            uiOutput(ns("week_his_bar"))
        )    
    )
}


# server ----------------------------------------------------------------------------------------- #
week_his_Server <- function(id){
    moduleServer(id , function(input , output , session){
        ns <- session$ns
        #review table 출력

        ##오늘 날짜가 포함된 주의 날짜들을 출력
        ###오늘 날짜
        today <- as.Date(lubridate::now() |> format(x =_, "%Y-%m-%d"))
        ###오늘 날짜가 포함된 주의 첫날 : 일요일
        first_day_of_week <- floor_date(today, "week")
        ###오늘 날짜가 포함된 주의 첫날 :토요알
        last_day_of_week <- ceiling_date(today, "week") - days(1)
        ### 주의 날짜들
        week_dates <- seq(first_day_of_week, last_day_of_week, by = "day") 
        week_dates <- paste("'", week_dates, "'", collapse = ", ")
        print(week_dates)
        
        ## review table 중 주의 날짜들이 포함되는 날짜 , 참여자 , 음식점_id를 출력
        sql_review_table <- glue(
            "SELECT res_name, menu , date_visit , participants 
            FROM review INNER JOIN res ON review.res_id = res.id
            WHERE date_visit IN ({week_dates}) ORDER BY date_visit ;"
        )
        
        df_reivew <- dbGetQuery(con , sql_review_table) |> data.table()
        df_reivew$menu <- unlist(menuToList(table = df_reivew))

        

        #이번주의 식사기록 df_review 이용
        output$week_his_bar <- renderUI({
            # 오늘날짜애 대한 그 주의 기록이 있다면 출력
            ##역순으로 최대 7개 까지만 출력
            if(nrow(df_reivew)!=0L){
                count_seq <- rev(seq(nrow(df_reivew)))
                tags <- lapply(count_seq, function(i) {
                    tags$div(class = "dynamic-div",
                            style = "border: 1px solid black; border-radius: 10px; 
                            background-color: #4574C4; padding: 10px; 
                            margin-top: 15px; margin-bottom: 15px; margin-left: 10px; margin-right: 10px;",
                        tags$span(
                            style = "color: white;",
                            paste(
                                "•    ",  df_reivew[i,"date_visit"] ," : ", df_reivew[i,"res_name"] ,
                                "/" , df_reivew[i,"menu"] 
                            ),
                            tags$br(),
                            "참석자 : " , df_reivew[i,"participants"]
                        )
                    )
                })
            }else{
                # 오늘날짜애 대한 그 주의 기록이 없다면 "기록없음" 출력
                tags$div(class = "dynamic-div",
                            style = "border: 1px solid black; border-radius: 10px; 
                            background-color: #4574C4; padding: 10px; 
                            margin-top: 15px; margin-bottom: 15px; margin-left: 10px; margin-right: 10px;",
                        tags$span(
                            style = "color: white;",
                            paste(
                                "•    ","식사가록이 없습니다."
                            ),
                        )
                )
            }
        })
    })
}