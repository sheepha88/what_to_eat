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
            "select res_name, menu , date_visit , participants 
            from review inner join res on review.res_id = res.id
            where date_visit in ({week_dates}) order by date_visit ;"
        )
        
        df_reivew <- dbGetQuery(con , sql_review_table) |> data.table()

        
        df_reivew$menu <- sapply(menuToList(table = df_reivew) , function(x){paste(x , collapse = ",")})
        print(df_reivew)
        


        #이번주의 식사기록 df_review 이용
        output$week_his_bar <- renderUI({
            #역순으로 최대 7개 까지만 출력
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
        })

        

        






    })
}