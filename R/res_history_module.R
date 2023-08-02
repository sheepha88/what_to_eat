# UI --------------------------------------------------------------------------------------------- #
res_his_UI <- function(id){
    ns <- NS(id)
    
    fluidPage(
        id = id,
        fluidRow(
                uiOutput(ns("rec_his_bar"))
        )    
    )
}


# server ----------------------------------------------------------------------------------------- #
res_his_Server <- function(id){
    moduleServer(id , function(input , output , session){
        ns <- session$ns

        #새로고침 버튼 누르면 , 출력
        observeEvent(session$userData[["refreshClicked"]](), {

            #Recommend table 출력 후 행개수 추출
            user_id <- session$userData[["user_id"]]
            sql_his_1 <- glue("
                SELECT A.created, A.user_id, B.res_name, B.category, JSON_KEYS(B.menu) menu
                FROM recommend A
                LEFT JOIN res B ON A.res_id = B.id
                WHERE A.user_id = {user_id};
            ")

            # Query 내용 dataframe화 , 7개까지만 출력
            n_recs <- 7L
            df_his <- dbGetQuery(con, sql_his_1) |>
                as.data.table() |>
                tail( n_recs)

            # menu json -> string
            tmp_menu <- gsub("[", replacement = "", x = df_his$menu, fixed = TRUE) |> 
                gsub("]", replacement = "", x = _, fixed = TRUE) |> 
                gsub('"', replacement = "", x = _, fixed = TRUE)
            
            # df_his 메뉴컬럼에 string 화 된 menu 삽입
            df_his[,  menu := tmp_menu]

            cnt<- nrow(df_his)  #행개수 추출
            count <- reactiveVal(cnt)


            # recommend의 테이블 행개수가 0개 초과일때 = 사용자가 새로고침을 눌렀을때
            if(count()>0){
                
                #동적으로 
                output$rec_his_bar <- renderUI({
                    #역순으로 최대 7개 까지만 출력
                    
                    tags <- lapply( rev(seq_len(count())), function(i) {
                        tags$div(class = "dynamic-div",
                                style = "border: 1px solid black; border-radius: 10px; 
                                background-color: #2F5597; padding: 10px; 
                                margin-top: 15px; margin-bottom: 15px; margin-left: 10px; margin-right: 10px;",
                            tags$span(
                                style = "color: white;",
                                paste("•    ",  df_his[i,"res_name"] ," / ", df_his[i,"category"] , "/" , df_his[i,"menu"] )
                            )
                        )
                    
                    #div를 5개로 제한해서 show하는 코드 -> 시도중
                    # shinyjs::runjs("'.dynamic-div').hide()")
                    # shinyjs::runjs("'.dynamic-div div:nth-child(3)').show()")
                    })
                })
            }
        })
    })
}