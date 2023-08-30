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

        
        df <- data.table("res_name" = NULL , "category" = NULL , "menu" = NULL)
        #새로고침 버튼 누르면 , 출력
        observeEvent(session$userData[["refreshClicked"]](), {

            req(session$userData[["user_id"]]() != 0L)
            
            #Recommend table 출력 후 행개수 추출
            user_id <- session$userData[["user_id"]]()
            sql_his_1 <- glue("select * from recommend where user_id = {user_id} order by id;")
            df_his <- dbGetQuery(con, sql_his_1) |> data.table() #
            cnt<- nrow(df_his) |> as.integer() #행개수 추출
            count <- reactiveVal(cnt)


            # recommend의 테이블 행개수가 0개 초과일때 = 사용자가 새로고침을 눌렀을때
            if(count()>0){
                #recommend 테이블의 res_id를 참조하여 res table 접근 후 음식점 이름 , 카테고리 , 메뉴 추출
                rec_res_id <- df_his[cnt , "res_id"] |> as.integer()
                sql_his_2 <- glue("select res_name , category , menu  from res where id = {rec_res_id};")
                df_res_his <- dbGetQuery(con, sql_his_2) |> data.table()
                    menu_list <- fromJSON(df_res_his$menu) #menu json형태 list화
                    rec_menu_str <- names(menu_list) |> paste(collapse = ",") #메뉴만 추출
                df_res_his$menu <-rec_menu_str
                df <<- rbind(df , df_res_his)

                #추천음식점 기록 개수 제어 변수
                control_num <- 7L
                if(nrow(df)>control_num){
                    df<-tail(df , control_num)
                }
                
                #동적으로 
                output$rec_his_bar <- renderUI({
                    #역순으로 최대 7개 까지만 출력
                    count_seq <- rev(seq_len(min(count(), control_num)))
                    tags <- lapply(count_seq, function(i) {

                        if (i > 2L){
                            tags$div(class = "dynamic-div",
                                    style = "border: 1px solid black; border-radius: 10px; 
                                    background-color: #2F5597; padding: 10px; 
                                    margin-top: 15px; margin-bottom: 15px; margin-left: 10px; margin-right: 10px;",
                                tags$span(
                                    style = "color: white;",
                                    paste("•    ",  df[i,"res_name"] ," / ", df[i,"category"] , "/" , df[i,"menu"] )
                                )
                            )
                        }else{
                            tags$div(class = "dynamic-div sr-only",
                                    style = "border: 1px solid black; border-radius: 10px; 
                                    background-color: #2F5597; padding: 10px; 
                                    margin-top: 15px; margin-bottom: 15px; margin-left: 10px; margin-right: 10px;",
                                tags$span(
                                    style = "color: white;",
                                    paste("•    ",  df[i,"res_name"] ," / ", df[i,"category"] , "/" , df[i,"menu"] )
                                )
                            )
                        }



                    
                    #div를 5개로 제한해서 show하는 코드 -> 시도중
                    # shinyjs::runjs("'.dynamic-div').hide()")
                    # shinyjs::runjs("'.dynamic-div div:nth-child(3)').show()")
                    })

                    
                    
                    tags

                })

                
            }

            
        })
    })
}