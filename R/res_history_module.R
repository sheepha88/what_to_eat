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
res_his_Server <- function(id , parent){
    moduleServer(id , function(input , output , session){
        ns <- session$ns

        #새로고침 버튼 누르면 , 출력
        observeEvent(session$userData[["refreshClicked"]](), {

            #Recommend table 출력 후 행개수 추출
            user_id <- session$userData[["user_id"]]
            sql_his_1 <- glue("
                SELECT A.res_id, A.created, A.user_id, B.res_name, B.category, JSON_KEYS(B.menu) menu
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
                    
                    recList <- lapply( rev(seq_len(count())), function(i) {

                        res_id <- df_his[i, "res_id"]

                        fluidRow(
                            class = "dynamic-div",
                            style = "border: 1px solid black; border-radius: 10px; 
                            background-color: #2F5597; padding: 10px; 
                            margin-top: 10px; margin-bottom: 10px;  margin-right: 10px;",
                            column(
                                width = 8,
                                tags$div(
                                style = "color: white;",
                                paste("•    ",  df_his[i,"res_name"] ," / ", df_his[i,"category"] , "/" , df_his[i,"menu"] )
                                )   
                            ),
                            column(
                                width = 2,
                                tags$div(
                                    tags$h4( style = "font-size:15px; margin-left: 10px;" ,
                                        actionLink(
                                            inputId = ns(paste0("rec_modal_",i)) ,
                                            label = "자세히보기",
                                            class = "modal_class",
                                            onclick = glue("viewDetail({res_id})")
                                        )
                                    )
                                )
                            ),
                            column(
                                width = 2,
                                tags$div(
                                    actionLink(inputId = ns(paste0("go_review_",i)) , label = "리뷰쓰기")
                                )
                            )
                        )
                    })

                    tags$div(

                        # recommendation history
                        recList,

                        # javascript function to pass resId
                        singleton(tags$script(HTML(glue(
                            'function viewDetail(resId){
                                Shiny.setInputValue(\"{{ns("modalResId")}}\", resId);
                                document.getElementById(\"{{ns("dummyButtonViewDetail")}}\").click();
                            }', .open = "{{", .close="}}"
                        )))),

                        # dummy button
                        actionButton(
                            inputId = ns("dummyButtonViewDetail"),
                            label = "DUMMY button",
                            class = "sr-only"
                        ),
                    )
                    

                })
            }

        })

        observeEvent(input$dummyButtonViewDetail, {
            # print(input$modalResId)
            showModal(
                # modal UI
                modal_ui_file(
                    session = session,
                    rec_res_id = as.integer(input$modalResId)
                )
            )

        })



        observeEvent(input$rec_modal,{   
            showModal(
                # modal UI
                modal_ui_file(
                    output = output,
                    session = session,
                    rec_res_id = recommend_res_info()[["res_id"]]
                )
            )
        })

        # go_review_server(input , parent)
    })
}