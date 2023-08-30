# UI --------------------------------------------------------------------------------------------- #
res_his_UI <- function(id){
    ns <- NS(id)
    
    fluidPage(
        id = id,

        uiOutput(ns("rec_his_bar"))

    )
}


# server ----------------------------------------------------------------------------------------- #
res_his_Server <- function(id , parent){
    moduleServer(id , function(input , output , session){
        ns <- session$ns

        #새로고침 버튼 누르면 , 출력
        observeEvent(session$userData[["refreshClicked"]](), {

            req(session$userData[["user_id"]]()!=0L)

            #Recommend table 출력 후 행개수 추출
            user_id <- session$userData[["user_id"]]()
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

                        tagList(

                        fluidRow(
                            class = "dynamic-div pe-0",
                            style = "border: 1px solid black; border-radius: 10px; 
                            background-color: #2F5597; padding: 10px; 
                            margin-top: 10px; margin-bottom: 10px;  margin-right: 10px;",
                            fluidRow(
                                column(
                                    width = 12,
                                    class = "text-white",
                                    paste("•    ",  df_his[i,"res_name"] ," / ", df_his[i,"category"] , "/" , df_his[i,"menu"] )
                                )
                            ),
                            fluidRow(
                                class = "mt-1 me-0 pe-0 text-end",
                                column(
                                    width = 6,
                                    offset = 6,
                                    class = "pe-0",
                                    actionLink(
                                        inputId = ns(paste0("rec_modal_",i)),
                                        label = "자세히보기",
                                        class = "pe-1 text-beige",
                                        onclick = glue("viewDetail({res_id})")
                                    ),
                                    actionLink(
                                        inputId = ns(paste0("go_review_",i)),
                                        label = "리뷰쓰기",
                                        class = "text-beige",
                                        onclick = glue("viewReview({res_id})")
                                    )
                                )


                                # column(
                                #     width = 3,
                                #     style = "width:50%",
                                #     actionLink(
                                #         inputId = ns(paste0("rec_modal_",i)),
                                #         label = "자세히보기",
                                #         onclick = glue("viewDetail({res_id})")
                                #     )
                                # ),
                                # column(
                                #     width = 3,
                                #     style = "width:50%",
                                #     actionLink(
                                #         inputId = ns(paste0("go_review_",i)),
                                #         label = "리뷰쓰기",
                                #         onclick = glue("viewReview({res_id})")
                                #     )
                                # )
                            )

                            
                        )

                        )

                        
                    })

                    tagList(
                        # recommendation history
                        recList,

                        # javascript function to pass resId_자세히보기
                        singleton(tags$script(HTML(glue(
                            'function viewDetail(resId){
                                Shiny.setInputValue(\"{{ns("modalResId")}}\", resId);
                                document.getElementById(\"{{ns("dummyButtonViewDetail")}}\").click();
                            }', .open = "{{", .close="}}"
                        )))),

                        # javascript function to pass resId_리뷰쓰기
                        singleton(tags$script(HTML(glue(
                            'function viewReview(resId){
                                Shiny.setInputValue(\"{{ns("ReviewResId")}}\", resId);
                                document.getElementById(\"{{ns("dummyButtonReview")}}\").click();
                            }', .open = "{{", .close="}}"
                        )))),

                        # dummy button
                        ##자세히보기 버튼
                        actionButton(
                            inputId = ns("dummyButtonViewDetail"),
                            label = "DUMMY button",
                            class = "sr-only"
                        ),
                        ##리뷰쓰기 버튼
                        actionButton(
                            inputId = ns("dummyButtonReview"),
                            label = "DUMMY button",
                            class = "sr-only"
                        ),
                        
                    )

                    # tags$div(

                    #     # recommendation history
                    #     recList,

                    #     # javascript function to pass resId_자세히보기
                    #     singleton(tags$script(HTML(glue(
                    #         'function viewDetail(resId){
                    #             Shiny.setInputValue(\"{{ns("modalResId")}}\", resId);
                    #             document.getElementById(\"{{ns("dummyButtonViewDetail")}}\").click();
                    #         }', .open = "{{", .close="}}"
                    #     )))),

                    #     # javascript function to pass resId_리뷰쓰기
                    #     singleton(tags$script(HTML(glue(
                    #         'function viewReview(resId){
                    #             Shiny.setInputValue(\"{{ns("ReviewResId")}}\", resId);
                    #             document.getElementById(\"{{ns("dummyButtonReview")}}\").click();
                    #         }', .open = "{{", .close="}}"
                    #     )))),

                    #     # dummy button
                    #     ##자세히보기 버튼
                    #     actionButton(
                    #         inputId = ns("dummyButtonViewDetail"),
                    #         label = "DUMMY button",
                    #         class = "sr-only"
                    #     ),
                    #     ##리뷰쓰기 버튼
                    #     actionButton(
                    #         inputId = ns("dummyButtonReview"),
                    #         label = "DUMMY button",
                    #         class = "sr-only"
                    #     ),
                    # )
                })
            }

        })


        

        #자세히보기를 누르면 숨겨진 통합버튼을 클릭
        observeEvent(input$dummyButtonViewDetail, {
            showModal(
                # modal UI
                modal_ui_file(
                    session = session,
                    rec_res_id = as.integer(input$modalResId)
                )
            )

        })


        #리뷰쓰기
        observeEvent(input$dummyButtonReview,{   
            go_review_server(session = parent, review_res_id = as.integer(input$ReviewResId) )
        })
    })
}