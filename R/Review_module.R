# UI --------------------------------------------------------------------------------------------- #
review_UI <- function(id ){
    ns <- NS(id)
    fluidPage(
        id = id,
        tags$div(style = "display: flex; justify-content: center; align-items: center;",  # 가운데 정렬 스타일 적용
            fluidRow(style = "width: 500px; background : white; border:2px solid #E8EBEE; border-radius: 10px;",
                tags$h3(HTML("&nbsp;평점 & 리뷰작성") , style = "font-weight: bold;",class = "mt-2 mb-2"),
                tags$div(
                    class = "part_line",
                    fluidRow(
                        column(
                            width = 12,
                            tags$h4(
                                textOutput(outputId = ns("review_res_name_output")),
                                style = "text-align:center;"
                            )
                        )
                    )
                ),
                tags$div(
                    class = "part_line",
                    fluidRow(
                        column(
                            width = 3,
                            style = "margin-top:5px ;",
                            "방문일"
                        ),
                        column(
                            width = 9,
                            dateInput(inputId = ns("date_visit") , label = NULL , value = NULL , format = "yyyy-mm-dd")
                        )
                    ),
                    fluidRow(
                        column(
                            width = 3,
                            style = "margin-top:5px ;",
                            "참석자"
                        ),
                        column(
                            width = 9,
                            selectInput(
                                inputId = ns("participants") , 
                                label = NULL , 
                                choices = NULL,
                                selected = NULL,
                                multiple = TRUE
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            width = 3,
                            style = "margin-top:12px ;",
                            "평점"
                        ),
                        column(
                            width = 9,
                            shinyRatings(inputId = ns("star"),default = 3),
                            textOutput(outputId = ns('star_rating'))
                        )
                    ),
                    fluidRow(style = "margin-top:5px ;",
                        column(
                            width = 3,
                            "리뷰"
                        ),
                        column(
                            width = 9,
                            textAreaInput(
                                inputId = ns("review_area") ,
                                label = NULL ,
                                value = "",
                                width = "300px",
                                height = "200px"
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            width = 3,
                            "사진 첨부"
                        ),
                        column(
                            width = 9,
                            actionButton(
                                inputId = ns("add"),
                                label = "ADD",
                                icon = icon("plus" , class = "me-1"),
                                class="btn btn-outline-secondary btn-sm",
                                width = "80px",
                                style = "margin-right:10px;"
                            ),
                            actionButton(
                                inputId = ns("delete"),
                                label = "DELETE",
                                icon = icon("minus" , class = "me-1"),
                                class="btn btn-outline-secondary btn-sm",
                                width = "90px",
                                style = "margin-right:10px;"
                            ),
                            actionButton(
                                inputId = ns("reset"),
                                label = "RESET",
                                icon = icon("refresh" , class = "me-1"),
                                class="btn btn-outline-secondary btn-sm",
                                width = "100px"
                            ),
                            fluidRow(
                                column(
                                    width = 10,
                                    class = "ms-2 mt-4 mb-3 border-top",
                                    fluidRow(
                                        id = "here",
                                        class = "mt-4"
                                    )
                                )
                            )
                        )
                    ),
                    fluidRow(style = "justify-content: center;",

                        actionButton(
                            inputId = ns("upload") , 
                            label = "Upload",
                            style = "margin-bottom : 20px; ",
                            width = "100px",
                            class="btn btn-success"
                        )
                    )
                )
            )
        )
    )
}


# server ----------------------------------------------------------------------------------------- #
review_Server <- function(id , parent ){
    moduleServer(id , function(input , output , session){
        ns <- session$ns

        isolate({
            fileNum <- reactiveVal(0L)
        })

        dbTable <- session$userData[["dbTable"]]


        #음식점이름 출력
        res_id <- reactiveVal()
        observeEvent(session$userData[["review_res_id"]](),{
            
            res_id(session$userData[["review_res_id"]]())

            #id값으로 음식점 출력
            review_res_name <- dbTable$res[dbTable$res$id==res_id(),"res_name"]
            
            #rendering
            output$review_res_name_output <- renderText(unlist(review_res_name))

        })


        #참석자
        participants_list <- unlist(dbTable$user$username)
        updateSelectInput(
                inputId = "participants" , 
                choices = participants_list
            )
        #평점
        output$star_rating <- renderText({paste("No. of stars : ", input$star)})


        #이미지 업로드
        observeEvent(input$add , {
            req(input$add)
            fileNum(fileNum()+1L)
            addNewFile(wrap_id = "here" , i = fileNum())
        })

        #업로드 삭제
        observeEvent(input$delete , {
            req(input$delete)
            removeUI(selector = paste0("#addFile_",fileNum()))
            fileNum(fileNum()-1L)
        })

         #업로드 초기화
        observeEvent(input$reset , {
            req(input$reset)
            ui_remove <- paste0("#addFile_", 1:fileNum())
            sapply(ui_remove , function(x){
                removeUI(selector = x)
            })
            removeUI(selector = paste0("#addFile_",fileNum()))
            fileNum(0L)
        })

        #업로드완료
        observeEvent(input$upload,{
            req(input$upload)
            showModal(upload_modal(id = id))
        })

        #업로드 확인 후 DB업로드
        observeEvent(input$yes , {
            removeModal()

            res_id <- as.integer(res_id())
            user_id <- session$userData[["user_id"]]
            visit_date <- as.Date(input$date_visit)
            participants <- paste(input$participants , collapse =  ",")
            rating_TI <- as.numeric(input$star)
            comment <- input$review_area

            # DB넣기
            sql_script <- glue("
                INSERT INTO what_to_eatDB.review (
                    date_visit , participants , rating_TI , comment ,res_id ,  user_id
                    ) VALUES(
                    '{visit_date}' ,'{participants}' , {rating_TI} , '{comment}' ,  {res_id}, {user_id}
                    );
            ")

            # step3: send query to MySQL
            dbExecute(con, sql_script)
        })
    })
}