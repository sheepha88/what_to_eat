# UI --------------------------------------------------------------------------------------------- #
review_UI <- function(id){
    ns <- NS(id)
    fluidPage(
        id = id,
        class = "bg-white",
        style = "display: flex; justify-content: center; align-items: center;",
        fluidRow(
            # style = "width: 500px; background : white; border:2px solid #E8EBEE; border-radius: 10px;",
            class = "bg-white",
            style = "width: 500px; border:2px solid #E8EBEE; border-radius: 10px; ",
            tags$h3(
                HTML("&nbsp;평점 & 리뷰작성") , 
                style = "font-weight: bold;",
                class = "mt-2 mb-2"
            ),
            fluidRow(
                class = "part_line",
                tags$h5("음식점 선택", class = "mt-2 mb-2" ),
                selectInput(
                    inputId = ns("review_res_list") , 
                    label = NULL,
                    choices = NULL,
                    width = "100%"
                )
            ),
            fluidRow(
                class = "part_line",
                tags$h5("방문일", class = "mt-2 mb-2" ),
                dateInput(
                    inputId = ns("date_visit"),
                    label = NULL,
                    value = NULL,
                    format = "yyyy-mm-dd",
                    width = "100%"
                )
                
            ),
            fluidRow(
                class = "part_line",
                tags$h5("참석자", class = "mt-2 mb-2" ),
                selectInput(
                    inputId = ns("participants") , 
                    label = NULL , 
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    width = "100%"
                )
            ),
            fluidRow(
                class = "part_line",
                tags$h5("평점", class = "mt-2 mb-0" ),
                tags$div(
                    class = "text-center",  
                    shinyRatings(inputId = ns("star"), default = 3)
                ),
                tags$div(
                    class = "text-center",
                    textOutput(outputId = ns('star_rating'))
                )
            ),
            fluidRow(
                class = "part_line",
                tags$h5("리뷰", class = "mt-2 mb-2" ),
                textAreaInput(
                    inputId = ns("review_area") ,
                    label = NULL ,
                    value = "",
                    width = "100%",
                    height = "200px"
                )
                
            ),
            fluidRow(
                class = "part_line",
                tags$h5("사진 첨부", class = "mt-2 mb-2" ),
                    tags$div(
                        style = "display: flex;",
                        actionButton(
                            inputId = ns("add"),
                            label = "ADD",
                            icon = icon("plus" , class = "me-1"),
                            class = "btn btn-outline-secondary btn-sm",
                            width = "100%",
                            style = "margin-right: 10px;"
                        ),
                        actionButton(
                            inputId = ns("delete"),
                            label = "DELETE",
                            icon = icon("minus" , class = "me-1"),
                            class = "btn btn-outline-secondary btn-sm",
                            width = "100%",
                            style = "margin-right: 10px;"
                        ),
                        actionButton(
                            inputId = ns("reset"),
                            label = "RESET",
                            icon = icon("refresh" , class = "me-1"),
                            class = "btn btn-outline-secondary btn-sm",
                            width = "100%"
                        )
                    ),
                    tags$div(
                        id = "here",
                        class = "mt-4"
                    )
            ),
            fluidRow(
                class = "part_line",
                tags$div(
                    class = "btn-group",
                    role = "group",
                    
                    actionButton(
                        inputId = ns("upload") , 
                        label = "Upload",
                        width = "100px",
                        class="btn btn-success mb-4 me-1"
                    ),
                    actionButton(
                        inputId = ns("go_to_home"),
                        label = "Back to Home",
                        width = "100px",
                        class = "btn-dark mb-4"
                    )
                )
            )
        )
    )
}


# server ----------------------------------------------------------------------------------------- #
review_Server <- function(id, parent, db_table){
    moduleServer(id , function(input , output , session){
        ns <- session$ns

        isolate({
            fileNum <- reactiveVal(0L)
        })

        dbTable <- session$userData[["dbTable"]]
    

        #음식점 리스트 
        navbar_reactive <- reactiveVal(0)
        observeEvent(parent$input$navbarPage, {
            # print(parent$input$navbarPage)
            
            req(parent$input$navbarPage == "rating", db_table(), session$userData[["review_with_res_id"]]() == 0L)
            navbar_reactive(navbar_reactive()+1)

            ## 음식점 리스트 받기 from DB
            res_list <- db_table()$res$res_name
            res_list <- c("", as.character(db_table()$res$id))
            names(res_list) <- c("", db_table()$res$res_name)
            # print(res_list)
            
            ## 음식점 선택 UI 업데이트: 리스트 추가
            updateSelectInput(
                session,
                inputId = "review_res_list",
                selected = NULL,
                choices = res_list
            )

            ## 참석자 리스트
            participants_list <- db_table()$user$username
            updateSelectInput(
                inputId = "participants" , 
                choices = participants_list
            )

        })


        #음식점이름 출력        
        observeEvent(session$userData[["review_res_id"]](), {
            
            req(session$userData[["review_res_id"]]() %in% db_table()$res$id, session$userData[["review_with_res_id"]]() == 1L)
            
            ### 음식점 리스트 받기 from DB
            res_list <- db_table()$res$res_name
            res_list <- c("", as.character(db_table()$res$id))
            names(res_list) <- c("", db_table()$res$res_name)

            ## 음식점 선택 UI 업데이트: 리스트 추가 및 선택
            updateSelectInput(
                session,
                inputId = "review_res_list",
                choices = res_list,
                selected = as.character(session$userData[["review_res_id"]]())
            )

            ## 참석자 리스트
            participants_list <- db_table()$user$username
            updateSelectInput(
                inputId = "participants" , 
                choices = participants_list
            )


            
            # res_id(session$userData[["review_res_id"]]())

            #id값으로 음식점 출력
            # review_res_name <- dbTable$res[dbTable$res$id==res_id(),"res_name"]
            
            # #rendering
            # output$review_res_name_output <- renderText(unlist(review_res_name))

            # res_list <- db_table()$res$res_name
            # print(res_list)
            # print(session$userData[["review_res_id"]]())
            # updateSelectInput(
            #     session,
            #     inputId = "review_res_list",
            #     choices = res_list,
            #     selected = ifelse(navbar_reactive()=="rating" , NULL , unlist(review_res_name))
                
            # )
        })
        
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

            req(session$userData[["user_id"]]() != 0L)

            res_id <- as.integer(input$review_res_list)
            user_id <- session$userData[["user_id"]]()
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

        #홈으로 가기
        observeEvent(input$go_to_home, {
            updateNavbarPage(
                session = parent,
                inputId = "navbarPage",
                selected = "home"
            )
        })


    })
}