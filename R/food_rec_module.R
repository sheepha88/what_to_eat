# UI --------------------------------------------------------------------------------------------- #
food_rec_UI <- function(id){
    ns <- NS(id)
    
    fluidRow(
        id = id,
        tags$h1(
            style = "text-align: center; margin-bottom : 5px; font-weight: bold;",
            textOutput(ns("res_name"))
        ),
        tags$div(style = "text-align: center;",
            actionButton(
                inputId = ns("refreshButton"), 
                label = NULL,
                icon = icon("refresh", class = "refresh-icon"),
                class = "my-button mt-2 mb-4",  
            )
        ),
        tags$h6(
            uiOutput(ns("res_category"))
        ),
        tags$span(
            uiOutput(ns("rec_menu"))
        ),
        tags$span(
            class = "space"
        ),
        tags$span(
            tags$h4( 
                actionLink(inputId = ns("rec_modal") , label = "자세히보기")
            )   
        ),
        tags$span(
            tags$h4( 
                actionLink(
                    inputId = ns("go_review"),
                    label = "리뷰쓰기"
                )
            )   
        )
    )
}


# server ----------------------------------------------------------------------------------------- #
##parent : main 서버의 session을 modal함수의 인자로 받을때 쓰기 위해
food_rec_Server <- function(id, parent){
    moduleServer(id , function(input , output , session){
        ns <- session$ns
        #modal
        # modal_Server("Open_modal")
        
        isolate({
            resName <- reactiveVal("")

            recommend_res_info <- reactiveVal(NULL)
        })            

        # 출력함수 정의 -------------------------------------------------------------------------------- #
        # 1) DB에서 추천 음식점 정보 Loading
        # 2) 추천 음식점 정보 reactiveVal로 변환 -> UI는 reactiveVal의 값 변화에 대응하여 업데이트 되므로

        recommendRestaurant <- function() {
            # 음식점 정보 랜덤으로 테이블 출력 (연속 중복 X)
            sql_1 <- "SELECT * FROM res;"
            df_res <- dbGetQuery(con, sql_1) |> as.data.table()
            
            random_num <- sample(df_res$id, 1 , replace = FALSE) |> as.integer() 
           

            # 랜덤으로 출력된 Id에 해당하는 restaurant 테이블 출력
            sql_2 <- glue("SELECT * FROM res WHERE id = {random_num};")
            df_res_row <- dbGetQuery(con, sql_2) |> as.data.table()
            rec_name <- df_res_row$res_name # 음식점이름 변수 : res_name
            resName(rec_name)
            # [old way]
            # output$res_name_rec <- renderUI({
            #     tags$h1( rec_name )
            # })

            

            # 추천음식점의 카테고리 출력
            rec_category <- df_res_row$category
            output$res_category <- renderUI({
                tags$h6("카테고리 : ", rec_category , style = "font-size:15px; margin-bottom:0px;")
            })

            # 추천음식점의 메뉴 출력
            rec_menu <- df_res_row$menu
            menu_list <- fromJSON(rec_menu)
            rec_menu_str <- names(menu_list) |> paste(collapse = ",")
           
            output$rec_menu <- renderUI({
                tags$h6("메뉴 : ", rec_menu_str , style = "font-size:15px;")
            })

            # 추천음식점의 평점 출력
        
            rec_rating <- ifelse(is.na(df_res_row$rating_naver) ,"평점 없음" , df_res_row$rating_naver )
            

            # 추천음식점의 가격(평균) 출력
            rec_price <- priceToList(df_res_row)
            


            # 추천음식점의 거리 출력
            rec_distance <- df_res_row$distance

            #naver_url
            rec_url <- df_res_row$url_naver 

            #음식점 이름 , 카테고리 , 메뉴 반환
            return(
                list(
                    res_id = random_num, 
                    res_name = rec_name , 
                    res_cate = rec_category , 
                    res_menu = rec_menu_str,
                    res_rating = rec_rating,
                    res_price = rec_price,
                    res_distance = rec_distance,
                    res_url = rec_url
                )
            )
        }

        # 초기화면에서 추천음식점 출력 ------------------------------------------------------------------------ #
        # [new way]
        output$res_name <- renderText({
            resName() 
        })
        
        # 추천 레스토랑 정보 from DB
        res_info <- recommendRestaurant()
        recommend_res_info( res_info )

        created <- lubridate::now() |> format(x =_, "%Y-%m-%d %H:%M:%S.%S3")
        user_id <- session$userData[["user_id"]]
        res_id <-  res_info$res_id

        # step2: script 생성
        sql_script <- glue("
            INSERT INTO what_to_eatDB.recommend (user_id, res_id, created) VALUES(
                {user_id}, {res_id}, '{created}'
            );
        ")

        # step3: send query to MySQL
        # recommend 테이블에 추천 음식점 기록
        dbExecute(con, sql_script)


        #dbtable 가져오기
        dbTable <- session$userData[["dbTable"]]
            

        
        # 새로고침 버튼 클릭 시, 출력 ----------------------------------------------------------------------- 
        observeEvent(input$refreshButton,{
            req(input$refreshButton)
            
            # 추천음식점 기록
            # 추천음식점 출력 함수 반환값 변수지정
            # 랜덤 추천음식점 출력
            recommend_res_info(recommendRestaurant())
            

            
            # recommendRestaurant()
            # recommend_res_info(recommendRestaurant())

            # 반환값 global approach
            session$userData[["his_value"]] <- recommend_res_info()
            
            # #새로고침 버튼 global approach
            rnt <- sample(c(-1L, 1L), 1L)
            session$userData[["refreshClicked"]]( session$userData[["refreshClicked"]]() + rnt )

            # #새로고침 누르면 DB(recommend table)에 전 추천음식점 정보 업로드
            # step1: column value 생성
            created <- lubridate::now() |> format(x =_, "%Y-%m-%d %H:%M:%S.%S3")
            user_id <- session$userData[["user_id"]]
            res_id <-  recommend_res_info()[["res_id"]]

            # step2: script 생성
            sql_script <- glue("
                INSERT INTO what_to_eatDB.recommend (user_id, res_id, created) VALUES(
                    {user_id}, {res_id}, '{created}'
                );
            ")


            # step3: send query to MySQL
            dbExecute(con, sql_script)

            ## later: add try-catch

            # recommended way: write a script algorithimcally
        
        })

            
            
        # modal내용 출력
        observeEvent(input$rec_modal, {   
            showModal(
                # modal UI
                modal_ui_file(
                    session = session,
                    rec_res_id = recommend_res_info()[["res_id"]]
                )
            )
        })
        
        # 그냥 리뷰쓰기로 이동하기
            observeEvent(input$go_review ,{
                go_review_server(session = parent ,review_res_id = recommend_res_info()[["res_id"]] )
        })
    })
}