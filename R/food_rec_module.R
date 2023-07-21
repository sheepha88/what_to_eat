# UI --------------------------------------------------------------------------------------------- #
food_rec_UI <- function(id){
    ns <- NS(id)
    
    fluidPage(
        id = id,
        fluidRow(
            tags$span(style = "text-align: center;",
                uiOutput(ns("res_name_rec"))
            ),
            tags$div(style = "text-align: center;",
                actionButton(
                    inputId = ns("refreshButton"), 
                    label = NULL,
                    icon = icon("refresh", class = "refresh-icon"),
                    class = "my-button",  
                )
            ),
            tags$span(
                uiOutput(ns("res_category"))
            ),
            tags$span(
                uiOutput(ns("rec_menu"))
            ),
            tags$span(
                class = "space"
            ),
            tags$span(
                tags$h4("신선푸드 자세히보기")
            )
        )
    )
}


# server ----------------------------------------------------------------------------------------- #
food_rec_Server <- function(id){
    moduleServer(id , function(input , output , session){
        ns <- session$ns

        

        # 출력함수 정의 -------------------------------------------------------------------------------- #
        recommendRestaurant <- function() {
            # 음식점 정보 랜덤으로 테이블 출력 (연속 중복 X)
            sql_1 <- "select * from res;"
            df_res <- dbGetQuery(con, sql_1) |> data.table()
            random_num <- sample(df_res$id, 1 , replace = FALSE) |> as.integer()
            print(random_num)

            # 랜덤으로 출력된 Id에 해당하는 restaurant 테이블 출력
            sql_2 <- glue("select * from res where id='{random_num}';")
            df_res_row <- dbGetQuery(con, sql_2) |> data.table()
            rec_name <- df_res_row$res_name # 음식점이름 변수 : res_name
            print(rec_name)

            # 음식점 이름 UI에 출력
            output$res_name_rec <- renderUI({
                tags$h1(
                rec_name
                )
            })

            # 추천음식점의 카테고리 출력
            rec_category <- df_res_row$category
            output$res_category <- renderUI({
                tags$h4("카테고리 : ", rec_category)
            })

            # 추천음식점의 메뉴 출력
            rec_menu <- df_res_row$menu
            menu_list <- fromJSON(rec_menu)
            rec_menu_str <- names(menu_list) |> paste(collapse = ",")
            print(rec_menu_str)
            output$rec_menu <- renderUI({
                tags$h4("메뉴 : ", rec_menu_str)
            })

            #음식점 이름 , 카테고리 , 메뉴 반환
            return(list(res_id = random_num, res_name = rec_name , res_cate = rec_category , res_menu = rec_menu_str))
        }

        # 초기화면에서 추천음식점 출력 ------------------------------------------------------------------------ #
        recommendRestaurant()
        recommend_res_info <-recommendRestaurant()
        created <- lubridate::now() |> format(x =_, "%Y-%m-%d %H:%M:%S.%S3")
            user_id <- session$userData[["user_id"]]
            res_id <-  recommend_res_info$res_id

            # step2: script 생성
            sql_script <- glue("
                INSERT INTO testdb.recommend (user_id, res_id, created) VALUES(
	                {user_id}, {res_id}, '{created}'
                );
            ")


            # step3: send query to MySQL
            dbExecute(con, sql_script)

        # 새로고침 버튼 클릭 시, 출력 ----------------------------------------------------------------------- 
        observeEvent(input$refreshButton,{
            req(input$refreshButton)

            # 추천음식점 출력
            recommendRestaurant()


            ## 추천음식점 기록
            # 추천음식점 출력 함수 반환값 변수지정
            recommend_res_info <-recommendRestaurant()

            # 반환값 global approach
            session$userData[["his_value"]] <- recommend_res_info
            
            # #새로고침 버튼 global approach
            rnt <- sample(c(-1L, 1L), 1L)
            session$userData[["refreshClicked"]]( session$userData[["refreshClicked"]]() + rnt )

            # #새로고침 누르면 DB(recommend table)에 전 추천음식점 정보 업로드
            # step1: column value 생성
            created <- lubridate::now() |> format(x =_, "%Y-%m-%d %H:%M:%S.%S3")
            user_id <- session$userData[["user_id"]]
            res_id <-  recommend_res_info$res_id

            # step2: script 생성
            sql_script <- glue("
                INSERT INTO testdb.recommend (user_id, res_id, created) VALUES(
	                {user_id}, {res_id}, '{created}'
                );
            ")


            # step3: send query to MySQL
            dbExecute(con, sql_script)

            ## later: add try-catch

            # recommended way: write a script algorithimcally

        })
    })
}