# ------------------------------------------------------------------------------------------------ #
# library                                                                                          #
# ------------------------------------------------------------------------------------------------ #
library(shinyjs)
library(shiny)
library(glue)
library(RMySQL)
library(DBI)
library(data.table)
library(jsonlite)
library(rjson)
library(bslib)
library(lubridate)
library(dplyr)
library(DT)
library(shinyBS)

# ------------------------------------------------------------------------------------------------ #
# mysql connect                                                                                    #
# ------------------------------------------------------------------------------------------------ #

# RMySQL connect --------------------------------------------------------------------------------- #

## setup connection
con <- dbConnect(
  RMySQL::MySQL(), 
  user="root", 
  password="0000",
  host="127.0.0.1", 
  dbname = "testdb",
  client.flag=CLIENT_MULTI_RESULTS
)

## preliminaries
dbExecute(con, "SET NAMES 'utf8mb4' COLLATE 'utf8mb4_unicode_ci';") # reset character set
dbExecute(con, "SET SQL_SAFE_UPDATES = 0;") # allowing update

# utf control ------------------------------------------------------------------------------------ #
# dbSendQuery(con, "SET NAMES utf8;") 
# dbSendQuery(con, "SET CHARACTER SET utf8;") 
# dbSendQuery(con, "SET character_set_connection=utf8;")
# legacy code

# ------------------------------------------------------------------------------------------------ #
# At the close of the app                                                                          #
# ------------------------------------------------------------------------------------------------ #
onStop(function() {
  cat("Closing App & Database Connection\n")
  #추후삭제 예정
  # dbExecute(con, "DELETE FROM testdb.recommend")
  dbDisconnect(con)
  # poolClose(con)
})

# ------------------------------------------------------------------------------------------------ #
# utility functions                                                                                #
# ------------------------------------------------------------------------------------------------ #

# Row 추가 함수 -------------------------------------------------------------------------------------- #
addNewFile <- function(wrap_id, i){
  
  newFileUI <- fluidRow(
    id = paste0("addFile_",i),

    column(
      width = 1,
      tags$h4(i)
    ),

    column(
      width = 4,
      textInput(
        inputId = paste0("des_", i),
        label = "Description",
        value = ""
      )
    ),

    column(
      width = 4,
      fileInput(
        inputId = paste0("file_", i),
        label = "Upload File",
      ) |> shinyjs::disabled()
    ),

    column(
      width = 3,
      class = "align-self-center",
      downloadButton(
        outputId = paste0("down_",i),
        label = "Download File"
      ) |> shinyjs::disabled()
    )
  )
    
      insertUI(
        selector = paste0("#", wrap_id),
        where = "beforeEnd",
        ui = newFileUI
      )
}

# DB res table 의 메뉴  , 가격 벡터화 ------------------------------------------------------------------------- #

menuToList <- function(table){
    menu_list <- lapply(table$menu , fromJSON) #menu json형태 list화
    rec_menu_str <- lapply(menu_list , names)  #메뉴만 추출
    return(rec_menu_str)
}


priceToList <- function(table){
    price_list <- lapply(table$menu , fromJSON) #menu json형태 list화
    price_list_num <- lapply(price_list , as.numeric) 
    price_list_mean <- lapply(price_list_num , mean) 
    rec_price_str <- unname(price_list_mean)
    
    return(unlist(rec_price_str))
}






# db table가져오기 ----------------------------------------------------------------------------------- 기
getDBTable<-function(){
    # 음식점  테이블 출력
    sql_res <- "select * from res;"
    df_res <- dbGetQuery(con, sql_res) |> data.table()
    
    # 사용자 테이블 출력
    sql_user <- "select * from user;"
    df_user <- dbGetQuery(con, sql_user) |> data.table()

    
    # review 테이블 출력
    sql_review <- "select * from review;"
    df_review <- dbGetQuery(con, sql_review) |> data.table()

    # recommend 테이블 출력
    sql_recommend <- "select * from recommend;"
    df_recommend <- dbGetQuery(con, sql_recommend) |> data.table()

    # image 테이블 출력
    sql_image <- "select * from image;"
    df_image <- dbGetQuery(con, sql_image) |> data.table()

    #음식점 이름 , 카테고리 , 메뉴 반환
    return(
        list(res = df_res , user = df_user , review = df_review , recommend = df_recommend , image = df_image)
    )
}



# 자세히보기 modal ------------------------------------------------------------------------------------ #


# modal
## res_id를 인자로 받아서 다른 컬럼들 쫙 뿌리는 형태로 modal형성
### res table : res_id , category , menu , 평점 , 가격 , 위치
### review table : res_id , user_id , review
### image : res_id , 
### user : res_id , user_name

# name, category, menu, rating_naver, price, distance, url

modal_ui_file <- function(session , res_id){
    ns <- session$ns
    
    #table 모음
    df_res <- getDBTable()$res
    df_review <- getDBTable()$review
    df_image <- getDBTable()$image
    df_user <- getDBTable()$user

    #datatable
    res_info <- df_res %>% filter(id == res_id)
    #음식점 이름
    name = res_info$res_name
    #카테고리
    category = res_info$category
    #메뉴
    menu_list <- fromJSON(res_info$menu)
    menu <- names(menu_list) |> paste(collapse = ",")
    # 평점
    rating_naver <- ifelse(is.na(res_info$rating_naver) ,"평점 없음" , res_info$rating_naver )
    # 가격
    price <- priceToList(res_info)
    # 거리
    distance <- res_info$distance
    # 네이버 url
    url <- res_info$url_naver 
   
   #TI평점 출력
    df_TI_score <- df_review %>% 
                    filter(res_id == res_id) %>% 
                        select(rating_TI)

    mean_score <- round(mean(as.numeric(pull(df_TI_score)) ),3)
    

    
    modalDialog(
        title = " Information",
        tags$style(".modal-title { font-size: 30px; }"),
        fluidPage(
            fluidRow(
                column(
                    width = 12,
                    tags$h4(name)
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h4("카테고리 : ",category)
                )
            ),
            fluidRow(
                column(
                    width = 12,
                    tags$h4("메뉴 : ",menu)
                )
            ),      
            fluidRow(
                column(
                    width = 12,
                    tags$div(style = "display: flex;",
                        tags$span(
                            tags$h4("네이버 평점 : ",rating_naver)
                        ),
                        tags$span(style = "margin-left:3px; margin-top:11px;",
                            tags$a(
                                href = HTML(url),
                                target = "_blank",
                                "네이버 바로가기"
                            )
                        )
                    )
                )
            ),      
            fluidRow(
                column(
                    width = 12,
                    tags$h4("가격(평균) : ",price , "원")
                )
            ),      
            fluidRow(
                column(
                    width = 12,
                    tags$h4("위치(회사기준) : ",distance , "m")
                )
            ),            
            fluidPage(sylte = "background-color : #FAFBFC;",
                tags$h3(class = "part_line",
                    "TI history"
                ),
                fluidRow(
                    column(
                        width = 10,
                        tags$h4("평점(TI) : " , mean_score)
                    ),
                    column(
                        width = 2,
                        tags$h4(
                            actionLink(
                                inputId = ns("go_review_modal"),
                                label = "리뷰쓰기"
                            )
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 12,
                        tags$h4("리뷰")
                    )
                ),
                fluidPage(sylte = "background-color : #FAFBFC;",
                    #리뷰수대로 생성해야함
                    #함수사용
                    column(
                        width = 12,
                        tags$div(class = "part_line",
                            "dfs"
                        )
                    )
                )
            )

        ),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
            actionButton(
                inputId = "yes_file",
                label = "Yes",
                class = "btn-primary",
            ),
        modalButton("Close")
        )
    )
}


# modal에서 리뷰쓰기로 이동하기
go_review_modal_server <- function(input , session ){
    observeEvent(input$go_review_modal ,{
        removeModal()
        updateTabsetPanel(
            session = session,
            inputId = "navbarPage",
            selected = "rating"
        )
    })
}

# 리뷰쓰기 페이지 이동 함수 --------------------------------------------------------------------------------- 수
go_review_ui <- function(id ){
    ns <- NS(id)
    actionLink(
        inputId = ns("go_review"),
        label = "리뷰쓰기"
    )
}

go_review_server <- function(input , session ){
    observeEvent(input$go_review ,{
        updateTabsetPanel(
            session = session,
            inputId = "navbarPage",
            selected = "rating"
        )
    })
}
