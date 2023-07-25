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
  dbExecute(con, "DELETE FROM testdb.recommend")
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

# DB res table 의 메뉴 벡터화 ------------------------------------------------------------------------- #

menuToList <- function(table){
    menu_list <- lapply(table$menu , fromJSON) #menu json형태 list화
    rec_menu_str <- lapply(menu_list , names)  #메뉴만 추출
    return(rec_menu_str)
}




# db table가져오기 ----------------------------------------------------------------------------------- 기
getDBTable<-function(){
    print("sdfsdfsdg")
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

