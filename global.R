# ------------------------------------------------------------------------------------------------ #
# 글로벌 옵션                                                                                           #
# ------------------------------------------------------------------------------------------------ 션
options(
    digits.secs = 6,
    scipen = 999,
    shiny.maxRequestSize=15*1024^2
)  

# ------------------------------------------------------------------------------------------------ #
# library                                                                                          #
# ------------------------------------------------------------------------------------------------ #
suppressPackageStartupMessages({
    library(shinyjs)
    library(shiny)
    library(glue)
    library(RMySQL)
    #library(DBI)
    library(data.table)
    library(jsonlite)
    library(rjson)
    library(bslib)
    library(lubridate)
    library(dplyr)
    library(DT)
    library(shinyBS)
    library(shinyRatings)
    library(shinymanager)
    library(toastui)
    library(stringr)
    library(pool)
    library(config)
    library(shinyFeedback)
})
# ------------------------------------------------------------------------------------------------ #
# mysql connect                                                                                    #
# ------------------------------------------------------------------------------------------------ #

# RMySQL connect --------------------------------------------------------------------------------- #

## setup connection
db <- config::get(file = "config.yml")$db
# con <- dbPool(
#     RMySQL::MySQL(),
#     dbname = db$dbname,
#     host = db$host,
#     username = db$username,
#     password = db$password,
#     port = db$port
# )


con <- dbConnect(
  RMySQL::MySQL(), 
  user="root", 
  password="0000",
  host="127.0.0.1", 
  dbname = "what_to_eatDB",
  port= 3306
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
  # dbExecute(con, "DELETE FROM what_to_eatDB.recommend")
  dbDisconnect(con)
  # poolClose(con)
})

# onStop(function() {
#     cat("Closing App & Database Connection\n")
#     poolClose(con)
# })

# ------------------------------------------------------------------------------------------------ #
# login page setup                                                                                 #
# ------------------------------------------------------------------------------------------------ #
set_labels(
    language = "en",
    "Please authenticate" = "",
    "Login" = "로그인",
    "Username:" = "이메일",
    "Password:" = "비밀번호"
)

# ------------------------------------------------------------------------------------------------ #
# check credentials                                                                                #
# ------------------------------------------------------------------------------------------------ #
checkCredentials <- function(){
    
    function(user, password){
        
        # write up sql script
        sql_script <- glue("SELECT * FROM user WHERE approved = 1 AND email = '{user}' AND password = SHA('{password}');")
        
        # execute the script
        sql_result <- tryCatch(
            { dbGetQuery(con, sql_script) },
            error = function(err){
                msg <- "Database Connection Error: Fail to access to 'user' table"
                # print `msg` so that we can find it in the logs
                message(msg)
                # print the actual error to log it
                message(err)
                # show error `msg` to user.  User can then tell us about error and we can
                # quickly identify where it came from based on the value in `msg`
                return(NULL)
            }
        )

        # check validity of the user email address requested
        if ( nrow(sql_result) == 1L ){
            
            sel_cols <- c("id", "email", "username", "sys_role")
            setDT(sql_result)
           

            list( result = TRUE, user_info = as.list( sql_result[, sel_cols, with = FALSE] ) )
        
        }else{
            list(result = FALSE)
        }

    }

}



# ------------------------------------------------------------------------------------------------ #
# utility functions                                                                                #
# ------------------------------------------------------------------------------------------------ #

# Row 추가 함수 -------------------------------------------------------------------------------------- #
addNewFile <- function(wrap_id, i){
    newFileUI <- fluidRow(
                id = paste0("addFile_",i),

                column(
                    width = 1,
                    tags$h6(i)
                ),
                column(
                    width = 10,
                    fileInput(
                        inputId = paste0("file_", i),
                        label = "Upload Image"
                    ) 
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

    #각 메뉴 문자열 합치기 -> 원래 데이터프레임의 개수와 같게 하기 위함
    rec_menu_str <- sapply(rec_menu_str, function(x) paste(x, collapse = ", "))
    
    return(rec_menu_str)
}


priceToList <- function(table){
    price_list <- lapply(table$menu , fromJSON) #menu json형태 list화
    price_list_num <- lapply(price_list , as.numeric) 
    price_list_mean <- lapply(price_list_num , mean) 
    rec_price_str <- unname(price_list_mean)
    
    return(round(unlist(rec_price_str)))
}


# db table 리로딩 ----------------------------------------------------------------------------------- #
reload_db_table <- function(session){
    old_val <- session$userData[["table_reload"]]()  
    ran_val <- sample(c(-1, 1L), 1L)
    session$userData[["table_reload"]](old_val + ran_val)
}


# db table가져오기 ----------------------------------------------------------------------------------- 기
getDBTable <- function(){

    
    # 음식점  테이블 출력
    #deleted = 1 : admin page에서 삭제처리하지 않은 것만 추출
    sql_res <- "SELECT * FROM res WHERE deleted = 1;"
    df_res <- dbGetQuery(con, sql_res) |> data.table()
    
    # 사용자 테이블 출력
    sql_user <- "SELECT * FROM user;"
    df_user <- dbGetQuery(con, sql_user) |> data.table()

    
    # review 테이블 출력
    sql_review <- "SELECT * FROM review;"
    df_review <- dbGetQuery(con, sql_review) |> data.table()

    # recommend 테이블 출력
    sql_recommend <- "SELECT * FROM recommend;"
    df_recommend <- dbGetQuery(con, sql_recommend) |> data.table()

    # image 테이블 출력
    sql_image <- "SELECT * FROM image;"
    df_image <- dbGetQuery(con, sql_image) |> data.table()

    #음식점 이름 , 카테고리 , 메뉴 반환
    #reactivevalue로 전환

    #data_list <- reactiveValues()
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

modal_ui_file <- function(session , rec_res_id){
    ns <- session$ns
    
    #table 모음
    df<-getDBTable()
    df_res <- df$res
    df_review <- df$review
    df_image <- df$image
    df_user <- df$user

    #datatable
    res_info <- df_res %>% filter(id == rec_res_id)
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
   
    #TI평점 , 리뷰 , 사용자 출력
    #사용자 ID userid로 출력
    df_join <- inner_join(df_review , df_user , by = c("user_id" = "id"))  

    df_TI_review <- df_join %>% 
                    filter(res_id == rec_res_id) %>% 
                        select(rating_TI , comment , username)
    
    #TI 평정
    mean_score <- ifelse(
            nrow(df_TI_review)==0,
            "평점 없음",
            round(mean(as.numeric(df_TI_review$rating_TI) ),1)
        )
    



    
    reviewTags <- if(nrow(df_TI_review)!=0){
                        lapply(rev(seq(nrow(df_TI_review))) , function(x){
                            fluidRow(class = "part_line",
                                tags$h4(style = " font-weight: bold;",
                                    df_TI_review[x , "username"]
                                ),
                                tags$h4(
                                    icon("star" , style="color: gold;"),
                                    df_TI_review[x , "rating_TI"]
                                ),
                                tags$h4(
                                    df_TI_review[x, "comment"]
                                )
                            )
                        })
                    }else {
                    tags$div(
                            "리뷰 없음"
                        )
                    }

    #리뷰 출력
    modalDialog(
        title = " Information",
        tags$style(".modal-title { font-size: 30px; }"),
        fluidPage(
            fluidRow(
                tags$h5(name , style = "font-weight: bold;")
            ),
            fluidRow(
                tags$h4("카테고리 : ",category)
            ),
            fluidRow(
                tags$h4("메뉴 : ",menu)
            ),      
            fluidRow(
                tags$h4(
                    "네이버 평점 : ", 
                    rating_naver,
                    tags$a(
                        href = url,
                        target = "_blank",
                        "네이버 바로가기"
                    )
                )
            ),      
            fluidRow(
                tags$h4("가격(평균) : ",price , "원")
            ),      
            fluidRow(
                tags$h4("위치(회사기준) : ",distance , "m")
            ),            
            fluidPage(
                style = "background-color : #FAFBFC;",
                class = "border",
                tags$h3(class = "part_line",
                    "TI history"
                ),
                fluidRow(
                    column(
                        width = 10,
                        tags$h4("평점(TI) : " , icon("star" , style="color: gold;") ,mean_score)
                    )
                ),
                fluidRow(
                    column(
                        width = 12,
                        tags$h4("리뷰")
                    )
                ),
                fluidPage(style = "background-color : #FAFBFC;",
                    #리뷰수대로 생성해야함
                    #함수사용
                    column(
                        width = 12,
                        #uiOutput(ns("review_bar")),
                        reviewTags
                    )
                )
            )
        ),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
            modalButton("Close")
        )
    )
}



#  리뷰쓰기로 이동하기
go_review_server <- function( session  , review_res_id ){

    session$userData[["review_with_res_id"]](1L)
    
    updateTabsetPanel(
        session = session,
        inputId = "navbarPage",
        selected = "rating"
    )

    session$userData[["review_res_id"]](review_res_id)
}



# 업로드 modal
upload_modal<-function(id){
    ns <- NS(id)
    modalDialog(
    title = "평점 & 리뷰작성",
  
    fluidRow(
      tags$p(
        "업로드를 진행하시겠습니까?"
      )
    ),
    size = "m",
    easyClose = TRUE,
    footer = tagList(
      actionButton(
        inputId = ns("yes"),
        label = "확인",
        class = "btn-primary",
      ),
      modalButton("취소")
    )
  )
}


# 업로드 modal
data_upload_modal<-function(id){
    ns <- NS(id)
    modalDialog(
        title = "Data Add",
        fluidRow(
            column(
                width = 3,
                style = "margin-top:5px ;",
                "음식점 명"
            ),
            column(
                width = 9,
                textInput(
                    inputId = ns("input_res_name"),
                    label = NULL
                )
            )
        ),
        fluidRow(
            column(
                width = 3,
                "카테고리"
            ),
            column(
                width = 9,
                checkboxGroupInput(
                    inputId = ns("input_res_category"),
                    label = NULL,
                    choices = c("한식", "양식", "일식", "중식", "배달" , "기타"),
                    selected = NULL,
                    inline = TRUE
                )
            )
        ),
        fluidRow(
            column(
                width = 3,
                style = "margin-top:5px ;",
                "네이버 평점"
            ),
            column(
                width = 9,
                numericInput(
                    inputId = ns("input_res_naver_rating"),
                    label = NULL,
                    value = NULL
                )
            )
        ),
        fluidRow(
            column(
                width = 3,
                style = "margin-top:5px ;",
                "위치 (m)"
            ),
            column(
                width = 9,
                numericInput(
                    inputId = ns("input_res_distance"),
                    label = NULL,
                    value = NULL
                    )
            )
        ),
        fluidRow(
            column(
                width = 3,
                style = "margin-top:5px ;",
                "Naver URL"
            ),
            column(
                width = 9,
                textInput(
                    inputId = ns("input_naver_url"),
                    label = NULL
                )
            )
        ),
        fluidRow(
            column(
                width = 3,
                "메뉴 & 가격",
            ),
            column(
                width = 4,
                "메뉴"
            ),
            column(
                width = 5,
                "가격"
            ),
        ),
        fluidRow(
            column(
                width = 3,
                actionButton(
                    inputId = ns("addButton"),
                    label = NULL,
                    icon = icon("plus" , class = "me-1"),
                    class="btn btn-outline-secondary btn-sm",
                    width = "30px",
                    style = "margin-left : 23px; margin-top : 3px;"
                ),
                actionButton(
                    inputId = ns("deleteButton"),
                    label = NULL,
                    icon = icon("minus" , class = "me-1"),
                    class="btn btn-outline-secondary btn-sm",
                    width = "30px",
                    style = "margin-top : 3px;"
                )
            ),
            column(
                width = 4,
                textInput(
                    inputId = ns("input_res_menu_1"),
                    label = NULL
                )
            ),
            column(
                width = 4,
                numericInput(
                    inputId = ns("input_res_price_1"),
                    label = NULL,
                    value = NULL
                )
            ),
        ),
        fluidRow(
            id = "space"
        ),
        fluidRow(
            uiOutput(ns("data_upload_msg"))
        ),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
            actionButton(
                inputId = ns("yes"),
                label = "확인",
                class = "btn-primary",
            ),
            modalButton("닫기")
        )

    )
}


#data add row 추가함수
addNewRow <- function(session, wrap_id, i){
    ns <- session$ns
    paste_id = ifelse(wrap_id=="space" , "" , "edit_")
    newRowUI <- fluidRow(
                id = paste0(paste_id,"addRow_",i),
                column(
                    width = 3,
                    actionButton(
                        inputId = ns(paste0("addButton_",i)),
                        label = NULL,
                        icon = icon("plus" , class = "me-1"),
                        class="btn btn-outline-secondary btn-sm",
                        width = "30px",
                        style = "margin-left : 23px; margin-top : 3px;",
                        class = "sr-only"
                        
                    ),
                    actionButton(
                        inputId =ns(paste0("deleteButton_",i)),
                        label = NULL,
                        icon = icon("minus" , class = "me-1"),
                        class="btn btn-outline-secondary btn-sm",
                        width = "30px",
                        style = "margin-top : 3px;",
                        class = "sr-only"
                    )
                ),
                column(
                    width = 4,
                    textInput(
                        inputId = ns(paste0(paste_id,"input_res_menu_",i)),
                        label = NULL
                    )
                ),
                column(
                    width = 4,
                    numericInput(
                        inputId = ns(paste0(paste_id,"input_res_price_",i)),
                        label = NULL,
                        value = NULL
                    )
                )
    )

    insertUI(
        selector = paste0("#", wrap_id),
        where = "beforeBegin",
        ui = newRowUI
    )

}



# 삭제 modal
delete_modal<-function(id){
    ns <- NS(id)
    modalDialog(
    title = "경고",
    fluidRow(
        tags$p(
            "삭제 하시겠습니까?"
        )
    ),
    fluidRow(
        uiOutput(ns("data_delete_msg"))
    ),
    size = "m",
    easyClose = TRUE,
    footer = tagList(
      actionButton(
        inputId = ns("delete_yes"),
        label = "확인",
        class = "btn-primary",
      ),
      modalButton("닫기")
    )
  )
}


#수정 modal
data_edit_modal<-function(id){
    ns <- NS(id)
    modalDialog(
        title = "Data Edit",
        fluidRow(
            column(
                width = 3,
                style = "margin-top:5px ;",
                "음식점 명"
            ),
            column(
                width = 9,
                textInput(
                    inputId = ns("edit_res_name"),
                    label = NULL
                )
            )
        ),
        fluidRow(
            column(
                width = 3,
                "카테고리"
            ),
            column(
                width = 9,
                checkboxGroupInput(
                    inputId = ns("edit_res_category"),
                    label = NULL,
                    choices = c("한식", "양식", "일식", "중식", "배달" , "기타"),
                    selected = NULL,
                    inline = TRUE
                )
            )
        ),
        fluidRow(
            column(
                width = 3,
                style = "margin-top:5px ;",
                "네이버 평점"
            ),
            column(
                width = 9,
                numericInput(
                    inputId = ns("edit_res_naver_rating"),
                    label = NULL,
                    value = NULL
                )
            )
        ),
        fluidRow(
            column(
                width = 3,
                style = "margin-top:5px ;",
                "위치 (m)"
            ),
            column(
                width = 9,
                numericInput(
                    inputId = ns("edit_res_distance"),
                    label = NULL,
                    value = NULL
                )
            )
        ),
        fluidRow(
            column(
                width = 3,
                style = "margin-top:5px ;",
                "Naver URL"
            ),
            column(
                width = 9,
                textInput(
                    inputId = ns("edit_naver_url"),
                    label = NULL
                )
            )
        ),
        fluidRow(
            class = "mb-2",
            column(
                width = 3,
                "메뉴 & 가격",
            ),
            column(
                width = 4,
                "메뉴"
            ),
            column(
                width = 5,
                "가격"
            ),
        ),
        fluidRow(
            column(
                width = 3,
                actionButton(
                    inputId = ns("edit_addButton"),
                    label = NULL,
                    icon = icon("plus" , class = "me-1"),
                    class="btn btn-outline-secondary btn-sm",
                    width = "30px",
                    style = "margin-left : 23px; margin-top : 3px;"
                ),
                actionButton(
                    inputId = ns("edit_deleteButton"),
                    label = NULL,
                    icon = icon("minus" , class = "me-1"),
                    class="btn btn-outline-secondary btn-sm",
                    width = "30px",
                    style = "margin-top : 3px;"
                )
            ),
            column(
                width = 4,
                textInput(
                    inputId = ns(paste0("edit_input_res_menu_1")),
                    label = NULL
                )
            ),
            column(
                width = 4,
                numericInput(
                    inputId = ns(paste0("edit_input_res_price_1")),
                    label = NULL,
                    value = NULL
                )
            )
            
        ),
        fluidRow(
            uiOutput(ns("edit_UI"))
        ),
        fluidRow(
            id = "edit_space",
        ),
        fluidRow(
            uiOutput(ns("data_edit_msg"))
        ),
        size = "m",
        easyClose = TRUE,
        footer = tagList(
                actionButton(
                    inputId = ns("edit_yes"),
                    label = "확인",
                    class = "btn-primary",
                ),
                modalButton("닫기")
            )
    )
}

