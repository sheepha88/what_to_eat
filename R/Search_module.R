# UI --------------------------------------------------------------------------------------------- #
search_UI <- function(id){
    ns <- NS(id)
    fluidPage(
        id = id,
        tags$div(style = "display: flex; justify-content: center; align-items: center;",  # 가운데 정렬 스타일 적용
            fluidRow(style = "width: 500px; background : white; border:2px solid #E8EBEE; border-radius: 10px;",
                
                tags$h3(HTML("&nbsp;음식점 검색") , style = "font-weight: bold;"),
                tags$div(
                    class = "part_line",
                    tags$h4(
                        "음식점 명", style = "margin-left: 10px; margin-bottom: 0px; " , 
                    ),
                    tags$div(
                        style = "margin-left: 10px;",
                        selectInput(
                            inputId = ns("name") , 
                            label = NULL , 
                            width = "95%",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE
                        )
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4(
                        "카테고리", style = "margin-left: 10px; margin-bottom: 0px;",
                    ),
                    tags$div(style = "margin-left:10px; ",
                        checkboxGroupInput(
                            inputId = ns("category") , label = NULL,
                            choices = c("한식", "양식", "일식", "중식", "배달" , "기타"),
                            selected = NULL 
                        )
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4(
                        "메뉴", style = "margin-left: 10px; margin-bottom: 0px;",
                        uiOutput(ns("menu")) 
                    ),
                    tags$div(
                        tags$div(
                            style = "margin-left: 10px;",
                            selectInput(
                                inputId = ns("menu") , 
                                label = NULL , 
                                width = "95%",
                                choices = NULL,
                                selected = NULL,
                                multiple = TRUE
                            )
                        )   
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4("평점", style = "margin-left: 10px; margin-bottom: 0px;",
                        tags$div(style = "font-size : 13px;",
                        checkboxGroupInput(inputId = ns("score"), label = NULL ,  choices = c("TI" , "네이버") , inline=TRUE , selected = NULL )
                        ),
                    sliderInput(inputId = ns("score_range") , label = NULL , min = 0L , max = 5L , value=c(0L,0L) , step = 0.5L, width = "95%")    
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4("가격", style = "margin-left: 10px; margin-bottom: 0px;",
                    sliderInput(inputId = ns("price_range") , label = NULL , min = 0L , max = 50000L , value=0L , step = 1000L, width = "95%")
                    )
                ),
                tags$div(
                    class = "part_line",
                    tags$h4("위치(m)", style = "margin-left: 10px; margin-bottom: 0px;",
                    sliderInput(inputId = ns("distance_range") , label = NULL , min = 0L , max = 2000L , value=0L , step = 100L, width = "95%")
                    )
                ),
                tags$div(
                    class = "part_line",
                    style = "text-align: center; margin-bottom: 0px; ",
                    actionButton(
                        inputId=ns("Done_search") ,
                        label = "검색",
                        style = "font-weight: bold; ",
                        width = "20%",
                        class = "btn btn-success"
                    )
                ),
                tags$div(
                    class = "part_line",
                    DT::dataTableOutput(
                    ns("table")
                    )
                )
            )
        )
    )
}


# server ----------------------------------------------------------------------------------------- #
search_Server <- function(id){
    moduleServer(id , function(input , output , session){
        ns <- session$ns
        reactive_dbTable <- reactiveVal(NULL)
        reactive_dbTable(session$userData[["dbTable"]])


        #dbtable 가져오기
        dbTable <- session$userData[["dbTable"]]

        #db table 중 res table에서 음식점 이름 가져오기
        res_name_choices <- dbTable$res$res_name

        #db table 중 res table에서 메뉴 가져와서 json 형태로 변환 후 vector로 변환
        res_menu <- unlist(menuToList(dbTable$res))


        #테이블 업데이트 되면 검색팝업도 업데이트
        observeEvent(reactive_dbTable() , {
            #음식점 검색
            updateSelectInput(
                inputId = "name" , 
                choices = dbTable$res$res_name
            )
            #메뉴 검색
            updateSelectInput(
                inputId = "menu" , 
                choices = res_menu
            )
        })

        
        #검색결과 조회
        observeEvent(input$Done_search,{

            #데이터프레임 선언
            df_res <- dbTable$res

            #df_res의 메뉴컬럼 -> 메뉴 , 가격(평균)으로 만들기
            ##메뉴 json -> 리스트
            df_res$menu <- menuToList(dbTable$res) 
            ##가격 json -> 리스트
            df_res$price <- priceToList(dbTable$res)
            
            df_result <- df_res%>% filter(
                                    ifelse(is.null(input$name) , TRUE , FALSE) | res_name %in% input$name
                                    )%>%
                                filter(
                                    ifelse(is.null(input$category) , TRUE , FALSE) | category %in% input$category
                                    )%>%

                                #메뉴를 찾을때는 filter(grepl)사용
                                #dataframe의 메뉴는 현재 "보쌈,김치찌개" 형식으로 되어있는 길이1개의 캐릭터로 구성
                                #따라서 grepl을 통해 특정문자열을 포함하는 행을 찾는것으로 진행해야함    
                                filter(
                                    ifelse(is.null(input$menu) , TRUE , FALSE) | grepl(paste(input$menu, collapse = '|'), menu)
                                    )%>% 
                                filter(
                                    ifelse(sum(input$score_range)==0L , TRUE , FALSE) | between(rating_naver , input$score_range[1L],input$score_range[2L])
                                    )%>%
                                filter(
                                    ifelse(input$price_range==0L , TRUE , FALSE) | price < input$price_range
                                    )%>%
                                filter(
                                    ifelse(input$distance_range==0L , TRUE , FALSE) | distance < input$distance_range
                                    )%>%
                                select(
                                    res_name , category , menu , rating_naver , price , distance
                                    )
                            

            #DataFrame UI 출력
            ##출력되는 데이터프레임 컬럼이름
            colums_names = c("음식점 이름" , "카테고리" , "메뉴"  ,"평점","가격" ,"위치(m)"  )
            output$table <- DT::renderDataTable(
                DT::datatable(df_result, escape=FALSE, colnames = colums_names,
                  options = list(
                    pageLength = 20, Width = '500px',
                    columnDefs = list(
                                    list( targets = 1, width = '100px'),
                                    list( targets = 2, width = '80px'),
                                    list( targets = 3, width = '200px'),
                                    list( targets = 4, width = '50px'),
                                    list( targets = 5, width = '50px'),
                                    list( targets = 6, width = '50px')
                                ),
                    transpose = TRUE,
                    scrollX = TRUE,
                    dom = "ftp"
                    
                ))
            )

        })
    })
}