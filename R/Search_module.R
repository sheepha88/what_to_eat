# UI --------------------------------------------------------------------------------------------- #
search_UI <- function(id){
    ns <- NS(id)
    fluidPage(
        id = id,
        tags$div(style = "display: flex; justify-content: center; align-items: center;",  # 가운데 정렬 스타일 적용
            fluidRow(style = "width: 500px; background : white; border:2px solid #E8EBEE; border-radius: 10px;",
                tags$h3("음식점 검색" , style = "font-weight: bold;"),
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
                    sliderInput(inputId = ns("score_range") , label = NULL , min = 0L , max = 5L , value=0L , step = 0.5L, width = "95%")    
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
                        style = "font-weight: bold; background-color:#F27F0E; margin-bottom: 10px;",
                        width = "20%"
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
                choices = res_name_choices
            )
            #메뉴 검색
            updateSelectInput(
                inputId = "menu" , 
                choices = res_menu
            )
        })

        
        #검색결과 조회
        observeEvent(input$Done_search,{
            
            
            #입력값 모음 - 값없으면 아예 안나옴
            
            # print(input$name , input$category , input$menu , 
            # input$price_range , input$distance_range)
            print(dbTable$res$res_name)
            print(input$menu)
            print(is.vector(input$menu))
            new_menu <- menuToList(dbTable$res)
            print(new_menu)
            print((menuToList(dbTable$res))[[1]])
            print(is.vector(menuToList(dbTable$res)))
            
            







            df_result <- dbTable$res %>%
                #menu json -> vector
                mutate(new_menu = menuToList(dbTable$res))%>%
                    #음식점이름 입력값 포함하는 행 출력
                    
                    filter(res_name %in% input$name) %>%
                        #카테고리 포함하는 행 출력
                        filter(category %in% input$category ) %>%



                            # filter(new_menu  %in% input$menu)  %>%
                            # filter(any(unlist(strsplit(new_menu , ",")) %in% input$menu) ) %>%

                                # filter(price <= input$price_range ) %>%

                                #     filter(distance <= input$distance_range ) %>%

                                        #컬럼추출
                                        select(res_name , category , new_menu  , distance)
                       

            print(df_result)
            
            

            #DataFrame UI 출력
            ##출력되는 데이터프레임 컬럼이름
            colums_names = c("음식점 이름" , "카테고리" , "메뉴"  ,"위치(m)" )
            output$table <- DT::renderDataTable(
                DT::datatable(df_result, escape=FALSE, colnames = colums_names,
                  options = list(
                    pageLength = 20, autoWidth = TRUE,
                    columnDefs = list(
                                    list( targets = 1, width = '100px'),
                                    list( targets = 2, width = '80px'),
                                    list( targets = 3, width = '200px'),
                                    
                                    list( targets = 4, width = '50px')
                                ),
                    transpose = TRUE,
                    scrollX = TRUE
                    
                  ))
            )





        
        })

        
    })
}