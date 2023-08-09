# UI --------------------------------------------------------------------------------------------- #
modal_UI <- function(id){ 
    
    tags$span(
        tags$h4( style = "font-size:15px; margin-left: 10px;" ,
            actionLink(inputId = ("rec_modal") , label = "자세히보기")
        )
    
    )

    

}


# server ----------------------------------------------------------------------------------------- #
modal_Server <- function(id , modal_source){ 
        ns <- session$ns
        print("되는데")
        
        modal_list <- modal_source()
        print(modal_list)
        observeEvent(input$rec_modal,{
                
                showModal(modal_ui_file(
                    name = modal_list[["res_name"]],
                    category = modal_list[["res_cate"]],
                    menu = modal_list[["res_menu"]],
                    rating_naver = NULL,
                    price = NULL,
                    distance = NULL
                ))
            })
        
        
       



        
        

        

}