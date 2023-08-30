ui <- fluidPage(
  useShinyjs(),
  includeScript("www/custom.js"),
  includeCSS("www/styles.css"),

  theme = bs_theme(version = 5, font_scale = .8),


  #HTML function 지정
    tags$head(tags$script(type = "text/javascript", 
                        HTML("function getUploadTarget(value , id) {
        
        Shiny.setInputValue(value,id, {priority: 'event'});
        
        ;}"))
    ),

#    tags$head(tags$script(src="custom.js")),
    tags$style(
        HTML("
            .custom-font {
            border: none;
            outline: none;
            text-align: center;
            background-color: #00BFE5;
            }
            .nav-container {
                dislplay: flex;
                flex-direction: row;
                width: 100%;
                margin: 0;
                padding: 0;
                list-style-type: none;
                background-color: grey;
            }
            .nav-item {
                padding: 15px;
                cursor: pointer;
            }
            .nav-item a {
                text-align: center;
                text-decoration: none;
                color: white;
            }
            .my-icon {
                color: white;
                font-size: 50px;
            }
            .refresh-icon {
                color: grey;
                font-size: 20px;
                display: flex;
            }
            .space {
                width: 4px;
                height: auto;
                display: inline-block;
            }

        ")
    ),
    # fab_button --------------------------------------------------------------------------------- #
    fab_button(
        actionButton(
            inputId = "logout",
            label = "Logout",
            icon = icon("sign-out")
        ),
        # actionButton(
        #     inputId = "go_to_admin",
        #     label = "관리자 페이지",
        #     icon = icon("user")
        # ),
        actionButton(
            inputId = "go_to_review",
            label = "리뷰",
            icon = icon("star")
        ),
        actionButton(
            inputId = "go_to_search",
            label = "검색",
            icon = icon("search")
        ),
        actionButton(
            inputId = "go_to_home",
            label = "Home",
            icon = icon("house")
        ),
        inputId = "fab",
        position = "bottom-right"
    ),

    # TITLE , Navbarpage ------------------------------------------------------------------------- #
    fluidRow(
        tags$h1(
            tags$button(
                id = "title",
                class = "btn action-button",
                tags$img(
                    class = "img-fluid login-img mb-2",
                    title = "logo",
                    src = "what-to-eat-logo-wide.png"
                ),style = "border-color: transparent;"
            ),
            class = "custom-font",
            position = "fixed-top"
        )
    ),
    fluidRow(
        
        tags$div(
            style = "background-color : #FAFBFC;",
            actionButton(
                inputId = "hidden_admin_page",
                label = "Admin Page",
                style = "display:none;",
                class = "mx-3 mt-3 btn-outline-dark "
            )
        ),
        tags$div(
            style = "background-color : #FAFBFC;",
            class = "navbar_class",
            navbarPage(
                id = "navbarPage",
                title = "What to eat",
                
                #Home page
                tabPanel(
                    title = "Home",
                    value = "home",
                    home_UI("Home")
                ),
                tabPanel(
                    title = "음식점 검색",
                    value = "search",
                    search_UI("Search")
                ),
                tabPanel(
                    title = "평점 & 리뷰작성",
                    value = "rating",
                    review_UI("Review")
                ),
                tabPanel(
                    title = "ADMIN",
                    value = "admin",
                    Admin_UI("Admin"),
                    style = "display: none;"
                )
            ),
        
        
        )
    ),
)|> tagAppendAttributes(tag=_,  .cssSelector = "nav.navbar ", class = "sr-only")



# ------------------------------------------------------------------------------------------------ #
# authentication                                                                                   #
# ------------------------------------------------------------------------------------------------ #
secure_app(
    ui,
    id = "auth",

    # 로그인 top UI
    tags_top = tags$div(
        tags$img(
            class = "img-fluid login-img",
            title = "logo",
            src = "what-to-eat-logo-square.png"
        )
    ),    

    # 로그인 bottom UI
    tags_bottom = tags$div(

        tags$p(
            "관리자 문의:",
            style = "font-size: 1.4rem;",
            tags$a(
                href = "mailto:jeongwoo.yang@trialinformatics.com", 
                target = "_top", 
                "jeongwoo.yang@trialinformatics.com"
            ),
        ),

        # 회원가입 modal ui
        tags$div(
            class="modal fade", id="signup_modal", tabindex="-1", role="dialog",

            tags$div(
                class = "modal-dialog",
                role = "document",
                tags$div(
                    class = "modal-content",
                    tags$div(
                        class = "modal-header",
                        actionButton(
                            inputId = "signup_close_1",
                            label = HTML("&times;"),
                            class = "close",
                            title = "창 닫기",
                            `data-dismiss`="modal"
                        ),
                        h2(class = "text-center", "회원가입")
                    ),
                    tags$div(
                        class = "modal-body",
                        tags$div(
                            class = "text-left",
                            tags$p(
                                style = "font-size: 1.2rem; margin-bottom: 3rem;",
                                tags$span(
                                    "* ",
                                    style = "color:#dc3545; font-size:1.2rem;"
                                ),
                                "필수입력사항"
                            )
                        ),
                        textInput(
                            inputId = "user_name",
                            label = "이름",
                            placeholder = "이름을 입력해주세요.",
                            width = "100%"
                        ) |> tagAppendAttributes(tag = _, class = "required"),
                        textInput(
                            inputId = "user_email",
                            label = "회사 이메일 주소",
                            placeholder = "이메일을 입력해주세요.",
                            width = "100%"
                        ) |> tagAppendAttributes(tag = _, class = "required"),
                        passwordInput(
                            inputId = "password_1",
                            label = "비밀번호",
                            placeholder = "비밀번호를 입력하세요.",
                            width = "100%"
                        ) |> tagAppendAttributes(tag = _, class = "required"),
                        passwordInput(
                            inputId = "password_2",
                            label = "비밀번호 확인",
                            placeholder = "비밀번호 확인을 입력하세요.",
                            width = "100%"
                        ) |> tagAppendAttributes(tag = _, class = "required"),

                        uiOutput("signup_msg") # 회원가입 신청 성공 메시지 display
                    ),

                    tags$div(
                        class = "modal-footer",
                        tags$div(
                            class = "text-left",
                            tags$p(
                                style = "font-size: 1.2rem; margin-bottom: 3rem;",
                                "관라자 검토 및 승인 후, 회원가입이 완료됩니다."
                            )
                        ),                        
                        actionButton(
                            inputId = "signup_submit",
                            label = "회원가입 신청",
                            title = "회원가입 신청",
                            class = "btn-primary"
                        ),
                        actionButton(
                            inputId = "signup_close_2",
                            class = "btn btn-default",
                            `data-dismiss` = "modal",
                            title = "창 닫기",
                            "닫기"
                        )
                    )
                )
            )
        ),

        shinyFeedback::useShinyFeedback(),
        shinyjs::useShinyjs(),
        includeCSS("www/signup.css"),
        includeScript("www/signup.js"),
    
    ),

    fab_position = "none"
)


    