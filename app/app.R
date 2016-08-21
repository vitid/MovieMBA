library(shiny)
library(DT)
library(stringr)
library(sqldf)
library(ggplot2)

assoc_rules = read.csv("www/assoc_rules_web.csv",stringsAsFactors = FALSE)

lhs_title = paste(unique(assoc_rules$left_title),collapse = '","')
lhs_title = str_c("\"",lhs_title,"\"")

rhs_title = paste(unique(assoc_rules$right_title),collapse = '","')
rhs_title = str_c("\"",rhs_title,"\"")

ui = navbarPage(title = "Movie Recommendation with Market Basket Analysis",
    tabPanel("App",
    tags$head(
        HTML('<link rel="stylesheet" href="//code.jquery.com/ui/1.12.0/themes/base/jquery-ui.css">'),
        HTML('<script src="https://code.jquery.com/ui/1.12.0/jquery-ui.js"></script>'),
        HTML(
            str_c(
            '
                 <script>
                var lhsTitles = [',lhs_title,']
                var rhsTitles = [',rhs_title,']
                </script>
            '
            )
        ),
        HTML('
        <script>
             $(document).ready(function(){
                $("#filmLhs").autocomplete({
                    source: lhsTitles,
                    //send Shiny.onInputChange because Shiny does not recognize text changed by js!!
                    close: function(event,ui){
                        Shiny.onInputChange("filmLhs",$("#filmLhs").val());
                    }
                })
                $("#filmRhs").autocomplete({
                    source: rhsTitles,
                    //send Shiny.onInputChange because Shiny does not recognize text changed by js!!
                    close: function(event,ui){
                        Shiny.onInputChange("filmRhs",$("#filmRhs").val());
                    }
                })
                //submit when the page first loaded
                setTimeout(function(){
                    Shiny.onInputChange("submit","1");
                },100)
             }
             )
        </script>
             ')
    ),
    fluidRow(
        column(3,
               wellPanel(
                   HTML('<h2 style="text-align: center">LHS film</h2>'),
                   textInput(
                       inputId = "filmLhs",
                       label = "Film name",
                       placeholder = "name"
                   ),
                   fluidRow(
                       column(6,
                              textInput(
                                  inputId = "fromYearLhs",
                                  label = "From year",
                                  placeholder = "from year"
                              )),
                       column(6,
                              textInput(
                                  inputId = "toYearLhs",
                                  label = "To year",
                                  placeholder = "to year"
                              ))
                   )
               ),
               wellPanel(
                   HTML('<h2 style="text-align: center">RHS film</h2>'),
                   textInput(
                       inputId = "filmRhs",
                       label = "Film name",
                       placeholder = "name"
                   ),
                   fluidRow(
                       column(6,
                              textInput(
                                  inputId = "fromYearRhs",
                                  label = "From year",
                                  placeholder = "from year"
                              )),
                       column(6,
                              textInput(
                                  inputId = "toYearRhs",
                                  label = "To year",
                                  placeholder = "to year"
                              ))
                   )
               ),
               wellPanel(
                   textInput(
                       inputId = "yearApart",
                       label = "Released years should be far apart at least",
                       placeholder = "#yrs."
                   ),
                   textInput(
                       inputId = "numCommonGenre",
                       label = "Number of common genres",
                       placeholder = "#common genres"
                   )
               ),
               actionButton(
                   inputId = "submit",
                   label = "Submit",
                   width = "100%"
               )
        ),
        
        column(9,
               plotOutput("distPlot"),
               dataTableOutput("showTable")
        )
    )
    ),
    tabPanel("Report",
        HTML('
             <h2>Please look at <a href="http://rpubs.com/vitidN/203264">this site</a></h2>
             ')
    )
)

server = function(input,output){
    show_rules = eventReactive(input$submit,{
        query_string = "select *
        from assoc_rules
        where 1 = 1
        "
        if(str_length(input$filmLhs) > 0){
            query_string = str_c(query_string, " and left_title = '",input$filmLhs,"' ")
        }
        if( str_length(input$fromYearLhs) > 0 ){
            query_string = str_c(query_string, " and left_year >= ",input$fromYearLhs)
        }
        
        if( str_length(input$toYearLhs) > 0 ){
            query_string = str_c(query_string, " and left_year <= ",input$toYearLhs)
        }
        
        if(str_length(input$filmRhs) > 0){
            query_string = str_c(query_string, " and right_title = '",input$filmRhs,"' ")
        }
        
        if( str_length(input$fromYearRhs) > 0 ){
            query_string = str_c(query_string, " and right_year >= ",input$fromYearRhs)
        }
        
        if( str_length(input$toYearRhs) > 0 ){
            query_string = str_c(query_string, " and right_year <= ",input$toYearRhs)
        }
        
        if(str_length(input$yearApart) > 0){
            query_string = str_c(query_string, " and ( left_year - right_year >= ", 
                                 input$yearApart,
                                 " or right_year - left_year >= ",input$yearApart, " ) ")
        }
        
        if(str_length(input$numCommonGenre) > 0){
            query_string = str_c(query_string," and common_genre = ",input$numCommonGenre)
        }
        
        sqldf(query_string)
    })
    output$showTable = renderDataTable({
        show_rules()
    })
    output$distPlot = renderPlot({
        
        df = show_rules()["left_year"]
        colnames(df) = "year"
        df$movie_side = "lhs"
        
        
        df2 = show_rules()["right_year"]
        colnames(df2) = "year"
        df2$movie_side = "rhs"
        
        df=rbind(df,df2)
        
        ggplot(df) + geom_density(aes(year,fill=movie_side,color=movie_side),alpha = 0.1) +
            ggtitle("Distribution of years for films in the rule")
    })
    
}

shinyApp(ui = ui,server = server)