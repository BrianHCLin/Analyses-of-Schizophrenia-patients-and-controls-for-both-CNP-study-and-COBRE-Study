library(shinydashboard)
library(ggplot2)
library(maxent)
library(dplyr)

# Loading Datasets From Shiny_Data File
Data_shiny <- read.csv("~/Desktop/Shiny_Data/Stats_141_Combined_Data.csv")
Data_Logi <- read.csv("~/Desktop/Shiny_Data/Stats_141_Combined_Data2.csv")
Data_CNP_Logi <- read.csv("~/Desktop/Shiny_Data/Data_CNP_Logi.csv")
Data_train <- read.csv("~/Desktop/Shiny_Data/Data_train.csv")
Data_COBRE_Logi <- read.csv("~/Desktop/Shiny_Data/Data_COBRE_Logi.csv")
Data_CNP_Logi$Subject_Type <- as.factor(Data_CNP_Logi$Subject_Type)

# Loading Logistic Models From Shiny_Data File
logistic_m1 <- load.model("~/Desktop/Shiny_Data/logistic_model_cnp.RData")
logistic_m2 <- load.model("~/Desktop/Shiny_Data/logistic_model_cobre.RData")
logistic_m3 <- load.model("~/Desktop/Shiny_Data/logistic_model.RData")

# Loading Data for Interaction Plots From Shiny_Data File
load("~/Desktop/Shiny_Data/gg_int.rda")
load("~/Desktop/Shiny_Data/gg_int2.rda")
load("~/Desktop/Shiny_Data/gg_int3.rda")

# Loading Data for Effect Plot From Shiny_Data File
load("~/Desktop/Shiny_Data/effect_merged.rda")

# Loading Data for Cutoff Plots From Shiny_Data File
load("~/Desktop/Shiny_Data/acc_info.rda")
load("~/Desktop/Shiny_Data/acc_info2.rda")
load("~/Desktop/Shiny_Data/acc_info3.rda")

# Loading Data for ROC plots From Shiny_Data File
load("~/Desktop/Shiny_Data/roc_info1.rda")
load("~/Desktop/Shiny_Data/roc_info2.rda")

# Loading Data for Confusion Matrix Plot From Shiny_Data File
load("~/Desktop/Shiny_Data/cm_info_cnp.rda")
load("~/Desktop/Shiny_Data/cm_info_cobre.rda")

# UI function
ui <- dashboardPage(
  dashboardHeader(title = "Schizophrenia Data Analysis", titleWidth = 300),
  
  # Sidebar Design
  
  dashboardSidebar( width = 300,
    sidebarMenu(
      hr(),
      # Merged Data
      menuItem("Merged Data", icon = icon("database"), selected = TRUE,
               menuSubItem("Exploratory Plots", tabName = "plot_merged", icon = icon("bar-chart")),
               menuSubItem("Hypothesis Tests", tabName = "test_merged", icon = icon("table")),
               menuSubItem("Interaction Plot", tabName = "interact_merged", icon = icon("line-chart")),
               menuSubItem("Effect Plot", tabName = "effect_merged", icon = icon("line-chart")),
               menuSubItem("Cutoff Plot", tabName = "cutoff_merged", icon = icon("line-chart"))),
      # CNP Data
      menuItem("CNP Data", icon = icon("database"),
               menuSubItem("Exploratory Plots", tabName = "plot_cnp", icon = icon("bar-chart")),
               menuSubItem("Hypothesis Tests", tabName = "test_cnp", icon = icon("table")),
               menuSubItem("Interaction Plot", tabName = "interact_cnp", icon = icon("line-chart")),
               menuSubItem("Cutoff Plot", tabName = "cutoff_cnp", icon = icon("line-chart")),
               menuSubItem("Confusion Matrix", tabName = "cm_cnp", icon = icon("th-large")),
               menuSubItem("ROC Plot", tabName = "roc_cnp", icon = icon("line-chart"))),
      # COBRE Data
      menuItem("COBRE Data", icon = icon("database"),
               menuSubItem("Exploratory Plots", tabName = "plot_cobre", icon = icon("bar-chart")),
               menuSubItem("Hypothesis Tests", tabName = "test_cobre", icon = icon("table")),
               menuSubItem("Interaction Plot", tabName = "interact_cobre", icon = icon("line-chart")),
               menuSubItem("Cutoff Plot", tabName = "cutoff_cobre", icon = icon("line-chart")),
               menuSubItem("Confusion Matrix", tabName = "cm_cobre", icon = icon("th-large")),
               menuSubItem("ROC Plot", tabName = "roc_cobre", icon = icon("line-chart")))
    )
  ),
  
  # Items to show in main panel
  
  dashboardBody(
    tabItems(
      
      # Merged Data
      
      tabItem(tabName = "plot_merged",
              
              # Boxes need to be put in a row (or column)
              
              fluidRow(
                column(width = 3,
                       box(width = NULL, selectInput("variable", "Variable:", choices = c("Age", "Gender", "Ethnicity", "Education")),
                           conditionalPanel(condition = "input.variable == 'Age' | input.variable == 'Education'", 
                                            radioButtons("plot_type_1", "Plot Type:", choices = c("Boxplot", "Density Curve"), selected = "Boxplot")),
                           conditionalPanel(condition = "input.variable == 'Gender' | input.variable == 'Ethnicity'", 
                                            radioButtons("plot_type_2", "Plot Type:", choices = c("Barplot"), selected = "Barplot"))
                       )
                ),
                column(width = 9,
                       box(width = NULL, plotOutput("plot1",height = 500), collapsible = TRUE,
                           title = "Plot", status = "primary", solidHeader = TRUE
                       )
                )
              )
      ),
      tabItem(tabName = "test_merged",
                  box(width = NULL, radioButtons("test_type", "Hypothesis Tests:", choices = c("ANOVA", "Two Sample t Tests", "Chi-square Tests"), selected = "ANOVA")),
                  box(width = NULL, status = "primary", solidHeader = TRUE, title="Table",                
                    downloadButton('downloadTable', 'Download'),
                    br(),br(),
                    tableOutput("table_merged")
                  )
      ),
      tabItem(tabName = "interact_merged",
              fluidRow(
                box(width = 8, plotOutput("interactplot_merged", height = 500), collapsible = TRUE,
                    title = "Interaction Plot", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("interactplot_merged_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      tabItem(tabName = "effect_merged",
              fluidRow(
                box(width = 8, plotOutput("effectplot_merged", height = 500), collapsible = TRUE,
                    title = "Effect Plot", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("effectplot_merged_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      tabItem(tabName = "cutoff_merged",
              fluidRow(
                box(width = 8, plotOutput("cutoffplot_merged", height = 500), collapsible = TRUE,
                    title = "Cutoff Plot", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("cutoffplot_merged_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      
      # CNP Data
      
      tabItem(tabName = "plot_cnp",
              fluidRow(
                column(width = 3,
                       box(width = NULL, selectInput("variable_cnp", "Variable:", choices = c("Age", "Gender", "Ethnicity", "Education")),
                          conditionalPanel(condition = "input.variable_cnp == 'Age' | input.variable_cnp == 'Education'", 
                                           radioButtons("plot_type_cnp1", "Plot Type:", choices = c("Density Histogram"), selected = "Density Histogram")),
                          conditionalPanel(condition = "input.variable_cnp == 'Gender' | input.variable_cnp == 'Ethnicity'", 
                                           radioButtons("plot_type_cnp2", "Plot Type:", choices = c("Barplot"), selected = "Barplot"))
                       )
                ),
                column(width = 9,
                      box(width = NULL, plotOutput("plot_cnp",height="500px"), collapsible = TRUE,
                          title = "Plot", status = "primary", solidHeader = TRUE
                      )
                )
              )
      ),
      tabItem(tabName = "test_cnp",
              box(width = NULL, status = "primary", solidHeader = TRUE, title="ANOVA Table For Logistic Model",                
                  downloadButton('downloadTable2', 'Download'),
                  br(),br(),
                  tableOutput("table_cnp")
              )
      ),
      tabItem(tabName = "interact_cnp",
              fluidRow(
                box(width = 8, plotOutput("interactplot_cnp", height = 500), collapsible = TRUE,
                    title = "Interaction Plot", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("interactplot_cnp_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      tabItem(tabName = "cutoff_cnp",
              fluidRow(
                box(width = 8, plotOutput("cutoffplot_cnp", height = 500), collapsible = TRUE,
                    title = "Cutoff Plot", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("cutoffplot_cnp_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      tabItem(tabName = "cm_cnp",
              fluidRow(
                box(width = 8, plotOutput("cmplot_cnp", height = 500), collapsible = TRUE,
                    title = "Confusion Matrix", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("cmplot_cnp_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      tabItem(tabName = "roc_cnp",
              fluidRow(
                box(width = 8, plotOutput("rocplot_cnp", height = 500), collapsible = TRUE,
                    title = "ROC Plot", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("rocplot_cnp_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      
      # COBRE Data
      
      tabItem(tabName = "plot_cobre",
              fluidRow(
                column(width = 3,
                       box(width = NULL, selectInput("variable_cobre", "Variable:", choices = c("Age", "Gender", "Ethnicity", "Education")),
                           conditionalPanel(condition = "input.variable_cobre == 'Age' | input.variable_cobre == 'Education'", 
                                            radioButtons("plot_type_cobre1", "Plot Type:", choices = c("Density Histogram"), selected = "Density Histogram")),
                           conditionalPanel(condition = "input.variable_cobre == 'Gender' | input.variable_cobre == 'Ethnicity'", 
                                            radioButtons("plot_type_cobre2", "Plot Type:", choices = c("Barplot"), selected = "Barplot"))
                       )
                ),
                column(width = 9,
                       box(width = NULL, plotOutput("plot_cobre",height="500px"), collapsible = TRUE,
                           title = "Plot", status = "primary", solidHeader = TRUE
                       )
                )
              )
      ),
      tabItem(tabName = "test_cobre",
              box(width = NULL, status = "primary", solidHeader = TRUE, title="ANOVA Table For Logistic Model",                
                  downloadButton('downloadTable3', 'Download'),
                  br(),br(),
                  tableOutput("table_cobre")
              )
      ),
      tabItem(tabName = "interact_cobre",
              fluidRow(
                box(width = 8, plotOutput("interactplot_cobre", height = 500), collapsible = TRUE,
                    title = "Interaction Plot", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("interactplot_cobre_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      tabItem(tabName = "cutoff_cobre",
              fluidRow(
                box(width = 8, plotOutput("cutoffplot_cobre", height = 500), collapsible = TRUE,
                    title = "Cutoff Plot", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("cutoffplot_cobre_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      tabItem(tabName = "cm_cobre",
              fluidRow(
                box(width = 8, plotOutput("cmplot_cobre", height = 500), collapsible = TRUE,
                    title = "Confusion Matrix", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("cmplot_cobre_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      ),
      tabItem(tabName = "roc_cobre",
              fluidRow(
                box(width = 8, plotOutput("rocplot_cobre", height = 500), collapsible = TRUE,
                    title = "ROC Plot", status = "primary", solidHeader = TRUE),
                box(width = 4, htmlOutput("rocplot_cobre_explain"), collapsible = TRUE, 
                    title = "Interpretation", status = "primary", solidHeader = TRUE)
              )
      )
    )
  )
)


server <- function(input, output) {

  # Merged Data
  
  output$plot1 <- renderPlot({
    plot_Data <- data.frame(x = Data_shiny[[input$variable]], Study = Data_shiny$Study)
    if(input$variable == "Age" || input$variable == "Education"){
    if(input$plot_type_1 == "Boxplot") {
      p <- ggplot(na.omit(plot_Data), aes(Study, x)) + geom_boxplot(fill = "steelblue") + ylab(input$variable) + xlab("Study")
    } else if(input$plot_type_1 == "Density Curve"){
      p <- ggplot(na.omit(plot_Data), aes(x, fill = Study)) + geom_density(alpha = 0.4) + ylab("Density") + xlab(input$variable)
      }
    }
    if(input$variable == "Gender" || input$variable == "Ethnicity"){
        p <- ggplot(na.omit(plot_Data), aes(x, fill = Study)) + geom_bar() + ylab("Frequency") + xlab(input$variable)
    }
    print(p)
  })

  table_merged_function <- reactive({
    if(input$test_type == "ANOVA") {
      anova_table <- anova(logistic_m3, test = "Chisq")
      junk <- read.table(textConnection(capture.output(anova(logistic_m3, test = "Chisq"))[11:39]), fill = TRUE)
      junk$V7 <- as.character(junk$V7)
      var_names <- variable.names(Data_Logi)
      var_names[1] <- "Null"
      var_names[27] <- "Age:Subject_TypeSchizophrenia"
      tab <- cbind(Variable = var_names, anova_table, Significance = junk$V7[1:27])
      return(tab)
    } else if(input$test_type == "Two Sample t Tests") {
      t1 <- t.test(Age~Study, data = Data_Logi)
      t2 <- t.test(Education~Study, data = Data_logi)
      tab <- data_frame(Variable = c(t1$data.name, t2$data.name), `t Statistics` = c(t1$statistic, t2$statistic), 
                 `Degrees of Freedom` = c(t1$parameter, t2$parameter), `p Value` = c(t1$p.value, t2$p.value), `Confidence` = c(t1$conf.int[1], t2$conf.int[1]), `Interval` = c(t1$conf.int[2], t2$conf.int[2]))
      return(tab)
    } else if(input$test_type == "Chi-square Tests") {
      c1 <- chisq.test(table(Data_logi$Study, Data_logi$Gender))
      c2 <- chisq.test(table(Data_logi$Study, Data_logi$Ethnicity))
      c3 <- chisq.test(table(Data_logi$Study, Data_logi$Subject_Type))
      tab <- data_frame(Variable = c(c1$data.name, c2$data.name, c3$data.name), `Chi-square` = c(c1$statistic, c2$statistic, c3$statistic), `Degrees of Freedom` = c(c1$parameter, c2$parameter, c3$parameter), `p Value` = c(c1$p.value, c2$p.value, c3$p.value))
      return(tab)
    }
  })
  
  output$table_merged <- renderTable({
    table_merged_function()
  }, digits = 6)
  
  output$downloadTable <- downloadHandler(
    filename = "table.csv",
    content = function(file) {
      write.csv(table_merged_function(), file)
    }
  )
  
  output$interactplot_merged <- renderPlot({
    plot(gg_int3)
  })
  
  output$interactplot_merged_explain <- renderUI({
    HTML(
      paste0("Small [i][j] entries having small [i][i] entries indicate an interaction between variable i and variable j", '<br/>', '<br/>',
             "Here, there is no interaction between any of the variables, but an interaction term between age and subject type (Control vs Schizophrenia)
             is included in the model to confirm if there is truly no interaction between the two variables")
    )
  })
  
  output$effectplot_merged <- renderPlot({
    plot(effect_merged,ask = FALSE)
  })
  
  output$effectplot_merged_explain <- renderUI({
    HTML(
      paste0("Lines would cross if there is interaction between two variables and would be parallel if there is no interaction", '<br/>', '<br/>',
             "According to the effect plot, as age increases, the probability of being in COBRE data increases for 
             both control and schizophrenia patients. Since the trends are about the same for both groups and the 
             two lines are nearly parallel, there is no interaction between age and subject type (Control vs Schizophrenia)")
      )
  })
  
  output$cutoffplot_merged <- renderPlot({
    accuracy_info3$plot
  })
  
  output$cutoffplot_merged_explain <- renderUI({
    HTML(
      paste0("Cross validation was used on the logistic model to find the most optimal cutoff for highest accuracy", '<br/>', '<br/>',
             "According to the cutoff plot and taking account of the true proportion in the data, the most optimal cutoff is at 0.55", '<br/>', '<br/>',
             "A high true positive rate is not required for predicting the study, thus confusion matrix and 
             ROC curve analyses will not be carried out")
    )
  })
  
  # CNP Data
  
  filtered_cnp <- Data_shiny %>% filter(Study == "CNP")
  
  output$plot_cnp <- renderPlot({
    plot_Data2 <- data.frame(x = filtered_cnp[[input$variable_cnp]])
    if(input$variable_cnp == "Age" || input$variable_cnp == "Education"){
      p <- ggplot(na.omit(plot_Data2)) + geom_histogram(aes(x, y = ..density..), color = "dimgray") + geom_density(aes(x, y = ..density..), fill = "steel blue", alpha = 0.4) + ylab("Density") + xlab(input$variable_cnp)
    }
    if(input$variable_cnp == "Gender" || input$variable_cnp == "Ethnicity"){
      p <- ggplot(na.omit(plot_Data2), aes(x)) + geom_bar() + ylab("Frequency") + xlab(input$variable_cnp)
    }
    print(p)
  })
  
  table_cnp_function <- reactive({
    anova_table2 <- anova(logistic_m1, test = "Chisq")
    junk2 <- read.table(textConnection(capture.output(anova(logistic_m1, test = "Chisq"))[11:28]), fill = TRUE)
    junk2$V7 <- as.character(junk2$V7)
    var_names2 <- variable.names(Data_CNP_Logi)
    var_names2[1] <- "NULL"
    tab2 <- cbind(Variable = var_names2, anova_table2, Significance = junk2$V7)
    return(tab2)
  })
  
  output$table_cnp <- renderTable({
    table_cnp_function()
  }, digits = 6)
  
  output$downloadTable2 <- downloadHandler(
    filename = "table.csv",
    content = function(file) {
      write.csv(table_cnp_function(), file)
    }
  )
  
  output$interactplot_cnp <- renderPlot({
    plot(gg_int)
  })
  
  output$interactplot_cnp_explain <- renderUI({
    HTML(
      paste0("Small [i][j] entries having small [i][i] entries indicate an interaction between variable i and variable j", '<br/>', '<br/>',
             "Here, there is no interaction between any of the variables, so no interaction term is added to the logistic model")
    )
  })
  
  output$cutoffplot_cnp <- renderPlot({
    accuracy_info$plot
  })
  
  output$cutoffplot_cnp_explain <- renderUI({
    HTML(
      paste0("Cross validation was used on the logistic model to find the most optimal cutoff for highest accuracy", '<br/>', '<br/>',
             "According to the cutoff plot and taking account of the true proportion in the data, the most optimal cutoff is at 0.75")
    )
  })
  
  output$cmplot_cnp <- renderPlot({
    cm_info$plot
  })
  
  output$cmplot_cnp_explain <- renderUI({
    HTML(
      paste0("Logistic model works well when the ratio between two classes is around 1:1, but in CNP data, the ratio between the two categories of subject type 
             is around 7:3, with controls being the majority", '<br/>', '<br/>',
             "Thus the low true positive rate shown in the plot could be due to the imbalance in the proportion of subject types in the data"
             )
    )
  })
  
  output$rocplot_cnp <- renderPlot({
    grid.draw(roc_info$plot)
  })
  
  output$rocplot_cnp_explain <- renderUI({
    HTML(
      paste0("ROC curve analysis was used to find the optimal cutoff that improves the true positive rate", '<br/>', '<br/>',
             "The cost of false negative is set to be 1500 and that of false positive is set to be 1000. The higher cost for a false
             negative allows us to get a higher true positive rate without losing too much accuracy", '<br/>', '<br/>',
             "Based on the plot, the optimal cutoff is at 0.69")
    )
  })
  
  # COBRE Data
  
  filtered_cobre <- Data_shiny %>% filter(Study == "COBRE")
  
  output$plot_cobre <- renderPlot({
    plot_Data3 <- data.frame(x = filtered_cobre[[input$variable_cobre]])
    if(input$variable_cobre == "Age" || input$variable_cobre == "Education"){
      p <- ggplot(na.omit(plot_Data3)) + geom_histogram(aes(x, y = ..density..), color = "dimgray") + geom_density(aes(x, y = ..density..), fill = "steel blue", alpha = 0.4) + ylab("Density") + xlab(input$variable_cobre)
    }
    if(input$variable_cobre == "Gender" || input$variable_cobre == "Ethnicity"){
      p <- ggplot(na.omit(plot_Data3), aes(x)) + geom_bar() + ylab("Frequency") + xlab(input$variable_cobre)
    }
    print(p)
  })
  
  table_cobre_function <- reactive({
    anova_table3 <- anova(logistic_m2, test = "Chisq")
    var_names3 <- variable.names(Data_COBRE_Logi)
    var_names3[1] <- "NULL"
    junk3 <- read.table(textConnection(capture.output(anova(logistic_m2, test = "Chisq"))[11:27]), fill = TRUE)
    junk3$V7 <- as.character(junk3$V7)
    tab3 <- cbind(Variable = var_names3, anova_table3, Significance = junk3$V7)
    return(tab3)
  })
  
  output$table_cobre <- renderTable({
    table_cobre_function()
  }, digits = 6)
  
  output$downloadTable3 <- downloadHandler(
    filename = "table.csv",
    content = function(file) {
      write.csv(table_cobre_function(), file)
    }
  )
  
  output$interactplot_cobre <- renderPlot({
    plot(gg_int2)
  })
  
  output$interactplot_cobre_explain <- renderUI({
    HTML(
      paste0("Small [i][j] entries having small [i][i] entries indicate an interaction between variable i and variable j", '<br/>', '<br/>',
             "Here, there is no interaction between any of the variables, so no interaction term is added to the logistic model")
    )
  })
  
  output$cutoffplot_cobre <- renderPlot({
    accuracy_info2$plot
  })
  
  output$cutoffplot_cobre_explain <- renderUI({
    HTML(
      paste0("Cross validation was used on the logistic model to find the most optimal cutoff for highest accuracy", '<br/>', '<br/>',
             "According to the cutoff plot the most optimal cutoff is at 0.425")
    )
  })
  
  output$cmplot_cobre <- renderPlot({
    cm_info2$plot
  })
  
  output$cmplot_cobre_explain <- renderUI({
    HTML(
      paste0("The logistic model for COBRE data with cutoff at 0.425 has a true positive rate of 72.9%", '<br/>', '<br/>',
             "Logistic model works well when the ratio between two classes is around 1:1. In COBRE data, the ratio between the two categories of subject type 
             is more balanced compared to CNP data", '<br/>', '<br/>',
             "Thus the true positive rate is higher for COBRE data"
      )
    )
  })
  
  output$rocplot_cobre <- renderPlot({
    grid.draw(roc_info2$plot)
  })
  
  output$rocplot_cobre_explain <- renderUI({
    HTML(
      paste0("ROC curve analysis was used to find the optimal cutoff that further improves the true positive rate", '<br/>', '<br/>',
             "The cost of false negative is set to be 1500 and that of false positive is set to be 1000. The higher cost for a false
             negative allows us to get a higher true positive rate without losing too much accuracy", '<br/>', '<br/>',
             "Based on the plot, the optimal cutoff is at 0.43, which is close to the optimal cutoff obtained by Cross Validation analysis")
    )
  })
  
}


shinyApp(ui, server)