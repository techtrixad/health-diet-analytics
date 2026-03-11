# -------------------------------
# LOAD LIBRARIES
# -------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(bslib)
library(shinycssloaders)
library(rmarkdown)
library(webshot)

# -------------------------------
# LOAD DATASET
# -------------------------------

food_data <- read.csv("food_data.csv", stringsAsFactors = FALSE)

# -------------------------------
# DATA PREPROCESSING
# -------------------------------

food_data <- food_data %>%
  mutate(
    Calories = ifelse(is.na(Calories), mean(Calories, na.rm = TRUE), Calories),
    Protein  = ifelse(is.na(Protein), mean(Protein, na.rm = TRUE), Protein),
    Carbs    = ifelse(is.na(Carbs), mean(Carbs, na.rm = TRUE), Carbs),
    Fat      = ifelse(is.na(Fat), mean(Fat, na.rm = TRUE), Fat),
    Fiber    = ifelse(is.na(Fiber), mean(Fiber, na.rm = TRUE), Fiber),
    Sugar    = ifelse(is.na(Sugar), mean(Sugar, na.rm = TRUE), Sugar),
    Sodium   = ifelse(is.na(Sodium), mean(Sodium, na.rm = TRUE), Sodium)
  )

food_data <- distinct(food_data)

food_data$Type <- as.factor(food_data$Type)
food_data$Meal <- as.factor(food_data$Meal)

# -------------------------------
# UI
# -------------------------------

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#2C7BE5",
    secondary = "#00D97E"
  ),
  
  tags$head(
    
    tags$style(HTML("
    
    body{
      transition: background 0.5s;
    }
    
    .card{
      padding:20px;
      border-radius:15px;
      margin-bottom:20px;
      box-shadow:0 4px 20px rgba(0,0,0,0.1);
      transition: all 0.3s;
      background:white;
    }
    
    .card:hover{
      transform: translateY(-5px);
      box-shadow:0 10px 30px rgba(0,0,0,0.2);
    }
    
    .title{
      font-size:30px;
      font-weight:bold;
      color:#2C7BE5;
      text-align:center;
      margin-bottom:20px;
    }
    
    "))
  ),
  
  titlePanel(div(class="title","AI Diet Recommendation & Health Analytics System")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxInput("darkmode","Enable Dark Mode"),
      
      numericInput("age","Age",22),
      
      selectInput("gender","Gender",
                  choices=c("Male","Female")),
      
      numericInput("height","Height (cm)",170),
      numericInput("weight","Weight (kg)",70),
      
      selectInput("activity","Activity Level",
                  
                  choices=c(
                    "Sedentary"=1.2,
                    "Light Activity"=1.375,
                    "Moderate Exercise"=1.55,
                    "Very Active"=1.725
                  )),
      
      selectInput("goal","Fitness Goal",
                  choices=c(
                    "Weight Loss",
                    "Maintain Weight",
                    "Weight Gain"
                  )),
      
      selectInput("diet_type","Diet Preference",
                  choices=c("Veg","NonVeg")),
      
      br(),
      
      actionButton("recommend",
                   "Generate Diet Plan",
                   class="btn-primary"),
      
      br(),br(),
      
      downloadButton("download_pdf","Download Diet Report"),
      downloadButton("download_img","Download Image")
      
    ),
    
    mainPanel(
      
      fluidRow(
        
        column(6,
               
               div(class="card",
                   
                   h3("BMI Result"),
                   
                   textOutput("bmi_text"),
                   textOutput("bmi_category"),
                   
                   plotlyOutput("bmi_gauge") %>% withSpinner()
                   
               )
        ),
        
        column(6,
               
               div(class="card",
                   
                   h3("Daily Calories Needed"),
                   
                   textOutput("calorie_need")
                   
               )
        )
        
      ),
      
      div(class="card",
          
          h3("Recommended Diet Plan"),
          
          tableOutput("food_table")
          
      ),
      
      fluidRow(
        
        column(6,
               
               div(class="card",
                   
                   h3("Calories Distribution"),
                   
                   plotlyOutput("calorie_chart") %>% withSpinner()
                   
               )
        ),
        
        column(6,
               
               div(class="card",
                   
                   h3("Macronutrient Analysis"),
                   
                   plotlyOutput("macro_chart") %>% withSpinner()
                   
               )
        )
        
      ),
      
      div(class="card",
          
          h3("Nutrition Dashboard"),
          
          plotlyOutput("nutrition_dashboard") %>% withSpinner()
          
      )
      
    )
  )
)

# -------------------------------
# SERVER
# -------------------------------

server <- function(input, output, session) {
  
  observe({
    
    if(input$darkmode){
      
      session$setCurrentTheme(
        bs_theme(
          bootswatch="darkly",
          primary="#00D97E"
        )
      )
      
    } else{
      
      session$setCurrentTheme(
        bs_theme(
          bootswatch="cosmo",
          primary="#2C7BE5"
        )
      )
      
    }
    
  })
  
  observeEvent(input$recommend,{
    
    height_m <- input$height/100
    bmi <- input$weight/(height_m^2)
    
    output$bmi_text <- renderText({
      paste("Your BMI:", round(bmi,2))
    })
    
    category <- if(bmi < 18.5){
      "Underweight"
    } else if(bmi < 25){
      "Healthy"
    } else if(bmi < 30){
      "Overweight"
    } else{
      "Obese"
    }
    
    output$bmi_category <- renderText({
      paste("Category:",category)
    })
    
    output$bmi_gauge <- renderPlotly({
      
      plot_ly(
        type="indicator",
        mode="gauge+number",
        value=bmi,
        title=list(text="BMI Gauge"),
        gauge=list(
          axis=list(range=list(NULL,40)),
          steps=list(
            list(range=c(0,18.5),color="lightblue"),
            list(range=c(18.5,24.9),color="lightgreen"),
            list(range=c(25,29.9),color="orange"),
            list(range=c(30,40),color="red")
          )
        )
      )
      
    })
    
    if(input$gender=="Male"){
      
      bmr <- 10*input$weight + 6.25*input$height - 5*input$age + 5
      
    } else{
      
      bmr <- 10*input$weight + 6.25*input$height - 5*input$age - 161
      
    }
    
    tdee <- bmr * as.numeric(input$activity)
    
    target_calories <- if(input$goal=="Weight Loss"){
      
      tdee - 500
      
    } else if(input$goal=="Weight Gain"){
      
      tdee + 500
      
    } else{
      
      tdee
      
    }
    
    output$calorie_need <- renderText({
      paste("Daily Calories Needed:",round(target_calories))
    })
    
    recommended <- food_data %>%
      filter(Type==input$diet_type)
    
    breakfast <- recommended %>% filter(Meal=="Breakfast") %>% sample_n(2)
    lunch <- recommended %>% filter(Meal=="Lunch") %>% sample_n(2)
    snack <- recommended %>% filter(Meal=="Snack") %>% sample_n(2)
    dinner <- recommended %>% filter(Meal=="Dinner") %>% sample_n(2)
    
    diet_plan <- bind_rows(breakfast,lunch,snack,dinner)
    
    output$food_table <- renderTable({
      diet_plan
    })
    
    calorie_plot <- ggplot(diet_plan,
                           aes(x=Meal,
                               y=Calories,
                               fill=Meal))+
      geom_bar(stat="identity")+
      theme_minimal()
    
    output$calorie_chart <- renderPlotly({
      ggplotly(calorie_plot)
    })
    
    macro_data <- diet_plan %>%
      select(Meal,Protein,Carbs,Fat) %>%
      pivot_longer(-Meal,
                   names_to="Nutrient",
                   values_to="Value")
    
    macro_plot <- ggplot(macro_data,
                         aes(x=Meal,
                             y=Value,
                             fill=Nutrient))+
      geom_bar(stat="identity",
               position="dodge")+
      theme_minimal()
    
    output$macro_chart <- renderPlotly({
      ggplotly(macro_plot)
    })
    
    nutrition_plot <- ggplot(diet_plan,
                             aes(x=Food,
                                 y=Calories,
                                 fill=Meal))+
      geom_bar(stat="identity")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle=45,hjust=1))
    
    output$nutrition_dashboard <- renderPlotly({
      ggplotly(nutrition_plot)
    })
    
    output$download_img <- downloadHandler(
      
      filename=function(){
        "diet_dashboard.png"
      },
      
      content=function(file){
        
        png(file,width=1200,height=800)
        print(calorie_plot)
        dev.off()
        
      }
    )
    
    output$download_pdf <- downloadHandler(
      
      filename = function(){
        paste("Diet_Report_",Sys.Date(),".pdf",sep="")
      },
      
      content = function(file){
        
        tempReport <- file.path(tempdir(), "report.Rmd")
        
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(
          diet_table = diet_plan,
          calorie_plot = calorie_plot,
          macro_plot = macro_plot,
          nutrition_plot = nutrition_plot,
          bmi = bmi,
          category = category,
          calories = target_calories
        )
        
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        
      }
    )
    
  })
  
}

# -------------------------------
# RUN APP
# -------------------------------

shinyApp(ui=ui, server=server)
