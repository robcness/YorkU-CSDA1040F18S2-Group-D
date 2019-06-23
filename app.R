
# PROOF OF CONCEPT - By Group D For YorkU CSDA1040 Class

library(shiny)
#library(tidyverse)
library(Matrix)
library(magrittr)# to use pipe (%>%) operator
library(dplyr)  # to use mutate(), select()) functions
library(tidyr) # to use spread() function
library(tibble) # to use enframe(), rowfunction

# ui.R

item_list <- readRDS("item_list.rds")
#item_list <- readRDS("C:/Users/joegy/OneDrive/Documents/York University/Advanced Predictive Analytics/Advanced Methods of Data Analysis - Hashmat 2.0/Assignments/Assignment_1/Mkt_Basket/DU_MktBasketAnalysis/item_list.rds")

ui <- fluidPage(
    
    # App title ----
    headerPanel("Product Recommender for Online Retailer"),
    h3("Proof of Concept - Developed by Group D for YORKU CSDA1040"),
    
    fluidRow(
        
        # Input selection
        column(6, 
               h4(align="center", HTML("Select Items, Complete Transaction <br/> Below For Suggested Items")),    
               wellPanel(
                   selectInput("input_item1", "Item #1", choices = c("", item_list)),
                   selectInput("input_item2", "Item #2", choices = c("", item_list)),
                   selectInput("input_item3", "Item #3", choices = c("", item_list)),
                   selectInput("input_item4", "Item #4", choices = c("", item_list)),
                   selectInput("input_item5", "Item #5", choices = c("", item_list)),
                   actionButton("submit", HTML("<b>","Complete Your Purchase"), width='80%')
               )
        ),
        
        # Output table
        column(6,
               h4(tags$b("Other Items You Might Be Interested In")), 
               h5(tags$i("(NOTE: Not all combinations of products return suggestions)")),
               tableOutput("item_recom")
        )
    )
    
    # COMMENTS    
    #fluidRow(                                    
    #    column(10,
    #           p("This is a proof of concept")
    #    )
    
)


# server.R

# Load algorithm implementations and similarity calculations
source("cf_algorithm.R")
source("similarity_measures.R")

past_orders_matrix <- readRDS("past_orders_matrix.rds")

server <- function(input,output) {
    
    output$item_recom <- renderTable({
        # react to submit button
        input$submit
        # gather input in string
        customer_order <- 
            isolate(
                   unique(c(input$input_item1, input$input_item2, input$input_item3, 
                         input$input_item4, input$input_item5))
            )
      
        # put in a matrix format
        new_order <- item_list %>%
            # Add a 'value' column with 1's for customer order items
            mutate(value = as.numeric(Description %in% customer_order)) %>%
            # Spread into sparse matrix format
            spread(key = Description, value = value) %>%
            # Change to a matrix
            as.matrix() %>% 
            # Convert to class "dgCMatrix"
            as("dgCMatrix")
        
        # Add new order to retail matrix - binding 2 matrices
        all_orders_dgc <- t(rbind(new_order,past_orders_matrix))
        
        # Set items to predict range
        items_to_predict <- which(all_orders_dgc[ ,1] == 0)
        #items_to_predict <- 1:nrow(all_orders_dgc)
        # Set user to 1
        users <- c(1)
        # Set prediction indices
        prediction_indices <- as.matrix(expand.grid(items_to_predict, users = users))
        
        # Run IBCF model
        recomm <- predict_cf(all_orders_dgc, prediction_indices, 
                             "ibcf", FALSE, cal_cos, 3, FALSE, 4000, 2000)
        
        # Put recommended products into a dataframe
        recomm[,users] %>% 
            as.data.frame() %>% 
            rownames_to_column("Recommended Items Appear Here After Purchase") %>% 
            filter(.>0) %>% 
            select("Recommended Items Appear Here After Purchase")
        
    })
}

shinyApp(ui = ui, server = server)


