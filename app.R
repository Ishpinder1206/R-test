# Required libraries
library(shiny)
library(ggplot2)


max_cr_score <- 900  #maximum credit score is 900
max_loan_period <- 360 #maximum loan period is 360 months

# Creating a function for interest
interest_func <- function(cr_score) {
  if(cr_score < 693) {
    return(NULL)  # Cut-off credit score is 693, below 693, not eligible for loan
  } else if (cr_score <= 742) {
    return(10)  # Rate of interest for a score less than or equal to 742 will be 10
  } else if(cr_score <= 789) {
    return(9.5)  # Rate of interest for a score less than or equal to 789 will be 9.5
  } else {
    return(9)  # ROI for a credit score >789 will be 9
  }
}

# Calculation of EMI (equated monthly installments)
emi_func <- function(loan_amount, monthly_income, ROI, loan_period) {
  monthly_ROI <- ROI / 12 / 100 # monthly rate of interest
  emi <- loan_amount * monthly_ROI * ((1 + monthly_ROI) ^ loan_period) / ((1 + monthly_ROI) ^ loan_period - 1) # Formula for calculation of EMI
  return(emi)
}

# Function to assess credit risk on credit score and EMI/NMI ratio
cr_risk <- function(cr_score, loan_amount, monthly_income, loan_period) {
  
  ROI <- interest_func(cr_score)
  if(is.null(ROI)) {
    return("No loan can be sanctioned due to poor credit score.")
  }
  emi <- emi_func(loan_amount, monthly_income, ROI, loan_period) #using emi function defined above
  nmi_ratio <- (emi / monthly_income) * 100  # emi nmi ratio
  
  # Credit risk assessment according to emi/nmi and credit score
  if(nmi_ratio > 50) {
    return("Loan amount cannot be given due to high emi/nmi. Check for any existing borrowings of the applicant")
  } else if(cr_score <= 742) {
    return("High chances of default. Necessary due diligence is required before sanctioning the loan. Check for any written-off or settled loans")
  } else if(cr_score <= 789) {
    return("Medium chances of default. Necessary due diligence is required before sanctioning the loan. Check for any written-off or settled loans")
  } else { 
    return("Low chances of default. Please do the necessary due diligence and check for any written-off or settled loans")
  }
}

#UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap'); 
      body {
        background-color: Lightblue;
        color: Darkblue;
      }
      h2 {
        font-family: 'Times New Roman', Times;
      }
      .shiny-input-container {
        color: #333333;
      }
      .shiny-text-output {
        font-size: 18px;
        margin-bottom: 20px;
      }
    "))
  ), #layout of the page having various fields and defining their minimum and maximum values
  titlePanel("Credit Risk Assessment"), 
  sidebarLayout(
    sidebarPanel(
      numericInput("cr_score", "Credit Score:", value = 700, min = 300, max = 900), # credit score
      numericInput("monthly_income", "Monthly Income after deductions ($):", value = 2000, min = 0),
      numericInput("loan_amount", "Loan Amount ($):", value = 20000, min = 0),
      numericInput("loan_period", "Loan Period (Months):", value = 12, min = 1, max = 360),
      actionButton("assess", "Assess Risk")
    ),
    mainPanel(
      textOutput("error_message"), # for error message of credit score and loan period, if not within the described range
      uiOutput("main_ui") # scatter plot and credit assessment messages(output) 
    )
  )
)

#Server
server <- function(input, output) {
  observeEvent(input$assess, {
    error_message <- NULL # defining null value, that is when no error
    
    #loop defining various errors, for error output
    if (input$cr_score < 300 || input$cr_score > max_cr_score) {
      error_message <- "Credit score must be between 300 and 900."
    } else if (input$loan_amount < 0) {
      error_message <- "Loan amount cannot be negative."
    } else if (input$monthly_income < 0) {
      error_message <- "Monthly income of the applicant cannot be negative."
    } else if (input$loan_period < 1 || input$loan_period > max_loan_period) {
      error_message <- "Loan period must be between 1 and 360."
    }
    
    
    output$error_message <- renderText({
      error_message
    })
    
    #defining for error free entries ,UI outputs 
    output$main_ui <- renderUI({
      if (is.null(error_message)) {
        tagList(
          textOutput("risk_assessment"),
          plotOutput("credit_score_scat")
        )
      }
    })
    
    if (is.null(error_message)) {
      risk <- cr_risk(input$cr_score, input$loan_amount, input$monthly_income, input$loan_period)
      output$risk_assessment <- renderText({
        risk
      })
    }
    
  
    output$credit_score_scat <- renderPlot({
      if (!is.null(input$cr_score)) {
        credit_data <- data.frame(
          Category = factor(c("Total Score", "Cut-off Score", "Score of the Applicant")),
          Score = c(900, 693, input$cr_score),
          Labels = c(900, 693, paste("Applicant:", input$cr_score))
        )
        
        #scatter plot for credit score
        ggplot(credit_data, aes(x = Category, y = Score)) +
          geom_line(group = 1, color = "darkblue") +
          geom_point(size = 5, color = "darkblue") +
          geom_text(aes(label = Labels), nudge_y = 15, color = "darkblue", size = 6) +
          labs(title = "Credit Score Comparison", x = "", y = "Credit Score") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.text.y = element_text(color = "darkblue", size = 14),
            axis.title.y = element_text(size = 14)
          )
      }
    }, height = 250)
  })
}


shinyApp(ui = ui, server = server)
