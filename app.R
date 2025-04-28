############################################################
# 7-Tab Shiny App with bs4Dash
# Each plot is isolated, no side-by-side
# Many additional analyses & text outputs
############################################################
library(shiny)
library(bs4Dash)
library(metafor)
library(readr)
library(ggplot2)
library(DT)

# optional: library(dmetar)

# Sample data
sample_data <- data.frame(
  study = c("Study 1", "Study 2", "Study 3", "Study 4"),
  mean = c(5.2, 4.8, 6.1, 5.5),
  lower = c(4.6, 4.2, 5.3, 4.9),
  upper = c(5.8, 5.4, 6.9, 6.1),
  group = c("A", "A", "B", "B"),
  covariate = c(1, 2, 3, 4)
)

ui <- bs4DashPage(
  header = bs4DashNavbar(title = "One-Group Meta (7-Tab Version)"),
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem("Data & Setup", tabName = "tab_setup", icon = icon("database")),
      bs4SidebarMenuItem("Main Meta", tabName = "tab_meta", icon = icon("chart-line")),
      bs4SidebarMenuItem("Forest & Funnel", tabName = "tab_forest", icon = icon("align-left")),
      bs4SidebarMenuItem("Subgroup & Regression", tabName = "tab_subgroup", icon = icon("layer-group")),
      bs4SidebarMenuItem("Diagnostics & Sensitivity", tabName = "tab_diag", icon = icon("tools")),
      bs4SidebarMenuItem("Additional Analyses", tabName = "tab_extra", icon = icon("plus-square")),
      bs4SidebarMenuItem("Advanced Plots", tabName = "tab_advanced", icon = icon("chart-area"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      
      ############################################################
      # 1) DATA & SETUP TAB
      ############################################################
      bs4TabItem(tabName = "tab_setup",
                 fluidRow(
                   box(
                     title = "Data Input & Settings", width = 12, status = "primary", solidHeader = TRUE,
                     fileInput("file", "Choose CSV File", accept = ".csv"),
                     helpText("CSV must have columns: study, mean, lower, upper, group, covariate"),
                     br(),
                     downloadButton("download_sample", "Download Sample CSV"),
                     br(), br(),
                     selectInput("conf_level", "Confidence Level:", choices = c(90, 95, 99), selected = 95),
                     selectInput("meta_method", "Estimation Method:",
                                 choices = c("REML", "DL", "HE", "SJ", "ML"), selected = "REML"),
                     checkboxInput("debug_mode", "Enable Debugging Logs?", FALSE),
                     hr(),
                     helpText("Make your selections above, then proceed to other tabs.")
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Data Table Preview", width = 12, status = "info", solidHeader = TRUE,
                     DTOutput("data_table")
                   )
                 )
      ),
      
      ############################################################
      # 2) MAIN META TAB
      ############################################################
      bs4TabItem(tabName = "tab_meta",
                 fluidRow(
                   box(
                     title = "Main Meta-Analysis Results", width = 12, status = "success", solidHeader = TRUE,
                     verbatimTextOutput("meta_summary"),
                     br(),
                     verbatimTextOutput("meta_ci"),
                     br(),
                     verbatimTextOutput("meta_tau"),
                     br(),
                     verbatimTextOutput("meta_interpretation"),
                     br(),
                     verbatimTextOutput("extended_interpretation"),
                     br(),
                     verbatimTextOutput("sig_test"),
                     br(),
                     verbatimTextOutput("prediction_interval"),
                     helpText("Displays the summary of random-effects meta-analysis, significance, and prediction interval.")
                   )
                 )
      ),
      
      ############################################################
      # 3) FOREST & FUNNEL TAB
      ############################################################
      bs4TabItem(tabName = "tab_forest",
                 fluidRow(
                   box(
                     title = "Forest Plot Options & Download", width = 12, status = "primary", solidHeader = TRUE,
                     textInput("forest_xlabel", "Forest X-Axis Label:", "Mean"),
                     textInput("forest_title", "Forest Title:", "Random-effects model"),
                     sliderInput("forest_xmin", "Forest X-Min:", min = -10, max = 10, value = 0, step = 0.1),
                     sliderInput("forest_xmax", "Forest X-Max:", min = 0, max = 20, value = 10, step = 0.1),
                     br(),
                     downloadButton("download_forest", "Download Forest Plot"),
                     hr(),
                     plotOutput("forest_plot", height = "500px")
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Funnel Plot Options & Download", width = 12, status = "warning", solidHeader = TRUE,
                     textInput("funnel_xlabel", "Funnel X-Axis Label:", "Mean"),
                     textInput("funnel_ylabel", "Funnel Y-Axis Label:", "Standard Error"),
                     sliderInput("funnel_xmin", "Funnel X-Min:", min = -10, max = 10, value = 3, step = 0.1),
                     sliderInput("funnel_xmax", "Funnel X-Max:", min = 0, max = 20, value = 7, step = 0.1),
                     br(),
                     downloadButton("download_funnel", "Download Funnel Plot"),
                     hr(),
                     plotOutput("funnel_plot", height = "500px")
                   )
                 )
      ),
      
      ############################################################
      # 4) SUBGROUP & REGRESSION TAB
      ############################################################
      bs4TabItem(tabName = "tab_subgroup",
                 fluidRow(
                   box(
                     title = "Subgroup Analysis (by group)", width = 12, status = "info", solidHeader = TRUE,
                     verbatimTextOutput("subgroup_analysis"),
                     br(),
                     downloadButton("download_subgroup_forest", "Download Subgroup Forest Plot"),
                     hr(),
                     plotOutput("subgroup_forest", height = "500px"),
                     helpText("Separate random-effects meta-analyses for each group if available.")
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Meta-Regression (on covariate)", width = 12, status = "secondary", solidHeader = TRUE,
                     verbatimTextOutput("meta_regression"),
                     helpText("Explores how a continuous covariate might explain heterogeneity.")
                   )
                 )
      ),
      
      ############################################################
      # 5) DIAGNOSTICS & SENSITIVITY
      ############################################################
      bs4TabItem(tabName = "tab_diag",
                 fluidRow(
                   box(
                     title = "Heterogeneity & Leave-One-Out", width = 12, status = "info", solidHeader = TRUE,
                     verbatimTextOutput("heterogeneity_stats"),
                     br(),
                     verbatimTextOutput("leave_one_out"),
                     helpText("Displays Q-statistic, I², H², plus leave-one-out analysis.")
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Baujat Plot", width = 12, status = "warning", solidHeader = TRUE,
                     downloadButton("download_baujat", "Download Baujat Plot"),
                     hr(),
                     plotOutput("baujat_plot", height = "500px"),
                     helpText("Examines which studies contribute most to heterogeneity and overall effect.")
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Sensitivity (Remove 1 Study)", width = 12, status = "danger", solidHeader = TRUE,
                     selectInput("remove_study", "Remove which study?", choices = c("None")),
                     verbatimTextOutput("remove_study_res"),
                     helpText("Choose a study to omit and see how the summary changes.")
                   )
                 )
      ),
      
      ############################################################
      # 6) ADDITIONAL ANALYSES
      ############################################################
      bs4TabItem(tabName = "tab_extra",
                 fluidRow(
                   box(
                     title = "Cumulative Meta-Analysis", width = 12, status = "primary", solidHeader = TRUE,
                     downloadButton("download_cumulative", "Download Cumulative Plot"),
                     hr(),
                     plotOutput("cum_forest", height = "500px"),
                     helpText("Cumulative forest in ascending order of effect size.")
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Fixed-Effect Model (Comparison)", width = 12, status = "info", solidHeader = TRUE,
                     verbatimTextOutput("fixed_effect_summary"),
                     downloadButton("download_fixed_forest", "Download Fixed-Effect Plot"),
                     hr(),
                     plotOutput("fixed_effect_forest", height = "400px"),
                     helpText("Quick check under a fixed-effect model.")
                   )
                 )
      ),
      
      ############################################################
      # 7) ADVANCED PLOTS
      ############################################################
      bs4TabItem(tabName = "tab_advanced",
                 fluidRow(
                   box(
                     title = "Effect Size Distribution", width = 12, status = "success", solidHeader = TRUE,
                     downloadButton("download_effect_dist", "Download Distribution Plot"),
                     hr(),
                     plotOutput("effect_dist", height = "400px"),
                     helpText("Histogram of study means with a reference line at the mean of means.")
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Forest Plot (Sorted by Mean)", width = 12, status = "secondary", solidHeader = TRUE,
                     downloadButton("download_sorted_forest", "Download Sorted Forest Plot"),
                     hr(),
                     plotOutput("forest_sorted", height = "400px"),
                     helpText("Displays a forest plot after sorting data from smallest to largest effect.")
                   )
                 ),
                 fluidRow(
                   box(
                     title = "Descriptive Stats & Density Plot", width = 12, status = "info", solidHeader = TRUE,
                     verbatimTextOutput("descriptive_stats"),
                     downloadButton("download_density", "Download Density Plot"),
                     hr(),
                     plotOutput("density_plot", height = "400px"),
                     helpText("Shows basic stats (min, max, median, mean, n) and a density plot of observed means.")
                   )
                 )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ############################################################
  # REACTIVE: DATA INPUT
  ############################################################
  dataInput <- reactive({
    if (!is.null(input$file)) {
      df <- read_csv(input$file$datapath)
    } else {
      df <- sample_data
    }
    z <- qnorm(1 - (1 - as.numeric(input$conf_level)/100)/2)
    df$sei <- (df$upper - df$lower) / (2 * z)
    if (input$debug_mode) {
      cat("\n[DEBUG] Data Input\n")
      cat("Confidence Level:", input$conf_level, "\n")
      cat("Method:", input$meta_method, "\n")
      cat("Data (first rows):\n")
      print(head(df))
    }
    df
  })
  
  # Update remove-study list
  observe({
    df <- dataInput()
    updateSelectInput(session, "remove_study", choices = c("None", df$study))
  })
  
  ############################################################
  # MAIN META-ANALYSIS
  ############################################################
  metaResult <- reactive({
    df <- dataInput()
    method <- input$meta_method
    if (input$debug_mode) {
      cat("\n[DEBUG] metaResult() invoked\n")
      cat("Using method:", method, "\n")
      cat("Effect sizes (yi):\n")
      print(df$mean)
      cat("Standard errors (sei):\n")
      print(df$sei)
    }
    rma(yi = df$mean, sei = df$sei, method = method)
  })
  
  predResult <- reactive({
    res <- metaResult()
    tryCatch({
      predict(res, level = as.numeric(input$conf_level))
    }, error=function(e) NULL)
  })
  
  ############################################################
  # META RESULTS OUTPUTS
  ############################################################
  output$meta_summary <- renderPrint({
    summary(metaResult())
  })
  output$meta_ci <- renderPrint({
    r <- metaResult()
    paste(input$conf_level, "% CI for pooled mean:", round(r$ci.lb, 3), "to", round(r$ci.ub, 3))
  })
  output$meta_tau <- renderPrint({
    r <- metaResult()
    paste("Tau-squared (between-study variance):", round(r$tau2, 3))
  })
  output$meta_interpretation <- renderPrint({
    r <- metaResult()
    paste(
      "Based on the meta-analysis, the pooled mean is",
      round(r$b, 2), "with a",
      input$conf_level, "% confidence interval from",
      round(r$ci.lb, 2), "to",
      round(r$ci.ub, 2), ".",
      "This suggests the true average value in the population is likely within this range."
    )
  })
  output$extended_interpretation <- renderPrint({
    r <- metaResult()
    i2 <- round(r$I2, 2)
    paste(
      "Heterogeneity (I-squared):", i2, "%.",
      "If I-squared is above ~50%, interpret the pooled estimate with caution."
    )
  })
  output$sig_test <- renderPrint({
    r <- metaResult()
    pval <- r$pval
    est <- round(r$b, 2)
    if (pval < 0.05) {
      paste("The pooled estimate of", est, "is statistically significant (p=", round(pval,4), ").")
    } else {
      paste("The pooled estimate of", est, "is not statistically significant (p=", round(pval,4), ").")
    }
  })
  output$prediction_interval <- renderPrint({
    pr <- predResult()
    if (is.null(pr)) {
      "No prediction interval (insufficient data or single-study)."
    } else {
      paste0(
        "Prediction Interval (",
        input$conf_level, "%): [",
        round(pr$pi.lb,2), ", ",
        round(pr$pi.ub,2), "]"
      )
    }
  })
  
  ############################################################
  # DATA TABLE
  ############################################################
  output$data_table <- renderDT({
    datatable(dataInput(), options = list(pageLength=5))
  })
  output$download_sample <- downloadHandler(
    filename = function(){ "sample_meta_data.csv" },
    content = function(file){
      write_csv(sample_data, file)
    }
  )
  
  ############################################################
  # FOREST & FUNNEL (Tab 3)
  ############################################################
  ## Forest
  plotForest <- function(){
    df <- dataInput()
    r <- metaResult()
    forest(
      r,
      slab = df$study,
      xlab = input$forest_xlabel,
      xlim = c(input$forest_xmin, input$forest_xmax),
      mlab = input$forest_title
    )
  }
  output$forest_plot <- renderPlot({ plotForest() })
  output$download_forest <- downloadHandler(
    filename = function(){"forest_plot.png"},
    content = function(file){
      png(file, width=900, height=600)
      plotForest()
      dev.off()
    }
  )
  
  ## Funnel
  plotFunnel <- function(){
    funnel(
      metaResult(),
      xlab = input$funnel_xlabel,
      ylab = input$funnel_ylabel,
      xlim = c(input$funnel_xmin, input$funnel_xmax)
    )
  }
  output$funnel_plot <- renderPlot({ plotFunnel() })
  output$download_funnel <- downloadHandler(
    filename = function(){"funnel_plot.png"},
    content = function(file){
      png(file, width=900, height=600)
      plotFunnel()
      dev.off()
    }
  )
  
  ############################################################
  # SUBGROUP & REGRESSION (Tab 4)
  ############################################################
  # Subgroup
  output$subgroup_analysis <- renderPrint({
    df <- dataInput()
    if (!"group" %in% names(df)) return("Group variable not found.")
    method <- input$meta_method
    tryCatch({
      summary(
        rma(yi = df$mean, sei = df$sei, mods = ~ factor(group),
            data = df, method = method)
      )
    }, error = function(e){
      paste("Subgroup analysis failed:", e)
    })
  })
  plotSubgroupForest <- function(){
    df <- dataInput()
    method <- input$meta_method
    groups <- unique(df$group)
    par(mfrow=c(length(groups),1))
    for(g in groups){
      sub_df <- df[df$group == g,]
      if(nrow(sub_df)>1){
        r_sub <- rma(yi=sub_df$mean, sei=sub_df$sei, method=method)
        forest(r_sub,
               slab = sub_df$study,
               xlab=paste("Mean (Group:", g,")"),
               mlab=paste("RE model - group", g)
        )
      } else {
        plot.new()
        text(0.5,0.5,paste("Not enough studies for group",g))
      }
    }
  }
  output$subgroup_forest <- renderPlot({
    df <- dataInput()
    if(!"group" %in% names(df)){
      plot.new()
      text(0.5,0.5,"Group variable not found.")
      return()
    }
    plotSubgroupForest()
  })
  output$download_subgroup_forest <- downloadHandler(
    filename = function(){"subgroup_forest.png"},
    content = function(file){
      png(file, width=900, height=600)
      df <- dataInput()
      if(!"group" %in% names(df)){
        plot.new()
        text(0.5,0.5,"Group variable not found.")
      } else {
        plotSubgroupForest()
      }
      dev.off()
    }
  )
  
  # Regression
  metaRegResult <- reactive({
    df <- dataInput()
    if(!"covariate" %in% names(df)) return(NULL)
    method <- input$meta_method
    tryCatch({
      rma(yi=df$mean, sei=df$sei, mods=~ covariate,
          data=df, method=method)
    }, error=function(e) NULL)
  })
  output$meta_regression <- renderPrint({
    r <- metaRegResult()
    if(is.null(r)) return("Covariate not found or meta-regression failed.")
    summary(r)
  })
  
  ############################################################
  # DIAGNOSTICS & SENSITIVITY (Tab 5)
  ############################################################
  output$heterogeneity_stats <- renderPrint({
    r <- metaResult()
    paste0(
      "Q-statistic: ", round(r$QE,2),
      ", p=", round(r$QEp,4), "\n",
      "I-squared: ", round(r$I2,2), "%\n",
      "H2 (approx): ", round(r$H2,2)
    )
  })
  output$leave_one_out <- renderPrint({
    r <- metaResult()
    tryCatch({
      print(leave1out(r))
    }, error=function(e){
      paste("Could not perform leave-one-out analysis:", e)
    })
  })
  
  # Baujat
  plotBaujat <- function(){
    baujat(metaResult(), main="Baujat Plot")
  }
  output$baujat_plot <- renderPlot({
    tryCatch({
      plotBaujat()
    },error=function(e){
      plot.new()
      text(0.5,0.5,paste("Baujat plot failed:", e))
    })
  })
  output$download_baujat <- downloadHandler(
    filename=function(){"baujat_plot.png"},
    content=function(file){
      png(file, width=900, height=600)
      tryCatch({
        plotBaujat()
      },error=function(e){
        plot.new()
        text(0.5,0.5,paste("Baujat plot failed:", e))
      })
      dev.off()
    }
  )
  
  # Sensitivity remove 1
  output$remove_study_res <- renderPrint({
    rm_study <- input$remove_study
    if(rm_study=="None"){
      return("No study removed.")
    }
    df <- dataInput()
    method <- input$meta_method
    df_sub <- df[df$study!=rm_study,]
    summary(rma(yi=df_sub$mean, sei=df_sub$sei, method=method))
  })
  
  ############################################################
  # ADDITIONAL ANALYSES (Tab 6)
  ############################################################
  # Cumulative
  plotCumulative <- function(){
    df <- dataInput()
    df_ord <- df[order(df$mean),]
    method <- input$meta_method
    res_cum <- rma(yi=df_ord$mean, sei=df_ord$sei, method=method)
    cumres <- cumul(res_cum, order=1:nrow(df_ord))
    forest(cumres, xlab="Mean", mlab="Cumulative Meta")
  }
  output$cum_forest <- renderPlot({
    plotCumulative()
  })
  output$download_cumulative <- downloadHandler(
    filename=function(){"cumulative_plot.png"},
    content=function(file){
      png(file, width=900, height=600)
      plotCumulative()
      dev.off()
    }
  )
  
  # Fixed-effect
  fixedResult <- reactive({
    df <- dataInput()
    rma(yi=df$mean, sei=df$sei, method="FE")
  })
  output$fixed_effect_summary <- renderPrint({
    summary(fixedResult())
  })
  plotFixedForest <- function(){
    resF <- fixedResult()
    df <- dataInput()
    forest(resF, slab=df$study, xlab="Mean", mlab="Fixed-effect model")
  }
  output$fixed_effect_forest <- renderPlot({
    plotFixedForest()
  })
  output$download_fixed_forest <- downloadHandler(
    filename=function(){"fixed_effect_forest.png"},
    content=function(file){
      png(file, width=900, height=600)
      plotFixedForest()
      dev.off()
    }
  )
  
  ############################################################
  # ADVANCED PLOTS (Tab 7)
  ############################################################
  # Effect Dist
  plotEffectDist <- function(){
    df <- dataInput()
    hist(df$mean, breaks=5, col="skyblue",
         main="Distribution of Observed Means",
         xlab="Observed Mean", border="white")
    abline(v=mean(df$mean), col="red", lwd=2)
    legend("topright", legend=c("Mean of means"), lty=1, col="red")
  }
  output$effect_dist <- renderPlot({
    plotEffectDist()
  })
  output$download_effect_dist <- downloadHandler(
    filename=function(){"effect_distribution.png"},
    content=function(file){
      png(file, width=900, height=600)
      plotEffectDist()
      dev.off()
    }
  )
  
  # Forest sorted
  plotSortedForest <- function(){
    df <- dataInput()
    df_ord <- df[order(df$mean),]
    method <- input$meta_method
    res_sort <- rma(yi=df_ord$mean, sei=df_ord$sei, method=method)
    forest(res_sort, slab=df_ord$study, xlab="Mean", mlab="Sorted by Mean")
  }
  output$forest_sorted <- renderPlot({
    plotSortedForest()
  })
  output$download_sorted_forest <- downloadHandler(
    filename=function(){"sorted_forest.png"},
    content=function(file){
      png(file, width=900, height=600)
      plotSortedForest()
      dev.off()
    }
  )
  
  # Descriptive Stats & Density
  output$descriptive_stats <- renderPrint({
    df <- dataInput()
    c(
      min   = min(df$mean),
      max   = max(df$mean),
      median= median(df$mean),
      mean  = mean(df$mean),
      n     = nrow(df)
    )
  })
  plotDensity <- function(){
    df <- dataInput()
    plot(density(df$mean), main="Density of Observed Means",
         xlab="Mean", col="blue", lwd=2)
    rug(df$mean)
  }
  output$density_plot <- renderPlot({
    plotDensity()
  })
  output$download_density <- downloadHandler(
    filename=function(){"density_plot.png"},
    content=function(file){
      png(file, width=900, height=600)
      plotDensity()
      dev.off()
    }
  )
}

shinyApp(ui, server)
