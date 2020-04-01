#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinysky)
library(ShinyItemAnalysis)
library(data.table)
library(DT)

#library(tidyverse)
# package below also has package recosystem, which has matrix factorization
# utility
library(recommenderlab)

# for better ggplots


# Define UI for application that draws a histogram
shinyUI(fluidPage(

  tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
  tags$style(type="text/css","#search { top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px
                       !important; color: blue;font-size: 20px;font-style: italic;}"),

    # Application title
    titlePanel("Recommendation System"),

    tabsetPanel(
                tabPanel('EDA',
                  fluidRow(
                      column(3,
                             selectInput(inputId='dataset',
                                label = 'Choose data',
                                choices = c(Books = 'Books',
                                            Users = 'Users',
                                            Ratings = 'Ratings'))),

                # Show a plot of the generated distribution
                column(9,
                       mainPanel(
                    tabsetPanel(
                        tabPanel('Data',
                                 DT::dataTableOutput('Data')),
                        tabPanel('Summary',
                                 verbatimTextOutput('Summary'),
                                 conditionalPanel(condition = 'input.dataset == "Ratings"',
                                                  selectInput(inputId = 'ratings_stat',
                                                              label = 'Choose ratings',
                                                              choices = c('Explicit Ratings',
                                                                          'Implicit Ratings')),
                                 fluidRow(
                                   column(6,
                                      tableOutput('most_rated_table')),
                                   column(6,
                                      tableOutput('top_rated_table'))))),
                        tabPanel('Graphs',
                                 selectInput(inputId = 'plot_variable',
                                             label = 'Which variable to Plot?',
                                             choices = NULL),
                                 plotOutput('eda_plot'))
                    )
                ))
            )),
               tabPanel('Recommender',
                  sidebarLayout(
                    sidebarPanel(
                      textInput.typeahead('test_id','write user id',
                                           local = data.frame(user_id = c(rownames(getData(e,'known')))),
                                           valueKey = 'user_id',
                                           tokens = c(1:length(rownames(getData(e,'known')))),
                                           template = HTML("<p class='repo-name'>{{user_id}}</p>")),
                      br(),br(),
                      selectInput(inputId = 'user_model',
                                  label = 'Select Model',
                                  choices = c('User Based Model - Explicit',
                                              'Item Based Model - Explicit',
                                              'User Based Model - Implict',
                                              'Item Based Model - Implicit'),
                                  selected = 'User Based Model - Explicit'),
                      conditionalPanel(
                        condition = 'input.user_model == "User Based Model - Explicit"',
                        selectInput(inputId = 'ubcf_model',
                                    label = 'Select Algorithm',
                                    choices = c('SVD Funk',
                                                'LIBMF',
                                                'KNN'),
                                    selected = 'KNN')),
                      conditionalPanel(
                        condition = 'input.user_model == "Item Based Model - Explicit"',
                        selectInput(inputId = 'ibcf_model',
                                    label = 'Select Algorithm',
                                    choices = ''))
                      ),
                    mainPanel(
                      fluidRow(
                        column(4,
                               tableOutput('user_statistics')),
                        column(4,
                               tableOutput('top_rated')),
                        column(4,
                               tableOutput('recomend_rated'))),
                               br(),br(),
                               textOutput('page_text'),
                               br(),
                               DT::dataTableOutput('recomend_table')
                            )
                    )
                  ),
                  tabPanel('Model',
                           sidebarLayout(
                             sidebarPanel(
                                  selectInput(inputId = 'model_select',
                                              label = 'Model',
                                              choices = c('Explicit Model',
                                                          'Implicit Model')),
                                  radioButtons(inputId='new_model',
                                               label = 'Create new model?',
                                               choices = c('Yes','No')),
                                  conditionalPanel(condition = "input.new_model == 'Yes'",
                                                   selectInput('new_model_choose',
                                                               label = 'Choose model',
                                                               choices = c('UBCF - KNN','UBCF - SVDF', 'UBCF - LIBMF')),
                                  conditionalPanel(condition = "input.new_model_choose == 'UBCF - KNN'",
                                              numericInput(inputId = 'nn_new',
                                                           label = 'Number of nearest neighbours',
                                                            value = 10),
                                              radioButtons(inputId = 'sample_new',
                                                           label = 'Sample?',
                                                           choices = c('Yes','No')),
                                              radioButtons(inputId = 'normalize_new_nn',
                                                           label = 'Normalize?',
                                                           choices = c('Yes','No'))),
                                  conditionalPanel(condition = "input.new_model_choose == 'UBCF - SVDF'",
                                                   numericInput(inputId = 'k_new',
                                                                label = 'Number of latent variables',
                                                                value = 10
                                                   ),
                                                   numericInput(inputId = 'min_epochs_new',
                                                                label = 'Minimum number of epochs',
                                                                value = 50
                                                   ),
                                                   numericInput(inputId = 'max_epochs_new',
                                                                label = 'Maximum number of epochs',
                                                                value = 200
                                                   ),
                                                   numericInput(inputId = 'lambda_new',
                                                                label = 'lambda',
                                                                value = 0.05
                                                   ),
                                                   numericInput(inputId = 'gamma_new',
                                                                label = 'gamma',
                                                                value = 0.01
                                                   ),
                                                   numericInput(inputId = 'min_improvement_new',
                                                                label = 'Minimum improvement',
                                                                value = 0.0000001
                                                   ),
                                                   radioButtons(inputId = 'normalize_svdf',
                                                                label = 'Normalize?',
                                                                choices = c('Yes','No')
                                                   )),
                                  conditionalPanel(condition = "input.new_model_choose == 'UBCF - LIBMF'",
                                                   numericInput(inputId = 'dim_new',
                                                                label = 'Number of latent variables',
                                                                value = 15
                                                   ),
                                                   numericInput(inputId = 'costl2_p',
                                                                label = 'L2 regularization parameter for p',
                                                                value = 0.01
                                                   ),
                                                   numericInput(inputId = 'costl2_q',
                                                                label = 'L2 regularization parameter for q',
                                                                value = 0.01
                                                   ),
                                                   numericInput(inputId = 'nthreads',
                                                                label = 'Number of threads',
                                                                value = 4
                                                   )),
                                  numericInput(inputId = 'train_split',
                                               label = 'How many percent of data will be used for training?',
                                               value = 0.8),
                                  selectInput(inputId  = 'method_train',
                                              label = 'Method for splitting',
                                              choices = c('cross','split')),
                                  numericInput(inputId = 'given_train', label = 'Given items to recommender',
                                               value = 15),
                                  numericInput(inputId = 'goodrates_train',label = 'Good rating',value = 5),
                                  actionButton(inputId = 'predict',label = 'Predict'))),
                                  #conditionalPanel(condition = "input.model_select == 'Implicit Model'",
                                  #                  selectInput('impl_model',
                                  #                              label = 'Model',
                                  #                              choices = c('UBCF','IBCF')))),
                             mainPanel(
                                  br(),
                                  tableOutput('model_info'),
                                  br(),
                                  selectInput(inputId = 'error_distribution',
                                              label = 'Show error distribution',
                                              choices = c('RMSE','MSE','MAE')),
                                  plotOutput('distribution_error'),
                                  p('In table below are error statistic in total'),
                                  tableOutput('model_stat'),
                                  p('In table below are errors by each user'),
                                  dataTableOutput('user_error_stat')

                             )))),
                tabPanel('New Model')))
