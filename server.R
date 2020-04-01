#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

    output$Data <- renderDataTable({

        if(input$dataset == 'Users') {

            data.table(users)

        } else if(input$dataset == 'Books') {

            #head(books[,c('ISBN','Book-Title','Book-Author','Publisher',
            #'Year-Of-Publication')],20)

            data.table(books[,c('ISBN','Book-Title','Book-Author','Publisher',
                                'Year-Of-Publication')])

        } else {

            #head(merge_data,20)
            data.table(merge_data2)
        }

    }, filter = 'top')

    output$Summary <- renderPrint({

        if(input$dataset == 'Users') {

            summary(users)

        } else if(input$dataset == 'Books') {

            summary(books)
        } else {

            summary(book_ratings)
        }

    })

    output$eda_plot <- renderPlot({

        if(input$dataset == 'Users') {

            users <- users[,colnames(users)[-1]]
            col_types <- sapply(users,class)
            columns<-col_types[which(col_types %in% c('numeric','integer'))]
            updateSelectInput(session,'plot_variable', choices = columns)
            users[!is.na(users$Age),] %>% ggplot() +
                geom_histogram(aes(Age),
                               fill = 'red',
                               col = 'black') +
                theme_app()

        } else if(input$dataset == 'Books') {

            col_types <- sapply(books,class)
            columns<-col_types[which(col_types %in% c('numeric','integer'))]
            updateSelectInput(session,'plot_variable', choices = columns)
            books %>% filter(between(`Year-Of-Publication`,1900,2020)) %>%
                ggplot() +
                geom_histogram(aes(`Year-Of-Publication`),fill = 'blue',
                               binwidth = 3, col = 'black')
        } else {

            col_types <- sapply(book_ratings,class)
            columns<-col_types[which(col_types %in% c('numeric','integer'))]
            updateSelectInput(session,'plot_variable', choices = columns)
            book_ratings %>% ggplot() +
              geom_histogram(aes(`Book-Rating`), fill = 'blue',col ='black') +
              theme_app()
        }
    })

    user_stat <- reactive({

        dat <- merge_data2[-which(merge_data2$`Book-Rating` == 0),] %>%
          filter(`User-ID` == input$test_id)
        max_u <- max(dat$`Book-Rating`)
        max_b <- dat$`ISBN`[which.max(dat$`Book-Rating`)]
        max_b <- books %>% filter(`ISBN` == max_b) %>% select(`Book-Title`)
        min_u <- min(dat$`Book-Rating`)
        min_b <- dat$`ISBN`[which.min(dat$`Book-Rating`)]
        min_b <- books %>% filter(`ISBN` == min_b) %>% select(`Book-Title`)
        avg_rate <- mean(dat$`Book-Rating`)
        n <- dat %>% summarise(n = n())
        df <- data.frame(rows = c('Min Rated Book',
                                  'Max Rated Book',
                                  'Average Rating',
                                  'Number of Ratings'),
                         book = c(min_b$`Book-Title`,max_b$`Book-Title`,'-----','-----'),
                         value = c(as.integer(min_u),
                                   as.integer(max_u),
                                   avg_rate,
                                   as.integer(n$n)))
        colnames(df)[1] <- ''
        df

    })

    top_rate <- reactive({

        dat <- merge_data2[-which(merge_data2$`Book-Rating` == 0),] %>%
                            filter(`User-ID` == 12538)
        dat <- dat %>% arrange(desc(`Book-Rating`)) %>%
                    select(`Book-Title`,`Book-Rating`)

        colnames(dat) <- c('Book','User Rating')

        number_books_total <- length(dat$`Book`)

        if(number_books_total <= 5) {


            dat

        } else {

            dat[1:5,]
        }


    })

    recomend_rate <- reactive({


        #idx <- which(levels(ratings_explicit$`User-ID`) == input$test_id)
        #recom <- predict(test_model2, rating_exp[idx,],5)

        #choose recom predicts



        if(input$user_model == 'User Based Model - Explicit') {

          if(input$ubcf_model == 'KNN') {

          recomendations <- as(recom_UBCF,'list')


          } else if(input$ubcf_model == 'LIBMF') {

            recomendations <- as(recom_LIBMF2,'list')


          } else {

            recomendations <- as(recom_svd,'list')
          }
        }


        titles <- books %>% filter(`ISBN` %in% unlist(recomendations[input$test_id])) %>% select(`Book-Title`)
        df <- data.frame(books = titles$`Book-Title`)
        colnames(df) <- c('Top 5 recommended books')
        df


    })

    compare_model <- reactive({


        bks <- top_rate()$Book
        dat <- books %>% filter(`Book-Title` %in% bks) %>%
                select(`ISBN`)
        bks <- dat$ISBN
        url_base <- 'https://www.whatshouldireadnext.com/isbn/'
        list_books <- list()
        err_vec <- rep(FALSE,5)

        for(i in 1:5) {

            url <- paste0(url_base,bks[i])
            err <- tryCatch(read_html(url),
                            error = function(e) e)

            if(!(any(class(err) %in% c("simpleError","error")))) {

                err_vec[i] <- TRUE
                nodes <- html_nodes(err,'h3.books__book-row__details__title')

                list_books[[i]] <- gsub('\\(.*\\)','',
                                        str_trim(gsub('\\n','',html_text(nodes)),
                                                 side = 'both'))
            } else {

                next
            }

        }

        if(all(err_vec == FALSE)) {

            txt <- paste0('Sorry for any book, from top 5 rated book list, is not recommendation on page: ',url_base)
            return(list(txt = txt, vec = 'Nothing'))

        } else {

            txt <- paste0('Table below is showing recommendation for books based on top 5 recommender books from user ',input$test_id,
                    ' from https://www.whatshouldireadnext.com')
            return(list(txt = txt,vec = unlist(list_books)))

        }

    })

    get_error <- reactive({

      if(input$model_select == 'Explicit Model') {

         d <- cbind(err_libmf,err_ubcf,err_svdf,err_ubcf_s)
         d <- as.data.frame(d)
         colnames(d) <- c('RMSE_LIBMF','MSE_LIBMF','MAE_LIBMF','RMSE_UBCF','MSE_UBCF',
                          'MAE_UBCF','RMSE_SVDF','MSE_SVDF','MAE_SVDF','RMSE_UBCF_S','MSE_UBCF_S','MAE_UBCF_S')
         d <- d[,c(paste0(input$error_distribution,'_LIBMF'),paste0(input$error_distribution,'_UBCF'),
                                                                    paste0(input$error_distribution,'_SVDF'),
                                                                    paste0(input$error_distribution,'_UBCF_S'))]
         melt_d <- melt(d)
         melt_d <- melt_d[grep(input$error_distribution,melt_d$variable),]

         return(list(d = d,melt_d = melt_d))

      } else{

        d <- cbind(err_libmf,err_ubcf,err_svdf,err_ubcf_s)

      }


    })



    output$user_statistics <- renderTable({

        user_stat()

    })

    output$top_rated <- renderTable({

        top_rate()

    })

    output$recomend_rated <- renderTable({

        recomend_rate()

    })

     output$page_text <- renderText({

         result <- compare_model()
         txt <- result$txt
         txt

     })

    output$recomend_table <- renderDataTable({

        data.table(compare_model()$vec)

    })

    output$distribution_error <- renderPlot({

      d <- get_error()[['melt_d']]
      ggplot(d,aes(x = variable, y = value)) + geom_boxplot(aes(fill = d$variable)) + ylab(input$error_distribution) + xlab('Model')


    })

    output$model_stat <- renderTable({

      d <- get_error()[['d']]
      res <- t(apply(d,2,function(x) mean(x,na.rm = TRUE)))
      colnames(res) <- c(paste0('AVG_',input$error_distribution,'_LIBMF'),
                         paste0('AVG_',input$error_distribution,'_UCBF'),
                         paste0('AVG_',input$error_distribution,'_SVDF'),
                         paste0('AVG_',input$error_distribution,'_UCBF_S'))

      res

    })

    output$user_error_stat <- renderDataTable({

      get_error()[['d']]

    })


    rating_stat <- reactive({

      if(input$ratings_stat == 'Explicit Ratings') {

        merge <- merge_data2[which(merge_data2$`Book-Rating` != 0),] %>%
          group_by(`Book-Title`) %>%
          summarise(n = n(),
                    avg = mean(`Book-Rating`))
        dat <-  merge %>%
                arrange(desc(n))
        colnames(dat) <- c('Top 10 most rated books','Number of Explicit ratings','Average Rating')
        dat <- dat[1:10,]
        dat2 <- merge %>% filter(n >= 50) %>%
          arrange(desc(avg))
        colnames(dat2) <- c('Top 10 rated books','Number of Explicit ratings','Average Rating')
        dat2 <- dat2[1:10,]

        return(list(dat = dat,dat2 = dat2))

      } else {

        merge <- merge_data2[-which(merge_data2$`Book-Rating` != 0),] %>%
          group_by(`Book-Title`) %>%
          summarise(n = n())

        dat <- merge %>%
              arrange(desc(n))
        colnames(dat) <- c('Top 10 most rated books','Number of Implicit ratings')
        dat <- dat[1:10,]


        return(list(dat = dat,dat2 = 'Nothing'))

      }

    })

    output$top_rated_table <- renderTable({


      rat <- rating_stat()

      if(class(rat$dat2) != 'character') {

          rat$dat2

      }

    })

    output$most_rated_table <- renderTable({

      rating_stat()$dat

    })


    output$model_info <- renderTable({


      models <- c('LIBMF','Funk SVD','K-NN')
      params <- c('dim = 15; nthread =4; costp_l2 = 1; costq_l2 = 0.9',
                  'k = 6;max_epochs = 50;min_epochs=50;gamma = 0.015; lambda =0.001;min_improvement=0.000001;nomarlize=center',
                  'nn = [10,20];sample = FALSE; normalize = center')
      df <- data.frame(model = models, parameters = params)
      colnames(df) <- c('Model','Parameters')
      df

    })




})
