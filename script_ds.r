# The goal is create book recommendation app with some statistics,
# metrics, information about users, their like history, recommendation
# history,etc.

# LIBRARIES ----

library(tidyverse)
# package below also has package recosystem, which has matrix factorization
# utility
library(recommenderlab)

# for better ggplots

library(ShinyItemAnalysis)

#DATA LOADING ----

book_ratings <- read_delim('BX-Book-Ratings.csv', col_names =TRUE,
                           delim = ';',locale = locale(encoding = 'UTF-8'))
books <- read_delim('BX-Books.csv', col_names =TRUE,delim = ';',
                    locale = locale(encoding = 'UTF-8'))
users <- read_delim('BX-Users.csv', col_names =TRUE,delim = ';',
                    locale = locale(encoding = 'UTF-8'))

#DATA STRUCTURE ----

str(book_ratings)
str(books)
str(users)

head(book_ratings)
head(books)
head(users)

# it seems, that only valuable information we have, are user-id,location,
# book title, isbn, and maybe book autho, and also rating

# also there are age missing values, but it should be estimated with regression
# it should be valuable information on book rating..


# there are not so much NA's values, there is only 10 missing ratings -
# it should be error in measure, or stg like that

# but i find some NULL values in dataset - what does it mean ?
# check number of NULL values in dataset

apply(books,2,function(x) sum(x == 'NULL',na.rm = TRUE))
apply(book_ratings,2,function(x) sum(x == 'NULL',na.rm = TRUE))
apply(users,2,function(x) sum(x == 'NULL',na.rm = TRUE))

# we see that only in age's are NULL values, but it should not be a
# problem, i think in this case, i do not really need age for this
# analysis

# DATA PREPROCESSING ----

summary(books)


View(books[is.na(books$`Year-Of-Publication`),])

# here is the interesting thing, if we look no book title, for those data,
# there are moved year of publication and author into bad columns

nas<-books[is.na(books$`Year-Of-Publication`),]
nas_list <- str_split(nas$`Book-Title`,';')
#every list which has only one component i will delete

idx_del <- which(unlist(lapply(nas_list,function(x) length(x))) == 1)
nas_list <- nas_list[-idx_del]
authors <- lapply(nas_list,function(x) str_sub(x[2],start = 2))
titles <- lapply(nas_list,function(x) x[1])
titles <- lapply(titles,function(x) str_sub(gsub('\\\\+','',x),start = 1,end = -2))
nas$`Year-Of-Publication` <- nas$`Book-Author`
nas <- nas[-idx_del,]
nas$`Book-Author` <- unlist(authors)
nas$`Book-Title` <- unlist(titles)
nas <- nas[-163,]
books[is.na(books$`Year-Of-Publication`),][-idx_del,][-163,] <- nas
books$`Year-Of-Publication` <- as.integer(books$`Year-Of-Publication`)

# the goal is to merge datasets into one
# in the main dataset, i need userid's,their ratings,book titles, ISBNS's
# maybe the location

# as i saw some anomalies in dataset, like lot of duplicates in names of the
# books, just because different name of author, or other publisher, or same name
# but the book has different story like e.g Ranma (Ranma 1/2) - there is like
# 6 rows with same name, but if you saw the picture of the books, there is
# different stories - but we have those books under other name..bad bad dataset

# e.g books %>% filter(`Book-Title` == "Ranma 1/2 (Ranma 1/2)")
# e.g books[grep('Harry Potter and',books),]

View(books %>% filter(`Book-Title` == "Ranma 1/2 (Ranma 1/2)"))
View(books[grep('Ranma',books),])
View(books[grep('Harry Potter and',books),])

# so by the case above, it won't be easy to remove all duplicates, but let's
# assume, that all duplicate values are just same book, but different publisher
# or just some anomaly in author name, or something like that


# again, some other example - also duplicates in book title, does not mean, that
# book is from the same author e.g : books %>% count(`Book-Title`,sort = TRUE)
# View(books %>% filter(`Book-Title` == 'Selected Poems'))

View(books %>% filter(`Book-Title` == 'Selected Poems'))

# some strings are not UTF-8 encoding
Encoding(books$`Book-Title`) <- 'UTF-8'

#some authors has initials with spaces, some not e.g J. K. R..; J.K. R..
books$`Book-Author` <- gsub('\\W\\s','.',books$`Book-Author`)

# keep just book title
books$`Book-Title` <- gsub('\\(.*\\)','',books$`Book-Title`)

# take every author names in upper case
books$`Book-Author`<- toupper(iconv(books$`Book-Author`,'UTF-8',
                                   'UTF-8',sub = ''))

# remove white spaces from both sides of string
books$`Book-Title` <-str_trim(iconv(books$`Book-Title`,'UTF-8',
                                   'UTF-8',sub = ''),'both')

# remove duplicates
books<-books[!duplicated(books[,c('Book-Title','Book-Author')]),]

books$`Book-Title` <- gsub("[^a-zA-Z .:?!-]", "", books$`Book-Title`)
books$`Book-Author` <- gsub("[^a-zA-Z .:?!-]", "", books$`Book-Author`)
books$`Publisher` <- gsub("[^a-zA-Z .:?!-@]", "", books$`Publisher`)

# remove NA's

# i think, if the year of publication is missing, is not relevant in our case
# but we see, that 10 NA's book ratings are here, so let's delete them

# okay right now, we need to merge dataset into one, with most important
# columns
# merge_data <- book_ratings %>% inner_join(books)
# merge_data <- merge_data %>% dplyr::select(`User-ID`,`Book-Title`,`Book-Rating`)

# summary of all datasets


# here is the interesting thing, if we look no book title, for those data,
# there are moved year of publication and author into bad columns

# nas<-books[is.na(books$`Year-Of-Publication`),]
# nas_list <- str_split(nas$`Book-Title`,';')
# #every list which has only one component i will delete
#
# idx_del <- which(unlist(lapply(nas_list,function(x) length(x))) == 1)
# nas_list <- nas_list[-idx_del]
# authors <- lapply(nas_list,function(x) str_sub(x[2],start = 2))
# titles <- lapply(nas_list,function(x) x[1])
# titles <- lapply(titles,function(x) str_sub(gsub('\\\\+','',x),start = 1,end = -2))
# nas$`Year-Of-Publication` <- nas$`Book-Author`
# nas <- nas[-idx_del,]
# nas$`Book-Author` <- unlist(authors)
# nas$`Book-Title` <- unlist(titles)
# nas <- nas[-163,]
# books[is.na(books$`Year-Of-Publication`),][-idx_del,][-163,] <- nas
# books$`Year-Of-Publication` <- as.integer(books$`Year-Of-Publication`)

# distribution of books - except zero


books %>% filter(between(`Year-Of-Publication`,1900,2020)) %>% ggplot() +
          geom_histogram(aes(`Year-Of-Publication`),fill = 'blue',
                         binwidth = 3, col = 'black') +
          theme_app()

# looks like lot of books are published near year 2000, but in this
# dataset is nothing really interesting

summary(users)

# age is defined as string column, because of NULL values - maybe
# bad data gathering, or something like that, or just user not
# fill his age

# convert to integers

users$Age <- as.integer(users$Age)

summary(users)

# here are more anomalies - about 110763 users are NA's, or NULL,
# also there are users whose has above 100 years - that's not real
# check distribution of age of users

users[!is.na(users$Age),] %>% ggplot() + geom_boxplot(aes(y=Age)) +
      theme_app()

users[!is.na(users$Age),] %>% ggplot() + geom_histogram(aes(Age),
                                                        fill = 'red',
                                                        col = 'black') +
  theme_app()

# we see there is lot of outliers, but in our future analysis, age
# will be not important, just for statistics, and tables, etc. but
# not for our model..lets assume, that every user with age above 100
# years, is old user,and sample age between(80,100)

users <- as.tbl(transform(users,
                   Age = ifelse(Age >= 100,NA,Age)))

users[!is.na(users$Age),] %>% ggplot() + geom_histogram(aes(Age),
                                                        fill = 'red',
                                                        col = 'black') +
  theme_app()

# also there is some location - it is in format city, region, country
loc_list <- str_split(users$Location,', ')
city <- unlist(lapply(loc_list,function(x) x[1]))
region <- unlist(lapply(loc_list,function(x) x[2]))
country <- unlist(lapply(loc_list,function(x) x[3]))
users <- users %>% select(User.ID,Age) %>% mutate(city = city,
                                                  region = region,
                                                  country = country)
users$`User.ID` <- as.factor(users$`User.ID`)
colnames(users)[1] <- 'User-ID'

ggplot(users,aes(users$country)) + geom_bar()

#look from which country are our users - stupid statistic

frequency_states <- users %>% count(country,sort = TRUE)
frequency_states <- frequency_states[1:20,]$country
most_countries <- users %>% filter(country %in% frequency_states)
ggplot(most_countries,aes(most_countries$country)) + geom_bar() +
  theme_app() + theme(axis.text.x = element_text(size = 7, angle = 90))

# most users are from USA

# let's look on ratings

book_ratings <- book_ratings[!is.na(book_ratings$`Book-Rating`),]

# okay right now, we need to merge dataset into one, with most important
# columns
merge_data <- book_ratings %>% inner_join(books)
merge_data <- merge_data %>% dplyr::select(`User-ID`,`ISBN`,`Book-Rating`)
merge_data2 <- book_ratings %>% inner_join(books)
merge_data2 <- merge_data2 %>% dplyr::select(`User-ID`,`Book-Title`,`ISBN`,`Book-Rating`)
summary(merge_data)

# there is no anomalies
# right now, it should be ok, to merge books with ratings, and consider
# those books - i do not say, that my way of doing this is correct, but
# let's see

#there was 1149780 of ratings, but right now we have 863906 which is
# still quite nice

# let's do some EDA here

# distribution of ratings

ggplot(merge_data) + geom_histogram(aes(merge_data$`Book-Rating`),
                                    fill = 'green',
                                    col = 'black') +
                     theme_app()

# there is a huge number of implicit ratings, let's divide those rankings
merge_data %>% count(`Book-Rating`)

#explicit ratings
merge_data %>% filter(`Book-Rating` != 0) %>% ggplot() +
                geom_histogram(aes(`Book-Rating`),binwidth = 1,
                               fill = 'yellow',
                               col = 'black') +
                theme_app()

# lot of users gave ratings above 5.0
merge_data %>% filter(`Book-Rating` != 0) %>% count(`Book-Rating`)
merge_data %>% filter(`Book-Rating` != 0) %>% summary()
# median is 8 - most used ratings

# 20 most rated films

most_rated <- merge_data %>% count(`Book-Title`,sort = TRUE)
most_rated <- most_rated[1:20,]
most_rated <- merge_data %>% filter(`Book-Title` %in% most_rated$`Book-Title`)
ggplot(most_rated,aes(most_rated$`Book-Title`)) + geom_bar() + theme_app() +
  theme(axis.text.x = element_text(size = 7,angle = 90)) +
  xlab('Movies') + ylab('Counts') + ggtitle('20 most rated movies')

# Wild Animus look like most rated movie - also implicit and explicit - it is
# also top rated ? For this, we should exclude implict ratings, because we do not
# know if implict rating mean positive experience with that book


# it is wild animus also top rated ?

#explicit rating

top_rated <- ratings_explicit %>% group_by(`ISBN`) %>%
        summarise(sum_rating = sum(`Book-Rating`))
top_rated <- top_rated %>% arrange(desc(sum_rating))
top_rated <- top_rated[1:20,]

ggplot(top_rated,aes(top_rated$`Book-Title`, fill = top_rated$`Book-Title`)) +
  geom_bar(aes(x = top_rated$`Book-Title`,y = top_rated$sum_rating),stat= 'identity') + theme_app() +
  theme(axis.text.x = element_text(size = 7,angle = 90)) +
  xlab('Movies') + ylab('Counts') + ggtitle('20 top rated movies')


# it seems that most rated movie, is not the best movie by users
# table of top 20 rated movies

# table: movie,total_points,average_points,top_country,avg_age
summarise_data <- ratings_explicit %>% inner_join(users)
summarise_data %>% group_by(`Book-Title`) %>%
  summarise(sum_rating = sum(`Book-Rating`,na.rm = T),
            avg_rating = mean(`Book-Rating`,na.rm = T),
            avg_age = mean(`Age`,na.rm = T),
            n = n()
            ) %>% arrange(desc(sum_rating))

# we see that most rated wild animus film get's quite low rating
# also it makes sense to divide dataset at explcit and implicit ratings
# because, if we consider boht ratings in one statitics, results should be
# skewed

#


frequency_rating <- as.data.frame(table(book_ratings$`ISBN`))
freq_rating_users <- as.data.frame(table(book_ratings$`User-ID`))

# there is lot of users, who are rate just one book - aroung 58166,
# that's lot of rows to remove..we will see
# ok ,let's try



# the last step needed is to create rating matrix

# creeate two rating matrix, for implicit model and for explicit model
rows_explicit <- which(merge_data$`Book-Rating` != 0)
ratings_implict <- merge_data[-rows_explicit,]
ratings_implict$`User-ID` <- as.factor(ratings_implict$`User-ID`)
ratings_implict$`ISBN` <- as.factor(ratings_implict$`ISBN`)

ratings_explicit <- merge_data[rows_explicit,]

idx_low_rates<-ratings_explicit %>% count(`User-ID`) %>% filter(n <= 30) %>%
  select(`User-ID`)
idx_low_rates <- unique(idx_low_rates$`User-ID`)
ratings_explicit <- ratings_explicit %>% filter(!(`User-ID` %in% idx_low_rates))
#ratings_explicit <- ratings_explicit %>% filter(!(`User-ID` %in% c(11676,98391)))
ratings_explicit$`ISBN` <- as.factor(ratings_explicit$`ISBN`)
ratings_explicit$`User-ID` <- as.factor(ratings_explicit$`User-ID`)

top_rated <- ratings_explicit %>% group_by(`ISBN`) %>%
  summarise(sum_rating = sum(`Book-Rating`))
top_rated <- top_rated %>% arrange(desc(sum_rating))
top_rated <- top_rated[1:20,]

ggplot(top_rated,aes(top_rated$`Book-Title`, fill = top_rated$`Book-Title`)) +
  geom_bar(aes(x = top_rated$`Book-Title`,y = top_rated$sum_rating),stat= 'identity') + theme_app() +
  theme(axis.text.x = element_text(size = 7,angle = 90)) +
  xlab('Movies') + ylab('Counts') + ggtitle('20 top rated movies')


# it seems that most rated movie, is not the best movie by users
# table of top 20 rated movies

# table: movie,total_points,average_points,top_country,avg_age
summarise_data <- ratings_explicit %>% inner_join(users)
summarise_data %>% group_by(`Book-Title`) %>%
  summarise(sum_rating = sum(`Book-Rating`,na.rm = T),
            avg_rating = mean(`Book-Rating`,na.rm = T),
            avg_age = mean(`Age`,na.rm = T),
            n = n()
  ) %>% arrange(desc(sum_rating))

# we see that most rated wild animus film get's quite low rating
# also it makes sense to divide dataset at explcit and implicit ratings
# because, if we consider boht ratings in one statitics, results should be
# skewed



# create two matrix for explicit ratings and impicit ratings

# create index for mapping indices to books, users

# explicit rating model
idx_users_exp <- ratings_explicit %>% group_indices(`User-ID`)
idx_books_exp <- ratings_explicit %>% group_indices(`ISBN`)

write.table(idx_users_exp,'idx_users_exp.txt', row.names = FALSE,
            col.names = FALSE)
write.table(idx_books_exp,'idx_books_exp.txt', row.names = FALSE,
            col.names = FALSE)

write_csv(ratings_explicit,'ratings_explicit.csv')


# create sparse matrix for explicit ratings

sparse_exp <- sparseMatrix(i = idx_users_exp,
                           j = idx_books_exp,
                           x = ratings_explicit$`Book-Rating`,
                           dimnames = list(levels(ratings_explicit$`User-ID`),
                                           levels(ratings_explicit$`ISBN`)))


rating_exp <- as(sparse_exp, 'realRatingMatrix')


# also create the same for implict ranking

idx_users_imp <- ratings_implict %>% group_indices(`User-ID`)
idx_books_imp <- ratings_implict %>% group_indices(`ISBN`)

# create sparse matrix for explicit ratings

sparse_imp <- sparseMatrix(i = idx_users_imp,
                           j = idx_books_imp,
                           x = ratings_implict$`Book-Rating`,
                           dimnames = list(levels(ratings_implict$`User-ID`),
                                           levels(ratings_implict$`ISBN`)))

rating_imp <- as(sparse_imp, 'realRatingMatrix')
rating_imp <- binarize(rating_imp,minRating = 0)

# MODEL ----

#creating evaluation scheme

e <- evaluationScheme(rating_exp,
                      train = 0.8, method = 'split', given = 30, goodRating = 5)

model_UBCF <- Recommender(getData(e,'train'), method = 'UBCF',
                          parameter = list(nn = 20))
recom_UBCF <- predict(model_UBCF,getData(e,'known'),5)
recom_UBCF2 <- predict(model_UBCF,getData(e,'known'),type = 'ratings')

model_UBCF_smaller <- Recommender(getData(e,'train'), method = 'UBCF',
                                  parameter = list(nn = 10))
recom_UBCF_smaller <- predict(model_UBCF_smaller,getData(e,'known'),5)
recom_UBCF2_smaller <- predict(model_UBCF_smaller,getData(e,'known'),type = 'ratings')

model_LIBMF <- Recommender(getData(e,'train'), method = 'LIBMF',
                           parameter = list(dim = 15, nthread =4,
                                            costp_l2 = 1,costq_l2 = 0.9))
recom_LIBMF <- predict(model_LIBMF,getData(e,'known'),type = 'ratings')
recom_LIBMF2 <- predict(model_LIBMF,getData(e,'known'),5)
model_svd <- Recommender(getData(e,'train'), method = 'SVDF', parameter= list(k = 6, max_epochs = 50, verbose = TRUE))
recom_svd <- predict(model_svd,getData(e,'known'),5)
recom_svdf2 <- predict(model_svd,getData(e,'known'),type = 'ratings')

err_ubcf <- calcPredictionAccuracy(recom_UBCF2,getData(e,'unknown'),byUser = TRUE)
err_ubcf_s <- calcPredictionAccuracy(recom_UBCF2_smaller,getData(e,'unknown'),byUser = TRUE)
err_libmf <- calcPredictionAccuracy(recom_LIBMF,getData(e,'unknown'),byUser = TRUE)
err_svdf <- calcPredictionAccuracy(recom_svdf2,getData(e,'unknown'),byUser = TRUE)


d <- cbind(err_libmf,err_ubcf,err_svdf,err_ubcf_s)
