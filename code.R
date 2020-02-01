# set working directory
setwd("Z:/yelp-version 6")

# read in data
business <- read.csv("yelp_business.csv") # 174567 businesses in all

# subset dataframe for required data
LV.businesses <- subset(business, city == "Las Vegas") # 26775 businesses
LV.restaurants <- subset(LV.businesses, grepl("*Restaurant*", LV.businesses$categories)) #5902 restaurants

# write subsetted dataframes for future use
write.csv(LV.businesses, file = "LV_businesses.csv", row.names = FALSE)
write.csv(LV.restaurants, file = "LV_restaurants.csv", row.names = FALSE)

# subset the business_attribs_hrs dataset for LV restaurants
LV.business.attribs.hrs <- read.csv("LV_business_attribs_hrs.csv")
LV.restaurant.attribs.hrs <- merge(LV.restaurants, LV.business.attribs.hrs, by = "business_id")
write.csv(LV.restaurant.attribs.hrs, file = "LV_restaurant_attribs_hrs.csv", row.names = FALSE)

# subset the business_checkins dataset for LV restaurants
LV.business.checkins <- read.csv("LV_business_checkins.csv")
LV.restaurant.checkins <- merge(LV.restaurants, LV.business.checkins, by = "business_id")
write.csv(LV.restaurant.checkins, file = "LV_restaurant_checkins.csv", row.names = FALSE)

# subset the business_tips dataset for LV restaurants
LV.business.tips <- read.csv("LV_business_tips.csv")
LV.restaurant.tips <- merge(LV.restaurants, LV.business.tips, by = "business_id")
write.csv(LV.restaurant.tips, file = "LV_restaurant_tips.csv", row.names = FALSE)

# subset the business_reviews_users dataset for LV restaurants
LV.business.reviews.users <- read.csv("LV_business_reviews_users.csv")
LV.restaurants.reviews.users <- merge(LV.restaurants, LV.business.reviews.users, by = "business_id")
write.csv(LV.restaurants.reviews.users, file = "LV_restaurants_reviews_users.csv", row.names = FALSE)

# load all data for doing analysis
attribs.hrs <- read.csv("LV_restaurant_attribs_hrs.csv")
checkins <- read.csv("LV_restaurant_checkins.csv")
tips <- read.csv("LV_restaurant_tips.csv")
restaurants <- read.csv("LV_restaurants.csv")
reviews.users <- read.csv("LV_restaurants_reviews_users.csv")

# decide on a criteria to identify super-users in Las Vegas
colnames(reviews.users)
NROW(unique(reviews.users$user_id)) # 337874 unique users/reviewers
no.of.reviews.per.users <- aggregate(business_id ~ user_id, data = reviews.users, FUN = NROW) # calculate the number of reviews per user/reviewer
NROW(subset(no.of.reviews.per.users, business_id > 100)) # 257 users
NROW(subset(no.of.reviews.per.users, business_id > 150)) # 115 users
NROW(subset(no.of.reviews.per.users, business_id > 200)) # 58 users
NROW(subset(no.of.reviews.per.users, business_id > 250)) # 34 users
NROW(subset(no.of.reviews.per.users, business_id > 300)) # 15 users --> seems like a decent choice to go forward with

# we find out the super-users based on identified criteria
library(dplyr)
subsetted.restaurant.reviews.and.dates <- select(reviews.users, business_id, user_id, date) # all restaurants here, with business & user & date
subsetted.restaurant.reviews.and.dates$date <- (as.character(subsetted.restaurant.reviews.and.dates$date) %>% as.Date)
## temp1 <- aggregate(business_id ~ user_id, data = temp, FUN = NROW) --- this is rendered useless now, we have an extract in the csv
list.of.super.users <- subset(temp1, business_id > 300)[1]
number.of.reviews.posted <- subset(temp1, business_id > 300)[2]
LV.super.users <- cbind(list.of.super.users, number.of.reviews.posted)
LV.super.users <- LV.super.users %>% rename(num_of_reviews = business_id)
write.csv(LV.super.users, file = "LV_super_users_vs_number_of_reviews.csv", row.names = FALSE)

# moving on, we subset for the restaurants that our super-users have reviewed
super.users.and.reviewed.restaurants <- inner_join(temp, list.of.super.users, by = "user_id")
write.csv(super.users.and.reviewed.restaurants, file = "LV_super_users_and_restaurants_reviewed.csv", row.names = FALSE)
temp3 <- select(super.users.and.reviewed.restaurants, business_id, user_id)
temp4 <- aggregate(user_id ~ business_id, data = temp3, FUN = NROW)
temp4[order(temp4$user_id, decreasing = TRUE),][1,1]
subset(super.users.and.reviewed.restaurants, business_id == "qqs7LP4TXAoOrSlaKRfz3A") 

# getting the dates this restaurant was reviewed by super-users
# over here, we find out some info about reviews posted for the restaurant mentioned above
tmp1 <- subset(subsetted.restaurant.reviews.and.dates, business_id == "qqs7LP4TXAoOrSlaKRfz3A") # 1093 reviews in all
summary(tmp1$date)
# for this restaurant --
 # min date is 2010-04-08
 # mean is 2014-07-22
 # median is 2014-07-07
 # max is 2017-12-07
# now we try to find the impact created by our super-user (bLbSNkLggFnqwNNzzq-Ijw)
# this was when 3nDUQBjKyVor5wV0reJChg reviewed : 2014-07-08
NROW(subset(subsetted.restaurant.reviews.and.dates, business_id == "qqs7LP4TXAoOrSlaKRfz3A" & date > "2014-07-08")) # 543 
NROW(subset(subsetted.restaurant.reviews.and.dates, business_id == "qqs7LP4TXAoOrSlaKRfz3A" & date < "2014-07-08")) # 549

# now we create a loop to find out the impact of a specific super-user on our set of restaurants
sample.of.restaurants <- unique(super.users.and.reviewed.restaurants[1])
#set.seed(10)
#sample.of.restaurants 
super.users.and.reviewed.restaurants[sample(nrow(super.users.and.reviewed.restaurants), 50), ][1] # randomly select 50 restaurants
super.user <- "bLbSNkLggFnqwNNzzq-Ijw" # choose the super-user who has posted most reviews, for better hit-rate
unique(select(subset(reviews.users, user_id == "bLbSNkLggFnqwNNzzq-Ijw"), name.y)) # Stefany
collected.stats <- data.frame(business_id = character(), reviews.before = numeric(), reviews.after = numeric())
collected.stats$business_id <- as.character(collected.stats$business_id)
for (i in 1:nrow(sample.of.restaurants)) {
 restaurant.business.id <- as.character(sample.of.restaurants[i,])
 tmp2 <- subset(subsetted.restaurant.reviews.and.dates, business_id == restaurant.business.id)
 date.super.user.visited <- as.integer(subset(tmp2, user_id == "bLbSNkLggFnqwNNzzqIjw")[3])
 if ((is.na(date.super.user.visited))) {}
 else {
 collected.stats[i,1] <- as.character(restaurant.business.id)
 temmp1 <- subset(subsetted.restaurant.reviews.and.dates, date < date.super.user.visited & business_id == restaurant.business.id)
 number.of.reviews.posted <- nrow(temmp1);
 if (number.of.reviews.posted != 0) {days.elapsed <- as.numeric((aggregate(date ~ business_id, data = temmp1, FUN = max)[2] - aggregate(date ~ business_id, data = temmp1, FUN = min)[2])[1])}
 collected.stats[i,2] <- number.of.reviews.posted/days.elapsed
 # collected.stats[i,2] <- nrow(subset(subsetted.restaurant.reviews.and.dates, date < date.super.user.visited & business_id == restaurant.business.id))
 temmp2 <- subset(subsetted.restaurant.reviews.and.dates, date > date.super.user.visited & business_id == restaurant.business.id)
 number.of.reviews.posted <- nrow(temmp2);
 if (number.of.reviews.posted != 0) {days.elapsed <- as.numeric((aggregate(date ~ business_id, data = temmp2, FUN = max)[2] - aggregate(date ~ business_id, data = temmp2, FUN = min)[2])[1])}
 collected.stats[i,3] <- number.of.reviews.posted/days.elapsed
 # collected.stats[i,3] <- nrow(subset(subsetted.restaurant.reviews.and.dates, date >
date.super.user.visited & business_id == restaurant.business.id))
 }
}
# nrow(subset(subsetted.restaurant.reviews.and.dates, date > "2014-09-22" & business_id == "Wyjk6RBeOPQr7td5Tqwksw")) # user_id == "bLbSNkLggFnqwNNzzq-Ijw" & business_id == "Tv19MQrLgdsvSG0myMYZBw"
# nrow(subset(subsetted.restaurant.reviews.and.dates, date < "2014-09-22" & business_id == "Wyjk6RBeOPQr7td5Tqwksw")) # user_id == "bLbSNkLggFnqwNNzzq-Ijw" & business_id == "Tv19MQrLgdsvSG0myMYZBw"

# for hypothesis testing and t-test
collected.stats.non.na <- subset(collected.stats, business_id != "NA")
write.csv(collected.stats.non.na, file = "stats_2.csv", row.names = FALSE)

# for generating word cloud
selected.columns.for.word.cloud <- select(reviews.users, business_id, user_id, date, text)
temp.merged <- merge(selected.columns.for.word.cloud, collected.stats.non.na, by="business_id")
merged.and.selected <- select(temp.merged, business_id, user_id, date, text)
merged.and.selected$date <- (as.character(merged.and.selected$date) %>% as.Date)
reviews.posted.before <- data.frame(text = character())
reviews.posted.before$text <- as.character(reviews.posted.before$text)
reviews.posted.after <- data.frame(text = character())
reviews.posted.after$text <- as.character(reviews.posted.after$text)
for (i in 1:nrow(merged.and.selected)) {
 res.bus.id <- as.character(merged.and.selected[i,1])
 tmpx <- subset(merged.and.selected, business_id == res.bus.id)
 date.sup.usr.vistd <- subset(tmpx, user_id == "bLbSNkLggFnqwNNzzq-Ijw")[3]
 tempxx1 <- subset(tmpx, date < date.sup.usr.vistd & business_id == res.bus.id)
 for (j in 1:nrow(tempxx1)) {
 reviews.posted.before <- add_row(reviews.posted.before, text = tempxx1[j,4])
 }
 tempxx2 <- subset(tmpx, date > date.sup.usr.vistd & business_id == res.bus.id)
 for (k in 1:nrow(tempxx2)) {
 	 reviews.posted.after <- add_row(reviews.posted.after, text = tempxx2[k,4])
 }
}
write.csv(reviews.posted.before, file = "before_wc.csv", row.names = FALSE)
write.csv(reviews.posted.after, file = "after_wc.csv", row.names = FALSE)
