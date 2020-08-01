library(readxl)
library(dplyr)
library(janitor)
library(lubridate)

#Load the data
transactions = read_excel("data.xlsx",sheet = 2)
customer.demo = read_excel("data.xlsx",sheet=4)
customer.add = read_excel("data.xlsx",sheet=5)

############ TRANSACTIONS DATA ##########################################

#View data summaries

str(transactions)

#Completeness 
sum(!complete.cases(transactions))
sum(is.na(transactions$online_order))
sum(is.na(transactions$brand))
sum(is.na(transactions$product_class))

#Uniqueness
length(transactions$transaction_id)
length(unique(transactions$transaction_id))

anyDuplicated(transactions$transaction_id) 

#Validity

is.numeric(transactions$transaction_id)
is.numeric(transactions$product_id)
is.numeric(transactions$customer_id)
is.numeric(transactions$list_price)
is.numeric(transactions$standard_cost)
is.logical(transactions$online_order)


sum(is.na(as.Date(transactions$transaction_date,format = "%Y-%m-%d")))
max(year(transactions$transaction_date))
min(year(transactions$transaction_date))

# To identify if there are any problems in encoding and to check if all categories are valid
levels(factor(transactions$online_order))
levels(factor(transactions$order_status))
levels(factor(transactions$brand))
levels(factor(transactions$product_line))
levels(factor(transactions$product_class))
levels(factor(transactions$product_size))


#Convert internalized excel date format to standard format

transactions$product_first_sold_date = excel_numeric_to_date(transactions$product_first_sold_date)

sum(!(transactions$transaction_date > transactions$product_first_sold_date),na.rm = TRUE) #This is 0 => No product has transaction date greater than first sold date - no inconsistencies


############ CUSTOMER DEMOGRAPHIC DATA ##########################################

# View Summary

str(customer.demo)
# Convert excel format dates to  normal ones. Here, we first take stock of NAs because dates older than 1900 tend to get stores by excel as just dates and not numbers and this results in NAs when converting them. To avoid loss of such dates we omit the indices having non numeric values while conversion
indices = which(!is.na(as.numeric(customer.demo$DOB)))
customer.demo$DOB[indices]=format(excel_numeric_to_date(as.numeric(customer.demo$DOB[indices])),"%Y-%m-%d")
customer.demo$DOB=as.Date(customer.demo$DOB)

#Completeness

sum(!complete.cases(customer.demo))
sum(!complete.cases(transactions))/nrow(customer.demo) * 100

colSums(is.na(customer.demo))

#In job category NAs are present as n/a
length(grep("n/a",customer.demo$job_industry_category))

#Uniqueness
length(unique(customer.demo$customer_id))

#Validity
is.numeric(customer.demo$customer_id)
is.numeric(customer.demo$past_3_years_bike_related_purchases)
is.numeric(customer.demo$tenure)

temp = customer.demo[which(is.na(as.Date(customer.demo$DOB,format = "%Y-%m-%d"))),]
sum(is.na(temp$DOB))

levels(factor(customer.demo$gender))
levels(factor(customer.demo$job_industry_category))
levels(factor(customer.demo$wealth_segment))
levels(factor(customer.demo$deceased_indicator))
levels(factor(customer.demo$owns_car))

#Accuracy and consistency

max(customer.demo$DOB,na.rm = TRUE)
min(customer.demo$DOB,na.rm = TRUE)

customer.demo = customer.demo[order(customer.demo$DOB),]


############ CUSTOMER ADDRESS DATA ###################### 

#Completeness
sum(complete.cases(customer.add))

#Uniqueness

length(unique(customer.add$customer_id))

#Validity
is.numeric(customer.add$customer_id)
is.numeric(customer.add$customer_id)
is.character(customer.add$address)
is.numeric(customer.add$postcode)
is.numeric(customer.add$property_valuation)

levels(factor(customer.add$state))
levels(factor(customer.add$country))

# Cross checking across tables
max(customer.add$customer_id)
max(customer.demo$customer_id)

## Check which values in address/trans data are not in demo data
x = customer.add$customer_id %in% customer.demo$customer_id
customer.add$customer_id[!x]

x = transactions$customer_id %in% customer.demo$customer_id
transactions$customer_id[!x]

