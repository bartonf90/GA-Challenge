library(tidyverse) 
library(lubridate)
library(Hmisc)
library(naniar) #for replace_with_na function
library(openxlsx)
library(zoo)
library(scales)
library(hrbrthemes)
library(RColorBrewer)

options(scipen = 999)

addsToCart <- read_csv("DataAnalyst_Ecom_data_addsToCart.csv")
sessionCounts <- read_csv("DataAnalyst_Ecom_data_sessionCounts.csv")

names(sessionCounts)

addsToCart <- addsToCart %>%
  rename_at(vars(contains("dim_")), funs(str_replace(., "dim_", "")))

# creating monthLabel and defining levels for tbl joins and ggplot

addsToCart <- addsToCart %>%
  mutate(monthLabel = month(month, label = TRUE))

addsToCart$monthLabel <- factor(addsToCart$monthLabel, 
                                 levels=c("Jul", "Aug", "Sep", "Oct",
                                          "Nov", "Dec", "Jan", "Feb", 
                                          "Mar", "Apr", "May", "Jun"))

# data cleaning before aggregating

sessionCounts <- sessionCounts %>%
  rename_at(vars(contains("dim_")), funs(str_replace(., "dim_", "")))

## checking for NA and duplicates

sum(is.na(sessionCounts)) ## (not set) values noted
sum(duplicated(sessionCounts))

length(unique(sessionCounts$browser))

# checking browser column since 57 unique values seems high

list(unique(sessionCounts$browser))

# values could be cleaned up but will not remove any illogical values
# since later aggregations will not include this column

sessionCounts <- replace_with_na(sessionCounts, 
                          replace = list(browser = c("error", "(not set)")))

# sum(is.na(sessionCounts$browser))

## how can there be rows with 0 for transactions and > 0 QTY values
## and vice versa? may not fully understand constraints of each metric

# dealing with dates and defining factor levels for months

sessionCounts$date <- mdy(sessionCounts$date)

sessionCounts <- sessionCounts %>%
  mutate(month = month(date), monthLabel = month(date, label = TRUE), 
         year = year(date))

sessionCounts$monthLabel <- factor(sessionCounts$monthLabel, 
                                levels=c("Jul", "Aug", "Sep", "Oct",
                                         "Nov", "Dec", "Jan", "Feb", 
                                         "Mar", "Apr", "May", "Jun"))


# aggregating
## first table: Month * Device

byMonthDevice <- sessionCounts %>%
  group_by(month, monthLabel, year, deviceCategory) %>%
  summarise(sessions_sum = sum(sessions),
            transactions_sum = sum(transactions),
            QTY_sum = sum(QTY),
            ECR = round(transactions_sum / sessions_sum, 3))

## second table: Month over Month for all metrics

byMonth <- sessionCounts %>%
  group_by(monthLabel) %>%
  summarise(sessions_sum = sum(sessions),
            transactions_sum = sum(transactions),
            QTY_sum = sum(QTY), ECR = round(transactions_sum / sessions_sum, 4))

# need other metric from addsToCart table

byMonth_all <- addsToCart %>%
  inner_join(byMonth) %>%
  rename(addsToCart_sum = addsToCart)

byMonth_all$year <- NULL

# Reordering columns by date first then likely flow of website activity

byMonth_all <- byMonth_all[c(1, 3, 4, 2, 5, 6, 7)]

# adding absolute and relative differences over month

lagRelative <- byMonth_all %>%
  mutate_at(3:7, funs(rel = . - lag(.)))

MoM <- lagRelative %>%
  mutate_at(3:7, funs(abs = . - first(.)))

MoM_lastTwo <- tail(MoM, 2)



# slide 1 -----------------------------------------------------------------

# monthly performance overview

# new date columns for ggplot needs
byMonthDevice$ymd <- paste(byMonthDevice$month, "1", byMonthDevice$year)
byMonthDevice$ymd <- mdy(byMonthDevice$ymd)

# same as above
byMonthDevice$deviceCategory <- factor(byMonthDevice$deviceCategory, 
                                       levels=c("desktop", "tablet", "mobile"),
                                       ordered = TRUE)

p1 <- ggplot(byMonthDevice, aes(x = ymd, y = ECR)) +
  geom_line(aes(color = deviceCategory), size = 1.4) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") + 
    ylim(0, NA) +
  labs(x="2012-2013",
       title = "Monthly Online Retailer Performance* by Device",
       subtitle = "*Defined by monthly ECR (transactions/sessions) over the course of 1 year") +
  scale_color_brewer(palette = "Dark2") +
  theme_ipsum_rc() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  theme(legend.text = element_text(size=12), 
        legend.title = element_text(12),
        axis.text.x = element_text(11)) +
  labs(color = "Device Category")

# p1

# accompanying table for p1 plot since data labels are too busy

# p1_table <- byMonthDevice %>%
#   group_by(monthLabel, deviceCategory) %>%
#   summarise(ECR = sum(ECR))

p1_table_sp <- p1_table %>%
  spread(monthLabel, ECR)


# slide 2 -----------------------------------------------------------------

# compare ECR by device w/o time

# byDevice <- byMonthDevice %>%
#   group_by(deviceCategory) %>%
#   summarise(sessions_all = sum(sessions_sum),
#             transactions_all = sum(transactions_sum)) %>%
#   mutate(ECR_all = round(transactions_all / sessions_all, 4))

p2 <- ggplot(byDevice, aes(x = deviceCategory, y = ECR_all, label = ECR_all)) +
  geom_col() + labs(x = "Device Category", y = "ECR",
                  title = "Online Retailer Performance* by Device",
                  subtitle = "*Defined by ECR (transactions/sessions)") +
  theme_ipsum_rc() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + geom_label()

p2

# slide 3 -----------------------------------------------------------------

# dig into raw session and transaction counts by device

p3_tbl <- byMonthDevice %>%
  select(ymd, deviceCategory, transactions_sum, sessions_sum) 

# p3_tbl$deviceCategory <- factor(p3_tbl$deviceCategory, levels=c("desktop", "mobile","tablet"), ordered = TRUE)

p3_sessions <- ggplot(p3_tbl, aes(x = ymd, y = sessions_sum)) +
  geom_line(aes(color = deviceCategory), size = 1.4) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") + 
  ylim(0, NA) +
  labs(x = "2012-2013", y = "Session Counts",
       title = "Monthly Session Counts by Device",
       subtitle = "Higher session counts on mobiles compared to tablets nearly every month") +
  scale_color_brewer(palette = "Dark2") +
  theme_ipsum_rc() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  theme(legend.text = element_text(size=12), 
        legend.title = element_text(12),
        axis.text.x = element_text(11)) +
  labs(color = "Device Category")

p3_sessions

# accompanying table for p1 plot since data labels are too busy

# p3_table <- p3_tbl %>%
#   group_by(monthLabel, deviceCategory) %>%
#   summarise(sessions = sum(sessions_sum))
# 
# p3_table_sp <- p3_table %>%
#   spread(monthLabel, sessions)


# exports -----------------------------------------------------------------

byMonthDevice$month <- NULL
byMonthDevice$year <- NULL
byMonthDevice$ymd <- NULL

MoM_lastTwo$month <- NULL

list <- list("byMonthDevice" = byMonthDevice, "MoM" = MoM_lastTwo)
write.xlsx(list, file = "output.xlsx")
write.xlsx(list, file = "output.xlsx", asTable = TRUE)
