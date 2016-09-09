library(dplyr)
library(ggplot2)

usage <- read.delim('U:\\ud_sql\\usage.tsv', sep = '\t')
prod <-  read.delim('U:\\ud_sql\\production.tsv', sep = '\t')
df <- merge(usage, prod, by = 'ORDER')




df_grouped <- df %>%
  group_by(ORDER) %>%
  summarise(TOTAL_USAGE = sum(USAGE)) 

df <- merge(df, df_grouped, by = 'ORDER')


# convert to date types
df <- transform(df, 
                REC_DATE = as.Date(as.character(REC_DATE), format='%Y%m%d'),
                USED_DATE = as.Date(as.character(USED_DATE), format='%Y%m%d'),
                FINISH_DATE = as.Date(as.character(FINISH_DATE), format='%Y%m%d'),
                BRIX = as.numeric(paste(BRIX))
                )

df <- transform(df, USAGE = -USAGE)

df <- transform(df,
                REC_YEAR = as.numeric(format(REC_DATE, '%Y')),
                STORED_DAYS = USED_DATE - REC_DATE,
                USAGE_PCT = USAGE / TOTAL_USAGE
                )
df <- transform(df, 
                PRODUCTION_YIELDED = USAGE_PCT * PRODUCTION
                )


head(df)


ggplot(aes(x = as.numeric(format(FINISH_DATE,'%Y')), y = PRODUCTION_YIELDED), data = df) + 
  geom_line(aes(color = VARIETY), stat = 'summary', fun.y = 'sum')


ggplot(aes(x = as.numeric(format(FINISH_DATE,'%Y')), y = USAGE), data = df) + 
  geom_bar(stat = 'summary', fun.y = 'sum') +
  scale_color_brewer()






ggplot(aes(x = FINISH_DATE , y = BRIX), 
       data = subset(df, BRIX > 0)) + 
  geom_jitter(alpha = .1, color = '#2ca25f') +
  scale_y_continuous(limits = c(7,25), breaks = seq(7,25,1)) +
  stat_smooth(span = 1) +
  ggtitle('Brix levels over time') +
  xlab('Time') +
  ylab('Brix level')



ggplot(aes(x = STORED_DAYS), data = subset(df, STORED_DAYS > 0)) +
  geom_histogram(binwidth = 5) +
  facet_wrap( ~ REC_YEAR) +
  scale_x_continuous(limits = c(0,365)) +
  scale_y_sqrt() +
  ggtitle('Days Between Receit and Usage by Year Received') +
  xlab('Number of days') +
  ylab('Count of Lots') 
  

