
#All over the world, Numerous telecom companies are present. 
#To keep up in the competition and expand their business client have to invest in the market. 
#But, due to increasing competition, company is facing severe loss of revenue and loss of potential customers. 
#So, the client wants to find out the reasons of losing customers by measuring customer loyalty to regain the lost customers.


setwd('J:/Jishnu')

dat <- read.csv('bigml_59c28831336c6604c800002a.csv', header = T, stringsAsFactors = T)

summary(dat)

head(dat, 15)

str(dat)

library(dplyr)
library(ggplot2)

ggplot(data=dat, aes(x=reorder(state, -account.length), y=account.length, fill=churn)) + geom_bar(stat = 'identity') + theme_bw() +
  labs(x = 'States', y = 'Total account length', title = 'Total accounts and churn rate per state')

ggplot(dat, aes(account.length, fill = churn)) + geom_density(alpha = 0.5) 

corr_matrix <- corrplot(cor(dat[sapply(dat, is.numeric)]))
corr_matrix


dat$Total_Calls = (dat$total.day.calls + dat$total.eve.calls + dat$total.night.calls)
dat$Total_Charge = (dat$total.day.charge + dat$total.eve.charge + dat$total.night.charge)



str(dat)

dat$total.day.calls <- as.numeric(dat$total.day.calls)
total.day.charge 
dat$total.eve.calls <- as.numeric(dat$total.eve.calls)
total.eve.charge
dat$total.night.calls <- as.numeric(dat$total.night.calls)
total.night.charge
dat$total.intl.calls <- as.numeric(dat$total.intl.calls)
total.intl.charge 


ggplot(dat, aes(x=Total_Calls, y=Total_Charge, colour = churn)) +
              scale_color_brewer(palette = 'Dark2') + 
              geom_point() + geom_smooth(method = 'lm') + 
              labs(x='Total Calls', y='Total Charge', title='Total Calls vs Charge Scatterplot') +
              theme(panel.background = element_blank(), axis.line = element_line(color = 'black'))

ggplot(dat, aes(x=total.day.calls, y=total.day.charge, colour = churn)) +
              scale_color_brewer(palette = 'Dark2') + 
              geom_point() + geom_smooth(method = 'lm') + 
              labs(x='Total Day Calls', y='Total Day Charge', title='Total Day Calls vs Day Charge Scatterplot') +
              theme(panel.background = element_blank(), axis.line = element_line(color = 'black'))

ggplot(dat, aes(x=total.eve.calls, y=total.eve.charge, colour = churn)) +
              scale_color_brewer(palette = 'Dark2') + 
              geom_point() + geom_smooth(method = 'lm') + 
              labs(x='Total Eve Calls', y='Total Eve Charge', title='Total Eve Calls vs Eve Charge Scatterplot') +
              theme(panel.background = element_blank(), axis.line = element_line(color = 'black'))

ggplot(dat, aes(x=total.night.calls, y=total.night.charge, colour = churn)) +
              scale_color_brewer(palette = 'Dark2') + 
              geom_point() + geom_smooth(method = 'lm') + 
              labs(x='Total Night Calls', y='Total Night Charge', title='Total Night Calls vs Night Charge Scatterplot') +
              theme(panel.background = element_blank(), axis.line = element_line(color = 'black'))

library(Boruta)

set.seed(1234)

boruta <- Boruta(churn~., data=dat, doTrace=2, maxRuns = 100)

plot(boruta, las=2)

library(caTools)

set.seed(123)

split = sample.split(dat$churn, SplitRatio = 0.75)

training_set = subset(dat, split == TRUE)

test_set = subset(dat, split == FALSE)


library(randomForest)

model <- randomForest(churn ~ customer.service.calls + international.plan
                      + total.day.minutes + total.day.charge + total.intl.calls
                      + total.intl.charge + total.intl.minutes + voice.mail.plan
                      + number.vmail.messages + total.eve.minutes + total.eve.charge
                      + total.night.minutes + total.night.charge, data = training_set)
summary(model)
str(model)

prediction <- predict(model,test_set[-21], type='class')

library(caret)

cm <- confusionMatrix(prediction, test_set$churn)

cm






