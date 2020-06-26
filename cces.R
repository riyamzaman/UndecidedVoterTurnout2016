#####################################
## CCES Undecided Voter Analysis ####
## Name: Riyam Zaman ################
#####################################

# Working Directory Path
setwd("C:/Users/riyam/Desktop/DSPS")

# Read .dta file
library(foreign)
cc <- read.dta("CCES.dta")

# Rows in dataset
nrow(cc)
head(cc)
names(cc)
ncol(cc)

# Extract relevant columns for project
df <- data.frame(cc$V101, cc$CC16_364, cc$CC16_425a, cc$CC416_25b_1,
                 cc$CC416_25b_2, cc$CC416_25b_3, cc$CC416_25b_4,
                 cc$CC16_421a, cc$CC16_401, cc$CC16_410a)

# Rename columns for readability
colnames(df)[1] <- "ID"
colnames(df)[2] <- "plan_to_vote"
colnames(df)[3] <- "contacted"
colnames(df)[4] <- "in_person"
colnames(df)[5] <- "phone"
colnames(df)[6] <- "email_or_text"
colnames(df)[7] <- "mailed"
colnames(df)[8] <- "ideology"
colnames(df)[9] <- "voted"
colnames(df)[10] <- "candidate"

# Subset dataset by voters who responded "No" or "Undecided"
# to whether they planned on voting in 2016
df_undecided <- subset(df, df$plan_to_vote == "No" |
                         df$plan_to_vote == "Undecided")

# Since we're trying to see the influence of campaign
# strategies (or lack thereof) on swaying undecided
# or non-voters and whether they ultimately vote,
# I will subset out N/A values in the voted column

df_und <- subset(df_undecided, !is.na(df_undecided$voted) &
                   df_undecided$voted != "I attempted to vote but did not or could not.")

# Number of rows of voters that are to be included in the
# analysis.
nrow(df_und)

# Code reasons for voting in "voted" columns to be 0 or 1
# for analytical purposes. 0 = did not vote, 1 = voted.
# Code 1 for: I definitely voted in the General Election.
# 0 for all other messages

df_und$voted.r <- ifelse(df_und$voted == "I definitely voted in the General Election.", "1", "0")
df_und$voted.r <- as.numeric(df_und$voted.r)

df_und$voted.r[df_und$candidate == "I didn't vote in this election"] <- "0"
df_und$voted.r <- as.numeric(df_und$voted.r)

# In-person coding
df_und$in_person <- ifelse(df_und$in_person == "Yes", "1", "0")
df_und$in_person <- as.numeric(df_und$in_person)

# Phone coding
df_und$phone <- ifelse(df_und$phone == "Yes", "1", "0")
df_und$phone <- as.numeric(df_und$phone)

# E-mail/Text coding
df_und$email_or_text <- ifelse(df_und$email_or_text == "Yes", "1", "0")
df_und$email_or_text <- as.numeric(df_und$email_or_text)

# Mailed coding
df_und$mailed <- ifelse(df_und$mailed == "Yes", "1", "0")
df_und$mailed <- as.numeric(df_und$mailed)

# Conact coding
df_und$contacted <- ifelse(df_und$contacted == "Yes", "1", "0")
df_und$contacted <- as.numeric(df_und$contacted)

sum(df_und$voted.r)
# 917/2267 undecided voters voted

# CONTROL
# Number of undecided voters that weren't contacted
no_contact <- subset(df_und, df_und$contacted == "0")
sum(no_contact$voted.r)
# ^ 633/929 of those that voted were not contacted
# 633/1640 of those that were not contacted voted (38.60%)

# CONTACTED
contact <- subset(df_und, df_und$contacted == "1")
sum(contact$voted.r)
nrow(contact)
# 284/626 of those that were contacted voted (45.37%)

# Calculate proprotions of voter_outreach_method/total_voted(?)
# Voted/No contact, 38.60%
prop.nocontact <- sum(no_contact$voted.r)/nrow(no_contact)
prop.nocontact

# Voted/in-person, 48.76%
# Find sum of people that voted if they were contacted in person
inp <-  subset(contact, contact$in_person == 1)
prop.inperson <- sum(inp$voted.r)/sum(contact$in_person)
prop.inperson
nrow(inp) #121

# Voted/phone, 46.05%
ph <- subset(contact, contact$phone == 1)
prop.phone <- sum(ph$voted.r)/sum(contact$phone)
prop.phone
nrow(ph) #430

# Voted/email or text, 46.45%
eot <- subset(contact, contact$email_or_text == 1)
prop.eot <- sum(eot$voted.r)/sum(contact$email_or_text)
prop.eot
nrow(eot) #183

# Voted/mail, 50%
m <- subset(contact, contact$mailed == 1)
prop.m <- sum(m$voted.r)/sum(contact$mailed)
prop.m
nrow(m) #212

# Make all NA values in df_und = 0
df_und[is.na(df_und)] = 0

# Make all no values in contacted = 0


# Create barplot using ggplot2
library(ggplot2)

bardf <- data.frame(outreach=c("No Contact","In-Person",
                               "Phone","Email/Text", "Mail"),
                    proportion=c(.3860,.4876,.4605,.4645,.5))
head(bardf)

p <- ggplot(data=bardf, aes(x=outreach,y=proportion, fill=outreach)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=proportion), vjust=1.6, color="white", size=3.5) +
  scale_fill_manual(values=c("#FFCD73","#FF9200","#D77B5F","#41222A","#C0413B")) +
  scale_x_discrete(limits=c("No Contact","In-Person","Phone","Email/Text","Mail")) +
  ggtitle("Effectiveness of Voter Outreach Strategies on Undecided Voters") +
  xlab("Voter Outreach Method") +
  ylab("Proportion of 2016 Voters By Outreach Method") +
  labs(fill = "Outreach Method") +
  #ylim(0,1) +
  theme(legend.position = "none")

p

# Regression Model

fit1 <- lm(voted.r ~ in_person, data=df_und)
coef(fit1)

fit2 <- lm(voted.r ~ phone, data=df_und)
coef(fit2)

fit3 <- lm(voted.r ~ email_or_text, data=df_und)
coef(fit3)

fit4 <- lm(voted.r ~ mailed, data=df_und)
coef(fit4)

# Best (Better than 4)
fit5 <- lm(voted.r ~ mailed + phone, data=df_und)
coef(fit5)

# Better than 1
fit6 <- lm(voted.r ~ in_person + phone, data=df_und)
coef(fit6)

# Performs well
fit7 <- lm(voted.r ~ mailed + phone +in_person, data=df_und)
coef(fit7)

# Performs well
fit8 <- lm(voted.r ~ mailed + phone + in_person + email_or_text, data=df_und)
coef(fit8)

fit9 <- lm(voted.r ~ mailed + in_person, data=df_und)
coef(fit9)

fit10 <- lm(voted.r ~ contacted, data=df_und)
coef(fit10)
summary(fit10)

fit11 <- lm(voted.r ~ email_or_text + phone, data=df_und)
coef(fit11)
summary(fit11)

summary(fit1) # In_Person (.05561)
summary(fit2) # Phone, significant at a 1% level (.00861)**
summary(fit3) # Email_or_Text
summary(fit4) # Mail, significant at a 1% level (.00291)**
summary(fit5) # Mail (.0281)* + Phone (.0905), (.002842)**
summary(fit6) # Phone (.0179)* + In_Person(.1240), (.009714)**
summary(fit7) # Mail (.0564) + Phone (.1062) + In_Person (.2767), (.00484)**
summary(fit8) # Everything (.01053)*
summary(fit9) # Mail (0.01)** + In_Person (0.23), (.005801)**
summary(fit10) # Contacted (.00332)

# Rank Order
# Fit5: Mail + Phone
# Fit4: Mail
# Fit7: Mail + Phone + In-Person
# Fit9
# Fit2

# Mail + Phone
p.fit5.00 <- predict(fit5, data.frame(mailed=0,phone=0), interval="confidence")
p.fit5.01 <- predict(fit5, data.frame(mailed=0,phone=1), interval="confidence")
p.fit5.10 <- predict(fit5, data.frame(mailed=1,phone=0), interval="confidence")
p.fit5.11 <- predict(fit5, data.frame(mailed=1,phone=1), interval="confidence")

# Mail
p.fit4.0 <- predict(fit4, data.frame(mailed=0), interval="confidence")
p.fit4.1 <- predict(fit4, data.frame(mailed=1), interval="confidence")

# Mail + Phone + In-Person
p.fit7.000 <- predict(fit7, data.frame(mailed=0,phone=0,in_person=0), interval="confidence")

p.fit7.001 <- predict(fit7, data.frame(mailed=0,phone=0,in_person=1), interval="confidence")
p.fit7.010 <- predict(fit7, data.frame(mailed=0,phone=1,in_person=0), interval="confidence")
p.fit7.100 <- predict(fit7, data.frame(mailed=1,phone=0,in_person=0), interval="confidence")

p.fit7.110 <- predict(fit7, data.frame(mailed=1,phone=1,in_person=0), interval="confidence")
p.fit7.101 <- predict(fit7, data.frame(mailed=1,phone=0,in_person=1), interval="confidence")
p.fit7.011 <- predict(fit7, data.frame(mailed=0,phone=1,in_person=1), interval="confidence")

p.fit7.111 <- predict(fit7, data.frame(mailed=1,phone=1,in_person=1), interval="confidence")


# Estimates fit5
est5 <- c(p.fit5.00[1], p.fit5.01[1], p.fit5.10[1], p.fit5.11[1])

# Styling Plot

alpha = 150 # Transparent points
colors5 <- c(rgb(56, 115, 144, alpha = alpha, maxColorValue = 255), 
          rgb(41, 86, 102, alpha = alpha, maxColorValue = 255),
          rgb(20, 59, 68, alpha = alpha, maxColorValue = 255),
          rgb(0, 42, 51, alpha = alpha, maxColorValue = 255))


## plot the vector. note x=1:4 because we have 4 estimates
plot(x=1:4, y= est5,
     col=colors5,
     pch=20,
     xaxt="n", 
     ylim=c(0,1),
     frame.plot=FALSE,
     xlab="",
     ylab="Predicted Voter Turnout Likelihood")
## add labels
text(x=1:4, y= 1, labels= c("M0,P0", "M0,P1", "M1,P0", "M1,P1"), cex=.5, pos=1)

title("Voter Outreach by Mail (M) and Phone (P)", adj = 1, 
      cex.main = 0.8, font.main = 2, col.main = "black", pch=20)

# Confidence Interval Lines
lines(c(1,1), c(p.fit5.00[2], p.fit5.00[3]), col=colors5[1])
lines(c(2,2), c(p.fit5.01[2], p.fit5.01[3]), col=colors5[2])
lines(c(3,3), c(p.fit5.10[2], p.fit5.10[3]), col=colors5[3])
lines(c(4,4), c(p.fit5.11[2], p.fit5.11[3]), col=colors5[4])

#############

# Estimates fit4
est4 <- c(p.fit4.0[1], p.fit4.1[1])

# Styling Plot

alpha = 150 # Transparent points
colors4 <- c(rgb(80, 51, 74, alpha = alpha, maxColorValue = 255), 
             rgb(65, 66, 42, alpha = alpha, maxColorValue = 255))


## plot the vector. note x=1:2 because we have 2 estimates
plot(x=1:2, y= est4,
     col=colors4,
     pch=20, cex=1.5,
     xaxt="n", 
     ylim=c(0,1),
     frame.plot=FALSE,
     xlab="",
     ylab="Predicted Voter Turnout Likelihood")
## add labels
text(x=1:2, y= 1, labels= c("M0", "M1"), cex=.7, pos=1)

title("Voter Outreach by Mail (M)", adj = 1, 
      cex.main = 0.8, font.main = 2, col.main = "black", pch=20)

# Confidence Interval Lines
lines(c(1,1), c(p.fit4.0[2], p.fit4.0[3]), col=colors4[1])
lines(c(2,2), c(p.fit4.1[2], p.fit4.1[3]), col=colors4[2])

#####################
# Estimate Fit7, Mail + Phone + In-Person

est7 <- c(p.fit7.000[1], p.fit7.010[1], p.fit7.001[1], p.fit7.100[1],
          p.fit7.011[1],p.fit7.110[1],p.fit7.101[1],p.fit7.111[1])

# Styling Plot

alpha = 150 # Transparent points
colors7 <- c(rgb(54, 31, 64, alpha = alpha, maxColorValue = 255), 
             rgb(26, 50, 44, alpha = alpha, maxColorValue = 255),
             rgb(64, 45, 35, alpha = alpha, maxColorValue = 255),
             rgb(102, 46, 65, alpha = alpha, maxColorValue = 255),
             rgb(106, 59, 40, alpha = alpha, maxColorValue = 255), 
             rgb(59, 77, 39, alpha = alpha, maxColorValue = 255),
             rgb(49, 79, 93, alpha = alpha, maxColorValue = 255),
             rgb(80, 71, 114, alpha = alpha, maxColorValue = 255))


## plot the vector. note x=1:8 because we have 8 estimates
plot(x=1:8, y= est7,
     col=colors7,
     pch=20,
     xaxt="n", 
     ylim=c(0,1),
     frame.plot=FALSE,
     xlab="",
     ylab="Predicted Voter Turnout Likelihood")
## add labels
text(x=1:8, y= 1, labels= c("M0,P0,I0", "M0,P1,I0", "M0,P0,I1", "M1,P0,I0",
                            "M0,P1,I1", "M1,P1,I0", "M1,P0,I1", "M1,P1,I1"), 
     cex=.5, pos=1)

title("Voter Outreach by Mail (M), Phone (P), and In-Person (I)", adj = 1, 
      cex.main = 0.8, font.main = 2, col.main = "black", pch=20)

# Confidence Interval Lines
lines(c(1,1), c(p.fit7.000[2], p.fit7.000[3]), col=colors7[1])
lines(c(2,2), c(p.fit7.010[2], p.fit7.010[3]), col=colors7[2])
lines(c(3,3), c(p.fit7.001[2], p.fit7.001[3]), col=colors7[3])
lines(c(4,4), c(p.fit7.100[2], p.fit7.100[3]), col=colors7[4])
lines(c(5,5), c(p.fit7.011[2], p.fit7.011[3]), col=colors7[5])
lines(c(6,6), c(p.fit7.110[2], p.fit7.110[3]), col=colors7[6])
lines(c(7,7), c(p.fit7.101[2], p.fit7.101[3]), col=colors7[7])
lines(c(8,8), c(p.fit7.111[2], p.fit7.111[3]), col=colors7[8])

#####################################################################################
summary(fit5)
summary(fit4)
summary(fit7)
