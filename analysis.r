library(plyr)
library(ggplot2)

source("util.r")

# Read in wage data for school districts
school_pay <- read.csv("data/2014-school-districts.csv")

# Read in API scores
api_scores_names <- scan("fieldnames.txt", what=character())
api_scores_widths <- scan("width.txt", what=numeric())
api_scores <- read.fwf("data/api13gtx.txt",
                       col.names=api_scores_names,
                       width = api_scores_widths)

districts <- api_scores[api_scores$rtype == "D",]

# Aggregate individual wage data by district
pay.agg <- ddply(school_pay, .(Agency), summarize,
                 mean.pay=mean(Total.Pay...Benefits),
                 sddev.pay=sd(Total.Pay...Benefits),
                 max.pay=max(Total.Pay...Benefits),
                 count.pays=length(Total.Pay...Benefits),
                 total.pays=sum(Total.Pay...Benefits))

top.agency <- head(arrange(pay.agg, desc(total.pays)),10)$Agency

ggplot(school_pay[school_pay$Agency %in% top.agency,], aes(x=Agency,y=Total.Pay...Benefits)) +
    geom_boxplot() +
    labs(x="Total Pay with Benefits", y="School District", title="Distribution of Wages for Major School Districts in CA") +
    coord_flip()

# Reduce columns
api.short <- districts[,c("dname","valid","tested","avg_ed","api13")]

# Clean district name field
api.short$dname <- trim(api.short$dname)

# Join pay and API score datasets
d <- merge(pay.agg, api.short, by.x="Agency", by.y="dname")

# Filter out districts that are too small
d <- d[d$tested > 500,]

d$wageperstudent <- d$total.pays/d$tested
d$student.emp <- d$tested / d$count.pays

ggplot(d, aes(x=wageperstudent, y=api13)) + geom_point() +
    geom_smooth(method="lm", se=FALSE, color="blue", formula = y ~ x) +
    geom_text(x=30000, y=950, label = lm_eqn(lm(api13 ~ wageperstudent, d)), parse=TRUE) +
    labs(x="Dollars Paid as Wages per Student", y="2013 API Score",
         title="Correlation Between School District Wages and API Score")

ggsave("outputs/corr-wage-api.png")

ggplot(d, aes(x=avg_ed, y=api13)) + geom_point() +
    geom_smooth(method="lm", se=FALSE, color="blue", formula = y ~ x) +
    geom_text(x=2, y=950, label = lm_eqn(lm(api13 ~ avg_ed, d)), parse=TRUE) +
    labs(x="Average Parent Education Level", y="2013 API Score",
         title="Correlation Between Parent Education Level and API Score")

ggsave("outputs/corr-parent-api.png")

write.csv(d, "outputs/school-district-joined.csv")
