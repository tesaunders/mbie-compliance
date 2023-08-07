library(tidyverse)
library(forcats)
library(purrr)

df <- read_csv("data/conzul-data-raw.csv")
wos <- read_csv("data/wos-subjects.csv")

# Select & rename columns

df <- df %>% 
  select(doi = 1, 
         status = 4, 
         journal = 14, 
         year = 15,
         publisher = 16,
         oa = 17,
         genre = 18, 
         embargo = 29, 
         crossref = 34, 
         subject = 52, 
         rutherford = 55,
         marsden = 56,
         royalsoc = 57,
         mbie = 58,
         hrc = 59) %>% 
  filter(year == "2021")

# Collapse subjects to their most basic level

df$subject <- gsub(";.*","",df$subject)

colnames(wos) <- c("Arts & Humanities", "Life Sciences & Medicine", 
                   "Physical Sciences & Mathematics", 
                   "Social Sciences", "Technology & Engineering")

# Rename subject factors to wos categories

new_ids <- map(wos, ~ .x[!is.na(.x)])

df$subject <- fct_collapse(df$subject, !!!new_ids)

df$subject <- recode(df$subject, 
                     `Arts & Humanities - Other Topics` = 'Arts & Humanities', 
                     `Life Sciences & Biomedicine - Other Topics` = 'Life Sciences & Medicine',
                     `Science & Technology - Other Topics` = 'Technology & Engineering',
                     `Social Sciences - Other Topics` = 'Social Sciences')

df$subject <- fct_explicit_na(df$subject, "Unknown")

# Rename embargo factors

df$embargo <- fct_explicit_na(df$embargo, "NoData")

df$embargo <- fct_collapse(df$embargo, 
                           NotCompliant = c("18 months", "24 months", "2y", "36 months"),
                           Compliant = c("None", "6 months", "3 months", "12 months"))

df$embargo <- factor(df$embargo, levels = c('NotCompliant', 'NoData', 'Compliant'))

# Replace gold/diamond 'embargo' with 'compliant'

df <- within(df, embargo[embargo == 'NoData' & status == "gold"] <- 'Compliant')
df <- within(df, embargo[embargo == 'NoData' & status == "diamond"] <- 'Compliant')

# Summarise

df2 <- df %>% 
  group_by(subject) %>%
  count(embargo) %>% 
  mutate(
    pct = n / sum(n) * 100,
  )

# For bar total text

totals <- df %>% 
  group_by(subject) %>% 
  summarise(
    total = n(),
  )

# Plot compliance by discipline

ggplot(df2, aes(x = subject, y = pct, fill = embargo)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values=c("red", "black","darkgreen")) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Incoming MBIE policy compliance for 2021 publications by NZ university-affiliated researchers") +
  xlab("Subject Area") +
  ylab("") +
  labs(fill = "Embargo") +
  scale_x_discrete(limits = c("Unknown", "Arts & Humanities",
                              "Social Sciences", "Technology & Engineering",  
                              "Physical Sciences & Mathematics", 
                              "Life Sciences & Medicine")) +
  geom_text(aes(subject, 1.05, label = total, fill = NULL), data = totals) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("compliance-discipline.png", width = 10, height = 6)




