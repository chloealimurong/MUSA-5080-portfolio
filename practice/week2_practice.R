## 1.
library(tidyverse)
library(tidycensus)

## 2.
pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

dim(pa_income)
glimpse(pa_income)
head(pa_income)

# Pennsylvania has 67 counties. Does my row count match? Why or why not?
# My rows match according to dim() and glimpse(), indicating that there are 67 rows or observations for each variable including county name.

## 3.
pa_income$GEOID
as.numeric("01001")
# the leading zero was omitted in the result, thus why GEOIDs are always text instead of numbers.

## 4.
filter(pa_income, estimate > 60000)
# prediction: fewer than 67 rows.
# result: 56 rows

# Counties where the MOE is bigger than 3000
counties_3000 <- filter(pa_income, moe >3000)
head(counties_3000$NAME)

# There are 15 total.

#Counties where the estimate is under 50000
counties_50000 <- filter(pa_income, estimate < 50000)
head(counties_50000$NAME)
# Just one, Cameron County.

# 5.
select(pa_income, NAME, estimate, moe)
# prediction: the number of rows change bc we are just grabbing the columns (variables)
# result: rows remained the same and the columns were dropped

# Show only GEOID and estimate
select(pa_income, GEOID, estimate)

# 6.
mutate(pa_income, moe_pct = moe / estimate * 100)
# prediction: 67 rows x 6 columns (5+1)

#moe_pct
pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)
pa_income

# moe_pct is telling me the MOE as a percentage of the total

# 7.
arrange(pa_income, moe_pct)
arrange(pa_income, desc(moe_pct))

# the number of rows do not change, its the same just different order and nothing was added or removed.

# 8.

step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

pa_income |>
  filter(moe_pct > 5) |>
  arrange(desc(moe_pct)) |>
  select(NAME, estimate, moe, moe_pct)

# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct

worst <- pa_income |>
  filter(moe_pct > 8) |>
  arrange(desc(estimate)) |>
  select(NAME, moe_pct)

# 9.
pa_income <- mutate(pa_income, reliable = moe_pct < 5)

pa_income |>
  group_by(reliable) |>
  summarize(n=n(),
            avg_income = mean(estimate))






