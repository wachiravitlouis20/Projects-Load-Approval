## Load package for Projects
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)

## Load data to R
df <-read_csv("loan_data.csv")
glimpse(df)
summary(df$person_age)
view(df)


## Clean data
df_clean <- df %>% 
  filter(person_age <= 100) %>%
  filter(person_emp_exp <= 65)
summary(clean_data)
nrow(df) # 45000
nrow(df_clean) # 44992

## Histrogram check range
ggplot(data = df_clean, aes(x = person_income)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  scale_x_log10() + 
  labs(title = "Distribution of Applicant Income (Log Scale)",
       x = "Income (Log Scale)",
       y = "Frequency")

count(df_clean$loan_status_label)

## Add new column 
df_clean <- df_clean %>%
  mutate(loan_status_label = factor(loan_status, 
                                    levels = c(0, 1), 
                                    labels = c("Approved", "Not Approved")))

glimpse(df_clean)

## boxplot Credit Score vs. Loan status
ggplot(data = df_clean, aes(x = loan_status_label, y = credit_score, fill = loan_status_label)) +
  geom_boxplot() +
  labs(title = "Credit Score vs. Loan Status",
       x = "Loan Status",
       y = "Credit Score") + # <--- ตรวจสอบตรงนี้ให้แน่ใจว่าเป็น y = credit_score
  theme_minimal() +
  scale_fill_manual(values=c("Approved"="lightblue", "Not Approved"="red")) +
  guides(fill="none")

## 
ggplot(data = df_clean, aes(y = loan_intent, fill = loan_status_label)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Loan Status by Loan Intent",
       x = "Proportion",
       y = "Loan Intent",
       fill = "Loan Status") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) # แสดงแกน X เป็นเปอร์เซ็นต์


install.packages("ggcorrplot")

library(ggcorrplot)

# 1. เลือกเฉพาะคอลัมน์ที่เป็นตัวเลข
numeric_df <- df_clean %>%
  select(where(is.numeric))

# 2. คำนวณค่าสหสัมพันธ์
corr_matrix <- cor(numeric_df, use = "complete.obs") # use="complete.obs" เพื่อจัดการกรณีมี NA

# 3. สร้าง Heatmap
ggcorrplot(corr_matrix,
           type = "lower",    # แสดงแค่ครึ่งล่างของ matrix
           lab = TRUE,        # แสดงตัวเลขค่า correlation
           lab_size = 3,
           colors = c("salmon", "white", "#56B4E9")) + # กำหนดโทนสี
  labs(title = "Correlation Matrix of Numerical Features")


## re-check 
data_check <- df_clean %>%
  group_by(loan_status_label) %>%
  summarise(
    count = n(),
    mean_credit_score = mean(credit_score),
    median_credit_score = median(credit_score),
    min_credit_score = min(credit_score),
    max_credit_score = max(credit_score)
  )
print(data_check)
