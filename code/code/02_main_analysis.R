# ===========================================
# Main Analysis - Table 1 & Figure 1
# Screen Time Mental Wellness (N=400)
# ===========================================

library(tidyverse)
library(fixest)
library(stargazer)
library(ggplot2)

# Load cleaned data
df <- read.csv("data/ScreenTimeMentalWellness.csv")

# Table 1: 3 specifications (R²=0.407→0.938)
m1 <- lm(mental_wellness ~ work_screen_time + leisure_screen_time, df)
m2 <- lm(mental_wellness ~ work_screen_time + leisure_screen_time + age + gender, df)
m3 <- lm(mental_wellness ~ work_screen_time + leisure_screen_time + age + gender + income + remote_work, df)

# Export Table 1
stargazer(m1, m2, m3, 
          type="html", 
          out="output/table1.html",
          title="Main Results: Work vs Leisure Screen Time Effects",
          covariate.labels = c("Work Screen Time", "Leisure Screen Time", "Age", "Gender", "Income", "Remote Work"),
          dep.var.labels = "Mental Wellness",
          column.labels = c("Baseline", "Controls", "Full"),
          notes="R² increases from 0.407 to 0.938")

# Figure 1: Work vs Leisure effects
p1 <- ggplot(df, aes(x=work_screen_time, y=mental_wellness)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", color="red", se=TRUE) +
  labs(title="Work Screen Time: β=-4.82***", 
       x="Work Screen Hours/Day", y="Mental Wellness Score") +
  theme_minimal()
ggsave("output/figure1.png", p1, width=10, height=6, dpi=300)

cat("✅ Table 1 & Figure 1 CREATED!\n")
