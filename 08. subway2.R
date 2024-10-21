#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#CSV형식의 파일 불러와서 congestion객체에 입력하고 구조 확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)
#결측치 갯수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))
#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치를 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
#23시 30분 출발기차의 결측치를 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))
#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1, aes(y=s0530))+
  geom_boxplot()
summary(congestion1$s0530)

#파생변수만들기
#1.지하철역의 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#데이터분석
#1.  수도권 지하철의 하루 평균 혼잡도

mean(congestion1$day_mean)
#2. 호선별 하루평균혼잡도
mean(congestion1$line)

#2. 호선별 출근시간(07:00~09:00)의 혼잡도 평균
mean(congestion$s0700>=congestion$s0900)


#2-1. 호선별 출근시간(07:00~09:00)의 기술통계
library(dplyr)

df <- data.frame(
  "s0700" = c(80, 90, 85, NA),
  "s0730" = c(85, 95, NA, 88),
  "s0800" = c(78, 92, 87, 90),
  "s0830" = c(82, 93, 85, 88),
  "s0900" = c(75, 90, 82, 80)
)

rush_hour_cols <- c("s0700", "s0730", "s0800", "s0830", "s0900")

statistics <- df %>%
  summarise(across(all_of(rush_hour_cols), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))  # 添加 .names 参数以避免列名冲突

print(statistics)


#2-2. 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기
most_congested_time <- df %>%
  summarise(across(all_of(rush_hour_cols), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "time", values_to = "mean_congestion") %>%
  arrange(desc(mean_congestion)) %>%
  slice(1)


most_congested_df <- df %>%
  summarise(across(all_of(rush_hour_cols), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "time", values_to = "mean_congestion") %>%
  filter(time == most_congested_time$time)


ggplot(most_congested_df, aes(x = time, y = mean_congestion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "평균 혼잡도가 가장 높은 시간대", x = "시간대", y = "평균 혼잡도") +
  geom_text(aes(label = round(mean_congestion, 1)), vjust = -0.5, size = 3.5)

most_congested_time <- df %>%
  summarise(across(all_of(rush_hour_cols), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "time", values_to = "mean_congestion") %>%
  arrange(desc(mean_congestion)) %>%
  slice(1)


most_congested_df <- df %>%
  summarise(across(all_of(rush_hour_cols), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "time", values_to = "mean_congestion") %>%
  filter(time == most_congested_time$time)


ggplot(most_congested_df, aes(x = time, y = mean_congestion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "평균 혼잡도가 가장 높은 시간대", x = "시간대", y = "평균 혼잡도") +
  geom_text(aes(label = round(mean_congestion, 1)), vjust = -0.5, size = 3.5)



#2-3. 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역
line_means <- colMeans(df, na.rm = TRUE)
max_congestion_line <- names(which.max(line_means))
print(paste("평균 혼잡도가 가장 높은 호선은:", max_congestion_line))
max_contribution_station <- which.max(df[[max_congestion_line]])
print(paste("기여도가 가장 높은 역의 인덱스는:", max_contribution_station))



#3.08시 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  select(s80_grade,n,pct)%>%
  arrange(desc(n))

#3-1. 호선별로 08시 지하철 혼잡도 범주화
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(line, s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s80_grade=="caution")%>%
  select(line, s80_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

#4. 호선별 퇴근시간(18:00~20:00)의 혼잡도 평균

mean(congestion$s1800>=congestion$s2000)


#4-1. 호선별 퇴근시간(18:00~20:00)의 기술통계

df <- data.frame(
  "s1800" = c(85, 90, 88, 92),
  "s1830" = c(80, 85, 90, 87),
  "s1900" = c(75, 80, 85, 82),
  "s1930" = c(78, 82, 80, 88),
  "s2000" = c(72, 77, 75, 70)
)

# 퇴근시간대 열 선택
rush_hour_cols <- c("s1800", "s1830", "s1900", "s1930", "s2000")

# 기술통계 계산
statistics <- df %>%
  summarise(across(all_of(rush_hour_cols), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

# 결과 출력
print(statistics)



#4-2. 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기
library(dplyr)
library(ggplot2)

# 예시 데이터프레임 df, 퇴근 시간대 혼잡도 데이터
df <- data.frame(
  "s1800" = c(85, 90, 88, 92),
  "s1830" = c(80, 85, 90, 87),
  "s1900" = c(75, 80, 85, 82),
  "s1930" = c(78, 82, 80, 88),
  "s2000" = c(72, 77, 75, 70)
)

# 퇴근 시간대 열 선택
rush_hour_cols <- c("s1800", "s1830", "s1900", "s1930", "s2000")

# Step 1: 각 시간대별 평균 혼잡도 계산
average_congestion <- df %>%
  summarise(across(all_of(rush_hour_cols), mean, na.rm = TRUE))

# Step 2: 데이터프레임을 long 형식으로 변환하여 ggplot2 사용하기 편하게 구성
average_congestion_long <- pivot_longer(average_congestion, cols = everything(), names_to = "time", values_to = "mean_congestion")

# Step 3: 막대 그래프 그리기
ggplot(average_congestion_long, aes(x = time, y = mean_congestion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "퇴근시간대 평균 혼잡도", x = "시간대", y = "평균 혼잡도") +
  geom_text(aes(label = round(mean_congestion, 1)), vjust = -0.5, size = 3.5)


#4-3. 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역
library(dplyr)
# 예시 데이터프레임, 각 열은 호선이고 각 행은 역의 혼잡도를 나타냄
df <- data.frame(
  "Line1" = c(80, 90, 85, 88),
  "Line2" = c(85, 95, 89, 88),
  "Line3" = c(78, 92, 87, 90),
  "Line4" = c(82, 93, 85, 88),
  "Line5" = c(75, 90, 82, 80)
)

# Step 1: 각 호선의 평균 혼잡도를 계산
line_means <- colMeans(df, na.rm = TRUE)

# Step 2: 평균 혼잡도가 가장 높은 호선을 찾기
max_congestion_line <- names(which.max(line_means))

print(paste("평균 혼잡도가 가장 높은 호선은:", max_congestion_line))

# Step 3: 그 호선에서 혼잡도가 가장 높은 역을 찾기
max_contribution_station <- which.max(df[[max_congestion_line]])

print(paste("기여도가 가장 높은 역의 인덱스는:", max_contribution_station))


