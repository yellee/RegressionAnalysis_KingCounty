# 파일 읽기
par(mfrow=c(2,2))


###################################################################################
file <- read.csv(file="kc_house_data.xlsx")
portion <- file[,c(3,13:16)]
variables <- colnames(portion)
###################################################################################

# 각 variable별로 histogram 작성
# > interval.width
#   [1] 268.7296

for( i in 1:4 ){
     hist(table(portion[,i]), main = paste("Histogram of", variables[i]))
}

###################################################################################

# shapiro.test(variables)
# skewed 되어서 => transformation을 해야할수도....


# basement랑 yr_renovated가 0이 아닌 값들을 모아서 히스토그램
par(mfrow=c(2,1))

###################################################################################
# basement와 renovated가 0이 너무 많아서 값이 있는 것과 분류
# (1)
install.packages("dplyr")
library(dplyr)

is_basement <- dplyr::select(portion, sqft_basement) %>%
              dplyr::filter(sqft_basement!=0)
hist(table(is_basement))

is_yrrenovated <- dplyr::select(portion, yr_renovated) %>%
                  dplyr::filter(yr_renovated!=0)
hist(table(is_yrrenovated))

# (2)
ap <- portion[portion$sqft_basement!=0,]
bp <- portion[portion$yr_renovated!=0,]

###################################################################################

# scatterplot matrix
scatterplot_matrix <- pairs(portion)
# price와 basement, above는 선형관계를 보이지만,
# price와 건립년도, 리모델링년도는 선형관계를 보이지 않는다.
# above와 basement도 선형관계를 보일뿐, 나머지 독립변수들간 선형관계는 보이지 않는다.


# correlation matrix
correlation matrix <- cor(portion)


# correlation test
## 독립변수(지상면적, 지하면적, 건립년도, 리모델링 년도)와 종속변수(가격) 간의 관계
### (1) 지상면적 : 유의하다
cor.test(portion$sqft_above, portion$price, method = "pearson") # 
### (2) 지하면적 : 유의하다
cor.test(portion$sqft_basement, portion$price, method = "pearson")
### (3) 건립년도 : 유의하다
cor.test(portion$yr_built, portion$price, method = "spearman")
### (4) 리모델링 년도 : 유의하다
cor.test(portion$yr_renovated, portion$price, method = "spearman")
### 3,4번은 scatterplot에서 선형 관계를 보이지 않아 spearman을 씀.


## 독립변수 간 상관관계 : cor.test(data$variable, data$variable, method = "pearson/kendall/spearman")
# 1) 지상면적 ~ 지하면적 : pearson : -0.052 / 유의하다
# 2) 지상면적 ~ 건립년도 : pearson, kendall, spearmnan / 유의하다
# 3) 지상면적 ~ 리모델링년도 : 세 방법 모두 / 유의하다
# 4) 지하면적 ~ 건립년도 : 세 방법 모두 / 유의하다
# 5) 지하면적 ~ 리모델링년도 : 세 방법 모두 / 유의하다
# 6) 건립년도 ~ 리모델링년도 : 세 방법 모두 / 유의하다


# 회귀분석은 계수(coefficient) 정확한 추정을 위해 OLS를 가정하는데, 이 때의 가정 중
# 하나는 독립변수(X)들끼리 독립이라는 것이다. 이로 볼 때, yr_built와 sqft_above가
# 상관성이 있다는 것은 multicollinearity(다중공선성) 문제가 발생할 수 있다.


# price와 선형관계를 보이는 basement,yr_renovated와의 고화질 산점도
ggplot(data=bp, aes(x=sqft_basement, y=price)+
+ geom_point(shape=19, size=3, colour="red") +
+ ggtitle("scatter plot : sqft_basement vs. price")
+ )

ggplot(data=ap, aes(x=yr_renovated, y=price)) +
+ geom_point(shape=19, size=3, colour="red") +
+ ggtitle("scatter plot : yr_renovated vs. price")


# 모든 변수를 넣은 회귀모형을 만들고, VIF를 구했더니,
# sqft_above와 sqft_basement가 높은 선형관계를 가진다고 말함.
# 책에서 이와 같은 다중공선성이 발생하면 후진소거법(Backward Elimination Method)가 좋다고 함.
# 그래서 이를 수행하였더니, sqft_basement가 첫번째 Full model에서 빠졌고,
# MSE와 관련된 척도 AIC가 빠짐으로써 유의미하게 줄지 않았다.
# 이는 sqft_basement가 그리 중요한 변수는 아님을 말하는 것 같다.


