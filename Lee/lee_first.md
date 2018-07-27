# 파일 읽기
par(mfrow=c(2,2))


###################################################################################
file <- readxl::read_xlsx(path="kc_house_data.xlsx")
portion <- file[,c(3,13:16)]
###################################################################################

# 각 variable별로 histogram 작성
# > interval.width
#   [1] 268.7296

variables <- c("sqft_above","sqft_basement","yr_built","yr_renovated")
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
ap <- portion[sqft_basement!=0,]
bp <- portion[yr_renovated!=0,]

###################################################################################

# scatterplot matrix
scatterplot_matrix <- pairs(portion)

## 보면 sqft_above, sqft_basement와 price는 어느 정도의 선형관계를 보인다. (Y와 X간)
## sqft_above와 sqft_basement도 어느 정도의 선형관계를 보인다. 

# correlation matrix
correlation matrix <- cor(portion)

# correlation test
cor.test(portion$sqft_above, portion$yr_built, method="pearson") # 유의하다(significant)
cor.test(portion$sqft_above, portion$price, method="pearson") # 유의하다(significant)

# 회귀분석은 계수(coefficient) 정확한 추정을 위해 OLS를 가정하는데, 이 때의 가정 중
# 하나는 독립변수(X)들끼리 독립이라는 것이다. 이로 볼 때, yr_built와 sqft_above가
# 상관성이 있다는 것은 multicollinearity(다중공선성) 문제가 발생할 수 있다.



# is_basement ~ price
cor.test(dp$sqft_basement,dp$price, method = "pearson")


	Pearson's product-moment correlation

data:  dp$sqft_basement and dp$price
t = 41.081, df = 8485, p-value <
0.00000000000000022
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.3894071 0.4249018
sample estimates:
      cor 
0.4073082 


# is_yrrenovated ~ price
cor.test(cp$yr_renovated,cp$price, method = "pearson")

	Pearson's product-moment correlation

data:  cp$yr_renovated and cp$price
t = 3.8943, df = 912, p-value = 0.0001057
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.06357587 0.19115408
sample estimates:
     cor 
0.127894 

######################################################################################
ggplot(data=dp, aes(x=sqft_basement, y=price)+
+ geom_point(shape=19, size=3, colour="red") +
+ ggtitle("scatter plot : sqft_basement vs. price")
+ )

#######################################################################################
ggplot(data=cp, aes(x=yr_renovated, y=price)) +
+ geom_point(shape=19, size=3, colour="red") +
+ ggtitle("scatter plot : yr_renovated vs. price")
