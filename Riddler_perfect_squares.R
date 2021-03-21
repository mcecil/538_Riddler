
library(dplyr)

## Let a be a two digit integer
##a can be represented as digits a1a2
## in other words a = 10a1 + a2, where a1 and a2 are integers 0-9

## square a
## a^2 = (10a1 + a2)^2 = 100a1^2 + 20a1a2 + a2^2

## we want to subtract the ones digit from a^2
## the easiest way to do this is to take different cases 
## for the ones digit, a2.

## if a2 = 0, 1, 2, 3 we subtract a2^2 = 0, 1, 4, 9 and then divide by 10.
## if a2 = 4-9, we subtract a2^2, but we have to account 
## for the extra tens digit

## for example if a2 = 4, to drop the ones digit
## we are really subtracting 6 and dividing by 10.
## we can represent this as subtracting a2^2 or 16,
## then adding ten and finally dividing by 10.

a2_digits <- seq(0, 9)
tens_digits_to_add <- floor((a2_digits^2)/10)

## we have a^2 = 100a1 + 20a1a2 + a2^2
## let's refer to the tens digit to add above as d2. 

## if we drop the ones digit from a^2, this is equivalent to 
## (100a1^2 + 20a1a2 + a2^2 - a2^2 + 10d2)/10
## in other words, subtracting a2^2 adding a tens digit
## and dividing by 10

## this gives us, after dropping the ones digit
## 10a1^2 + 2a1a2 + d2

## so we now have the conditions:
## a1 and a2 are digits 0-9
## 10a1^2 + 2a2a1 + d2 = b^2, a square number
## or 10a1^2 + 2a2a1 + (d2 - b^2) = 0

## let's apply the quadratic formula on a1
## a1 = (-2a2 +-sqrt(4a2^2 - 40(d2 - b^2)))/20
## because a1 and a2 are positive, we can remove the +- and 
## just take the + expression
## a1 = (-2a2 + sqrt(4a2^2 - 40(d2 - b^2)))/20

##we need the discriminant 4a2^2 - 40(d2 - b^2) 
## to be a positive square number 
## discriminant is 4a2^2 - 40d2 + 40b^2

disc <- paste0((4 * a2_digits^2) - (40 * tens_digits_to_add), " + 40b^2")
print(disc)

## we also need the entire expression for a1 to equal a single digit
## a1 = (-2a2 + sqrt(disc)/20
## plugging in our vector for a2 

a1 <- paste0("(", (-2 * a2_digits), " + sqrt(", disc, "))/20")
print(a1)

## if we take the example for 16^2 = 256, we use the expression
## then a1 = 1, a2 = 6, d = 3 and b = 5
## the a1 vector above has 10 expressions for a2 digits 0...9
## because a2 = 6, we take the 7th expression
## (-12 + sqrt(24 + 40b^2))/20
## if we plug in b = 5, we see that the result is
## (-12 + sqrt(1024))/20 = (-12 + 32)/20 = 20/20 = 1



## we can see that our conditions now are that 
## the numerator for the a1 expression is divisible by 20
## and b itself must be an integer
## to be divisible by 20, while b is also an integer. 

## the easiest way I can think of is to take 
## each of the expressions for a1, iterate through all
## integer values of b, and see if the result is an integer.
## importantly, these expressions work even if our initial integer
## a has more than 2 digits. we can just think of a2 as the last 
## digit in a, and a1 as all the other digits.

max_b <- 1000000

good_pairs <- lapply(seq(0,9), function(x){
  print(x)
  a2 <- x
  good_pair_df <- data.frame("a1" = double(),
                             "a2" = double(),
                             "a" = double(),
                             "b" = double(),
                             "a_squared" = double(),
                             "b_squared" = double())
  for(b in seq(1, max_b)){
    a1 <- (-2*x + sqrt((4 * a2_digits[x + 1]^2) - (40 * tens_digits_to_add[x + 1]) + (40 * b^2)))/20
    
    ## check if a1 is a whole number
    if(a1%%1==0){
      a <- 10*a1 + a2
      new_row <- data.frame(a1, a2, a, b, a^2, b^2)
      names(new_row) <- c("a1", "a2", "a", "b", "a_squared", "b_squared")
      good_pair_df <- rbind(good_pair_df, new_row)
    } ## good pair check
  }## b loop
  return(good_pair_df)
})

all_good_pairs <- do.call(rbind, good_pairs) %>% arrange(a_squared)

options(scipen = 999)
print(all_good_pairs)
write.csv(all_good_pairs, "all_good_pairs.csv")


## BONUS

## a = 100a0 + 10a1 + a2
## a^2 = (100a0 + 10a1 + a2)(100a0 + 10a1 + a2)
## a^2 = 10000a0^2 + 2000a0a1 + 200a0a2 + 100a1^2 + 20a1a2 + a2^2

max_a0 <- 1000000
l <- list()
for(i in seq(5000, 10000)){
  print(i)
  
  for(j in seq(max_a0)){
    k <- (i*1000000) + j
    integer_ones_dropped <- floor(k/10)
    integer_ones_tens_dropped <- floor(k/100)
    if((sqrt(k)%%1 == 0) & 
       (sqrt(integer_ones_dropped)%%1 ==0) &
       (sqrt(integer_ones_tens_dropped)%%1 ==0)){
      print(paste0(k, " ", integer_ones_dropped, " ", integer_ones_tens_dropped))
      l <- append(l, k)
    }
  }
}
