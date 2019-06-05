################################################################################
# Title: ALY6040 - Exercise - Danlin Shen.R                                    #
# Created Date: May 23, 2019, 6:00:11 pm                                       #
# Author: Danlin Shen(shen.da@husky.neu.edu)                                   #
# Github: https://github.com/ZacksAmber/R                                      #
# Copyright (c) 2019 Danlin Shen                                               #
################################################################################

mpg
head(mpg)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point()