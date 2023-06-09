# Characteristics-Of-Poor-Province-in-Indonesia
I used 3 attributes to represent the factor of poverty in 34 provinces in Indonesia (unemployment rate, literacy rate, and mean years school). The data was obtained from bps.go.id as a result of National Economics Survey in March 2022. 

![image](https://github.com/dewikinasih/Characteristics-Of-Poor-Province-in-Indonesia/blob/f42ba476f488280a4a90e34b73e30b48f5c51eed/miskin.png)

The aim is to see how the condition of poor province based on the variables in the data. In this case, i used agglomerative hierarchical clustering for grouping province with their similarity. The dendogram shows us that we can make 5 different cluster of province based on their poverty rate.

There is 1 province that categorized as extremely high poverty rate. That province has extremely low quality of education. But, in low poverty rate provinces, there is still high rate of unemployment whereas their quality of education is much higher than other clusters

[Syntax R](https://github.com/dewikinasih/Characteristics-Of-Poor-Province-in-Indonesia/blob/ddb3944f1ddf8fba65059509e0906c022ce2ba6e/Characteristic%20of%20Poor%20Province.R)

Package : readxl, xlsx, tidyverse, factoextra, cluster

Medium Article : [Here](https://dewikinasih.medium.com/hierarchical-agglomerative-clustering-untuk-karakterisasi-provinsi-miskin-di-indonesia-dengan-9121f8e7afc7)
