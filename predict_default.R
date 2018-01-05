#### by Ni Ma 20171117
#### Read and split data
file = "~/Documents/MA NI/"
data_all<-read.csv(sprintf("%s/subset.csv", file), header =T, na.strings=c(""))
#data_train <-read.csv(sprintf("%s/data_train.csv", file), header =T, na.strings=c(""))
#data_valid <-read.csv(sprintf("%s/data_valid.csv", file), header =T, na.strings=c(""))

indices = sample(1:nrow(data_all), size=floor(5/7*nrow(data_all)))
data_train = data_all[indices,]
data_valid = data_all[-indices,]
write.csv(data_train,sprintf("%s/data_train.csv", file))
write.csv(data_valid,sprintf("%s/data_valid.csv", file))

sapply(data_train, function(x) sum(is.na(x))) #no missing values


#### Exploratory data analysis on training set
print(summary(data_train$X1))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 10000   50000  140000  167167  240000 1000000

summary(factor(data_train$X2))
#    1    2
# 5928 9071

summary(factor(data_train$X3))
# 0    1    2    3    4    5    6
# 6 5257 7029 2475   60  144   28

summary(factor(data_train$X4))
#  0    1    2    3
# 30 6784 8013  172

for (i in 7:12) {
    print(summary(factor(data_train[, i])))
}
#-2   -1    0    1    2    3    4    5    6    7    8
#1364 2777 7434 1856 1329  171   39   12    4    5    8

#-2   -1    0    1    2    3    4    5    6    7
#1864 2993 7916   16 1965  169   48   12    8    8

#-2   -1    0    1    2    3    4    5    6    7    8
#2024 2918 7914    3 1943  115   43   13    9   15    2

#-2   -1    0    1    2    3    4    5    6    7    8
#2142 2808 8283    1 1583   90   38   19    3   31    1

#-2   -1    0    2    3    4    5    6    7
#2229 2778 8462 1353   93   41    9    3   31

#-2   -1    0    2    3    4    5    6    7    8
#2401 2931 8099 1409   91   24    9   10   24    1


summary(factor(data_train$Y))
# 0     1
# 11698  3301

for (i in 13:(ncol(data_train)-1)) {
    print(summary(data_train[, i]))
}

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#-165580    3608   22683   51309   67934  964511
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#-69777    3071   21419   49230   64494  983931
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#-157264    2677   20304   47021   60738 1664089
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#-170000    2400   19145   43296   55099  891586
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#-81334    1802   18203   40348   50270  927171
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#-209051    1263   17162   38939   49158  961664
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0    1000    2097    5691    5012  873552
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0     832    2000    5946    5000 1684259
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#0.0    391.5   1800.0   5171.3   4589.5 896040.0
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#0.0    302.5   1500.0   4914.1   4012.0 621000.0
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#0.0    287.5   1505.0   4866.7   4100.0 417990.0
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0     118    1500    5378    4005  527143


pdf(sprintf("%s/%s_eda.pdf", file, names(data_train)[2]), height=6, width=8, bg="white")
hist(data_train[,2], main = sprintf("histogram of %s", names(data_train)[2]), xlab = sprintf("%s", names(data_train)[2]), breaks = 200)
dev.off()
for (i in 13:(ncol(data_train)-1)) {
    pdf(sprintf("%s/%s_eda.pdf", file, names(data_train)[i]), height=6, width=8, bg="white")
    hist(data_train[,i], main = sprintf("histogram of %s", names(data_train)[i]), xlab = sprintf("%s", names(data_train)[i]), breaks = 200)
    dev.off()
}

xtabs(~ X3 + Y, data_train)
#         Y
#X3     0    1
# 0     6    0
# 1  4244 1013
# 2  5385 1644
# 3  1849  626
# 4    57    3
# 5   134   10
# 6    23    5


xtabs(~ X4 + Y, data_train)
#        Y
#X4     0    1
# 0    28    2
# 1  5226 1558
# 2  6318 1695
# 3   126   46


X = data_train[,c('Y','X1','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19','X20','X21','X22','X23')]

cor(X)
#                Y         X1           X5          X6          X7            X8            X9          X10          X11
#Y    1.0000000000 -0.1621612  0.008139968  0.32809105  0.27553439  0.2443074552  0.2307628162  0.213600015  0.202370896
#X1  -0.1621612154  1.0000000  0.139388587 -0.27673487 -0.29689558 -0.2824618037 -0.2669180523 -0.253213540 -0.237988540
#X5   0.0081399684  0.1393886  1.000000000 -0.03516429 -0.04580346 -0.0459558584 -0.0450269114 -0.047755228 -0.040121135
#X6   0.3280910496 -0.2767349 -0.035164290  1.00000000  0.67068109  0.5748977292  0.5434272171  0.518056500  0.483827359
#X7   0.2755343942 -0.2968956 -0.045803455  0.67068109  1.00000000  0.7669203626  0.6607969435  0.623631732  0.578318259
#X8   0.2443074552 -0.2824618 -0.045955858  0.57489773  0.76692036  1.0000000000  0.7769829054  0.686806393  0.632824719
#X9   0.2307628162 -0.2669181 -0.045026911  0.54342722  0.66079694  0.7769829054  1.0000000000  0.822058956  0.719691589
#X10  0.2136000154 -0.2532135 -0.047755228  0.51805650  0.62363173  0.6868063931  0.8220589563  1.000000000  0.817399653
#X11  0.2023708958 -0.2379885 -0.040121135  0.48382736  0.57831826  0.6328247186  0.7196915892  0.817399653  1.000000000
#X12 -0.0170177249  0.2861463  0.049723010  0.18549086  0.23599565  0.2073469587  0.1991509227  0.202256827  0.203391087
#X13 -0.0093635823  0.2775469  0.048832590  0.18945505  0.23754408  0.2369877381  0.2215290301  0.223849007  0.223394612
#X14 -0.0053320583  0.2820209  0.051084088  0.18026300  0.22523190  0.2268884029  0.2416518363  0.239832796  0.235378558
#X15 -0.0024254004  0.2899714  0.050619328  0.17875396  0.22316048  0.2250064135  0.2426834550  0.267958706  0.260857821
#X16 -0.0007582323  0.2921813  0.051182737  0.18265155  0.22489981  0.2251202247  0.2405900177  0.264946513  0.286898606
#X17  0.0027288521  0.2884232  0.050687170  0.17999665  0.22158342  0.2218601436  0.2360402515  0.256510844  0.279759590
#X18 -0.0706793190  0.1927401  0.032241179 -0.07335301 -0.07864230 -0.0002536391 -0.0134063713 -0.010152019 -0.005390562
#X19 -0.0551509896  0.1774767  0.026072734 -0.06864949 -0.05776551 -0.0642085757  0.0008672341 -0.003994045 -0.008601327
#X20 -0.0570020333  0.1978895  0.029883346 -0.07279333 -0.05521411 -0.0499755324 -0.0672211361  0.008374496  0.004882815
#X21 -0.0633248920  0.1950895  0.026455877 -0.05497334 -0.03576265 -0.0378036826 -0.0387065561 -0.059101537  0.014096742
#X22 -0.0544556117  0.2185065  0.016927297 -0.05249327 -0.03529954 -0.0316697971 -0.0288868855 -0.032835204 -0.049548535
#X23 -0.0581166650  0.2162222  0.023369741 -0.06134374 -0.03946933 -0.0394899068 -0.0301184483 -0.022851561 -0.026377672
#            X12          X13          X14         X15           X16         X17           X18           X19          X20
#Y   -0.01701772 -0.009363582 -0.005332058 -0.00242540 -0.0007582323 0.002728852 -0.0706793190 -0.0551509896 -0.057002033
#X1   0.28614628  0.277546887  0.282020925  0.28997136  0.2921812687 0.288423175  0.1927400554  0.1774767474  0.197889487
#X5   0.04972301  0.048832590  0.051084088  0.05061933  0.0511827370 0.050687170  0.0322411793  0.0260727340  0.029883346
#X6   0.18549086  0.189455046  0.180263003  0.17875396  0.1826515533 0.179996651 -0.0733530130 -0.0686494925 -0.072793335
#X7   0.23599565  0.237544083  0.225231898  0.22316048  0.2248998099 0.221583423 -0.0786422972 -0.0577655091 -0.055214113
#X8   0.20734696  0.236987738  0.226888403  0.22500641  0.2251202247 0.221860144 -0.0002536391 -0.0642085757 -0.049975532
#X9   0.19915092  0.221529030  0.241651836  0.24268346  0.2405900177 0.236040251 -0.0134063713  0.0008672341 -0.067221136
#X10  0.20225683  0.223849007  0.239832796  0.26795871  0.2649465130 0.256510844 -0.0101520186 -0.0039940447  0.008374496
#X11  0.20339109  0.223394612  0.235378558  0.26085782  0.2868986062 0.279759590 -0.0053905620 -0.0086013268  0.004882815
#X12  1.00000000  0.949839447  0.885533313  0.85615577  0.8275950987 0.799465706  0.1344235635  0.1010924390  0.157976625
#X13  0.94983945  1.000000000  0.923605988  0.88871924  0.8587992853 0.828929845  0.2623590155  0.0907127588  0.148870760
#X14  0.88553331  0.923605988  1.000000000  0.91903183  0.8784396639 0.850456752  0.2271609703  0.3225373267  0.125497894
#X15  0.85615577  0.888719237  0.919031832  1.00000000  0.9377399501 0.898903888  0.2169777806  0.2132388929  0.287284118
#X16  0.82759510  0.858799285  0.878439664  0.93773995  1.0000000000 0.947533597  0.2053585013  0.1834531327  0.249637790
#X17  0.79946571  0.828929845  0.850456752  0.89890389  0.9475335966 1.000000000  0.1922941149  0.1832423975  0.234834719
#X18  0.13442356  0.262359015  0.227160970  0.21697778  0.2053585013 0.192294115  1.0000000000  0.3056745477  0.299851955
#X19  0.10109244  0.090712759  0.322537327  0.21323889  0.1834531327 0.183242397  0.3056745477  1.0000000000  0.297677857
#X20  0.15797662  0.148870760  0.125497894  0.28728412  0.2496377898 0.234834719  0.2998519555  0.2976778572  1.000000000
#X21  0.15958170  0.150686877  0.144441205  0.12441113  0.2982738085 0.253847374  0.2391064109  0.2211098843  0.267598330
#X22  0.17822596  0.164645369  0.203618260  0.17555922  0.1533665096 0.310483831  0.1679187237  0.2256846942  0.163559694
#X23  0.18143875  0.181099461  0.182997894  0.18162147  0.1647049749 0.115598400  0.2047678030  0.1500469920  0.177974842
#            X21         X22         X23
#Y   -0.06332489 -0.05445561 -0.05811667
#X1   0.19508953  0.21850651  0.21622217
#X5   0.02645588  0.01692730  0.02336974
#X6  -0.05497334 -0.05249327 -0.06134374
#X7  -0.03576265 -0.03529954 -0.03946933
#X8  -0.03780368 -0.03166980 -0.03948991
#X9  -0.03870656 -0.02888689 -0.03011845
#X10 -0.05910154 -0.03283520 -0.02285156
#X11  0.01409674 -0.04954854 -0.02637767
#X12  0.15958170  0.17822596  0.18143875
#X13  0.15068688  0.16464537  0.18109946
#X14  0.14444121  0.20361826  0.18299789
#X15  0.12441113  0.17555922  0.18162147
#X16  0.29827381  0.15336651  0.16470497
#X17  0.25384737  0.31048383  0.11559840
#X18  0.23910641  0.16791872  0.20476780
#X19  0.22110988  0.22568469  0.15004699
#X20  0.26759833  0.16355969  0.17797484
#X21  1.00000000  0.16535623  0.15600330
#X22  0.16535623  1.00000000  0.15861595
#X23  0.15600330  0.15861595  1.00000000



cor(data_train[,c('Y','X6','X7','X8','X9','X10','X11')])
#            Y        X6        X7        X8        X9       X10       X11
#Y   1.0000000 0.3280910 0.2755344 0.2443075 0.2307628 0.2136000 0.2023709
#X6  0.3280910 1.0000000 0.6706811 0.5748977 0.5434272 0.5180565 0.4838274
#X7  0.2755344 0.6706811 1.0000000 0.7669204 0.6607969 0.6236317 0.5783183
#X8  0.2443075 0.5748977 0.7669204 1.0000000 0.7769829 0.6868064 0.6328247
#X9  0.2307628 0.5434272 0.6607969 0.7769829 1.0000000 0.8220590 0.7196916
#X10 0.2136000 0.5180565 0.6236317 0.6868064 0.8220590 1.0000000 0.8173997
#X11 0.2023709 0.4838274 0.5783183 0.6328247 0.7196916 0.8173997 1.0000000

cor(data_train[,c('Y','X12','X13','X14','X15','X16','X17')])
#                Y         X12          X13          X14        X15           X16         X17
#Y    1.0000000000 -0.01701772 -0.009363582 -0.005332058 -0.0024254 -0.0007582323 0.002728852
#X12 -0.0170177249  1.00000000  0.949839447  0.885533313  0.8561558  0.8275950987 0.799465706
#X13 -0.0093635823  0.94983945  1.000000000  0.923605988  0.8887192  0.8587992853 0.828929845
#X14 -0.0053320583  0.88553331  0.923605988  1.000000000  0.9190318  0.8784396639 0.850456752
#X15 -0.0024254004  0.85615577  0.888719237  0.919031832  1.0000000  0.9377399501 0.898903888
#X16 -0.0007582323  0.82759510  0.858799285  0.878439664  0.9377400  1.0000000000 0.947533597
#X17  0.0027288521  0.79946571  0.828929845  0.850456752  0.8989039  0.9475335966 1.000000000

cor(data_train[,c('Y','X18','X19','X20','X21','X22','X23')])
#              Y         X18         X19         X20         X21         X22         X23
#Y    1.00000000 -0.07067932 -0.05515099 -0.05700203 -0.06332489 -0.05445561 -0.05811667
#X18 -0.07067932  1.00000000  0.30567455  0.29985196  0.23910641  0.16791872  0.20476780
#X19 -0.05515099  0.30567455  1.00000000  0.29767786  0.22110988  0.22568469  0.15004699
#X20 -0.05700203  0.29985196  0.29767786  1.00000000  0.26759833  0.16355969  0.17797484
#X21 -0.06332489  0.23910641  0.22110988  0.26759833  1.00000000  0.16535623  0.15600330
#X22 -0.05445561  0.16791872  0.22568469  0.16355969  0.16535623  1.00000000  0.15861595
#X23 -0.05811667  0.20476780  0.15004699  0.17797484  0.15600330  0.15861595  1.00000000



#### Data manipulation and preprocessing
data_train$X3[which(data_train$X3 == 6|data_train$X3 == 5|data_train$X3 == 4)]=0
data_train$X2 = factor(data_train$X2)
data_train$X3 = factor(data_train$X3)
data_train$X4 = factor(data_train$X4)
data_train$avgDelay = rowMeans(data_train[,c('X6','X7','X8','X9','X10','X11')])
data_train$avgState = rowMeans(data_train[,c('X12','X13','X14','X15','X16','X17')])
data_train$avgPay = rowMeans(data_train[,c('X18','X19','X20','X21','X22','X23')])

data_train$propState = data_train$avgState/data_train$X1
data_train$propPay = data_train$avgPay/data_train$X1
data_train$exp1 = data_train$X12+data_train$X18-data_train$X13
data_train$exp2 = data_train$X13+data_train$X19-data_train$X14
data_train$exp3 = data_train$X14+data_train$X20-data_train$X15
data_train$exp4 = data_train$X15+data_train$X21-data_train$X16
data_train$exp5 = data_train$X16+data_train$X22-data_train$X17
data_train$avgExp = rowMeans(data_train[,c('exp1','exp2','exp3','exp4','exp5')])
data_train$propExp = data_train$avgExp/data_train$X1
data_train$debtScore = rowSums(data_train[,c('X6','X7','X8','X9','X10')] * data_train[,c('exp1','exp2','exp3','exp4','exp5')])
data_train$propDebt = data_train$debtScore/data_train$X1

cor(data_train[,c('Y','avgDelay','avgPay','propPay','avgState','propState','avgExp','propExp','debtScore','propDebt')])
#                    Y    avgDelay      avgPay     propPay    avgState propState      avgExp     propExp   debtScore
#Y          1.00000000  0.29394815 -0.10056130 -0.03696564 -0.00614026 0.1305836 -0.10093139 -0.05386684  0.12212114
#avgDelay   0.29394815  1.00000000 -0.07058968  0.04748093  0.28023697 0.5637011 -0.03488089  0.05427435  0.31559615
#avgPay    -0.10056130 -0.07058968  1.00000000  0.59726067  0.33458939 0.0301163  0.77697446  0.34987270 -0.55815052
#propPay   -0.03696564  0.04748093  0.59726067  1.00000000  0.10600595 0.2715948  0.42744390  0.63713829 -0.31678784
#avgState  -0.00614026  0.28023697  0.33458939  0.10600595  1.00000000 0.5514328  0.46717566  0.17240550  0.09833689
#propState  0.13058365  0.56370106  0.03011630  0.27159480  0.55143283 1.0000000  0.13072486  0.36894233  0.18935610
#avgExp    -0.10093139 -0.03488089  0.77697446  0.42744390  0.46717566 0.1307249  1.00000000  0.57769902 -0.44178501
#propExp   -0.05386684  0.05427435  0.34987270  0.63713829  0.17240550 0.3689423  0.57769902  1.00000000 -0.17639012
#debtScore  0.12212114  0.31559615 -0.55815052 -0.31678784  0.09833689 0.1893561 -0.44178501 -0.17639012  1.00000000
#propDebt   0.16804023  0.40872407 -0.32901351 -0.31541926  0.08554334 0.2494177 -0.24691192 -0.13378348  0.70215382
#
#             propDebt
#Y          0.16804023
#avgDelay   0.40872407
#avgPay    -0.32901351
#propPay   -0.31541926
#avgState   0.08554334
#propState  0.24941775
#avgExp    -0.24691192
#propExp   -0.13378348
#debtScore  0.70215382
#propDebt   1.00000000


# standardization and save means and standard deviations
contvar = c('X','ID','X2','X3','X4','Y')
meanSd = data.frame(matrix(nrow=2,ncol = length(setdiff(names(data_train),contvar))))
colnames(meanSd) = setdiff(names(data_train),contvar)
for (name in setdiff(names(data_train),contvar)) {
    meanSd[,name][1] = mean(data_train[, name])
    meanSd[,name][2] = sd(data_train[, name])
    data_train[,name] = (data_train[, name]-mean(data_train[, name]))/sd(data_train[, name])
}



#### Logistic regression
logreg0=glm(Y~1,data=data_train,family=binomial)
logreg1=glm(Y~X1+X2+X3+X4+X5+X6+X7+propState+avgPay+propDebt,data=data_train,family=binomial)

step(logreg0,scope=formula(logreg1), direction="forward",k=2)

logreg = glm(formula = Y ~ X6 + avgPay + X7 + X3 + X1 + X4 + X2 + propState +propDebt, family = binomial, data = data_train)
summary(logreg)
#Call:
#glm(formula = Y ~ X6 + avgPay + X7 + X3 + X1 + X4 + X2 + propState +
#propDebt, family = binomial, data = data_train)
#
#Deviance Residuals:
#Min       1Q   Median       3Q      Max
#-3.3338  -0.6973  -0.5461  -0.2694   2.9813
#
#Coefficients:
#Estimate Std. Error z value Pr(>|z|)
#(Intercept) -3.88419    0.80120  -4.848 1.25e-06 ***
#X6           0.64237    0.02829  22.703  < 2e-16 ***
#avgPay      -0.38229    0.05294  -7.221 5.15e-13 ***
#X7           0.21979    0.02762   7.957 1.77e-15 ***
#X31          1.16705    0.25641   4.551 5.33e-06 ***
#X32          1.04339    0.25526   4.088 4.36e-05 ***
#X33          1.07765    0.25851   4.169 3.06e-05 ***
#X1          -0.17232    0.02964  -5.813 6.12e-09 ***
#X41          1.53514    0.75958   2.021 0.043277 *
#X42          1.31617    0.75979   1.732 0.083222 .
#X43          1.50764    0.78100   1.930 0.053558 .
#X22         -0.15731    0.04317  -3.644 0.000269 ***
#propState   -0.06373    0.02609  -2.442 0.014587 *
#propDebt     0.05724    0.02789   2.052 0.040158 *
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15809  on 14998  degrees of freedom
#Residual deviance: 13833  on 14985  degrees of freedom
#AIC: 13861
#
#Number of Fisher Scoring iterations: 5

prop.table( table(data_train$Y) )
# 0         1
# 0.7799187 0.2200813

logreg = glm(formula = Y ~ X6 + avgPay + X7 + X3 + X1 + X4 + X2 + propDebt, family = binomial, data = data_train)
predict1 = predict(logreg, type = 'response')

findCutoff <- function(pred) {
    cutoffs = seq(0,1,0.01)
    optCo = 0
    optPerf = 0
    for (cutoff in cutoffs) {
        confusion = table(data_train$Y, pred > cutoff)
        if (ncol(confusion) == 2) {
            trueNeg = confusion[1,1]/(confusion[1,1]+confusion[1,2])
            truePos = confusion[2,2]/(confusion[2,1]+confusion[2,2])
            perf = sqrt(trueNeg*truePos)
            if (perf > optPerf) {
                optCo = cutoff
                optPerf = perf
            }
        }
    }
    return (c(optCo, optPerf))
}

findCutoff(predict1)
#0.2300000 0.6792864

optCo = 0.23
table(data_train$Y, predict1 > optCo)
#FALSE TRUE
#0  8658 3040
#1  1243 2058

install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)
y= data.matrix(data_train$Y)
x = model.matrix(~X6 + avgPay + X7 + X3 + X1 + X4 + X2 + propDebt, data_train)
regRegular = glmnet(x, y, family = "binomial", alpha = 0)
cvlam <- cv.glmnet(x, y, family = "binomial", alpha = 0)
bestlam <- cvlam$lambda.min
#0.0149
coef(regRegular,s=bestlam)
#                      1
#(Intercept) -1.53351430
#(Intercept)  .
#X6           0.56565122
#avgPay      -0.27338811
#X7           0.22769501
#X31          0.20705019
#X32          0.10685304
#X33          0.13521496
#X1          -0.17577077
#X41          0.13348646
#X42         -0.06752447
#X43          0.10593657
#X22         -0.14282412
#propState   -0.05274007
#propDebt     0.07628097

predict2 = predict(regRegular,newx=x,s=bestlam, type="response")
findCutoff(predict2)
#0.230000 0.680868
#same optimal cutoff


#### random forest
install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)
rf <- randomForest(as.factor(Y) ~ .-ID, data = data_train, node_size = 10, ntree = 200, cutoff = c(3/4, 1/4))
predict3 = predict(rf, data_train, type = "response")
confusion = table(data_train$Y, predict3)
trueNeg = confusion[1,1]/(confusion[1,1]+confusion[1,2])
truePos = confusion[2,2]/(confusion[2,1]+confusion[2,2])
sqrt(trueNeg*truePos)
#0.9816732


#### Function for validation data
validPerf <- function(data_validation) {
    data_validation$X3[which(data_validation$X3 == 6|data_validation$X3 == 5|data_validation$X3 == 4)]=0
    data_validation$X2 = factor(data_validation$X2)
    data_validation$X3 = factor(data_validation$X3)
    data_validation$X4 = factor(data_validation$X4)
    data_validation$avgDelay = rowMeans(data_validation[,c('X6','X7','X8','X9','X10','X11')])
    data_validation$avgState = rowMeans(data_validation[,c('X12','X13','X14','X15','X16','X17')])
    data_validation$avgPay = rowMeans(data_validation[,c('X18','X19','X20','X21','X22','X23')])
    data_validation$propState = data_validation$avgState/data_validation$X1
    data_validation$propPay = data_validation$avgPay/data_validation$X1
    data_validation$exp1 = data_validation$X12+data_validation$X18-data_validation$X13
    data_validation$exp2 = data_validation$X13+data_validation$X19-data_validation$X14
    data_validation$exp3 = data_validation$X14+data_validation$X20-data_validation$X15
    data_validation$exp4 = data_validation$X15+data_validation$X21-data_validation$X16
    data_validation$exp5 = data_validation$X16+data_validation$X22-data_validation$X17
    data_validation$avgExp = rowMeans(data_validation[,c('exp1','exp2','exp3','exp4','exp5')])
    data_validation$propExp = data_validation$avgExp/data_validation$X1
    data_validation$debtScore = rowSums(data_validation[,c('X6','X7','X8','X9','X10')] * data_validation[,c('exp1','exp2','exp3','exp4','exp5')])
    data_validation$propDebt = data_validation$debtScore/data_validation$X1
    
    
    for (name in setdiff(names(data_validation),contvar)) {
        data_validation[,name] = (data_validation[,name]-meanSd[,name][1])/meanSd[,name][2]
    }
    x = model.matrix(~X6 + avgPay + X7 + X3 + X1 + X4 + X2 + propState +propDebt, data_validation)
    predict <- predict(logreg, data_validation, type = 'response')
    confusion1 = table(data_validation$Y, predict > optCo)
    trueNeg = confusion1[1,1]/(confusion1[1,1]+confusion1[1,2])
    truePos = confusion1[2,2]/(confusion1[2,1]+confusion1[2,2])
    perf1 = sqrt(trueNeg*truePos)
    x = model.matrix(~X6 + avgPay + X7 + X3 + X1 + X4 + X2 + propDebt, data_validation)
    predict <- predict(regRegular,newx=x,s=bestlam, type="response")
    confusion2 = table(data_validation$Y, predict > optCo)
    trueNeg = confusion2[1,1]/(confusion2[1,1]+confusion2[1,2])
    truePos = confusion2[2,2]/(confusion2[2,1]+confusion2[2,2])
    perf2 = sqrt(trueNeg*truePos)
    predict <- predict(rf, data_validation, type = "response")
    confusion3 = table(data_validation$Y, predict)
    trueNeg = confusion3[1,1]/(confusion3[1,1]+confusion3[1,2])
    truePos = confusion3[2,2]/(confusion3[2,1]+confusion3[2,2])
    perf3 = sqrt(trueNeg*truePos)
    return (c(perf1, perf2, perf3))
}

validPerf(data_valid)
#0.6787039 0.6746476 0.6977980
#model 1: the logistic regression without regularization is better


#### Function for testing data
testPerf <- function(data_testing) {
    data_testing$X3[which(data_testing$X3 == 6|data_testing$X3 == 5|data_testing$X3 == 4)]=0
    data_testing$X2 = factor(data_testing$X2)
    data_testing$X3 = factor(data_testing$X3)
    data_testing$X4 = factor(data_testing$X4)
    data_testing$avgDelay = rowMeans(data_testing[,c('X6','X7','X8','X9','X10','X11')])
    data_testing$avgState = rowMeans(data_testing[,c('X12','X13','X14','X15','X16','X17')])
    data_testing$avgPay = rowMeans(data_testing[,c('X18','X19','X20','X21','X22','X23')])
    data_testing$propState = data_testing$avgState/data_testing$X1
    data_testing$propPay = data_testing$avgPay/data_testing$X1
    data_testing$exp1 = data_testing$X12+data_testing$X18-data_testing$X13
    data_testing$exp2 = data_testing$X13+data_testing$X19-data_testing$X14
    data_testing$exp3 = data_testing$X14+data_testing$X20-data_testing$X15
    data_testing$exp4 = data_testing$X15+data_testing$X21-data_testing$X16
    data_testing$exp5 = data_testing$X16+data_testing$X22-data_testing$X17
    data_testing$avgExp = rowMeans(data_testing[,c('exp1','exp2','exp3','exp4','exp5')])
    data_testing$propExp = data_testing$avgExp/data_testing$X1
    data_testing$debtScore = rowSums(data_testing[,c('X6','X7','X8','X9','X10')] * data_testing[,c('exp1','exp2','exp3','exp4','exp5')])
    data_testing$propDebt = data_testing$debtScore/data_testing$X1
    
    
    for (name in setdiff(names(data_testing),contvar)) {
        data_testing[,name] = (data_testing[,name]-meanSd[,name][1])/meanSd[,name][2]
    }
    
    predict <- predict(logreg, data_testing, type = 'response')
    confusion = table(data_testing$Y, predict > optCo)
    return (confusion)
    
}







