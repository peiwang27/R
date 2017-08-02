wants <- c("knitr","rmarkdown","devtools","epicalc","mosaic",
           "showtext","pander","PerformanceAnalytics","fitdistrplus",
           "CircStats","MASS","mixtools","boot","TrialSize","vcd",
           "ggplot2","pspearman","gvlma","car","lmtest","leaps",
           "plyr","bootstrap","elrm","rms","Deducer","bestglm",
           "survival","robust","mlogit","nnet","VGAM","ordinal",
           "Sample.Size","phia","mvtnorm","pscl","mosaic","XML",
           "pipeR","Rcmdr","rgl","HH","DescTools","multcomp",
           "effects","sandwich","qcc","devtools","ggmap","mosaic",
           "Hmisc","pastecs","psych","doBy","gmodels","CircStats",
           "expm","koRpus","ldbounds","ggm","coin","DescTools",
           "Rcurl","maptools","rgdal","animation","leaflet",
           "polycor","pROC","rms","pgirmess","rateratio.test",
           "exactci","Deducer","VGAM","ordinal","AER","gplots",
           "AICcmodavg")
wants <- read.table('installed.txt')
has <- wants[,1] %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has, 1])
