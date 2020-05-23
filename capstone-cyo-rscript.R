# load or install the required libraries
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}
if (!require("httr")) {
  install.packages("httr")
  library(httr)
}
if (!require("knitr")) {
  install.packages("knitr")
  library(knitr)
}
if (!require("kableExtra")) {
  install.packages("kableExtra")
  library(kableExtra)
}
if (!require("caret")) {
  install.packages("caret")
  library(caret)
}
if (!require("randomForest")) {
  install.packages("randomForest")
  library(randomForest)
}
if (!require("gam")) {
  install.packages("gam")
  library(gam)
}

#load the dataset
raw_dat<- read_csv("https://github.com/syaqirafarouk/harvardx-capstone-cyo/raw/master/mushrooms_raw.csv")

#load the reference file associated with the dataset
GET("https://github.com/syaqirafarouk/harvardx-capstone-cyo/blob/master/capstone-cyo-attribute-information.xls?raw=true", write_disk(tf<- tempfile(fileext = ".xls")))
attributes <- read_excel(tf, sheet = 1)
rm("tf")

#make the table more presentable and fit into the report
kable(attributes, align = "c") %>%    
  column_spec(column = 1, bold = TRUE) %>%
  kable_styling(font_size = 4.5 )

#convert 'character' vectors into factors
dat<- as.data.frame(lapply(raw_dat[,1:ncol(raw_dat)],factor))

#visualise the data and modify according to our requirements as outlined in the report by merging categories or removing variables
dat %>% 
  ggplot(aes(cap.shape, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))

#merge categories by collapsing; rename and reorder the levels to look more presentable
dat$cap.shape<- dat$cap.shape %>%
  fct_collapse(b= c("b","c","k","s")) %>% 
  factor(labels = c("others","f","x")) %>%
  factor(levels = c("f","x","others"))

dat %>% 
  ggplot(aes(cap.surface, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))

dat %>% 
  ggplot(aes(cap.color, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$cap.color<- dat$cap.color %>% 
  fct_collapse(b= c("b","c","p","r","u")) %>% 
  factor(labels = c("others","e","g","n","w","y")) %>%
  factor(levels = c("e","g","n","w","y","others"))

dat %>% 
  ggplot(aes(bruises, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))

dat %>% 
  ggplot(aes(odor, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$odor<- dat$odor %>% 
  fct_collapse(a= c("a","c","l","m","p","s","y")) %>% 
  factor(labels = c("others","f","n")) %>%
  factor(levels = c("f","n","others"))

dat %>% 
  ggplot(aes(gill.attachment, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))

#remove variables suspected to have 'reporting errors'
dat<- dat %>% 
  select(-"gill.attachment")

dat %>% 
  ggplot(aes(gill.spacing, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666")) 

dat %>% 
  ggplot(aes(gill.size, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))

dat %>% 
  ggplot(aes(gill.color, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$gill.color<- dat$gill.color %>% 
  fct_collapse(e= c("e","k","o","r","u","y")) %>% 
  factor(labels = c("b","others","g","h","n","p","w")) %>%
  factor(levels = c("b","g","h","n","p","w","others"))

dat %>% 
  ggplot(aes(stalk.shape, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))

dat %>% 
  ggplot(aes(stalk.root, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
levels(dat$stalk.root)<- c("NA","b","c","e","r")
dat$stalk.root<- dat$stalk.root %>% 
  fct_collapse(c= c("c","r")) %>% 
  factor(labels = c("NA","b","others","e")) %>%
  factor(levels = c("b","e","NA","others"))

dat %>% 
  ggplot(aes(stalk.surface.above.ring, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$stalk.surface.above.ring<- dat$stalk.surface.above.ring %>% 
  fct_collapse(f= c("f","y")) %>% 
  factor(labels = c("others","k","s")) %>%
  factor(levels = c("k","s","others"))

dat %>% 
  ggplot(aes(stalk.surface.below.ring, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$stalk.surface.below.ring<- dat$stalk.surface.below.ring %>% 
  fct_collapse(f= c("f","y")) %>% 
  factor(labels = c("others","k","s")) %>%
  factor(levels = c("k","s","others"))

dat %>% 
  ggplot(aes(stalk.color.above.ring, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$stalk.color.above.ring<- dat$stalk.color.above.ring %>% 
  fct_collapse(b= c("b","c","e","g","n","o","y")) %>% 
  factor(labels = c("others","p","w")) %>%
  factor(levels = c("p","w","others"))

dat %>% 
  ggplot(aes(stalk.color.below.ring, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$stalk.color.below.ring<- dat$stalk.color.below.ring %>% 
  fct_collapse(b= c("b","c","e","g","n","o","y")) %>% 
  factor(labels = c("others","p","w")) %>%
  factor(levels = c("p","w","others"))

dat %>% 
  ggplot(aes(veil.type, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat<- dat %>% 
  select(-"veil.type")

dat %>% 
  ggplot(aes(veil.color, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$veil.color<- dat$veil.color %>% 
  fct_collapse(n= c("n","o","y")) %>% 
  factor(labels = c("others","w")) %>%
  factor(levels = c("w","others"))

dat %>% 
  ggplot(aes(ring.number, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))

dat %>% 
  ggplot(aes(ring.type, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$ring.type<- dat$ring.type %>% 
  fct_collapse(f= c("f","n")) %>% 
  factor(labels = c("e","others","l","p")) %>%
  factor(levels = c("e","l","p","others"))

dat %>% 
  ggplot(aes(spore.print.color, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$spore.print.color<- dat$spore.print.color %>% 
  fct_collapse(b= c("b","o","r","u","y")) %>% 
  factor(labels = c("others","h","k","n","w")) %>%
  factor(levels = c("h","k","n","w","others"))

dat %>% 
  ggplot(aes(population, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$population<- dat$population %>% 
  fct_collapse(a= c("a","c","n")) %>% 
  factor(labels = c("others","s","v","y")) %>%
  factor(levels = c("s","v","y","others"))

dat %>% 
  ggplot(aes(habitat, fill = class)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#33CCCC","#FF6666"))
dat$habitat<- dat$habitat %>% 
  fct_collapse(l= c("l","m","u","w")) %>% 
  factor(labels = c("d","g","others","p")) %>%
  factor(levels = c("d","g","p","others"))

#view simplified data
str(dat)


#create partitions for training: R version 3.5 and earlier, use set.seed(2)
set.seed(2, sample.kind = "Rounding")
test_index<- createDataPartition(dat$class, times = 1, p = 0.2, list = FALSE)
train_set<- dat[-test_index, ]
test_set<- dat[test_index, ]


#glm - logit regreession model: R version 3.5 and earlier, use set.seed(3)
set.seed(3, sample.kind = "Rounding")
fit_glm<- train(class ~ ., data = train_set, method = "glm", family = "binomial") #function to call the formula for fitting the model to our dataset
pred_glm<- predict(fit_glm, test_set)                #predict() function to predict using the fit
cm_glm<- confusionMatrix(pred_glm,test_set$class)    #can check for sensitivity,specificity,accuracy and which 'class' is used as 'Positive'

results<- tibble(model = "glm",                           #tabulate results
                 sensitivity = signif(cm_glm$byClass[1]),
                 specificity = signif(cm_glm$byClass[2]),
                 accuracy = signif(cm_glm$byClass[11]),
                 computation_time = "< 30 seconds")

kable(results, align = 'c') %>%
  kable_styling(full_width = FALSE)

                     
#knn - k-nearest neighbor model - may take awhile to run: R version 3.5 and earlier, use set.seed(5)
set.seed(5, sample.kind = "Rounding")
fit_knn<- train(class ~ ., data = train_set, method = "knn",    #use argument 'tuneGrid' to test different values of 'k', the tuning parameter
                tuneGrid = data.frame(k = seq(1,115,19)))
pred_knn<- predict(fit_knn, test_set)
cm_knn<- confusionMatrix(pred_knn,test_set$class)

fit_knn

plot(fit_knn) #plot the number of neighbors against the accuracy

results<- rbind(results,c("knn",
                          signif(cm_knn$byClass[1]),
                          signif(cm_knn$byClass[2]),
                          signif(cm_knn$byClass[11]),
                          "< 4 minutes with tuning")) 

kable(results, align = 'c') %>%
  kable_styling(full_width = FALSE)


#rforest - random forest - may take awhile to run: R version 3.5 and earlier, use set.seed(10)
set.seed(10, sample.kind = "Rounding")
fit_rf<- train(class ~ ., data = train_set, method = "rf",     #use argument 'tuneGrid' to test different values of 'mtry', the tuning parameter
               tuneGrid = data.frame(mtry = seq(2,20,4)),      #argument ntree=200 allows the model get more accurate results for 'variable importance'
               ntree = 200)
fit_rf

pred_rf<- predict(fit_rf, test_set)
cm_rf<- confusionMatrix(pred_rf,test_set$class)

results<- rbind(results,c("rf",
                          signif(cm_rf$byClass[1]),
                          signif(cm_rf$byClass[2]),
                          signif(cm_rf$byClass[11]),
                          "< 10 minutes with tuning"))

kable(results, align = 'c') %>%
  kable_styling(full_width = FALSE)

var_imp<- varImp(fit_rf)  #function to inspect the 'variable importance'
plot(var_imp, top = 20)   #plots in a graph


#gamloess - loess - local estimated scatterplot smoothing: R version 3.5 and earlier, use set.seed(7)
set.seed(7, sample.kind = "Rounding")
fit_gam<- train(class ~ ., data = train_set, method = "gamLoess")
pred_gam<- predict(fit_gam, test_set)
cm_gam<- confusionMatrix(pred_gam,test_set$class)

results<- rbind(results,c("loess",
                          signif(cm_gam$byClass[1]),
                          signif(cm_gam$byClass[2]),
                          signif(cm_gam$byClass[11]),
                          "< 1 minute"))

kable(results, align = 'c') %>%
  kable_styling(full_width = FALSE)


#lda - linear discriminant analysis - used to compare a technically incorrect choice of method: R version 3.5 and earlier, use set.seed(4)
set.seed(4, sample.kind = "Rounding")
fit_lda<- train(class ~ ., data = train_set, method = "lda")
pred_lda<- predict(fit_lda, test_set)
cm_lda<- confusionMatrix(pred_lda,test_set$class)

kable(tibble(sensitivity = signif(cm_lda$byClass[1]),
             specificity = signif(cm_lda$byClass[2]),
             accuracy = signif(cm_lda$byClass[11])), align = 'c') %>%
  kable_styling(full_width = FALSE)

#final adjustments to the results table to make it more presentable and rank the models according to the best choice
results<- rbind(results[1,],results[3,],results[4,],results[2,])
kable(results %>% 
        mutate(rank = c("1","2","3","4")) %>%
        select(rank,model,sensitivity,specificity,accuracy,computation_time), align = 'c') %>%
  kable_styling(full_width = FALSE)

#load the modified 'attribute information' after simplification of data
GET("https://github.com/syaqirafarouk/harvardx-capstone-cyo/blob/master/capstone-cyo-attribute-information.xls?raw=true", write_disk(tf<- tempfile(fileext = ".xls")))
sim_attributes <- read_excel(tf, sheet = 2)
rm("tf")

kable(sim_attributes, align = "c") %>%
  column_spec(column = 1, bold = TRUE) %>%
  kable_styling(font_size = 9 )