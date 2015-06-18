##### Application of the following classifiers using Pima-Diabetes dataset from the UCI:
## C4.5, Random Forest (RF), Adaboost, Ksvm, Logistic Regression (LR) and Discriminant Analysis (DA).

# install.packages("RWeka");
# install.packages("deldir");
# install.packages("randomForest");
# install.packages("ada");
# install.packages("kernlab");
# install.packages("MASS");
require(RWeka);
require(deldir);
require(randomForest);
require(stats);
require(ada);
require(kernlab);
require(MASS);

source("Function.r");


#header=False because in this data their is no names for the features in the begining
data<-read.csv("./data/pima-indians-diabetes.data.csv",sep=",",header=F);

# we can use also read.table and we can use in both read.table and read.csv with all this specification
#data<-read.table("/Users/dialaezzeddine/Desktop/Code_R/RevisionR/data/pima-indians-diabetes.data.csv", header=TRUE, sep=',', quote='"\'', dec='.', fill=FALSE, comment.char="#",  na.strings = "NA", nrows = -1, skip = 0, check.names = TRUE, strip.white = FALSE, blank.lines.skip = TRUE);


colnames(data)<-c("nbPregnant","concentGlucose","BloodPressure","SkinThikness","SerumInsulin","BodyMassIndex","PredegreeDfunct","age","class");

ndata<-nrow(data);
cdata<-ncol(data);

#x is the data without the class variable
# y is the class variable (if the class is 1,-1 we use as.integer of y ) the 9= cdata is the number of the class column in the Pima-Diabetes dataset

x<-as.matrix(data[1:ndata,1:(cdata-1)]);
y<-c(factor(data[1:ndata,cdata]));
#y<-as.integer(c(factor(data[1:ndata,cdata])));

dim<-ncol(x);
nbpts<-nrow(x);


####################################### Training set and test set
# training data and the validation data
# We will choose randomly the test and training set from the original data using the uniform law (runif) 
# We use the part where V10 = 1 as the test set and the other part where V10= 0 as training. 

RV<-RandomVector(nbpts,0.09);
data[,cdata+1]<-RV;  
       
# training data
# Eapp is training set without class and Capp is its class 
Eapp<-x[data[,cdata+1]==0,];
Capp<-y[data[,cdata+1]==0];

# test data
# Etest is test set without class and Ctest is its class 
Etest<-x[data[,cdata+1]==1,];
Ctest<-y[data[,cdata+1]==1];




###################### Classifiers

## Using 1 training set and 1 test set.
# Normally, we need to repeat this verification many time using many different sample from the original data to prevent Overfitting. It's the case of applying Cross-validation.

i<-0;
result<-numeric();

##### C4.5
# to use J48 we need the training and validation test to be data frames with the same formula which mean the same names and order of variables. the classes need to be 0 or 1 
Capp1<- (Capp-1);
Ctest1<-(Ctest-1);
Eapp1<-ensembleToDataFrameApprentissage(Eapp,factor(Capp1));
Etest1<-ensembleToDataFrameTest(Etest);
# formula used by c4.5
Form<-dataFrameFormula(Eapp1);

### classifier c4.5
i<-i+1;
c4.5<-J48(formula=Form, data=Eapp1,control = Weka_control(), options = NULL);
res<-predict(c4.5,Etest1,predict.all=TRUE);
result[i] <- PropErreur(res,Ctest1);



###classifier Random Forest
i<-i+1;
randF<-randomForest(Eapp,factor(Capp));
res1<-predict(randF,Etest,type="response");
result[i]<- PropErreur(res1,Ctest);



####AdaBoost
Eapp2<-ensembleToDataFrameApprentissage(Eapp,Capp);

###  classifier Adaboost
i<-i+1;
adab <- AdaBoostM1(formula=Form, data=Eapp1,control = Weka_control(), options = NULL);
res2  <- predict(adab, Etest1,predict.all=TRUE);
result[i] <- PropErreur(res2,Ctest1);

# ###  2nd method to calculate the classifier Adaboost
# i<-i+1;
# adab <- ada(classe~., data=Eapp2);
# res2  <- predict(adab, Etest1, type = "vector");
# result[i] <- PropErreur(res2,Ctest);


####classifier KSVM
i<-i+1;
svm<- ksvm(classe~., data=Eapp2,type="C-svc");
res3  <- predict(svm,Etest1 ,type="response");
result[i] <- PropErreur(res3,Ctest);


#### classifier Logistic Regression
i<-i+1;
reglog<-Logistic(formula=Form, data=Eapp1,control = Weka_control(), options = NULL);
res4<-predict(reglog,Etest1);
result[i]<- PropErreur(res4,Ctest1);

###classifier Discriminante Analysis
i<-i+1;
AD<-lda(Eapp,Capp1);
res5<-predict(AD,Etest)$class;
result[i] <- PropErreur(res5,Ctest1);


# matrices des resultats
matresult<-as.matrix(result);
rownames(matresult)<-c("C4.5","RandomForest","AdaBoost","Ksvm","LR","DA");
colnames(matresult)<-c("Error Rate");

matresult;

