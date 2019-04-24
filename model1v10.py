####### Import libraries   #####

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import nltk
#nltk.download()
import random
from nltk.corpus import stopwords

from sklearn.preprocessing import MultiLabelBinarizer
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import MultinomialNB
from sklearn.linear_model import SGDClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.multiclass import OneVsRestClassifier
from sklearn.metrics import hamming_loss
from sklearn import metrics
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import LinearSVC
from sklearn.model_selection import cross_val_score

####### Preparation of the dataset  #####

##Read the dataset
#df = pd.read_csv('radley_2.csv',delimiter=',')
df = pd.read_csv('radley.csv',delimiter=',')
##Transform object type into a list type
df.dropna(inplace=True)  
df['h1_topics']=df['h1_topics'].apply(lambda x:eval(x))


## Create tf_idf for Y  ##
multilabel_binarizer = MultiLabelBinarizer()
multilabel_binarizer.fit(df.h1_topics)
classes=multilabel_binarizer.classes_
classesDF=pd.DataFrame(classes)
Y = multilabel_binarizer.transform(df.h1_topics) # rows: document; columns: terms (labels)

## Create tf_idf for X ##
#sw = stopwords.words("english")
#vectorizer = TfidfVectorizer(ngram_range=(1,2),stop_words=sw)
vectorizer = TfidfVectorizer(ngram_range=(1,2))
X_tfidf=vectorizer.fit_transform(df.cotent)

####### OnevsRest method with different classifier #####

## Function to compute the Hamming score ( label-based accuracy) for the multi-label case
def hamming_score(y_true, y_pred, normalize=True, sample_weight=None):  
    acc_list = []
    for i in range(y_true.shape[0]):
        set_true = set( np.where(y_true[i])[0] )
        set_pred = set( np.where(y_pred[i])[0] )
        tmp_a = None
        if len(set_true) == 0 and len(set_pred) == 0:
            tmp_a = 1
        else:
            tmp_a = len(set_true.intersection(set_pred))/float(len(set_true.union(set_pred)) )
        acc_list.append(tmp_a)
    return np.mean(acc_list)

## Function to print accuracy score, hamming loss and hamming score 
def print_score(y_pred, clf):
    print("Clf: ", clf.__class__.__name__)
    print("Accuracy score: {}".format(metrics.accuracy_score(y_pred, y_test_tfidf)))
    print("Hamming loss: {}".format(hamming_loss(y_pred, y_test_tfidf))) #hamming_loss: fraction of labels that are incorrectly predicted
    print("Hamming score: {}".format(hamming_score(y_pred, y_test_tfidf)))
    print("---") 


####### Split in train and test #####

x_train_tfidf, x_test_tfidf, y_train_tfidf, y_test_tfidf = train_test_split(X_tfidf, Y, test_size=0.3, random_state=9000)

#Divide the train data set in different size to compare the accuracy score

liste= np.arange(len(y_train_tfidf))
np.random.shuffle(liste)
ints=np.arange(1000, 4000, 1000)



for idx, val in enumerate(ints):
    print(val)


val_traindata=[]
accuracy_traindata=[]
for idx, val in enumerate(ints):
    size_sel=liste[0:val]
    y_train= y_train_tfidf[size_sel,:]
    x_train= x_train_tfidf[size_sel,:]
    svc=LinearSVC()
    finalmodel=OneVsRestClassifier(svc)
    finalmodel.fit(x_train, y_train)
    y_pred = finalmodel.predict(x_test_tfidf)
    accuracyscore=metrics.accuracy_score(y_pred, y_test_tfidf)
    val_traindata.append(val)
    accuracy_traindata.append(accuracyscore)

valdf=pd.DataFrame(val_traindata)
accdf=pd.DataFrame(accuracy_traindata)
result1=pd.concat([valdf,accdf],axis=1)
result1.columns = ['size','accuracy_score']
  
result1.to_excel("accuracy.xlsx")  
    
## Different classifiers 
#lr = LogisticRegression()
#mn = MultinomialNB()
svc=LinearSVC()


## Run classifiers and print the scores
#for classifier in [lr, mn,svc]:
  #  clf = OneVsRestClassifier(classifier)
  #  clf.fit(x_train_tfidf, y_train_tfidf)
  #  y_pred = clf.predict(x_test_tfidf)
  #  print_score(y_pred, classifier)


####### Final model #####
finalmodel=OneVsRestClassifier(svc)
finalmodel.fit(x_train_tfidf, y_train_tfidf)
y_pred = finalmodel.predict(x_test_tfidf)
print_score(y_pred, svc)

## Function to transfer Y_pred (numerical matrix) in labels 

#def Y_labels(y):  
  #  df_y_labels = pd.DataFrame()
  #  for i in range(y.shape[0]):
   #     nums_column=np.where(y[i,:]>0)[0]
#        labels=classesDF.iloc[nums_column,0]
#        labelsDF=pd.DataFrame(labels)
#        labelsDF_t=labelsDF.transpose()
#        df_y_labels=pd.concat([df_y_labels , labelsDF_t])
#    data = df_y_labels.replace( np.nan, '',regex=True)
#    data=data.set_index([ np.arange(y.shape[0])])
#    test2=data.values.tolist()
#    result=[]
#    for xx in test2:
#        tmp=",".join([x for x in xx if x!=""])
#        result.append(tmp)
#    return result
#
#
#y_pred_labels=Y_labels(y_pred)
#y_test_labels=Y_labels(y_test_tfidf)




## cross validation for final model 
   
svc=LinearSVC()
finalmodel=OneVsRestClassifier(svc)
scores = cross_val_score(finalmodel, X_tfidf, Y, cv=2)
print("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))



## Create tf_idf for X ##
sw = stopwords.words("english")
#vectorizer = TfidfVectorizer(ngram_range=(1,2),stop_words=sw)
# run ngram_range parameter , stopword is fixed (false)
Best_param_2="sw" 
Best_accuracy_param_2=0

for i in range(2):
    
    val_par1=np.arange(2, 5, 1)
    val_par2 = Best_param_2
   
    result_score=[]
    for idx,val in enumerate (val_par1):
        vectorizer = TfidfVectorizer(ngram_range=(1,val),stop_words=eval(val_par2))
        X_tfidf=vectorizer.fit_transform(df.cotent)
        svc=LinearSVC()
        finalmodel=OneVsRestClassifier(svc)
        scores = cross_val_score(finalmodel, X_tfidf, Y, cv=2)
        result_score.append(scores.mean())
    Best_accuracy_param_1=max(result_score)
    Best_param_1=val_par1[result_score.index(max(result_score))]
    print("Best accuracy for parameter 1 for epoch",i,"is : ", Best_accuracy_param_1)
    print("Best value for parameter 1 for epoch",i,"is : ", Best_param_1)
    
    if Best_accuracy_param_1==Best_accuracy_param_2:
        print('the end', 'accuracy is ', Best_accuracy_param_1)
        print('parameter1 is',Best_param_1 )
        print('parameter2 is',Best_param_2 )
        break
   
    
    val_par2 = ["None", "sw"]
    result_score_2=[]
    for name in val_par2:
        vectorizer = TfidfVectorizer(ngram_range=(1,Best_param_1),stop_words=eval(name))
        X_tfidf=vectorizer.fit_transform(df.cotent)
        svc=LinearSVC()
        finalmodel=OneVsRestClassifier(svc)
        scores = cross_val_score(finalmodel, X_tfidf, Y, cv=2)
        result_score_2.append(scores.mean())
    Best_accuracy_param_2=max(result_score_2)
    Best_param_2=val_par2[result_score_2.index(max(result_score_2))]  
    print("Best accuracy for parameter 2 for epoch",i,"is : ", Best_accuracy_param_2)
    print("Best value for parameter 2 for epoch",i,"is :", Best_param_2)

  






    
result_score=[]
val_par1=np.arange(2, 5, 1)
for idx,val in enumerate (val_par1):
    vectorizer = TfidfVectorizer(ngram_range=(1,val),stop_words=sw)
    X_tfidf=vectorizer.fit_transform(df.cotent)
    svc=LinearSVC()
    finalmodel=OneVsRestClassifier(svc)
    scores = cross_val_score(finalmodel, X_tfidf, Y, cv=2)
    result_score.append(scores.mean())
Best_accuracy_param_1=max(result_score)
Best_param_1=val_par1[result_score.index(max(result_score))]
print("Best accuracy for parameter 1 is : ", Best_accuracy_param_1)
print("Best value for parameter 1 is : ", Best_param_1)






#print the score and add epochs
#stopword =none or sw (list of stopwords) ; and in using the best parameter for ngram_range
sw = stopwords.words("english")
val_par2 = ["None", "sw"]
result_score_2=[]

for name in val_par2:
    vectorizer = TfidfVectorizer(ngram_range=(1,Best_param_1),stop_words=eval(name))
    X_tfidf=vectorizer.fit_transform(df.cotent)
    svc=LinearSVC()
    finalmodel=OneVsRestClassifier(svc)
    scores = cross_val_score(finalmodel, X_tfidf, Y, cv=2)
    result_score_2.append(scores.mean())
Best_accuracy_param_2=max(result_score_2)
Best_param_2=val_par2[result_score.index(max(result_score_2))]  
print("Best accuracy for parameter 2 is : ", Best_accuracy_param_2)
print("Best value for parameter 2 is : ", Best_param_2)




vectorizer = TfidfVectorizer(ngram_range=(1,2))
X_tfidf=vectorizer.fit_transform(df.cotent)

for idx,val in enumerate (val_par1):
    print(val)
