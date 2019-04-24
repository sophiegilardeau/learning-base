#Installer et charger les packages
if(!require("XML")) {install.packages("XML");library("XML")} else {library("XML")}
if(!require("rJava")) {install.packages("rJava");library("rJava")} else {library("rJava")}
if(!require("tm")) {install.packages("tm");library("tm")} else {library("tm")}
if(!require("wordcloud")) {install.packages("wordcloud");library("wordcloud")} else {library("wordcloud")}
if(!require("qdap")) {install.packages("qdap");library("qdap")} else {library("qdap")}
if(!require("dendextend")) {install.packages("dendextend");library("dendextend")} else {library("dendextend")}
if(!require("ggplot2")) {install.packages("ggplot2");library("ggplot2")} else {library("ggplot2")}
if(!require("labeling")) {install.packages("labeling");library("labeling")} else {library("labeling")}
if(!require("dplyr")) {install.packages("dplyr");library("dplyr")} else {library("dplyr")}
if(!require("plotrix")) {install.packages("plotrix");library("plotrix")} else {library("plotrix")}
if(!require("FactoMineR")) {install.packages("FactoMineR");library("FactoMineR")} else {library("FactoMineR")}
if(!require("stringr")) {install.packages("stringr");library("stringr")} else {library("stringr")}
if(!require("shiny")) {install.packages("shiny");library("shiny")} else {library("shiny")}

############# Récupération du texte et transformation en corpus  #############

#Charger le texte
texte1<- htmlParse("Gilets_jaunes.html") 

#Création d'une fonction pour récupérer le texte
textes<-function(article){
  racine = xmlRoot(article)
  #le noeud body
  b2 = racine[["body"]]
  #décomposition en paragraphes
  #A la recherche de la balise <p>
  texte = xpathApply(b2,path="p",xmlValue)
  #transformer en vecteur
  vec.textes= unlist(texte)
  #Remplace les ponctuations (notamment les guillements) par un espace
  docs = str_replace_all(vec.textes, "[[:punct:]]", " ")
  #Remplace les caractères retour chariot et saut de ligne par un espace
  docs=gsub("\n"," ",docs)
  docs=gsub("\r"," ",docs)
  #faire savoir que chaque ligne du vecteur est un document
  docnew <- Corpus(VectorSource(docs))
  return(docnew)
}


doc1=textes(texte1)


##### Regroupement des textes par acte : chaque acte correspond à un document #####

# acte1
acte1=paste(content(doc1)[4:6],collapse = " ")
# acte2
acte2=paste(content(doc1)[8:10],collapse = " ")
# acte3
acte3=paste(content(doc1)[12:14],collapse = " ")
# acte4
acte4=paste(content(doc1)[16:18],collapse = " ")
# acte5
acte5=paste(content(doc1)[20:23],collapse = " ")
# acte6
acte6=paste(content(doc1)[25:27],collapse = " ")
# acte7
acte7=paste(content(doc1)[29:31],collapse = " ")
# acte8
acte8=paste(content(doc1)[33:34],collapse = " ")
# acte9
acte9=paste(content(doc1)[36:38],collapse = " ")
# acte10
acte10=paste(content(doc1)[40:42],collapse = " ")


all_actes=c(acte1,acte2,acte3,acte4,acte5,acte6,acte7,acte8,acte9,acte10)
#On obtient dix documents : un document par acte
all_actes=Corpus(VectorSource(all_actes))


############# Nettoyage du texte  #############

# Créer une fonction pour nettoyer le corpus 
Nettoyage <- function(corpus) {
  # Retire la ponctuation
  corpus <- tm_map(corpus,removePunctuation)
  # Passer en minuscule
  corpus <- tm_map(corpus,content_transformer(tolower))
  # Retirer les mots-outils français (stopwords)
  corpus <- tm_map(corpus, removeWords, stopwords("french")) 
  # Retirer les mots que l'on souhaite 
  corpus <- tm_map(corpus, removeWords, c("gilets jaunes","gilet","faits marquants","dont","entre")) 
  # Remplace les nombres par du texte
  corpus <- tm_map(corpus, removeNumbers) #ou remplacer nb par mot : replace_number(text)
  # Suppression des espaces en trop 
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

all_actes_clean=Nettoyage(all_actes)

############# Construction de la matrice documents-termes et de la matrice termes-documents #############

mdt = DocumentTermMatrix(all_actes_clean)
rownames(mdt) =c("Acte1","Acte2","Acte3","Acte4","Acte5","Acte6","Acte7","Acte8","Acte9","Acte10") 
mdt_m=as.matrix(mdt)
mtd=TermDocumentMatrix(all_actes_clean)
colnames(mtd) =c("Acte1","Acte2","Acte3","Acte4","Acte5","Acte6","Acte7","Acte8","Acte9","Acte10") 
mtd_m=as.matrix(mtd)

############# Evolution du nombre de manifestants et d'interpellations en fonction des actes #############

#récupération des chiffres par acte
all_actes_nombres=c()
#création d'une fonction pour récupérer les chiffres
nombre=function(data){
  gr <- gregexpr("[0-9\\.]+" , data )
  all_nombre=sapply(regmatches(data , gr) , as.numeric)
  return(all_nombre)
}
# acte1
acte_n_1_nb=content(doc1)[3]
acte_n_1=nombre(acte_n_1_nb)[-2,]
acte_n_1[1]=acte_n_1[1]*1000
all_actes_nombres=acte_n_1

# acte2
acte_n_2_nb=content(doc1)[7]
acte_n_2=nombre(acte_n_2_nb)[-2,]
acte_n_2[1]=acte_n_2[1]*1000
all_actes_nombres=cbind(all_actes_nombres,acte_n_2)
colnames(all_actes_nombres)[1]="acte_n_1"
# acte3
acte_n_3_nb=content(doc1)[11]
acte_n_3=nombre(acte_n_3_nb)[-2,]
acte_n_3[1]=acte_n_3[1]*1000
all_actes_nombres=cbind(all_actes_nombres,acte_n_3)
# acte4
acte_n_4_nb=content(doc1)[15]
acte_n_4=nombre(acte_n_4_nb)[c(-2,-4,-5),]
acte_n_4[1:2]=acte_n_4[1:2]*1000
acte_n_4[3]=1700
all_actes_nombres=cbind(all_actes_nombres,acte_n_4)
# acte5
acte_n_5_nb=content(doc1)[19]
acte_n_5=nombre(acte_n_5_nb)[-2,]
acte_n_5[1]=acte_n_5[1]*1000
all_actes_nombres=cbind(all_actes_nombres,acte_n_5)
# acte6
acte_n_6_nb=content(doc1)[24]
acte_n_6=nombre(acte_n_6_nb)[-2,]
acte_n_6[1]=33600
all_actes_nombres=cbind(all_actes_nombres,acte_n_6)
# acte7
acte_n_7_nb=content(doc1)[28]
acte_n_7=nombre(acte_n_7_nb)[-2,]
acte_n_7[1]=acte_n_7[1]*1000
all_actes_nombres=cbind(all_actes_nombres,acte_n_7)
# acte8
acte_n_8_nb=content(doc1)[32]
acte_n_8=nombre(acte_n_8_nb)[-2,]
acte_n_8[1]=acte_n_8[1]*1000
all_actes_nombres=cbind(all_actes_nombres,acte_n_8)
# acte9
acte_n_9_nb=content(doc1)[35]
acte_n_9=nombre(acte_n_9_nb)[c(-2,-3,-4,-5,-6),]
acte_n_9[1]=acte_n_9[1]*1000
all_actes_nombres=cbind(all_actes_nombres,acte_n_9)

# acte10
acte_n_10_nb=content(doc1)[39]
acte_n_10=acte_n_9
all_actes_nombres=cbind(all_actes_nombres,nombre(acte_n_10))

rownames(all_actes_nombres)=c("Manifestants","Interpellations","Gardes à vue")

#############  Réalisation de l'application R shiny  #############

ui <- fluidPage(
  titlePanel(h1("Evolution des actes des gilets jaunes",style = "color: #4d88ff")),
  
  tabsetPanel(
    tabPanel("Description",
             titlePanel(h3("Analyse descriptive",style = "color: #99bbff")),
             fluidRow(column(width=4,br(),plotOutput("frequence")),column(width=5,offset=-5,plotOutput("wordcloud",width="400px",height="400px")),column(width=3,plotOutput("association"))),
             fluidRow(column(width=4,br(),br(),sliderInput("top", "Nombre de mots ", min = 1,  max = 25, value = 10)),
                      column(width=5,sliderInput("freq", "Fréquence minimum:", min = 1,  max = 25, value = 3),
                                     sliderInput("max","Nombre maximum de mots:",min = 1,  max = 100,  value = 60)),
                      column(width=3,textInput("mots", "Choisir un mot :",value="mouvement"),br(),
                                  sliderInput("corlimite","Corrélation minimum:",min = 0.1,  max = 1,  value = 0.6)
                             ))
    ),

    

    tabPanel("Clustering",
              titlePanel(h3("Analyse de clusters",style = "color:#99bbff")),
              fluidRow(splitLayout(cellWidths = c("50%", "50%"),plotOutput("dendogramme",width="400px",height="300px"),plotOutput("acp",width="400px",height="250px"))),
              fluidRow(column(width=4,p("4 actes se distinguent : ",align = "center",style = "color:teal"),p("-acte4 ",align = "center",style = "color:teal"),p("-acte5 ",align = "center",style = "color:teal"),p("-acte9 ",align = "center",style = "color:teal"),p("-acte3 ",align = "center",style = "color:teal"),br(),p("Les actes 3 et 9 se ressemblent fortement. ",align = "center",style = "color:teal"),br(),
                     br(),br(),sliderInput("max_actes","Nombre maximum de mots:",min = 1,  max = 150,  value = 60)),
                     column(width=8,plotOutput("wordcloud_actes",width="550px",height="450px")))
    
     ),

    tabPanel("Comparaison",
              titlePanel(h3("Comparaison des actes",style = "color:#99bbff")),
              fluidRow(column(width=5,h3("Mots en commun", align = "center", style = "color:teal"),br(),br(),selectInput("acte_1", "Acte 1 à comparer:",c("Acte1","Acte2","Acte3","Acte4","Acte5","Acte6","Acte7","Acte8","Acte9","Acte10")),
                                      selectInput("acte_2", "Acte 2 à comparer:",c("Acte1","Acte2","Acte3","Acte4","Acte5","Acte6","Acte7","Acte8","Acte9","Acte10"),selected="Acte2")),
                        column(width=7,verticalLayout(
                                 splitLayout(
                                             plotOutput( "Nb_manifestants",width="220px",height="250px"),
                                             plotOutput("Nb_interpellations",width="220px",height="250px"))))
                      ),
              fluidRow(column(width=6,br(),br(),plotOutput("pyramide")),
                        column(width = 6, br(),br(),textInput("choix_mots_actes", "Choisir un mot :",value="mouvement"),
                                      span(textOutput("motselect"),style="color:teal",align = "center"),
                                      plotOutput("mots_actes",width="500px",height="300px")))
                      
    )

  )
)


server <- function(input, output) {
  
  
  ##### Bar plot des fréquences des mots  #####
   output$frequence <- renderPlot({
     frequences_termes <- freq_terms(content(all_actes_clean), top = input$top, at.least = 3)
     df_frequences_termes=as.data.frame(frequences_termes)
     ggplot(df_frequences_termes, aes(x=reorder(df_frequences_termes$WORD,df_frequences_termes$FREQ), y=df_frequences_termes$FREQ)) +
       geom_bar(stat='identity',color="bisque2", fill="bisque2") + 
       coord_flip() +
       ggtitle("Les mots les plus utilisés")+ylab("")+xlab("") +
       theme(plot.title = element_text(hjust=5,color="cyan4", size=16, face="bold.italic"),
             panel.background=element_rect(fill="transparent",colour=NA),
             axis.text.x = element_text(face="bold",color="#993333", size=11),
             axis.text.y = element_text(face="bold", color="salmon2", size=12),
             axis.ticks = element_blank())
   })
   
   output$wordcloud <- renderPlot({
     #Wordcloud 
     frequences_doc=colSums(mdt_m) #fréquence des mots calculée sur la matrice document terme
     pal <- brewer.pal(4, "Oranges")
     
     #wordcloud(names(frequences_doc),frequences_doc,min.freq=input$freq, max.words = input$max, colors = c("grey80","darkgoldenrod1","tomato")) # Affiche le wordcloud
     wordcloud(names(frequences_doc),frequences_doc,min.freq=input$freq, max.words = input$max, colors =pal)
     
     #title("Nuage de points",cex.main = 2,   font.main= 4, col.main= "blue")
     }) 
   
   ##### Bar plot des mots les plus corrélés avec un mot choisi #####
   output$association <- renderPlot({
     associations_doc=findAssocs(mdt,input$mots,corlimit=input$corlimite)# Regarde les mots les plus corrélés au mot choisi.
     associations_df_doc=list_vect2df(associations_doc,col2="word",col3="score") #Convertir en data frame avec noms des mots en colonne 2 et score en colonne 3
     ggplot(associations_df_doc, aes(score, word))+geom_point(shape=18,size =2.5,color="bisque3")+
       ggtitle("Associations de mots")+ylab("")+xlab("Score") +
       theme_minimal()+
       theme(plot.title = element_text( hjust=1.5,color="cyan4", size=16, face="bold.italic"),
             axis.title.x = element_text(color="#993333", size=12, face="bold"),
             axis.text.x = element_text(face="bold",color="salmon2", size=11)) 
   }) 
   
   ##### Dendogramme avec les distances entre les actes  #####
   output$dendogramme <- renderPlot({
     #matrice de distance
     mdt_dist=dist(mdt_m,method="euclidian") 
     hc1=hclust(mdt_dist,method="ward.D2")
     plot(hc1,main="",xlab="",ylab="",sub="",col= "salmon2")
     axis(side=2,at=9:15,col.axis="#993333",col="#993333")
   }) 
   
   ##### ACP sur les distances #####
   output$acp <- renderPlot({
     ##multidimensional scaling
     mds = cmdscale(mdt_dist,eig=T,k=2)
     #proportion de variance expliquee pour chaque axe 
     var_comp1=round(mds$eig[1]/sum(mds$eig) * 100,2)
     var_comp2=round(mds$eig[2]/sum(mds$eig) * 100,2)
     
     mds_df=as.data.frame(cbind(mds$points[,1],mds$points[,2]))
     colplot=c(1,1,1,2,3,1,1,1,4,1) # pour realiser 4 clusters 
     mds_df=cbind(mds_df,colplot)
     nomactes=c("Acte1","Acte2","Acte3","Acte4","Acte5","Acte6","Acte7","Acte8","Acte9","Acte10")
     ggplot(mds_df, aes(x=V1, y=V2))+
       geom_hline(yintercept=0,color = "grey") +
       geom_vline(xintercept=0,color = "grey") +ylab(paste("Composante 2 (",var_comp2,"%)"))+xlab(paste("Composante 1 (",var_comp1,"%)"))+
       scale_x_continuous(breaks=seq(-6, 12, 2),limits=c(-6,12))+
       scale_y_continuous(breaks=seq(-8, 8, 2),limits=c(-8,8))+
       geom_text(aes(label=nomactes,colour = factor(colplot)),hjust=0, vjust=0)+
       theme_classic()+ ggtitle("ACP")+
       theme(plot.title = element_text(hjust=0.5,color="cyan4", size=14, face="bold.italic"),legend.position='none')
     
   }) 
   
  
   
   
    
   
   
   ##### Wordcloud pour comparer les différents actes (documents)  #####
   output$wordcloud_actes<- renderPlot({
  comparison.cloud(mtd_m,scale=c(3,1),max.words = input$max_actes,match.colors=TRUE,title.size=2)
     
   }) 
   
   ##### Pyramide pour comparer deux actes  #####
   output$pyramide <- renderPlot({
     #Matrice composée des mots communs aux deux documents (avec leur fréquence pour chaque document)
     mots_communs=c()
     for (i in 1 : nrow(mtd_m)){
       if (mtd_m[i,input$acte_1] != 0 & mtd_m[i,input$acte_2] != 0) {
         somme=mtd_m[i,input$acte_1]+mtd_m[i,input$acte_2]
         if (somme >1) {
           mot=mtd_m[i,c(input$acte_1,input$acte_2)]
           mot=t(as.matrix(mot))
           rownames(mot)=rownames(mtd_m)[i]
           mots_communs=rbind(mots_communs,mot)
         }
       }
     }
     
     #Calcul de la différence des fréquences des mots entre les deux documents
     #Tri des mots communs par ordre croissant de ces différences
     diff_freq_mot=abs(mots_communs[,input$acte_1]-mots_communs[,input$acte_2])
     all_freq_mots=cbind(mots_communs,diff_freq_mot)
     mots_communs_trie=all_freq_mots[order(diff_freq_mot),]
     
     #Plot de la pyramide 
     pyramid.plot(mots_communs_trie[,input$acte_1], mots_communs_trie[,input$acte_2], labels = rownames(mots_communs_trie), 
                  top.labels = c(input$acte_1, "Mots", input$acte_2), main = "", unit = NULL)
   }) 
   
   #Réalisation du graphique : nombre de manifestants par acte
   output$Nb_manifestants<- renderPlot({
     manifestants_df=as.data.frame(all_actes_nombres["Manifestants",])
     colnames(manifestants_df)="frequence"
     manifestants_df=cbind(manifestants_df,actes)
     ggplot(manifestants_df,aes(x=actes,y=frequence))+geom_point(shape=16,size=5,col="bisque2")+geom_line(col="bisque2")+
       scale_x_continuous(breaks=seq(1, 10, 1),limits=c(1,10)) +
       scale_y_continuous(breaks=seq(0, 300000, 50000),limits=c(0,300000))+
       xlab("Actes")+ylab("")+ggtitle("Nombres de manifestants")+
       theme(plot.title = element_text( hjust=1,color="cyan4", size=14, face="bold.italic"),
             panel.background=element_rect(fill="transparent",colour=NA),
             axis.text.x = element_text(face="bold",color="#993333", size=11),
             axis.text.y = element_text(face="bold", color="salmon2", size=12),
             axis.title.x = element_text(color="#993333", size=12, face="bold"),
             axis.ticks = element_blank())
   }) 
   
   #Réalisation du graphique : nombre d'interpellations par acte 
   output$Nb_interpellations<- renderPlot({
     interpellations_df=as.data.frame(all_actes_nombres["Interpellations",])
     colnames(interpellations_df)="frequence"
     interpellations_df=cbind(interpellations_df,actes)
     ggplot(interpellations_df,aes(x=actes,y=frequence))+geom_point(shape=16,size=5,col="bisque2")+geom_line(col="bisque2")+
       scale_x_continuous(breaks=seq(1, 10, 1),limits=c(1,10)) +
       scale_y_continuous(breaks=seq(0, 2000,500),limits=c(0,2000))+
       xlab("Actes")+ylab("")+ggtitle("Nombres d'interpellations")+
       theme(plot.title = element_text( hjust=1,color="cyan4", size=14, face="bold.italic"),
             panel.background=element_rect(fill="transparent",colour=NA),
             axis.text.x = element_text(face="bold",color="#993333", size=11),
             axis.text.y = element_text(face="bold", color="salmon2", size=12),
             axis.title.x = element_text(color="#993333", size=12, face="bold"),
             axis.ticks = element_blank())
     
   }) 
   
   ##### Nombre d'apparitions du mot choisi en fonction des actes #####
   output$mots_actes<- renderPlot({
     freq=mtd_m[input$choix_mots_actes,which(mtd_m[input$choix_mots_actes,]!=0)]
     barplot(freq,names.arg =names(freq),col="bisque2",ylim=c(0,8))
     axis(side=2,col="violet", col.axis = "dark violet", lwd = 2)
     title("",cex.main = 1.2,font.main= 2, col.main= "blue")
   }) 
   
   output$motselect<-renderText({

     paste("Fréquence du mot ",input$choix_mots_actes," par acte")
     
   })
   
}

# Lancer l' application 
shinyApp(ui = ui, server = server)

