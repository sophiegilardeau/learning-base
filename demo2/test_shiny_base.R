#Charger les packages
library("XML")
library("tm")
library("wordcloud")
library("qdap")
library("dendextend")
library("ggplot2")
library("labeling")
library("dplyr")
library("plotrix")
library("FactoMineR")
library("stringr")
library("shiny")



############# Récupération des textes et transformation en corpus  #############

#Charger les textes
texte1<- htmlParse("Macron3.html") 
texte2<- htmlParse("Macron4.html") 

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
  docs = str_replace_all(vec.textes, "[[:punct:]]", "")
  #Remplace les caractères retour chariot et saut de ligne par un espace
  docs=gsub("\n"," ",docs)
  docs=gsub("\r"," ",docs)
  #faire savoir que chaque ligne du vecteur est un document
  docnew <- Corpus(VectorSource(docs))
  return(docnew)
}



doc1=textes(texte1)
doc2=textes(texte2)



##### Regroupement des deux textes #####

# Regrouper tous les documents du texte 1 en un seul document 
all_doc1=paste(content(doc1),collapse = " ")
# Regrouper tous les documents du texte 2 en un seul document 
all_doc2=paste(content(doc2),collapse = " ")
# Regroupe les deux textes 
all_textes=c(all_doc1,all_doc2)
#On obtient deux documents : doc 1 et doc 2
all_textes<- Corpus(VectorSource(all_textes))


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
  corpus <- tm_map(corpus, removeWords, c("gilets jaunes","macron","emmanuel")) 
  # Retirer les nombres
  corpus <- tm_map(corpus, removeNumbers) #ou remplacer nb par mot : replace_number(text)
  # Suppression des espaces en trop 
  corpus <- tm_map(corpus, stripWhitespace)
  #stemming 
  #corpus <- tm_map(corpus, stemDocument, language = "french") #on cherche les radicaux des termes
  # Remplacer les abbreviations
  #corpus <-tm_map(corpus, replace_abbreviation)
  # Remplace les contractions
  #corpus <-tm_map(corpus, replace_contraction)
  #corpus<-replace_contraction(corpus)
  return(corpus)
}

doc1_clean=Nettoyage(doc1)
doc2_clean=Nettoyage(doc2)
all_textes_clean=Nettoyage(all_textes)


library(gridExtra)
############# Construction de la matrice documents-termes et de la matrice termes-documents #############
#doc1
mdt1 = DocumentTermMatrix(doc1_clean)
mdt1_m=as.matrix(mdt1)
mtd1=TermDocumentMatrix(doc1_clean)
mtd1_m=as.matrix(mtd1)
#doc2
mdt2 = DocumentTermMatrix(doc2_clean)
mdt2_m=as.matrix(mdt2)
mtd2=TermDocumentMatrix(doc2_clean)
mtd2_m=as.matrix(mtd2)
#doc1 et 2
mdtall = DocumentTermMatrix(all_textes_clean)
rownames(mdtall) =c("doc1","doc2") # renomme les lignes 
mdtall_m=as.matrix(mdtall)
mtdall=TermDocumentMatrix(all_textes_clean)
colnames(mtdall) <- c("doc1","doc2") # renomme les colonnes
mtdall_m=as.matrix(mtdall)

frequences_doc1=colSums(mdt1_m) 
#doc1
frequences_termes_doc1 <- freq_terms(content(doc1_clean),top = 10, at.least = 3,extend = FALSE)
plot(frequences_termes_doc1)
#doc2
frequences_termes_doc2 <- freq_terms(content(doc2_clean), top = 10, at.least = 3,extend = FALSE)
plot(frequences_termes_doc2)

# Define UI for application that plots features of movies 
ui <- fluidPage(
  titlePanel("Gilets jaunes"),
  # Sidebar layout with a input and output definitions 
  sidebarLayout(position="left",   
    sidebarPanel(
      
      sliderInput("top", "Top des mots ", min = 1,  max = 25, value = 10),
     # sliderInput("max","Nombre maximum de mots:",min = 1,  max = 100,  value = 30)
      
        sliderInput("freq", "Fréquence minimum:", min = 1,  max = 25, value = 1),
         sliderInput("max","Nombre maximum de mots:",min = 1,  max = 100,  value = 30)
  ),

  
    
    # Outputs
    mainPanel(
    #  tabsetPanel(
  # tabPanel("Plot1", plotOutput(outputId = "plot1")),
   # tabPanel("Plot2",plotOutput(outputId = "plot2"))
  #  )
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "plot1",width="300px",,height="300px"), plotOutput(outputId = "plot2",width="300px",,height="300px")),
     # column(12,plotOutput(outputId = "plot1",width="400px",,height="400px"))
    
      plotOutput(outputId = "plot3")
      #plotOutput(outputId = "plot3")
      )
    )
)
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$plot1 <- renderPlot({
    frequences_termes_doc1 <- freq_terms(content(doc1_clean),top = input$top, at.least = 3,extend = FALSE)
    plot(frequences_termes_doc1)
  })
  
  output$plot2 <- renderPlot({
    frequences_termes_doc2 <- freq_terms(content(doc2_clean), top = input$top, at.least = 3,extend = FALSE)
    plot(frequences_termes_doc2)
    #title( "doc2")
  })
  

   output$plot3 <- renderPlot({
    wordcloud(names(frequences_doc1),frequences_doc1,min.freq=input$freq, max.words = input$max, colors = c("grey80","darkgoldenrod1","tomato")) # Affiche le wordcloud
    
  })
  
 
  
  
  
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)