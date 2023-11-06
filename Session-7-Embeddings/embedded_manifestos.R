library(readr)
library(uwot)
library(textdata)
library(coop)
library(quanteda)

N_DIM <- 300
glove <- embedding_glove6b(
  dimensions = N_DIM, 
  dir = here::here("Session-7-Embeddings/embeddings"), 
  return_path = TRUE, 
  manual_download = TRUE
)
word_matrix <- as.matrix(glove[,-1])
rownames(word_matrix) <- glove$token

df <- read_csv(here::here("Session-6-Distance-and-Dimensionality-Reduction/data/uk_manifestos.csv"))
corp <- corpus(df)
dfmat <- corp %>% tokens(remove_punc=TRUE) %>%
  tokens_remove(pattern=stopwords("en")) %>%
  dfm() 

common_features <- intersect(colnames(dfmat),rownames(word_matrix))
glove_dfmat <- dfmat[,common_features]
corpus_word_matrix <- word_matrix[common_features,]
doc_matrix <- glove_dfmat %*% corpus_word_matrix
df[1,1]
df[2,1]
print(cosine(doc_matrix[1,], doc_matrix[2,]))

# input
sentence <- "Everyone has the right to be happy."
# pre-processing
sentence <- stringr::str_to_lower(sentence)
dfmat <- sentence |> 
  tokens(remove_punct = TRUE) |> 
  tokens_remove(pattern = stopwords("en")) |> 
  dfm()

embeddings <- umap(as.matrix(doc_matrix), n_neighbors = 10, min_dist=0.01)
df$x <- embeddings[,1]
df$y <- embeddings[,2]

colordict <- c(
  "Labour"="red",
  "LibDems"="yellow",
  "Conservatives"="blue",
  "Greens"="green"
  )

library(plotly)
p <- ggplot(df, aes(x, y, colour=party, label=text)) + 
  geom_point(size=0.5, alpha = 0.5) + 
  scale_colour_manual(values=colordict) + 
  theme_bw() +
  coord_fixed()

ggplotly(p)
