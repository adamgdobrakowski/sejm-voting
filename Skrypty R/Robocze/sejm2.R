library(dplyr)
library(pdftools)

posiedzenia <- read.table("~/Projekt sejm/liczba_głosowań.txt")
colnames(posiedzenia) <- c("pos", "dzien", "miesiac", "rok", "r", "liczba")
liczba_glosowan <- posiedzenia %>% group_by(pos) %>% summarise(liczba_glosowan = sum(liczba))

readUrl <- function(pos, glos) {
  out <- tryCatch(
    {
      
      download.file(url = paste0("http://orka.sejm.gov.pl/Glos8.nsf/nazwa/", pos ,"_", glos,
                                 "/$file/glos_", pos, "_", glos, ".pdf"),
                    destfile = paste0("~/Projekt sejm/Dane PDF/", pos, "_", glos, ".pdf"))
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", pos, glos))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", pos, glos))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      message(paste("Processed URL:", pos, glos))
      message("Some other message at the end")
    }
  )    
  return(out)
}

for (i in 15:nrow(liczba_glosowan)) {
  for (glos in 1:liczba_glosowan$liczba_glosowan[i]) {
    pos <- liczba_glosowan$pos[i]
    readUrl(pos, glos)
  }
}

get_results <- function(pos, glos) {
  out <- tryCatch(
    {
      data <- pdf_text(paste0("~/Projekt sejm/Dane PDF/", pos, "/", glos, ".pdf"))
      data2 <- strsplit(data, split = "\n")
      data2 <- strsplit(unlist(data2), "(?<=za)", perl=TRUE)
      data2 <- strsplit(unlist(data2), "(?<=pr.)", perl=TRUE)
      data2 <- strsplit(unlist(data2), "(?<=ng.)", perl=TRUE)
      data2 <- strsplit(unlist(data2), "(?<=ws.)", perl=TRUE)
      data2 <- unlist(data2)
      
      res <- lapply(c(" za", "pr.", "ws.", "ng."), function(g) {
        za <- data2[substr(data2, nchar(data2) - 2, nchar(data2)) == g]
        za <- as.character(sapply(za, function(z) substr(z, 1, nchar(z) - 3)))
        za <- za[za == toupper(za)]
        za <- trimws(za)
        #print(paste(g, length(za == "")))
        #if (sum(za == "")) print(paste(g, "puste:", sum(za == "")))
        za[za == ""] <- "KRZYWONOS-STRYCHARSKA HENRYKA"
        za
      })
      names(res) <- c("za", "pr", "ws", "ng")
      if (sum(length(unlist(res))) != 460) print(sum(length(unlist(res))))
      res
    },
    error=function(cond) {
      message(paste("PDF does not seem to exist:", pos, glos))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(paste("PDF caused a warning:", pos, glos))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      #message(paste("Processed PDF:", pos, glos))
      #message("Some other message at the end")
    }
  )    
  return(out)

# if (file.exists(paste0("~/Projekt sejm/Dane PDF/", pos, "/", glos, ".pdf"))) {
# data <- pdf_text(paste0("~/Projekt sejm/Dane PDF/", pos, "/", glos, ".pdf"))
# data2 <- strsplit(data, split = "\n")
# data2 <- strsplit(unlist(data2), "(?<=za)", perl=TRUE)
# data2 <- strsplit(unlist(data2), "(?<=pr.)", perl=TRUE)
# data2 <- strsplit(unlist(data2), "(?<=ng.)", perl=TRUE)
# data2 <- strsplit(unlist(data2), "(?<=ws.)", perl=TRUE)
# data2 <- unlist(data2)
# 
# res <- lapply(c(" za", "pr.", "ws.", "ng."), function(g) {
#   za <- data2[substr(data2, nchar(data2) - 2, nchar(data2)) == g]
#   za <- as.character(sapply(za, function(z) substr(z, 1, nchar(z) - 3)))
#   za <- za[za == toupper(za)]
#   za <- trimws(za)
#   #print(paste(g, length(za == "")))
#   #if (sum(za == "")) print(paste(g, "puste:", sum(za == "")))
#   za[za == ""] <- "KRZYWONOS-STRYCHARSKA HENRYKA"
#   za
# })
# names(res) <- c("za", "pr", "ws", "ng")
# if (sum(length(unlist(res))) != 460) print(sum(length(unlist(res))))
# res
# } else {
#   print(paste("No such data:", pos, glos))
#   return(NULL)
# }
}

results <- lapply(1:nrow(liczba_glosowan), function(i) {
  pos <- liczba_glosowan$pos[i]
  print(pos)
  lapply(1:liczba_glosowan$liczba_glosowan[i], function(glos) {
    get_results(pos, glos)
  })
})

poslowie <- sort(unique(unlist(results)))

table_results <- matrix(nrow = length(poslowie), ncol = sum(liczba_glosowan$liczba_glosowan))
glosy <- c("za", "pr", "ws", "ng")
i <- 1
rownames(table_results) <- poslowie
for (pos in 1:length(results)) {
  for (glos in 1:length(results[[pos]])) {
    if (length(results[[pos]][[glos]]) == 4) {
      for (za in 1:4) {
        lista <- results[[pos]][[glos]][[za]]
        if (length(lista) > 0) {
          table_results[lista,i] <- glosy[za]
        }
      }
      i <- i + 1
    }
    else {
      print(paste(pos, glos, names(length(results[[pos]][[glos]]))))
    }
  }
}

table_mat <- table_results
table_mat[table_mat == "za"] <- 1
table_mat[table_mat == "pr"] <- -1
table_mat[table_mat == "ws"] <- 0
table_mat[table_mat == "ng"] <- 0
table_mat[is.na(table_mat)] <- 0
table_mat <- t(apply(table_mat, 1, as.numeric))
dim(table_mat)
#delete empty columns
table_mat <- table_mat[,colSums(abs(table_mat)) != 0]
table_mat <- table_mat[-1,]

a <- table_mat[250:300, 2000:2020]
colSums(table_mat)[7800:7985]
max(colSums(table_mat))

dim(table_mat)

## PARTIE

partie <- data.frame(posel = poslowie, partia = rep("", 508), stringsAsFactors = FALSE)
rownames(partie) <- partie$posel
pis <- read.table("~/Projekt sejm/Partie/pis", sep = ",")
partie[as.character(unlist(pis)),2] <- "pis"

pis <- read.table("~/Projekt sejm/Partie/po", sep = ",")
partie[as.character(unlist(pis)),2] <- "po"

pis <- read.table("~/Projekt sejm/Partie/kukiz", sep = ",")
partie[as.character(unlist(pis)),2] <- "kukiz"

pis <- read.table("~/Projekt sejm/Partie/konf", sep = ",")
partie[as.character(unlist(pis)),2] <- "konfederacja"

pis <- read.table("~/Projekt sejm/Partie/wis", sep = ",")
partie[as.character(unlist(pis)),2] <- "wis"

pis <- read.table("~/Projekt sejm/Partie/teraz", sep = ",")
partie[as.character(unlist(pis)),2] <- "teraz"

pis <- read.table("~/Projekt sejm/Partie/niez", sep = ",")
partie[as.character(unlist(pis)),2] <- "niezrzeszeni"

pis <- read.table("~/Projekt sejm/Partie/psl", sep = ",")
partie[as.character(unlist(pis)),2] <- "psl"

partie <- partie[-1,]
partie <- partie[1:507,]

partie <- partie[partie$partia != "",]
table_mat <- table_mat[partie$posel,]
dim(table_mat)

?prcomp
pca <- prcomp(table_mat)
pca$x
pca$sdev
plot(pca$x[,1:2])

## WIZUALIZACJA

library(ggplot2)
library(ggrepel)

etykiety <- partie[rownames(pca$x),]$partia
plot_posel <- partie[rownames(pca$x),]$posel
## Kolorowanie kodem ICD-10 (pierwszą literą)
plot_data <- cbind(as.data.frame(pca$x[plot_posel, 1:2]), partia = as.factor(etykiety), posel = plot_posel)

label_data <- subset(plot_data, posel %in%
                       c("KACZYŃSKI JAROSŁAW", "BASZKO MIECZYSŁAW KAZIMIERZ"))

ggplot(plot_data, aes(x = -plot_data[,1], y = plot_data[,2], colour = partia)) + geom_point(size=4) +
  xlab(element_blank()) + ylab(element_blank()) + geom_label_repel(data = plot_data,
                                                                   aes(x = -plot_data[,1], y = plot_data[,2]), label =
                                                                     plot_data$posel, size = 2)

d <- as.matrix(dist(pca$x[,1:2]))
d.pis <- d[partie$partia == "pis", partie$partia == "po"]

party_representation <- pca$x[,1:2]
party_representation <- data.frame(cbind(party_representation, party = partie$partia))
party_representation$PC1 <- as.numeric(levels(party_representation$PC1)[party_representation$PC1])
party_representation$PC2 <- as.numeric(levels(party_representation$PC2)[party_representation$PC2])

party_mean <- party_representation %>% group_by(party) %>% summarise(PC1 = mean(PC1), PC2 = mean(PC2))
party_mean <- data.frame(party_mean)
ggplot(party_mean, aes(x = -party_mean[,2], y = party_mean[,3], colour = party_mean$party)) + geom_point(size=5)

library(Rtsne)
tsne <- Rtsne(table_mat, dims = 2, initial_dims = 7985, check_duplicates=FALSE, verbose=TRUE, max_iter = 300)
plot_data <- cbind(as.data.frame(tsne$Y), partia = as.factor(etykiety), posel = plot_posel)
ggplot(plot_data, aes(plot_data[,1], plot_data[,2], colour = partie$partia)) + geom_point(size=2)

# Odległości między posłami
D <- apply(table_mat, 1, function(v) {
  apply(table_mat, 1, function(u) {
    sum(v == u) / ncol(table_mat)
  })
})
a <- D["MOSKAL KAZIMIERZ",]

# wewnątrz pis
D_pis <- D[partie$partia == "pis", partie$partia == "pis"]
sort(apply(D_pis, 1, mean))

trzon_pis <- names(a[a > 0.95])
min(D[trzon_pis,trzon_pis])

# pis - po
D_pis_po <- D[partie$partia == "pis", partie$partia == "po"]
sort(apply(D_pis_po, 1, mean))

sort(apply(D_pis_po, 2, mean))

plot_data1 <- plot_data[partie$partia == "pis",]
label_data <- subset(plot_data1, posel %in%
                       trzon_pis)
ggplot(plot_data1, aes(x = -plot_data1[,1], y = plot_data1[,2])) + geom_point(size=4) +
  xlab(element_blank()) + ylab(element_blank()) + geom_label_repel(data = label_data,
                                                                   aes(x = -label_data[,1], y = label_data[,2]), label =
                                                                     label_data$posel, size = 3)
