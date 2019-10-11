library(dplyr)
library(pdftools)

# path to the main folder:
PATH <- "~/Projekt sejm/"

# read table created from https://www.sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=posglos&NrKadencji=8
posiedzenia <- read.table("~/Projekt sejm/liczba_głosowań.txt")
colnames(posiedzenia) <- c("pos", "dzien", "miesiac", "rok", "r", "liczba")
liczba_glosowan <- posiedzenia %>% group_by(pos) %>% summarise(liczba_glosowan = sum(liczba))

# quite tricky function for extracting the results of voting from PDFs
get_results <- function(pos, glos) {
  out <- tryCatch(
    {
      data <- pdf_text(paste0(PATH, "Dane PDF/", pos, "/", glos, ".pdf"))
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
        za[za == ""] <- "KRZYWONOS-STRYCHARSKA HENRYKA"
        za
      })
      names(res) <- c("za", "pr", "ws", "ng")
      # if (sum(length(unlist(res))) != 460) print(sum(length(unlist(res))))
      res
    },
    error=function(cond) {
      message(paste("PDF does not seem to exist:", pos, glos))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    },
    warning=function(cond) {
      message(paste("PDF caused a warning:", pos, glos))
      message("Here's the original warning message:")
      message(cond)
      return(NULL)
    },
    finally={
    }
  )    
  return(out)
}

# create list of votes
results <- lapply(nrow(liczba_glosowan), function(i) {
  pos <- liczba_glosowan$pos[i]
  cat("posiedzenie", pos, "\n")
  lapply(1:liczba_glosowan$liczba_glosowan[i], function(glos) {
    get_results(pos, glos)
  })
})

# names of members
poslowie <- sort(unique(unlist(results)))

# votes in table
table_results <- matrix(nrow = length(poslowie), ncol = sum(liczba_glosowan$liczba_glosowan))
dim(table_results)
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
      print(paste("omitted sitted", pos, "voting", glos, names(length(results[[pos]][[glos]]))))
    }
  }
}

# transform votinig table to number table
table_mat <- table_results
table_mat[table_mat == "za"] <- 1
table_mat[table_mat == "pr"] <- -1
table_mat[table_mat == "ws"] <- 0
table_mat[table_mat == "ng"] <- 0
table_mat[is.na(table_mat)] <- 0
table_mat <- t(apply(table_mat, 1, as.numeric))
dim(table_mat)
# remove the rubbish
table_mat <- table_mat[-1,-1]
table_mat <- table_mat[,colSums(abs(table_mat)) > 1]

# we have the most important thing: clean data; we can save it
save(table_mat, file = paste0(PATH, "/voting_table.rda"))

votes_sums <- colSums(table_mat)
hist(votes_sums)
hist(votes_sums[(votes_sums < 20) & (votes_sums > - 20)])
