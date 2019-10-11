library(dplyr)

# path to the main folder; you need about 350 MB free memory
PATH <- "~/Projekt sejm/"

# read table created from https://www.sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=posglos&NrKadencji=8
posiedzenia <- read.table("~/Projekt sejm/liczba_głosowań.txt")
colnames(posiedzenia) <- c("pos", "dzien", "miesiac", "rok", "r", "liczba")
liczba_glosowan <- posiedzenia %>% group_by(pos) %>% summarise(liczba_glosowan = sum(liczba))

# function for downloading results of voting from sejm.org.pl
readUrl <- function(pos, glos) {
  out <- tryCatch(
    {
      dir.create(file.path(PATH, "Dane PDF/",  pos), showWarnings = FALSE)
      download.file(url = paste0("http://orka.sejm.gov.pl/Glos8.nsf/nazwa/", pos ,"_", glos,
                                 "/$file/glos_", pos, "_", glos, ".pdf"),
                    destfile = paste0(PATH, "Dane PDF/", pos, "/", glos, ".pdf"))
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", pos, glos))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", pos, glos))
      message("Here's the original warning message:")
      message(cond)
      return(NULL)
    },
    finally={
      message(paste("Processed URL:", pos, glos))
    }
  )    
  return(out)
}

# download the votings; it can take some time...
for (i in 1:nrow(liczba_glosowan)) {
  for (glos in 1:liczba_glosowan$liczba_glosowan[i]) {
    pos <- liczba_glosowan$pos[i]
    readUrl(pos, glos)
  }
}
