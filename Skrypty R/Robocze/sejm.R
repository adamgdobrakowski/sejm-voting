library(dplyr)

download.file(url = "http://orka.sejm.gov.pl/Glos8.nsf/nazwa/85_1/$file/glos_85_1.pdf",
              destfile = "~/Projekt sejm/Dane PDF/85_1.pdf")

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

for (i in 3:nrow(liczba_glosowan)) {
  for (glos in 1:liczba_glosowan$liczba_glosowan[i]) {
    pos <- liczba_glosowan$pos[i]
    readUrl(pos, glos)
  }
}

