file.names <- dir("~/Projekt sejm/Dane PDF", pattern =".pdf")
for (i in 2:9) {
  dir.create(paste0("~/Projekt sejm/Dane PDF/", i))
  file.names.pos <- file.names[(substr(file.names, 1, 1) == i)]
  file.names.pos <- file.names.pos[substr(file.names.pos, 2, 2) == "_"]
  len.names <- nchar(file.names.pos)
  file.names.pos2 <- sapply(1:length(file.names.pos), function(j) substr(file.names.pos[j], 3, len.names[j] - 4))
  sapply(1:length(file.names.pos), function (j)
    file.rename(from = paste0("~/Projekt sejm/Dane PDF/", file.names.pos[j]),
                 to = paste0("~/Projekt sejm/Dane PDF/", i, "/", file.names.pos2[j], ".pdf")))
}

for (i in 11:14) {
  dir.create(paste0("~/Projekt sejm/Dane PDF/", i))
  file.names.pos <- file.names[(substr(file.names, 1, 2) == i)]
  len.names <- nchar(file.names.pos)
  file.names.pos2 <- sapply(1:length(file.names.pos), function(j) substr(file.names.pos[j], 4, len.names[j] - 4))
  sapply(1:length(file.names.pos), function (j)
    file.rename(from = paste0("~/Projekt sejm/Dane PDF/", file.names.pos[j]),
                to = paste0("~/Projekt sejm/Dane PDF/", i, "/", file.names.pos2[j], ".pdf")))
}
