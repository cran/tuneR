lilyinput <- function(X, file = "Rsong.ly", 
    Dur = TRUE, grundton = "c", schlusselart = 1, takt = "4/4",
    endbar = TRUE,
    midi = TRUE, tempo = "2 = 60", 
    textheight = 220, linewidth = 150, indent = 0)
{
  # Notenzuweisung
  # 97 Einträge in Notentopf (a,,, bis a''''')
  if(Dur){
  notentopf <- 
    switch(grundton,
        d = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
        e = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "ais", "b"),
        f = c("c", "cis", "d", "es", "e", "f", "fis", "g", "as", "a", "bes", "b"),
        g = c("c", "cis", "d", "es", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
        a = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
        b = c("c", "des", "d", "es", "e", "f", "fis", "g", "as", "a", "bes", "b"),
        es = c("c", "des", "d", "es", "e", "f", "ges", "g", "as", "a", "bes", "b"),
        c("c", "cis", "d", "es", "e", "f", "fis", "g", "gis", "a", "bes", "b")
    )
  }
  else{
  notentopf <- 
    switch(grundton,
        h = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
        cis = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "ais", "b"),
        d = c("c", "cis", "d", "es", "e", "f", "fis", "g", "as", "a", "bes", "b"),
        e = c("c", "cis", "d", "es", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
        fis = c("c", "cis", "d", "dis", "e", "f", "fis", "g", "gis", "a", "bes", "b"),
        g = c("c", "des", "d", "es", "e", "f", "fis", "g", "as", "a", "bes", "b"),
        c = c("c", "des", "d", "es", "e", "f", "ges", "g", "as", "a", "bes", "b"),
        c("c", "cis", "d", "es", "e", "f", "fis", "g", "gis", "a", "bes", "b")
    )
  }  

  notentopf <- unlist(lapply(
    c(",,,", ",,", ",", "", "'", "''", "'''", "''''", "'''''"), 
        function(x) paste(notentopf, x, sep="")))[-c(1:9, 107:108)]
  # Initialisierung
  bindung <- toene <- character(length(X$noten)) 
  
  #Tonhöhe, -länge, -punktierung
  ton <- ifelse(is.na(X$noten), "r", notentopf[X$noten + 49])
  laenge <- ifelse(X$laenge %in% 2^(0:8), X$laenge, "")
  punkt <- ifelse(X$punkt, ".", "")
  # Abfangen von Beginn / Ende von Bindungen:
  if(sum(X$bindung) %% 2) 
    stop("Mehr Anfänge als Enden bei Bindebögen")
  bindung[which(X$bindung)] <- 
    rep(c("(", ")"), sum(X$bindung) %/% 2)
  # Zusammenführen der Notenzeichen:
  toene <- ifelse(bindung == ")", 
    paste(bindung, ton, laenge, punkt, sep = ""),
    paste(ton, laenge, punkt, bindung, sep = ""))

  # Notenschlüsselzuweisung:
  if(!(schlusselart %in% 1:4))
    stop(paste("Falsche Eingabe des Notenschlüssels!",
        "\nViolin - 1 , Bass - 2 , Alto - 3 , Tenor - 4\n"))
  notenschlussel <- switch(schlusselart, 
    "treble", "bass", "alto", "tenor")
  # Tonart
  art <- if(Dur) "\\major" else "\\minor"        

 # Grundton
  if(Dur){
    topf <- c("fis" , "h" , "e" , "a" , "d" , "g" , "c" , "f" , 
        "b" , "es" , "as" , "des" , "ges") 
    if(!(grundton %in% topf))
        stop(paste("Falsche Eingabe des Grundtons!\nDurtonarten:", 
            paste(topf, collapse = " "), "\n"))
  }
  else{
    topf <- c("cis" , "gis" , "dis" , "fis" , "h" , "e" , "a" , 
        "d" , "g" , "c" , "f" , "b" , "es")
    if(!(grundton %in% topf))
        stop(paste("Falsche Eingabe des Grundtons!\nMolltonarten:", 
            paste(topf, collapse = " "), "\n"))
  }
  if(grundton == "b") grundton <- "bes"
  else if(grundton == "h") grundton <- "b"

  # Lilypond Datei erzeugen:
  write(file = file,
    c("\\include \"paper16.ly\"", 
        "\\header{tagline = \"\"}",
        "\\score{", 
        "  \\notes{", 
        paste("    \\time", takt),
        paste("    \\key", grundton, art),
        paste("    \\clef", notenschlussel),
        paste("   ", toene),
    if(endbar) 
        "   \\bar \"|.\"",
        "  }",
        "  \\paper{",
        "    \\paperSixteen",
        paste("    textheight = ", textheight, ".\\mm", sep = ""),
        paste("    linewidth = ", linewidth, ".\\mm", sep = ""),
        paste("    indent = ", indent, ".\\mm", sep = ""),
        "  }",
    if(midi){ 
      c("  \\midi{", 
        paste("    \\tempo", tempo),
        "  }")
    },
        "}"))
}

## Test-Code:
#X <- data.frame(noten = c(3, 0, 1, 3, -4, -2, 0, 1, 3, 1, 0, -2, NA),  
#    laenge = c(2, 4, 8, 2, 2, 8, 8, 8, 8, 4, 4, 1, 1), 
#    punkt = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
#    bindung = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
#
#lilyinput(X, file = "c:/test.ly")
