# Das Programm matching_algorithmus wurde konzipiert, um die bestmoeglichen Paarungen zwischen Tuebinger Buddys und nach Tuebingen
# kommenden Austauschstudenten zu finden.
# Dabei wird mithilfe der Funktion punkte_algorithmus ein Wert fuer jede Paarung aus Tuebinger und Incoming berechnet, der wiedergibt,
# wie gut dieses Paar zusammenpasst. Dabei wird u.a. Wert auf ein aehnliches Studienfach und aehnliche Hobbies gelegt.
# Anschließend werden Tuebinger und Incomings dann basierend auf diesen Werten mittels des Gale-Shapley Algorithmus' gematched.
# Dieser wird vom R Paket "matchingR", Copyright Jan Tilly und Nick Janetos, zur Verfuegung gestellt.

library(matchingR)
library(stringi)
library(xlsx)

Spalte_Alter_Tuebingen = c(4)
Spalte_Geschlecht_Tuebingen = c(5)
Spalte_Studienfach_Tuebingen = c(6)
Spalte_Studienfach2_Tuebingen = c(7)
Spalte_Studienabschluss_Tuebingen = c(8)
Spalte_Land_Tuebingen = c(9)
Spalte_Uni_Tuebingen = c(10)
Spalte_Uni_Tuebingen_Freitext = c(11)
Spalte_Hobby1_Tuebingen = c(12)
Spalte_Hobby2_Tuebingen = c(13)
Spalte_Hobby3_Tuebingen = c(14)
Spalte_Sprachen_Tuebingen = c(15)
Spalte_Datum_Tuebingen = c(17)

Spalte_Alter_Incoming = c(4)
Spalte_Geschlecht_Incoming = c(5)
Spalte_Studienfach_Incoming = c(6)
Spalte_Studienfach2_Incoming = c(7)
Spalte_Studienabschluss_Incoming = c(8)
Spalte_Land_Incoming = c(9)
Spalte_Uni_Incoming = c(10)
Spalte_Uni_Incoming_Freitext = c(11)
Spalte_comlang_Incoming = c(12)
Spalte_Hobby1_Incoming = c(13)
Spalte_Hobby2_Incoming = c(14)
Spalte_Hobby3_Incoming = c(15)
Spalte_Sprache_Incoming = c(16)
Spalte_Kurs_Incoming = c(17)
Spalte_Datum_Incoming = c(18)

# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION MATCHING_PROGRAMM
#-----------------------------------------------------------------------------------------------------------------------------------------

# Funktion matching_programm fuehrt matching auf Datei mit Buddy Daten aus
# Variable "input_incoming" gibt dabei den Dateipfad zu der Tabelle mit den Informationen der Austauschstudenten an,
# Variable "input_tuebingen" gibt den Dateipfad zu der Tabelle mit den Informationen der Tuebinger Studenten
matching_programm <-
  function(file_incoming,
           file_tuebinger,
           dir_output_files,
           web_app)
  {
    file_kurzuebersicht   <- "Finalmatch_kurz.csv"
    file_gesamtuebersicht  <- "Finalmatch_ausfuehrlich.csv"
    
    xlsx_kurzuebersicht   <- "Finalmatch_kurz.xlsx"
    xlsx_gesamtuebersicht  <- "Finalmatch_ausfuehrlich.xlsx"
    
    # wenn kein Pfad für die Ausgabedateien angegeben wird, nimm das aktuelle Arbeitsverzeichnis
    # wenn FALSE, kommt die Abfrage von der web-app und nichts wird erzeugt
    if (!(web_app)) {
      if (missingArg(dir_output_files)) {
        dir_output <- getwd()
      } else {
        if (!(stri_sub(dir_output_files,-1) == "/")) {
          dir_output <- paste(dir_output_files, "/", sep = "")
        } else {
          dir_output <- dir_output_files
        }
      }
    }
    
    # definiere die Spalten die eingelesen werden sollen
    spalten_csv <- c('NULL',
                     NA,
                     NA,
                     NA,
                     'NULL',
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA,
                     NA)
    
    spalten_excel <- c(2,
                       3,
                       4,
                       6,
                       7,
                       8,
                       9,
                       10,
                       11,
                       12,
                       13,
                       14,
                       15,
                       16,
                       17,
                       18,
                       19,
                       20,
                       21,
                       22)
    
    # liest die beiden uebergebenen Tabellen ein
    if (stri_sub(file_incoming,-3) == "csv") {
      tabelle_incoming <-
        read.csv(
          file = file_incoming,
          colClasses = spalten_csv,
          header = T,
          sep = ";",
          na.strings = c("", "NA")
        )
    } else if (stri_sub(file_incoming,-4) == 'xlsx') {
      tabelle_incoming <-
        read.xlsx2(
          file = file_incoming,
          sheetIndex = 1,
          colIndex = spalten_excel,
          header = TRUE,
          colClasses = NA,
          as.data.frame = TRUE
        )
      # entferne leere Zeilen
      tabelle_incoming <-
        tabelle_incoming[!apply(is.na(tabelle_incoming) |
                                  tabelle_incoming == "", 1, all),]
    } else {
      stop("Falsches Dateiformat")
    }
    
    if (stri_sub(file_tuebinger,-3) == "csv")
    {
      tabelle_tuebingen <-
        read.csv(
          file = file_tuebinger,
          colClasses = spalten_csv,
          header = T,
          sep = ";",
          na.strings = c("", "NA")
        )
    } else if (stri_sub(file_tuebinger,-4) == "xlsx")
    {
      tabelle_tuebingen <-
        read.xlsx2(
          file = file_tuebinger,
          sheetIndex = 1,
          colIndex = spalten_excel,
          header = TRUE,
          colClasses = NA,
          as.data.frame = TRUE
        )
      # entferne leere Zeilen
      tabelle_tuebingen <-
        tabelle_tuebingen[!apply(is.na(tabelle_tuebingen) |
                                   tabelle_tuebingen == "", 1, all),]
    } else {
      stop("Falsches Dateifromat")
    }
    
    # gibt die Spalte an, in der steht, ob die Tuebinger bereit waeren, 2 Buddys zu betreuen
    Spalte_2_Buddys = c(17)
    
    #gibt die Spalte an, in der die E-Mail Adresse steht
    Spalte_Email_Incoming = c(3)
    Spalte_Email_Tuebingen = c(3)
    
    #gibt die Spalte an, in der der Vorname steht
    Spalte_Vorname_Incoming = c(2)
    Spalte_Vorname_Tuebingen = c(2)
    
    #gibt die Spalte an, in der der Nachname steht
    Spalte_Nachname_Incoming = c(1)
    Spalte_Nachname_Tuebingen = c(1)
    
    # merkt sich in den Variablen, wie viele Reihen die Tabellen haben, d.h. wie viele Buddys sich angemeldet haben
    anzahl_incomings = nrow(tabelle_incoming)
    anzahl_tuebinger = nrow(tabelle_tuebingen)
    
    #hier wird die Punkte-Matrix erstellt, in der die uebereinstimmungspunktzahl aller Buddy-Paare gespeichert werden wird
    punkte_matrix = matrix(nrow = anzahl_incomings, ncol = anzahl_tuebinger)
    
    #diese Schleifen iterieren ueber alle Paare von Tuebingern und Incomings
    for (i in 1:anzahl_incomings) {
      for (j in 1:anzahl_tuebinger) {
        # fuer jedes Paar aus Austauschstudentem und Tuebinger Buddy wird ein Wert berechnet;
        # je hoeher der Wert, umso besser passen die Teilnehmer zusammen
        # der Wert wird dabei in der Funktion punkte_algorithmus berechnet, die sich weiter unten befindet
        punkte_matrix[i, j] = punkte_algorithmus(tabelle_incoming[i, ], tabelle_tuebingen[j, ])
        
      }
    }
    
    matching = galeShapley.marriageMarket(t(punkte_matrix), punkte_matrix)
    
    # Gibt es eine ungleiche Zahl an Tuebingern und Incomings, so sind im oberen Matching Personen aus der groeßeren Gruppe uebrig geblieben
    # Gab es mehr Tuebinger als Incomings, so bleiben diese Tuebinger ungematcht
    # Gab es mehr Incomings als Tuebinger, so werden diese erneut gematchet, und zwar mit Tuebingern, die angegeben haben, 2 Buddys betreuen zu wollen
    
    #Variablen zum Merken der aus dem zweiten Matching genommenen Personen
    deleted_cols = c()
    deleted_rows = c()
    
    if (anzahl_tuebinger < anzahl_incomings) {
      for (j in 1:anzahl_tuebinger) {
        if (toString(tabelle_tuebingen[j, Spalte_2_Buddys]) == "No") {
          #es wird ueberprueft, ob der Tuebinger nur einen Buddy betreuen moechte
          deleted_cols <-
            c(deleted_cols, j) #falls ja, so merken wir uns die Person
        }
      }
      if (length(deleted_cols) > 0)
        punkte_matrix <-
          punkte_matrix[, -deleted_cols] #alle Tuebinger, die nur 1 Buddy betreuen moechten, werden aus Matrix geloescht
      
      for (i in 1:anzahl_incomings) {
        if (!(is.na(matching$proposals[i, 1]))) {
          #es wird ueberprueft, ob der Incoming schon gematcht wurde
          deleted_rows <-
            c(deleted_rows, i) #falls ja, so merken wir uns die Person
        }
      }
      
      if (length(deleted_rows) > 0) {
        punkte_matrix <-
          punkte_matrix[-deleted_rows, ]
      } #alle Incomings, die schon gematcht wurden, werden aus Matrix geloescht
      
      index_matrix <-
        vektor_neue_indizes(deleted_cols, anzahl_tuebinger) #nun muessen die Indizes der aktualisierten Matrix angepasst werden (vgl. vektor_neue_indizes)
      
      #auf den verbleibenden Buddys wird nun erneut ein Matching ausgefuehrt
      matching_2 = galeShapley.marriageMarket(t(punkte_matrix), punkte_matrix)
      
      # in buddy_matching werden wir nun das komplette Matching festhalten
      buddy_matching = c()
      buddy_matching = matching$proposals
      
      index = c(1)
      
      for (k in 1:anzahl_incomings) {
        if (is.na(buddy_matching[k, 1])) {
          #falls der Incoming beim ersten Matching ungematched blieb
          # wird nun im zweiten Matching nachgesehen, zu wem er gematcht wurde (u.U. immer noch NA)
          
          if (!(is.na(matching_2$proposals[index, 1]))) {
            buddy_matching[k, 1] <-
              reihe_index_umrechnung(matching_2$proposals[index, 1], index_matrix)
          }
          index <- index + 1
        }
      }
    } else{
      buddy_matching = c()
      buddy_matching = matching$proposals
    }
    
    # OUTPUT
    # NUN ERSTELLEN WIR EINE ueBERSICHTSTABELLE, IN DER DIE GEMATCHTEN PAARE NACHGESEHEN WERDEN
    
    # AUSFueRHLICHE ueBERSICHT
    
    #Anzahl von Spalten fuer Tabelle
    N <- ncol(tabelle_tuebingen) + ncol(tabelle_incoming)
    
    # Wir erstellen nun eine uebersicht, die erst die Daten des Incomings und dann des Tuebinger Buddys enthaelt
    ausfuehrliche_uebersicht <-
      data.frame(list(tabelle_incoming[1, ], tabelle_tuebingen[buddy_matching[1], ]))
    for (k in 2:nrow(buddy_matching)) {
      ausfuehrliche_uebersicht[k, c(1:ncol(tabelle_incoming))] = tabelle_incoming[k, ]
      ausfuehrliche_uebersicht[k, c((ncol(tabelle_incoming) + 1):N)] = tabelle_tuebingen[buddy_matching[k, 1], ]
    }
    
    # KURZueBERSICHT
    
    # Wir erstellen nun eine uebersicht, die erst ausgewaehlte Daten des Incomings und dann des Tuebinger Buddys enthaelt
    # Welche Spalten enthalten sein sollen, wird in den nachfolgenden Vektoren festgelegt
    auswahl_incoming = c(
      Spalte_Nachname_Incoming,
      Spalte_Alter_Incoming,
      Spalte_Studienfach_Incoming,
      Spalte_Studienfach2_Incoming,
      Spalte_Studienabschluss_Incoming,
      Spalte_Uni_Incoming,
      Spalte_Hobby1_Incoming,
      Spalte_Hobby2_Incoming,
      Spalte_Hobby3_Incoming,
      Spalte_Datum_Incoming
    )
    auswahl_tuebingen = c(
      Spalte_Nachname_Tuebingen,
      Spalte_Alter_Tuebingen,
      Spalte_Studienfach_Tuebingen,
      Spalte_Studienfach2_Tuebingen,
      Spalte_Studienabschluss_Tuebingen,
      Spalte_Uni_Tuebingen,
      Spalte_Hobby1_Tuebingen,
      Spalte_Hobby2_Tuebingen,
      Spalte_Hobby3_Tuebingen,
      Spalte_Datum_Tuebingen
    )
    
    Kurzuebersicht <-
      data.frame(list(tabelle_incoming[1, auswahl_incoming], tabelle_tuebingen[buddy_matching[1], auswahl_tuebingen]))
    for (k in 2:nrow(buddy_matching)) {
      Kurzuebersicht[k, c(1:length(auswahl_incoming))] = tabelle_incoming[k, auswahl_incoming]
      Kurzuebersicht[k, c((length(auswahl_incoming) + 1):(length(auswahl_incoming) +
                                                            length(auswahl_tuebingen)))] = tabelle_tuebingen[buddy_matching[k], auswahl_tuebingen]
    }
    
    if (!(web_app)) {
      # diese Tabelle wird nun als .csv Datei exportiert
      write.csv2(Kurzuebersicht,
                 file.path(dir_output, file_kurzuebersicht),
                 row.names = FALSE)
      write.csv2(
        ausfuehrliche_uebersicht,
        file.path(dir_output, file_gesamtuebersicht),
        row.names = FALSE
      )
      
      # diese Tabelle wird zusaetzlich als .xlsx Datei exportiert
      write.xlsx2(
        Kurzuebersicht,
        file.path(dir_output, xlsx_kurzuebersicht),
        sheetName = "Kurzübersicht",
        row.names = FALSE,
        showNA = TRUE
      )
      write.xlsx2(
        ausfuehrliche_uebersicht,
        file.path(dir_output, xlsx_gesamtuebersicht),
        sheetName = "Gesamtübersicht",
        row.names = FALSE,
        showNA = TRUE
      )
    }
    
    # WEBueBERSICHT
    
    # Wir erstellen nun eine uebersicht fuer die Ausgabe im UI, die erst ausgewaehlte Daten des Incomings und dann des Tuebinger Buddys enthaelt
    # Welche Spalten enthalten sein sollen, wird in den nachfolgenden Vektoren festgelegt
    
    if (web_app) {
      web_auswahl_incoming = c(
        Spalte_Nachname_Incoming,
        Spalte_Alter_Incoming,
        Spalte_Geschlecht_Incoming,
        Spalte_Studienfach_Incoming,
        Spalte_Sprache_Incoming,
        Spalte_Datum_Incoming
      )
      web_auswahl_tuebingen = c(
        Spalte_Nachname_Tuebingen,
        Spalte_Alter_Tuebingen,
        Spalte_Geschlecht_Tuebingen,
        Spalte_Studienfach_Tuebingen,
        Spalte_Sprachen_Tuebingen,
        Spalte_Datum_Tuebingen
      )
      
      webuebersicht <-
        data.frame(list(tabelle_incoming[1, web_auswahl_incoming], tabelle_tuebingen[buddy_matching[1], web_auswahl_tuebingen]))
      for (k in 2:nrow(buddy_matching)) {
        webuebersicht[k, c(1:length(web_auswahl_incoming))] = tabelle_incoming[k, web_auswahl_incoming]
        webuebersicht[k, c((length(web_auswahl_incoming) + 1):(
          length(web_auswahl_incoming) + length(web_auswahl_tuebingen)
        ))] = tabelle_tuebingen[buddy_matching[k], web_auswahl_tuebingen]
      }
      return(as.data.frame(webuebersicht))
    }
    
    
    
    
    #nicht gematchte Incomings
  }

# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION PUNKTE_ALGORITHMUS
#-----------------------------------------------------------------------------------------------------------------------------------------

# Diese Funktion bekommt jeweils eine Tabellenzeile der Incomingtablle (incoming) und eine Tabellenzeile der Tuebingertabelle (tuebingen)
# uebergeben, also die Informationen eines Tuebingers und eines Incomings
# Die Funktion berechnet dann eine Punktzahl, die angibt, wie gut diese zwei Teilnehmer zusammen passen
# Dabei werden fuer jede gleiche oder aehnliche Antwort in den Formularen Punkte vergeben
punkte_algorithmus <-
  function(incoming, tuebingen) {
    #header wird mit uebergeben
    
    # Definition der Gewichtungsvariablen: wie viele Punkte gibt welche uebereinstimmung #Default
    
    Studienfach_Gewichtung = c(10) #Punktzahl fuer dasselbe Studienfach #20
    Fachbereich_Gewichtung = c(8) #Punktzahl fuer denselben Fachbereich #15
    Fakultaet_Gewichtung = c(5) #Punktzahl fuer diesselbe Fakultaet #10
    BergTal_Gewichtung = c(1) #Punktzahl wenn beide am Berg oder im Tal studieren #5
    
    Hobby_Gewichtung = c(5) #Punktzahl fuer das genau gleiche Hobby #5
    Hobbykategorie_Gewichtung = c(2) #Hobby aus selber Kategorie (z.Bsp. Sport (nicht:Other))
    
    genaues_Alter_Gewichtung = c(8) # Punktzahl fuer genaue Altersuebereinstimmung #8
    ungefaehres_Alter_Gewichtung = c(6) #Punktzahl fuer bis zu 3 Jahre Altersunterschied #3
    
    Geschlecht_Gewichtung = c(5) #Punktzahl fuer dasselbe Geschlecht
    
    #beim Studienabschluss soll nur gewichtet werden, falls beide Doktoranden sind
    StudienabschlussPhD_Gewichtung = c(30) #Punktzahl falls beide Doktoranden sind
    
    Austauschuni_Gewichtung = c(8) #Punktzahl wenn Tuebinger Auslandssemester an Uni des Incomings gemacht hat #8
    Austauschland_Gewichtung = c(10) #Punktzahl wenn Tuebinger Auslandssemester in Herkunftsland des Incomings gemacht hat #10
    
    Land_Studiengang_Gewichtung = c(10) # Punktzahl wenn Incoming aus Land, das mit Studiengang des Tuebingers zu tun hat, bspw. Korea-Koreanistik
    
    Sprache_Gewichtung = c(15) #Punktzahl fuer uebereinstimmende Sprache #5
    
    Ankunftsdatum_0Wochen_Gewichtung = c(10) #Punktzahl falls der Tuebinger vor oder mit dem Incoming ankommt #20
    Ankunftsdatum_2Wochen_Gewichtung = c(6) #Punktzahl falls der Tuebinger bis zu 2 Wochen nach dem Incoming ankommt
    Ankunftsdatum_3Wochen_Gewichtung = c(3) #Punktzahl falls der Tuebinger bis zu 3 Wochen nach dem Incoming ankommt
    
    
    #Variable zum Speichern des Scores
    score = c(0)
    
    #-----------------------------------------------------------------------
    # STUDIENFACH
    #-----------------------------------------------------------------------
    
    studienfach_tue = tuebingen[Spalte_Studienfach_Tuebingen] #Variable in der Studienfach des Tuebingers gespeichert wird
    studienfach_in = incoming[Spalte_Studienfach_Incoming] #Variable in der Studienfach des Incomings gespeichert wird
    studienfach_tue_2 = tuebingen[Spalte_Studienfach2_Tuebingen] #Variable in der 2. Studienfach des Tuebingers gespeichert wird
    studienfach_in_2 = incoming[Spalte_Studienfach2_Incoming] #Variable in der 2. Studienfach des Incomings gespeichert wird
    
    # 1. STUDIENFACH
    
    #die studiengang_funktion gibt einen Vektor zurueck; Position 1 betitelt den Fachbereich des Studiengangs, Position 2 die Fakultaet
    studienfach_informationen_tue = studiengang_funktion(studienfach_tue)
    studienfach_fachbereich_tue = studienfach_informationen_tue[1]
    studienfach_fakultaet_tue = studienfach_informationen_tue[2]
    
    studienfach_informationen_in = studiengang_funktion(studienfach_in)
    studienfach_fachbereich_in = studienfach_informationen_in[1]
    studienfach_fakultaet_in = studienfach_informationen_in[2]
    
    if (is.na(studienfach_tue)) {
      studienfach_tue = "none"
    }
    if (is.na(studienfach_in)) {
      studienfach_in = "none"
    }
    
    if (studienfach_tue == studienfach_in) {
      #falls die Teilnehmer dasselbe studieren
      score <-
        score + Studienfach_Gewichtung #vergib Punktzahl fuer dasselbe Studienfach
    } else if (studienfach_fachbereich_tue == studienfach_fachbereich_in) {
      #falls Teilnehmer am selben Fachbereich studieren
      score <-
        score + Fachbereich_Gewichtung # vergib Punktzahl fuer denselben Fachbereich
    } else if (studienfach_fakultaet_tue == studienfach_fakultaet_in) {
      # falls Teilnehmer an der selben Fakultaet studieren
      score <-
        score + Fakultaet_Gewichtung # vergib Punktzahl fuer dieselbe Fakultaet
    } else if (berg_tal(studienfach_fakultaet_tue, studienfach_fakultaet_in)) {
      score <-
        score + BergTal_Gewichtung # vergib Punktzahl falls beide am Berg oder im Tal studieren
    }
    
    land = incoming[Spalte_Land_Incoming]
    
    if ((studienfach_tue == "Japanologie // Japanese Studies") &
        (land == "Japan")) {
      score <- score + Land_Studiengang_Gewichtung
    }
    if ((studienfach_tue == "Koreanistik // Korean Studies") &
        (land == "South Korea")) {
      score <- score + Land_Studiengang_Gewichtung
    }
    if ((studienfach_tue == "Indologie // Indology") &
        (land == "India")) {
      score <- score + Land_Studiengang_Gewichtung
    }
    if ((studienfach_tue == "Sinologie // Chinese Studies") &
        (
          land == "China" |
          land == "Taiwan, R.O.C." |
          land == "Hong Kong, SAR" |
          land == "Singapore" | land == "Macau"
        )) {
      score <- score + Land_Studiengang_Gewichtung
    }
    if ((studienfach_tue == "American Studies") & (land == "USA")) {
      score <- score + Land_Studiengang_Gewichtung
    }
    # if((studienfach_tue == "Anglistik // English Studies" | studienfach_tue == "Englisch // English language" | studienfach_tue == "English Linguistics")
    #   & (land == "United Kingdom" | land == "Ireland" | land == "Australia" | land == "New Zealand")){
    # score <- score + Land_Studiengang_Gewichtung
    #}
    
    if ((studienfach_tue == "Anglistik // English Studies")
        & (land == "United Kingdom" | land == "Ireland")) {
      score <- score + Land_Studiengang_Gewichtung
    }
    
    if ((studienfach_tue == "Skandinavistik // Scandinavian Studies") &
        (
          land == "Denmark" |
          land == "Sweden" |
          land == "Norway" | land == "Finland" | land == "Iceland"
        )) {
      score <- score + Land_Studiengang_Gewichtung
    }
    
    if ((
      studienfach_tue == "Slavistik // Slavic Studies" |
      studienfach_tue == "Russisch // Russian Language"
    ) &
    (
      land == "Latvia" |
      land == "Lithuania" |
      land == "Estonia" |
      land == "Russia" | land == "Ukraine" | land == "Belarus"
      |
      land == "Poland" |
      land == "Slovakia" |
      land == "Slovenia" |
      land == "Czech Republic" |
      land == "Croatia" | land == "Serbia"
      | land == "Bulgaria" | land == "Romania"
    )) {
      score <- score + Land_Studiengang_Gewichtung
    }
    
    if ((
      studienfach_tue == "Romanistik // Romance Language and Literature" |
      studienfach_tue == "Franzoesisch // French Language"
      |
      studienfach_tue == "Interkulturelle Deutsch-Franzoesische Studien // Intercultural German-French Studies"
      |
      studienfach_tue == "Italienisch // Italian Language" |
      studienfach_tue == "Portugiesisch // Portuguese Language"
      | studienfach_tue == "Spanisch // Spanish Language"
    ) &
    (
      land == "France" |
      land == "Italy" |
      land == "Portugal" |
      land == "Spain" |
      land == "Argentina" | land == "Brazil" | land == "Chile"
      |
      land == "Colombia" |
      land == "Ecuador" |
      land == "Mexico" | land == "Peru" | land == "Uruguay"
    )) {
      score <- score + Land_Studiengang_Gewichtung
    }
    
    if ((studienfach_tue == "Lateinamerikastudien // Latin American Studies") &
        (
          land == "Argentina" |
          land == "Brazil" |
          land == "Chile" | land == "Colombia" | land == "Ecuador"
          | land == "Mexico" | land == "Peru" | land == "Uruguay"
        )) {
      score <- score + Land_Studiengang_Gewichtung
    }
    
    # 2. STUDIENFACH
    
    #wenn Incoming zweites Studienfach hat
    if (!(is.na(studienfach_in_2))) {
      #hole Fachbereich und Fakultaet fuer den zweiten Studiengang mittels studiengang_funktion
      studienfach_informationen_in2 = studiengang_funktion(studienfach_in_2)
      studienfach_fachbereich_in2 = studienfach_informationen_in2[1]
      studienfach_fakultaet_in2 = studienfach_informationen_in2[2]
      
      if (studienfach_tue == studienfach_in_2) {
        #falls das zweite Studienfach mit dem Fach des Tuebingers uebereinstimmt
        score <-
          score + Studienfach_Gewichtung #vergib Punktzahl fuer selbes Studienfach
      } else if (studienfach_fachbereich_tue == studienfach_fachbereich_in2 &
                 !(studienfach_fachbereich_in == studienfach_fachbereich_tue)) {
        # falls der Fachbereich uebereinstimmt, und nicht schon Punkte vergeben wurden, weil das Erstfach im selben Fachbereich ist wie das des Tuebingers
        score <-
          score + Fachbereich_Gewichtung #vergib Punkte fuer selben Fachbereich
      } else if (studienfach_fakultaet_tue == studienfach_fakultaet_in2 &
                 !(studienfach_fakultaet_in == studienfach_fakultaet_tue)) {
        # falls die Fakultaet uebereinstimmt, und nicht schon Punkte vergeben wurden, weil das Erstfach an der selben Fakultaet ist wie der des Tuebingers
        score <-
          score + Fakultaet_Gewichtung #vergib Punkte fuer selbe Fakultaet
      } else if (berg_tal(studienfach_fakultaet_tue, studienfach_fakultaet_in2) &
                 !(berg_tal(studienfach_fakultaet_tue, studienfach_fakultaet_in))) {
        # falls der "Studienort" uebereinstimmt, und nicht schon Punkte vergeben wurden, weil der Studienort des ersten Fachs gleich ist wie der des Tuebingers
        score <- score + BergTal_Gewichtung
      }
      
    }
    
    #wenn Tuebinger zweites Studienfach hat
    if (!(is.na(studienfach_tue_2))) {
      #hole Fachbereich und Fakultaet fuer den zweiten Studiengang mittels studiengang_funktion
      studienfach_informationen_tue2 = studiengang_funktion(studienfach_tue_2)
      studienfach_fachbereich_tue2 = studienfach_informationen_tue2[1]
      studienfach_fakultaet_tue2 = studienfach_informationen_tue2[2]
      
      if (studienfach_tue_2 == studienfach_in) {
        #falls das zweite Studienfach mit dem Fach des Incomings uebereinstimmt
        score <-
          score + Studienfach_Gewichtung  #vergib Punktzahl fuer selbes Studienfach
      } else if (studienfach_fachbereich_tue2 == studienfach_fachbereich_in &
                 !(studienfach_fachbereich_in == studienfach_fachbereich_tue)) {
        # falls der Fachbereich uebereinstimmt, und nicht schon Punkte vergeben wurden, weil das Erstfach im selben Fachbereich ist wie das des Incomings
        score <-
          score + Fachbereich_Gewichtung #vergib Punkte fuer selben Fachbereich
      } else if (studienfach_fakultaet_tue2 == studienfach_fakultaet_in &
                 !(studienfach_fakultaet_in == studienfach_fakultaet_tue)) {
        # falls die Fakultaet uebereinstimmt, und nicht schon Punkte vergeben wurden, weil das Erstfach an der selben Fakultaet ist wie der des Incomings
        score <-
          score + Fakultaet_Gewichtung #vergib Punkte fuer selbe Fakultaet
      } else if (berg_tal(studienfach_fakultaet_tue2, studienfach_fakultaet_in) &
                 !(berg_tal(studienfach_fakultaet_tue, studienfach_fakultaet_in))) {
        # falls der "Studienort" uebereinstimmt, und nicht schon Punkte vergeben wurden, weil das Erstfach am selben Ort ist wie der des Incomings
        score <- score + BergTal_Gewichtung
      }
      
      #wenn Incoming und Tuebinger zweites Studienfach haben
      if (!(is.na(studienfach_in_2))) {
        #hole Fachbereich und Fakultaet fuer den zweiten Studiengang mittels studiengang_funktion
        studienfach_informationen_in2 = studiengang_funktion(studienfach_in_2)
        studienfach_fachbereich_in2 = studienfach_informationen_in2[1]
        studienfach_fakultaet_in2 = studienfach_informationen_in2[2]
        
        if (studienfach_tue_2 == studienfach_in_2) {
          #falls die beiden zweiten Studienfaecher uebereinstimmen
          score <-
            score + Studienfach_Gewichtung #vergib Punktzahl fuer selbes Studienfach
        } else if (studienfach_fachbereich_tue2 == studienfach_fachbereich_in2 &
                   !(studienfach_fachbereich_in2 == studienfach_fachbereich_tue)
                   &
                   !(studienfach_fachbereich_in == studienfach_fachbereich_tue2)) {
          # falls der Fachbereich uebereinstimmt, und nicht schon Punkte vergeben wurden, weil ein Erstfach im selben Fachbereich ist
          score <-
            score + Fachbereich_Gewichtung #vergib Punkte fuer selben Fachbereich
        } else if (studienfach_fakultaet_tue2 == studienfach_fakultaet_in2 &
                   !(studienfach_fakultaet_in2 == studienfach_fakultaet_tue)
                   &
                   !(studienfach_fakultaet_in == studienfach_fakultaet_tue2)) {
          # falls die Fakultaet uebereinstimmt, und nicht schon Punkte vergeben wurden, weil ein Erstfach an der selben Fakultaet ist
          score <-
            score + Fakultaet_Gewichtung #vergib Punkte fuer selbe Fakultaet
        }
      }
      
      
      if ((studienfach_tue_2 == "Japanologie // Japanese Studies") &
          (land == "Japan")) {
        score <- score + Land_Studiengang_Gewichtung
      }
      if ((studienfach_tue_2 == "Koreanistik // Korean Studies") &
          (land == "South Korea")) {
        score <- score + Land_Studiengang_Gewichtung
      }
      if ((studienfach_tue_2 == "Indologie // Indology") &
          (land == "India")) {
        score <- score + Land_Studiengang_Gewichtung
      }
      if ((studienfach_tue_2 == "Sinologie / Chinese Studies") &
          (
            land == "China" |
            land == "Taiwan, R.O.C." |
            land == "Hong Kong, SAR" |
            land == "Singapore" | land == "Macau"
          )) {
        score <- score + Land_Studiengang_Gewichtung
      }
      if ((studienfach_tue_2 == "American Studies") &
          (land == "USA")) {
        score <- score + Land_Studiengang_Gewichtung
      }
      
      # if((studienfach_tue_2 == "Anglistik // English Studies" | studienfach_tue_2 == "Englisch // English language" | studienfach_tue_2 == "English Linguistics")
      #   & (land == "United Kingdom" | land == "Ireland" | land == "Australia" | land == "New Zealand")){
      # score <- score + Land_Studiengang_Gewichtung
      #}
      
      if ((studienfach_tue == "Anglistik // English Studies")
          & (land == "United Kingdom" | land == "Ireland")) {
        score <- score + Land_Studiengang_Gewichtung
      }
      
      if ((studienfach_tue_2 == "Skandinavistik // Scandinavian Studies") &
          (
            land == "Denmark" |
            land == "Sweden" |
            land == "Norway" | land == "Finland" | land == "Iceland"
          )) {
        score <- score + Land_Studiengang_Gewichtung
      }
      
      if ((
        studienfach_tue_2 == "Slavistik // Slavic Studies" |
        studienfach_tue_2 == "Russisch // Russian Language"
      ) &
      (
        land == "Latvia" |
        land == "Lithuania" |
        land == "Estonia" |
        land == "Russia" | land == "Ukraine" | land == "Belarus"
        |
        land == "Poland" |
        land == "Slovakia" |
        land == "Slovenia" |
        land == "Czech Republic" |
        land == "Croatia" | land == "Serbia"
        | land == "Bulgaria" | land == "Romania"
      )) {
        score <- score + Land_Studiengang_Gewichtung
      }
      
      if ((
        studienfach_tue_2 == "Romanistik // Romance Language and Literature" |
        studienfach_tue_2 == "Franzoesisch // French Language"
        |
        studienfach_tue_2 == "Interkulturelle Deutsch-Franzoesische Studien // Intercultural German-French Studies"
        |
        studienfach_tue_2 == "Italienisch // Italian Language" |
        studienfach_tue_2 == "Portugiesisch // Portuguese Language"
        | studienfach_tue_2 == "Spanisch // Spanish Language"
      ) &
      (
        land == "France" |
        land == "Italy" |
        land == "Portugal" |
        land == "Spain" |
        land == "Argentina" | land == "Brazil" | land == "Chile"
        |
        land == "Colombia" |
        land == "Ecuador" |
        land == "Mexico" | land == "Peru" | land == "Uruguay"
      )) {
        score <- score + Land_Studiengang_Gewichtung
      }
      
      if ((studienfach_tue_2 == "Lateinamerikastudien // Latin American Studies") &
          (
            land == "Argentina" |
            land == "Brazil" |
            land == "Chile" | land == "Colombia" | land == "Ecuador"
            | land == "Mexico" | land == "Peru" | land == "Uruguay"
          )) {
        score <- score + Land_Studiengang_Gewichtung
      }
      
    }
    
    #------------------------------------------------------------------------------
    # HOBBYS
    #------------------------------------------------------------------------------
    
    if (is.na(tuebingen[Spalte_Hobby1_Tuebingen])) {
      tuebingen[Spalte_Hobby1_Tuebingen] = "none"
    }
    if (is.na(tuebingen[Spalte_Hobby2_Tuebingen])) {
      tuebingen[Spalte_Hobby2_Tuebingen] = "none"
    }
    if (is.na(tuebingen[Spalte_Hobby3_Tuebingen])) {
      tuebingen[Spalte_Hobby3_Tuebingen] = "none"
    }
    if (is.na(incoming[Spalte_Hobby1_Incoming])) {
      incoming[Spalte_Hobby1_Incoming] = "none"
    }
    if (is.na(incoming[Spalte_Hobby2_Incoming])) {
      incoming[Spalte_Hobby2_Incoming] = "none"
    }
    if (is.na(incoming[Spalte_Hobby3_Incoming])) {
      incoming[Spalte_Hobby3_Incoming] = "none"
    }
    
    # Vektor fuer alle Hobbies des Tuebingers
    hobbies_tue = c(unname(tuebingen[Spalte_Hobby1_Tuebingen]), unname(tuebingen[Spalte_Hobby2_Tuebingen]), unname(tuebingen[Spalte_Hobby3_Tuebingen]))
    # Vektor fuer alle Hobbies des Incomings
    hobbies_in = c(unname(incoming[Spalte_Hobby1_Incoming]), unname(incoming[Spalte_Hobby2_Incoming]), unname(incoming[Spalte_Hobby3_Incoming]))
    
    for (h in 1:3) {
      #gehe alle Hobbies des Incomings durch
      if (hobbies_in[[h]] == hobbies_tue[[1]] |
          hobbies_in[[h]] == hobbies_tue[[2]] |
          hobbies_in[[h]] == hobbies_tue[[3]]) {
        score <-
          score + Hobby_Gewichtung #falls das Hobby mit einem Hobby des Tuebingers uebereinstimmt, vergib Punktzahl fuer gleiches Hobby
      } else
        if (hobbykategorie_funktion(hobbies_in[[h]], hobbies_tue, hobbies_in, h)) {
          #andernfalls ueberpruefe mit hobbykategorie_funktion, ob es
          #in der selben Kategorie ist wie ein Hobby des Tuebingers
          score <-
            score + Hobbykategorie_Gewichtung #falls ja, vergib Punktzahl fuer selbe Hobbykategorie
        }
    }
    
    #------------------------------------------------------------------------------
    # ALTER
    #------------------------------------------------------------------------------
    
    if (is.na(tuebingen[Spalte_Alter_Tuebingen])) {
      tuebingen[Spalte_Alter_Tuebingen] = 0
    }
    if (is.na(incoming[Spalte_Alter_Incoming])) {
      incoming[Spalte_Alter_Incoming] = 100
    }
    
    alter_unterschied = abs(tuebingen[Spalte_Alter_Tuebingen] - incoming[Spalte_Alter_Incoming]) #berechne Altersunterschied
    if (alter_unterschied == 0) {
      #falls kein Altersunterschied, vergib Punkte fuer selbes Alter
      score <- score + genaues_Alter_Gewichtung
    } else if (alter_unterschied <= 3) {
      #falls Altersunterschied bis zu 3 Jahren, vergib Punkte fuer aehnliches Alter
      score <- score + ungefaehres_Alter_Gewichtung
    } else if (alter_unterschied >= 7) {
      #falls Altersunterschied groeßer gleich 7 Jahren, ziehe Punkte ab
      score <-
        score - ungefaehres_Alter_Gewichtung - genaues_Alter_Gewichtung
    }
    
    
    #------------------------------------------------------------------------------
    # GESCHLECHT
    #------------------------------------------------------------------------------
    
    if (is.na(tuebingen[Spalte_Geschlecht_Tuebingen])) {
      tuebingen[Spalte_Geschlecht_Tuebingen] = "none"
    }
    if (is.na(incoming[Spalte_Geschlecht_Incoming])) {
      incoming[Spalte_Geschlecht_Incoming] = "none"
    }
    
    if (tuebingen[Spalte_Geschlecht_Tuebingen] == incoming[Spalte_Geschlecht_Incoming]) {
      score <-
        score + Geschlecht_Gewichtung #falls selbes Geschlecht, vergib Punkte
    }
    
    #------------------------------------------------------------------------------
    # UNI + LAND
    #------------------------------------------------------------------------------
    
    if (is.na(incoming[Spalte_Land_Incoming])) {
      incoming[Spalte_Land_Incoming] = "none"
    }
    
    
    #falls Uni im Freitext steht wird sie auf anderes Feld uebertragen
    if (!(is.na(tuebingen[Spalte_Uni_Tuebingen_Freitext]))) {
      tuebingen[Spalte_Uni_Tuebingen] <-
        tuebingen[Spalte_Uni_Tuebingen_Freitext]
    }
    if (!(is.na(incoming[Spalte_Uni_Incoming_Freitext]))) {
      incoming[Spalte_Uni_Incoming] <-
        incoming[Spalte_Uni_Incoming_Freitext]
    }
    
    if (!(is.na(tuebingen[Spalte_Uni_Tuebingen]))) {
      #falls der Tuebinger ein Auslandssemester gemacht hat
      if (tuebingen[Spalte_Uni_Tuebingen] == incoming[Spalte_Uni_Incoming]) {
        score <-
          score + Austauschuni_Gewichtung #vergib Punkte falls Tuebinger an Uni von Incoming war
      } else
        if (tuebingen[Spalte_Land_Tuebingen] == incoming[Spalte_Land_Incoming]) {
          score <-
            score + Austauschland_Gewichtung #andernfalls vergib Punkte falls Tuebinger im Herkunftsland des Incomings war
        }
    }
    
    
    #------------------------------------------------------------------------------
    # SPRACHE
    #------------------------------------------------------------------------------
    
    if (is.na(tuebingen[[Spalte_Sprachen_Tuebingen]])) {
      tuebingen[[Spalte_Sprachen_Tuebingen]] = "none"
    }
    if (is.na(incoming[[Spalte_Sprache_Incoming]])) {
      incoming[[Spalte_Sprache_Incoming]] = "none"
    }
    
    # Sprachen werden aus Tabelle in Vektor uebertragen
    sprachen_tuebingen <-
      strsplit(toString(tuebingen[[Spalte_Sprachen_Tuebingen]]), ";")
    sprachen_incoming <-
      strsplit(toString(incoming[[Spalte_Sprache_Incoming]]), ";")
    #Anzahl der gesprochenen Sprachen werden gezaehlt
    anzahl_sprachen_tuebingen <- length(sprachen_tuebingen[[1]])
    anzahl_sprachen_incoming <- length(sprachen_incoming[[1]])
    
    for (s in 1:anzahl_sprachen_incoming) {
      # hier gibt es fuer alle identische Sprachen außer Englisch und Deutsch Punkte
      if (sprachen_incoming[[1]][s] != "German" &
          sprachen_incoming[[1]][s] != "English") {
        if (sprachen_incoming[[1]][s] %in% sprachen_tuebingen[[1]]) {
          score <-
            score + Sprache_Gewichtung #falls Sprache identisch und nicht Deutsch oder Englisch, vergib fuer jede gleiche Sprache Punkte
        }
      }
    }
    
    #------------------------------------------------------------------------------
    # ANKUNFT
    #------------------------------------------------------------------------------
    
    if (is.na(tuebingen[[Spalte_Datum_Tuebingen]])) {
      tuebingen[[Spalte_Datum_Tuebingen]] = "01.01.01"
    }
    if (is.na(incoming[[Spalte_Datum_Incoming]])) {
      incoming[[Spalte_Datum_Incoming]] = "01.01.01"
    }
    
    # Je groeßer die Zahl, umso mehr Tage ist der Buddy alleine in Tuebingen
    ankunftsdatum_unterschied = as.Date(strptime(toString(unname(tuebingen[[Spalte_Datum_Tuebingen]])), "%d.%m.%y")) - as.Date(strptime(toString(unname(incoming[[Spalte_Datum_Incoming]])), "%d.%m.%y"))
    if (ankunftsdatum_unterschied <= 0) {
      #falls der Tuebinger vor dem Buddy da ist
      score <- score + Ankunftsdatum_0Wochen_Gewichtung
    } else
      if (ankunftsdatum_unterschied <= 14) {
        #falls der Buddy nur bis zu 2 Wochen alleine ist
        score <- score + Ankunftsdatum_2Wochen_Gewichtung
      } else
        if (ankunftsdatum_unterschied <= 21) {
          #falls der Buddy zwischen 2 und 3 Wochen alleine ist
          score <- score + Ankunftsdatum_3Wochen_Gewichtung
        }
    
    #------------------------------------------------------------------------------
    # Studienabschluss
    #------------------------------------------------------------------------------
    
    if (is.na(tuebingen[Spalte_Studienabschluss_Tuebingen])) {
      tuebingen[Spalte_Studienabschluss_Tuebingen] = "none"
    }
    if (is.na(incoming[Spalte_Studienabschluss_Incoming])) {
      incoming[Spalte_Studienabschluss_Incoming] = "none"
    }
    
    if (tuebingen[Spalte_Studienabschluss_Tuebingen] == "Doktorand/in // PhD Student?" &
        incoming[Spalte_Studienabschluss_Incoming] == "PhD Student?") {
      score <-
        score + StudienabschlussPhD_Gewichtung #falls beide PhD, so werden Punkte vergeben
    }
    
    return (score)
  }

# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION STUDIENGANG_FUNKTION
#-----------------------------------------------------------------------------------------------------------------------------------------

# Diese Funktion bekommt als Argument den Studiengang eines Buddys uebergeben. Falls kein Studiengang angegeben wurde, gibt sie direkt
# NA zurueck. Andernfalls wird er mit allen hardgecodeten Studiengaengen abgeglichen, und bei einer uebereinstimmung wird der zugehoerige
# Fachbereich und die zugehoerige Fakultaet zurueck gegeben.h

studiengang_funktion <- function(studiengang) {
  if (is.na(studiengang)) {
    return(c(NA, NA))
  } else
    
    F1 = "Evangelisch-Theologische Fakultaet"
  F2 = "Katholisch-Theologische Fakultaet"
  F3 = "Juristische Fakultaet"
  F4 = "Medizinische Fakultaet"
  F5 = "Philosophische Fakultaet"
  F6 = "Wirtschafts- und Sozialwissenschaftliche Fakultaet"
  F7 = "Mathematisch-Naturwissenschaftliche Fakultaet"
  F8 = "Zentrum fuer Islamische Theologie"
  
  #------------------------------------------------------------------
  # FAKULTaeT 1: EVANGELISCH-THEOLOGISCHE FAKULTaeT
  #------------------------------------------------------------------
  
  if (studiengang == "Evangelische Theologie // Protestant Theology" |
      studiengang == "Judaistik // Jewish Studies") {
    return(c("Ev. Theologie", F1))
  }
  
  #------------------------------------------------------------------
  # FAKULTaeT 2: KATHOLISCH-THEOLOGISCHE FAKULTaeT
  #------------------------------------------------------------------
  
  else if (studiengang == "Katholische Theologie // Catholic Theology") {
    return(c("Kath. Theologie", F2))
  }
  
  #------------------------------------------------------------------
  # FAKULTaeT 3: JURISTISCHE FAKULTaeT
  #------------------------------------------------------------------
  
  else if (studiengang == "Rechtswissenschaft // Law") {
    return(c("Jura", F3))
  }
  
  #------------------------------------------------------------------
  # FAKULTaeT 4: MEDIZIN
  #------------------------------------------------------------------
  
  else if (studiengang == "Biomedical Technologies" |
           studiengang == "Medizin // Medicine"
           |
           studiengang == "Medizin - Biotechnologie // Medicine - Biotechnology" |
           studiengang == "Medizintechnik // Medical Technology"
           |
           studiengang == "Molekulare Medizin // Molecular Medicine" |
           studiengang == "Neuro- und Verhaltenswissenschaft // Neuro- and Behavioral Science"
           |
           studiengang == "Zahnmedizin // Dentistry" |
           studiengang == "Zellulaere und Molekulare Neurowissenschaften // Cellular and Molecular Neuroscience"
           |
           studiengang == "Experimentelle Medizin // Experimental Medicine (PhD)" |
           studiengang == "Hebammenwissenschaft // Midwifery"
           |
           studiengang == "Medizinische Strahlenwissenschaften // Medical Radiation Sciences"
           |
           studiengang == "Neuronale Informationsverarbeitung // Neural Information Processing" |
           studiengang == "Pflege // Nursing") {
    return(c("Medizin", F4))
  }
  
  #------------------------------------------------------------------
  #FAKULTaeT 5: PHILOSOPHISCHE FAKULTaeT
  #------------------------------------------------------------------
  
  else if (studiengang == "aegyptologie / Egyptology" |
           studiengang == "Archaeologie des Mittelalters // Archaeology of the Middle Ages"
           |
           studiengang == "IANES Vorderasiatische Archaeologie // Near Eastern Archaeology" |
           studiengang == "Klassische Archaeologie // Classical Archaeology"
           |
           studiengang == "Klassische Philologie // Classical Philology" |
           studiengang == "Kunstgeschichte // Art History"
           |
           studiengang == "Musikwissenschaft // Music" |
           studiengang == "Naturwissenschaftliche Archaeologie // Scientific Archaeology"
           |
           studiengang == "Religionswissenschaft // Religious Studies" |
           studiengang == "Ur- und Fruehgeschichte // Prehistory"
           |
           studiengang == "Ur- und Fruehgeschichtliche Archaeologie // Prehistorical Archaeology" |
           studiengang == "Vorderasiatische Archaeologie // Near Eastern Archaeology"
           |
           studiengang == "Altorientalische Philologie // Ancient Eastern Philology" |
           studiengang == "Griechisch // Greek"
           | studiengang == "Latein // Latin") {
    return(c("Altertums- und Kunstwissenschaften", F5))
  }
  
  else if (studiengang == "Ethnologie // Social and Cultural Anthropology" |
           studiengang == "Indologie // Indology"
           |
           studiengang == "Japanologie // Japanese Studies" |
           studiengang == "Koreanistik // Korean Studies"
           |
           studiengang == "Orient- und Islamwissenschaften // Oriental Studies"
           |
           studiengang == "Politik und Gesellschaft Ostasiens (MA) // East Asian History and Politics"
           |
           studiengang == "Sinologie / Chinese Studies"  |
           studiengang == "Islamwissenschaften // Islamic Studies"
           |
           studiengang == "Sprachen, Geschichte und Kulturen des Nahen Ostens // Languages, History and Cultures of the Near East") {
    return(c("Asien-Orient-Wissenschaften", F5))
  }
  
  else if (studiengang == "Allgemeine Rhetorik // Rhetorics" |
           studiengang == "Medienwissenschaft // Media Science" |
           studiengang == "Philosophie // Philosophy") {
    return(c("Philosophie - Rhetorik - Medien", F5))
  }
  
  else if (studiengang == "American Studies" |
           studiengang == "Anglistik // English Studies" |
           studiengang == "Germanistik // German Studies"
           |
           studiengang == "Internationale Literaturen // International Literatures" |
           studiengang == "Romanistik // Romance Language and Literature"
           |
           studiengang == "Skandinavistik // Scandinavian Studies" |
           studiengang == "Slavistik // Slavic Studies"
           |
           studiengang == "Cultures of the Global South" |
           studiengang == "Deutsch als Zweitsprache: Sprachdiagnostik und Sprachfoerderung // German as a Foreign Language: Linguistic Assessment and Language Support"
           |
           studiengang == "Deutsch // German Language" |
           studiengang == "Deutsche Literatur // German Literature" |
           studiengang == "Englisch // English Language"
           |
           studiengang == "English Linguistics" |
           studiengang == "Franzoesisch // French Language" |
           studiengang == "Interkulturelle Deutsch-Franzoesische Studien // Intercultural German-French Studies"
           |
           studiengang == "Italienisch // Italian Language" |
           studiengang == "Lateinamerikastudien // Latin American Studies"
           |
           studiengang == "Literatur- und Kulturtheorie // Literary and Cultural Theory" |
           studiengang == "Portugiesisch // Portuguese Language"
           |
           studiengang == "Russisch // Russian Language" |
           studiengang == "Spanisch // Spanish Language") {
    return(c("Neuphilologie", F5))
  }
  
  else if (studiengang == "Geschichte // History") {
    return(c("Geschichtswissenschaften", F5))
  }
  
  else if (studiengang == "Computerlinguistik // Computational Linguistics" |
           studiengang == "Sprachwissenschaften // Linguistics") {
    return(c("Sprachwissenschaft", F5))
  }
  
  #------------------------------------------------------------------
  #FAKULTaeT 6: WIRTSCHAFTS- UND SOZIALWISSENSCHAFTLICHE FAKULTaeT
  #------------------------------------------------------------------
  
  else if (studiengang == "Empirische Bildungsforschung und Paedagogische Psychologie // Empirical Educational Research"
           |
           studiengang == "Empirische Kulturwissenschaft // Empirical Cultural Studies" |
           studiengang == "Erwachsenenbildung // Adult Education"
           |
           studiengang == "Erziehungswissenschaft // Educational Science" |
           studiengang == "Politikwissenschaft // Political Science"
           |
           studiengang == "Friedensforschung und Internationale Politik // Peace Studies and International Politics"
           |
           studiengang == "Schulforschung und Schulentwicklung // Educational Research & Development/Social Work"
           |
           studiengang == "Sozialpaedagogik // Educational Science" |
           studiengang == "Soziologie // Sociology"
           |
           studiengang == "Sportwissenschaft // Sports Science" |
           studiengang == "Comparative + Middle East Politics and Society (CMEPS)"
           | studiengang == "Bildungswissenschaften // Teaching"
           |
           studiengang == "Demokratie und Regieren in Europa // Democracy and Governance in Europe") {
    return(c("Sozialwissenschaften", F7))
  }
  
  else if (studiengang == "Accounting and Finance (M.Sc.)" |
           studiengang == "Business Administration" |
           studiengang == "Economics"
           |
           studiengang == "European Economics (M.Sc.)" |
           studiengang == "European Management (M.Sc.)" |
           studiengang == "General Management (M.Sc.)"
           |
           studiengang == "Management & Economics (M.Sc.)" |
           studiengang == "International Business (M.Sc.)"
           |
           studiengang == "International Business Administration (BA)" |
           studiengang == "International Economics (BA)"
           |
           studiengang == "International Economics (M.Sc.)" |
           studiengang == "Data Science in Business and Economics"
           |
           studiengang == "Economics and Business Administration" |
           studiengang == "Economics and Finance") {
    return(c("Wirtschaftswisschenschaften", F6))
  }
  
  #------------------------------------------------------------------
  # FAKULTaeT 7: MATHEMATISCH-NATURWISSENSCHAFTLICHE FAKULTaeT
  #------------------------------------------------------------------
  
  
  else if (studiengang == "Biochemie // Biochemistry" |
           studiengang == "Pharmaceutical Sciences and Technologies"
           | studiengang == "Pharmazie // Pharmacy") {
    return(c("Pharmazie und Biochemie", F7))
  }
  
  else if (studiengang == "Bioinformatik // Bioinformatics" |
           studiengang == "Informatik // Computer Science"
           |
           studiengang == "Kognitionswissenschaft // Cognition Science" |
           studiengang == "Medieninformatik // Media Informatics"
           |
           studiengang == "Machine Learning" |
           studiengang == "Medizininformatik // Medical Informatics") {
    return(c("Informatik", F7))
  }
  
  else if (studiengang == "Biologie // Biology" |
           studiengang == "Nano-Science"
           |
           studiengang == "Evolution und oekologie // Evolution and Ecology" |
           studiengang == "Mikrobiologie // Microbiology"
           | studiengang == "Neurobiologie // Neurobiology"
           |
           studiengang == "Molekulare Zellbiologie und Immunologie // Molecular Cell Biology and Immunology") {
    return(c("Biologie", F7))
  }
  
  else if (studiengang == "Chemie // Chemistry") {
    return(c("Chemie", F7))
  }
  
  else if (studiengang == "Geographie // Geography" |
           studiengang == "Geologie // Geology" |
           studiengang == "Geooekologie // Geoecology"
           |
           studiengang == "Palaeoanthropologie // Palaeoanthropology" |
           studiengang == "Umweltwissenschaften // Environmental Science"
           |
           studiengang == "Humangeographie // Global Studies" |
           studiengang == "Applied + Environmental Geoscience AEG"
           | studiengang == "Geowissenschaften // Geosciences"
           |
           studiengang == "Physische Geographie - Umweltgeographie // Physical Geography - Environmental Geography"
           |
           studiengang == "Archaeological Sciences and Human Evolution") {
    return(c("Geowissenschaften", F7))
  }
  
  else if (studiengang == "Mathematik // Mathematics") {
    return(c("Mathematik", F7))
  }
  
  else if (studiengang == "Physik // Physics" |
           studiengang == "Mathematical Physics" |
           studiengang == "Naturwissenschaft und Technik // Natural Sciences and Technology"
           |
           studiengang == "Astro and Particle Physics" |
           studiengang == "Astronomie // Astronomy") {
    return(c("Physik", F7))
  }
  
  else if (studiengang == "Psychologie // Psychology" |
           studiengang == "Schulpsychologie // School Psychology") {
    return(c("Psychologie", F7))
  }
  
  #------------------------------------------------------------------
  # FAKULTaeT 8: ZENTRUM FueR ISLAMISCHE THEOLOGIE
  #------------------------------------------------------------------
  
  else if (studiengang == "Islamische Theologie // Islamic Theology" |
           studiengang == "Islamische Religionslehre // Islamic Religious Education") {
    return(c("Isl. Theologie", F8))
  }
  
  else{
    return(c("Fehler", "Fehler"))
  }
  
}

# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION BERG_TAL
#-----------------------------------------------------------------------------------------------------------------------------------------

#Diese Funktion gibt TRUE oder FALSE zurueck, je nachdem ob die beiden gegebenen Fakultaeten beide haupsaechlich am Berg oder im Tal sind
#Die einzigen beiden Bergfakultaeten sind die medizinische und die math-ntw. Fakultaet. Sind beides Bergler, wird sofort TRUE zurueck gegeben.
#(Da die berg_tal Funktion nur aufgerufen wird, falls die Fakultaeten verschieden sind, muss nicht geprueft werden, ob beide an der selben Fakultaet sind)
#Sind nicht beide Bergler, so wird ueberprueft ob einer von beiden Bergler ist, der andere muss dann Taler sein. Dann wird FALSE zurueckgegeben.
#Andernfalls sind beides Taler, und es wird TRUE zurueck gegeben.

berg_tal <- function(fakultaet1, fakultaet2) {
  #ueberpruefung ob beide am Berg studieren
  if ((
    fakultaet1 == "Medizinische Fakultaet" &
    fakultaet2 == "Mathematisch-Naturwissenschaftliche Fakultaet"
  ) |
  (
    fakultaet2 == "Medizinische Fakultaet" &
    fakultaet1 == "Mathematisch-Naturwissenschaftliche Fakultaet"
  )) {
    return(TRUE)
  } else if (fakultaet1 == "Medizinische Fakultaet" |
             fakultaet2 == "Mathematisch-Naturwissenschaftliche Fakultaet") {
    #ueberpruefen ob einer Bergler ist
    return(FALSE)
  } else{
    #andernfalls muessen beide im Tal studieren
    return(TRUE)
  }
  
}

# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION HOBBYKATEGORIE_FUNKTION
#-----------------------------------------------------------------------------------------------------------------------------------------

# Diese Funktion bekommt ein konkretes Hobby des Incomings (hobby_in), alle Hobbies des Tuebingers (hobbies_tue),
# alle Hobbies des Incomings (hobbies_in), und an welcher Stelle das Hobby hobby_in in den hobbies_in steht (h)
# Letzteres wichtig fuer das aufrufen der Funktion hobby_nicht_genutzt (siehe unten)
# Die hobbykategorie_funktion an sich ueberprueft, ob sich das uebergebene Hobby hobby_in in der selben Kategorie
# wie ein noch nicht genutztes Hobby (siehe hobby_nicht_genutzt) eines Tuebingers befindet

hobbykategorie_funktion <-
  function(hobby_in, hobbies_tue, hobbies_in, h)
  {
    Hobbies = c("Creativity", "Culture", "Media", "Music", "Sports")
    for (hobby in Hobbies) {
      if (startsWith(toString(hobby_in), hobby) &
          ((
            startsWith(toString(hobbies_tue[[1]]), hobby) &
            hobby_nicht_genutzt(h, hobbies_in, hobby)
          )
          |
          (
            startsWith(toString(hobbies_tue[[2]]), hobby) &
            hobby_nicht_genutzt(h, hobbies_in, hobby)
          )
          |
          (
            startsWith(toString(hobbies_tue[[3]]), hobby) &
            hobby_nicht_genutzt(h, hobbies_in, hobby)
          )
          )) {
        return(TRUE)
      }
    }
    return(FALSE)
    
  }

# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION HOBBY_NICHT_GENUTZT
#-----------------------------------------------------------------------------------------------------------------------------------------

# Diese Helferfunktion dient dazu, festzustellen, ob fuer die uebereinstimmung eines Hobbys (hobby) schon ein Hobby eines
# Tuebingers "genutzt" wurde.
# So hat der Incoming beispielsweise zwei Hobbies aus der Kategorie Sport angegeben, der Tuebinger eines.
# Damit die Funktion hobbykategorie_funktion nicht zweimal Punkte vergibt, da sich beide Sporthobbies des Incomings genau wie das eine
# Sporthobby des Tuebingers eben in der selben Kategorie befinden, wird hier fuer das Hobby des Incomings ueberprueft, ob ein vorhergehendes
# Hobby des Incomings sich bereits in derselben Kategorie wie das Tuebinger Hobby befunden hat.

hobby_nicht_genutzt <- function(h, hobbies_in, hobby)
{
  #ist es das erste Hobby aus der Liste so gibt es kein vorhergehendes Hobby und kann sich auch so nicht in der selben Kategorie befinden
  if (h == 1) {
    return(TRUE)
  } else {
    #andernfalls ueberpruefe, ob die Kategorie dieselbe ist wie die eines vorhergangenen Hobbys
    for (i in 1:(h - 1)) {
      if (startsWith(toString(hobbies_in[[i]]), hobby)) {
        return(FALSE)
      }
    }
    return(TRUE) # wenn nicht wurde das Hobby noch nicht genutzth
  }
}

# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION VEKTOR_NEUE_INDIZES
#-----------------------------------------------------------------------------------------------------------------------------------------

# Diese Funktion bekommt einen Vektor mit den alten Indizes der geloeschten Tuebinger (deleted_cols), und der Anzahl aller Tuebinger
# Daraus werden dann die Indizes der verbleibenden Tuebinger in der aktualisierten Matrix berechnet, d.h. die alten Indizes

vektor_neue_indizes <- function(deleted_cols, anzahl_tuebinger) {
  index_matrix = matrix(nrow = anzahl_tuebinger, ncol = 1) #hier werden die Indizes gespeichert
  j = c(1)
  for (i in 1:anzahl_tuebinger) {
    if (!(i %in% deleted_cols)) {
      index_matrix[i, 1] <- j
      j <- j + 1
    }
    
  }
  
  return(index_matrix) #gib Indize-Matrix zurueck
}


# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION REIHE_INDEX_UMRECHNUNG
#-----------------------------------------------------------------------------------------------------------------------------------------

# Diese Funktion bekommt den aktuellen Index und den mit obriger Funktion berechneten Index Vektor uebergeben
# Dann wird im Index Vektor der zugehoerige alte Index des gegebenen Indexes nachgesehen


reihe_index_umrechnung <- function(index, index_matrix) {
  for (i in 1:nrow(index_matrix)) {
    if (!(is.na(index_matrix[i, 1]))) {
      if (index_matrix[i, 1] == index) {
        return(i)
      }
    }
  }
}

# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION ZWEITES_FACH
#-----------------------------------------------------------------------------------------------------------------------------------------

#Funktion die zweites Studienfach zurueckgibt, falls vorhanden

zweites_fach <- function(fach) {
  if (!(is.na(fach))) {
    return(c(" , " , toString(fach), sep = "\n"))
  } else{
    return(sep = "\n")
  }
}