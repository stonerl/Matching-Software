# Dieses Programm versendet anhand der von matching_algorithm.R erstellten Tabelle nun die E-Mails an die Buddys.


library(mailR)

# ----------------------------------------------------------------------------------------------------------------------------------------
# FUNKTION EMAIL_VERSAND
#-----------------------------------------------------------------------------------------------------------------------------------------

# Funktion email_versand sendet E-Mails an Buddys
# Variable "input_tabelle" gibt dabei den Dateipfad zu der Tabelle mit den Informationen der Studenten an,
# die als "ausfuehrliche uebersicht" von matching_algorithm.R erstellt wurde
#(dabei stehen zuerst die Daten des Incomings und dann die des Tuebingers )


email_versand <- function(input_tabelle)
{
  #Tabelle wird eingelesen
  tabelle <-
    read.csv(file = input_incoming,
             header = T,
             na.strings = c("", "NA"))


  #gibt die Spalte an, in der die E-Mail Adresse steht
  Spalte_EMail_Incoming = c(4)
  Spalte_Email_Tuebingen = c(26)

  #gibt die Spalte an, in der der Vorname steht
  Spalte_Vorname_Incoming = c(3)
  Spalte_Vorname_Tuebingen = c(25)

  #gibt die Spalte an, in der der Nachname steht
  Spalte_Nachname_Incoming = c(2)
  Spalte_Nachname_Tuebingen = c(24)

  #gibt an, in welcher Spalte die jeweiligen Informationen stehen
  Spalte_Alter_Incoming = c(6)
  Spalte_Geschlecht_Incoming = c(7)
  Spalte_Studienfach_Incoming = c(8)
  Spalte_Studienfach2_Incoming = c(9)
  Spalte_Studienabschluss_Incoming = c(10)
  Spalte_Land_Incoming = c(11)
  Spalte_Uni_Incoming = c(12)
  Spalte_Uni_Incoming_Freitext = c(13)
  Spalte_comlang_Incoming = c(14)
  Spalte_Hobby1_Incoming = c(15)
  Spalte_Hobby2_Incoming = c(16)
  Spalte_Hobby3_Incoming = c(17)
  Spalte_Sprachen_Incoming = c(18)
  Spalte_Kurs_Incoming = c(19)
  Spalte_Datum_Incoming = c(20)



  Spalte_Alter_Tuebingen = c(28)
  Spalte_Geschlecht_Tuebingen = c(29)
  Spalte_Studienfach_Tuebingen = c(30)
  Spalte_Studienfach2_Tuebingen = c(31)
  Spalte_Studienabschluss_Tuebingen = c(32)
  Spalte_Land_Tuebingen = c(33)
  Spalte_Uni_Tuebingen = c(34)
  Spalte_Uni_Tuebingen_Freitext = c(35)
  Spalte_Hobby1_Tuebingen = c(36)
  Spalte_Hobby2_Tuebingen = c(37)
  Spalte_Hobby3_Tuebingen = c(38)
  Spalte_Sprache_Tuebingen = c(39)
  Spalte_Datum_Tuebingen = c(41)


  sender <- "exchange.buddy@uni-tuebingen.de"

  for (i in 1:nrow(tabelle)) {
    recipients <-
      c(toString(tabelle[i, Spalte_EMail_Incoming]), toString(tabelle[i, Spalte_EMail_Tuebingen]))
    send.mail(
      from = sender,
      to = recipients,
      subject = "Your Exchange Buddy – It’s a match!",
      body = paste(
        "Hello there!",
        sep = "\n",
        "Congratulations, you two have been matched as Exchange Buddies for the Winter Semester 2019/20!",
        sep = "\n",
        "We are very happy to let you know the contact info of your Buddy:",
        sep = "\n",
        underline("International Buddy:"),
        sep = "\n",
        "First Name: ",
        toString(tabelle[i, Spalte_Vorname_Incoming]),
        sep = "\n",
        "Last Name: ",
        toString(tabelle[i, Spalte_Nachname_Incoming]),
        sep = "\n",
        "Gender: ",
        toString(tabelle[i, Spalte_Geschlecht_Incoming]),
        sep = "\n",
        "Home Country: ",
        toString(tabelle[i, Spalte_Land_Incoming]),
        sep = "\n",
        "Home University: ",
        toString(tabelle[i, Spalte_Uni_Incoming]),
        sep = "\n",
        "Study Program: ",
        toString(tabelle[i, Spalte_Studienfach_Incoming]),
        zweites_Fach(tabelle[i, Spalte_Studienfach2_Incoming]),
        "E-Mail Address: ",
        toString(tabelle[i, Spalte_EMail_Incoming]) ,
        sep = "\n",
        "Expected Arrival Date: ",
        toString(tabelle[i, Spalte_Datum_Incoming]),
        sep = "\n",
        "Participation in Start or Kompakt Kurs?: ",
        toString(tabelle[i, Spalte_Kurs_Incoming]),
        sep = "\n",
        "Preffered communication language: ",
        toString(tabelle[i, Spalte_comlang_Incoming]),
        sep = "\n",

        underline("Tuebinger Buddy:"),
        sep = "\n",
        "First Name: ",
        toString(tabelle[i, Spalte_Vorname_Tuebingen]),
        sep = "\n",
        "Last Name: ",
        toString(tabelle[i, Spalte_Nachname_Tuebingen]),
        sep = "\n",
        "Gender: ",
        toString(tabelle[i, Spalte_Geschlecht_Tuebingen]),
        sep = "\n",
        "Study Program: ",
        toString(tabelle[i, Spalte_Studienfach_Tuebingen]),
        zweites_Fach(tabelle[i, Spalte_Studienfach2_Tuebingen]),
        "E-Mail Address: ",
        toString(tabelle[i, Spalte_EMail_Tuebingen]) ,
        sep = "\n"
      ),
      "Available in Tuebingen from: ",
      toString(tabelle[i, Spalte_Datum_Tuebingen]),
      sep = "\n",
      "Now it’s up to you to get in touch!",
      sep = "\n",
      "We hope that the two of you will have a great time getting to know each other, exploring the university and the city of Tuebingen.",
      sep = "\n",
      "@ the Tuebingen Buddy: Would you like to receive a Certificate confirming your participation in the Exchange Buddy Program?
            Then please fill out the attached document “Exchange Buddy Confirmation” at the end of the semester and have your International Buddy sign it.
            Would you like to receive ECTS credits for “Schluesselqualifikation Interkulturelle Kompetenz”, then please fill out the “Exchange Buddy Protokoll” (also attached),
            and – same procedure – have your International Buddy sign it at the end of the semester. To receive the Certificate or the ECTS,
            please hand in the respective form to exchange.buddy@uni-tuebingen.de between 1 August and 1 September 2019! Please be aware that we cannot guarantee any ECTS.",
      sep = "\n",
      "In case you have any questions or concerns in regards to the Exchange Buddy Program, please do not hesitate to contact us.",
      sep = "\n",
      "Have a wonderful time!",
      sep = "\n",
      "Best wishes,",
      sep = "\n",
      "Your",
      sep = "\n",
      "Exchange Buddy Team",
      sep = "\n",
      "International Office",
      sep = "\n",
      "University of Tuebingen",
      sep = "\n",
      "Wilhelmstr. 9",
      sep = "\n",
      "D-72074 Tuebingen",
      sep = "\n",
      "GERMANY",
      sep = "\n",
      "exchange.buddy@uni-tuebingen.de",
      sep = "\n"
    ),
    smtp = list(
      host.name = "smtpserv.uni-tuebingen.de",
      port = 25,
      user.name = "exchange.buddy@uni-tuebingen.de",
      passwd = "YOURPASSWORD",
      ssl = TRUE
    ),
    authenticate = TRUE
    send = TRUE
    attach.files = 'Users/Documents/')
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
