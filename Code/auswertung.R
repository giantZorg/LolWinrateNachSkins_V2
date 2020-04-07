###
# Auswertung vom Erscheinen von neuen Skins zu Patches/Gewinnratenänderung
###

rm(list = ls())

# Pfad anpassen
setwd('C:/Users/SUTY1/Documents/SonstigeAufgaben/LolWinrateNachSkins_V2')

# Pakete laden
pakete <- c('zoo', 'vioplot')

libList <- installed.packages()
for (paket in pakete) {
	if (!(paket %in% libList)) {install.packages(paket, dependencies = TRUE)}
	library(paket, character.only = TRUE)
}

# Optionen
options(stringsAsFactors = FALSE)


###
# Funktionen
strPad0 <- function(x) {
    if (nchar(x) == 1) {
        x <- paste0('0', x)
    }
    return(x)
}


###
# Parameter definieren
parameter <- list()

# Pfade
parameter[['Ergebnisse']] <- 'Ergebnisse/'
parameter[['Bilder']] <- 'Bilder/'

# Zeitformate
parameter[['ZeitFormatTag']] <- '%Y%m%d'

# Anzahl Tage vor und nach dem Herauskommen des Skins, aus welchen die Änderung in den Raten berechnet wird
parameter[['TageVorHerauskommenStart']] <- 40
parameter[['TageVorHerauskommenEnde']] <- 30
parameter[['TageNachHerauskommenStart']] <- 10
parameter[['TageNachHerauskommenEnde']] <- 20

# Anzahl Patche vorher und nachher, in welchen geschaut wird, ob der Champion in den Änderungen auftaucht
parameter[['PatcheVorher']] <- 4
parameter[['PatcheNachher']] <- 2

# Zeitraum, in dem Patche betrachtet werden
parameter[['BeginnBetrachtung']] <- '20150101'
parameter[['EndeBetrachtung']] <- '20200331'


###
# Daten einlesen

# Champions
champions <- read.csv(paste0(parameter[['Ergebnisse']], 'ChampionListe.csv'))

# Patchtage
patchTage <- read.csv(paste0(parameter[['Ergebnisse']], 'PatchTage.csv'))

# Gewinnrate, Beliebtheit und Bannrate laden
ratenGeladen <- read.csv(paste0(parameter[['Ergebnisse']], 'DatenVonLeagueofgraphs/DatenVonLeagueofgraphs.csv'))

# Informationen zum Auftreten in den Patchnotes laden
championsInPatchen <- read.csv(paste0(parameter[['Ergebnisse']], 'DatenVonPatchen/DatenVonPatchen.csv'))

# Herausgegebene Skins laden
championSkinErscheinungen <- read.csv(paste0(parameter[['Ergebnisse']], 'DatenVonSkins/DatenVonSkins.csv'))


###
# Daten aufarbeiten

# Patchtage: Tag zum Date umwandeln. Patchnummer aufschlüsseln
patchTage[['TagDate']] <- as.Date(as.character(patchTage[['Tag']]), format = parameter[['ZeitFormatTag']])

patchTage[['Saison']] <- as.integer(sapply(patchTage[['PatchNummer']], function(x) strsplit(x, '\\.')[[1]][1]))

patchTage[['Patch']] <- sapply(patchTage[['PatchNummer']], function(x) strsplit(x, '\\.')[[1]][2])
patchTage[['Patch_b']] <- FALSE
for (i in 1:dim(patchTage)[1]) {
    if ('b' == substr(patchTage[['Patch']][i], nchar(patchTage[['Patch']][i]), nchar(patchTage[['Patch']][i]))) {
        patchTage[['Patch_b']][i] <- TRUE
        patchTage[['Patch']][i] <- as.integer(substr(patchTage[['Patch']][i], 1, nchar(patchTage[['Patch']][i]) - 1))
    }
}


# Gewinnrate, Beliebtheit und Bannrate trennen
ratenGeladen[['TagDate']] <- as.Date(ratenGeladen[['Zeit']] / (1000*60*60*24))

ratenSepariert <- list()
for (rate in c('Gewinnrate', 'Bannrate', 'Beliebtheit')) {	# rate <- 'Gewinnrate'
	ratenAufgeteilt <- ratenGeladen[ratenGeladen[['Typ']] == rate, ]
	ratenNachChampion <- split(ratenAufgeteilt, ratenAufgeteilt[['Champion']])
	
	# Nach Zeit sortieren (sollte eigentlich schon richtig sein, aber trotzdem überprüfen)
	for (champ in names(ratenNachChampion)) {	# champ = 'Zyra'
		ratenNachChampion[[champ]] <- ratenNachChampion[[champ]][sort.int(ratenNachChampion[[champ]][['Zeit']], index.return = TRUE)$ix, c('Wert', 'TagDate')]
	}
	ratenSepariert[[rate]] <- ratenNachChampion
}

# Auftreten von Champions in Patchen aufteilen nach Champion und nach Patch
championInPatchenNachChamps <- split(championsInPatchen, championsInPatchen[['Champion']])
championInPatchenNachPatchen <- split(championsInPatchen, championsInPatchen[['Patch']])

# Herausgegebene Skins: Noch nicht erschienene Skins entfernen, dann Erscheinungsdatum zu Date umwandeln
championSkinErscheinungen <- championSkinErscheinungen[championSkinErscheinungen[['Erscheinungsdatum']] != '???',]

monateZuZahlen <- data.frame(list('MonatString' = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), 'MonatZahl' = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')))
datumListe <- vector("numeric", dim(championSkinErscheinungen)[1])

championSkinErscheinungen[['TagDate']] <- sapply(championSkinErscheinungen[['Erscheinungsdatum']], function(x) as.Date(paste0(substr(x, 8, 11), monateZuZahlen[monateZuZahlen[['MonatString']] == substr(x, 4, 6), 'MonatZahl'], substr(x, 1, 2)), '%Y%m%d'))


###
# Betrachtete Zeiträume definieren: Von Beginn bis Ende, immer jeweils 3 Monate
beginnDate <- as.Date(parameter[['BeginnBetrachtung']], format = parameter[['ZeitFormatTag']])

beginnJahr <- format(beginnDate, '%Y')
beginnMonat <- as.integer(format(beginnDate, '%m'))
beginnMonatAngepasst <- strPad0(as.character(((beginnMonat - 1) %/% 3) * 3 + 1))

endeDate <- as.Date(parameter[['EndeBetrachtung']], format = parameter[['ZeitFormatTag']])

endeJahr <- format(endeDate, '%Y')
endeMonat <- as.integer(format(endeDate, '%m'))
endeMonatAngepasst <- strPad0(as.character(((endeMonat - 1) %/% 3) * 3 + 3))

# Anfangs- und Endzeitpunkte auslesen
monatsAnfangsDaten <- seq(from = as.Date(paste0(beginnJahr, beginnMonatAngepasst, '01'), '%Y%m%d'), to = as.Date(paste0(endeJahr, endeMonatAngepasst, '01'), '%Y%m%d'), by = 'months')
nMonateGesamt <- length(monatsAnfangsDaten)

anfangDaten <- monatsAnfangsDaten[((1:(nMonateGesamt %/% 3)) - 1) * 3 + 1]

monatsAnfangsDaten <- c(monatsAnfangsDaten, seq(from = monatsAnfangsDaten[nMonateGesamt], length = 2, by = 'months')[2])
monatsEndDaten <- monatsAnfangsDaten - 1

endeDaten <- monatsEndDaten[(1:(nMonateGesamt %/% 3)) * 3 + 1]

# Quartale erstellen
quartale <- sapply(anfangDaten, function(x) paste0(format(x, '%Y'), '-', as.character((as.integer(format(x, '%m')) - 1) / 3 + 1)))


# Zu einem Datenframe zusammenfügen
zeitRaeume <- data.frame(list('AnfangZeitraum' = anfangDaten, 'EndeZeitraum' = endeDaten, 'Quartal' = quartale))


###
# Erscheinungsdatum für die Champions auslesen
erscheinungenListe <- lapply(split(championSkinErscheinungen, championSkinErscheinungen[['Champion']]), function(x) min(x[['TagDate']]))
anzahlSkinsListe <- lapply(split(championSkinErscheinungen, championSkinErscheinungen[['Champion']]), function(x) dim(x)[1])
erscheinungenDf <- data.frame('Champion' = names(erscheinungenListe), 'TagDate' = unlist(erscheinungenListe), 'AnzahlSkins' = unlist(anzahlSkinsListe))
erscheinungenDf[['AnzahlSkins']] <- erscheinungenDf[['AnzahlSkins']] - 1


###
# Häufigkeit der Champions in den Patchen [insgesamt und nach Patchen]
haeufigkeitenUndRatenNachQuartalen <- list()
for (typ in c('Bannrate', 'Beliebtheit', 'Gewinnrate', 'Haeufigkeit')) {
    haeufigkeitenUndRatenNachQuartalen[[typ]] <- erscheinungenDf[, 'Champion', drop = FALSE]
}
championListe <- erscheinungenDf[, 'Champion']

for (i in 1:dim(zeitRaeume)[1]) {
    # Championlisten aufteilen
    championsVorhanden <- erscheinungenDf[erscheinungenDf[['TagDate']] <= zeitRaeume[i, 'EndeZeitraum'], 'Champion', drop = FALSE]
    #championsNochNichtVorhanden <- erscheinungenDf[erscheinungenDf[['TagDate']] > zeitRaeume[i, 'EndeZeitraum'], 'Champion', drop = FALSE]

    # Relevante Patche heraussuchen
    relevantePatche <- patchTage[(patchTage[['TagDate']] >= zeitRaeume[i, 'AnfangZeitraum']) & (patchTage[['TagDate']] <= zeitRaeume[i, 'EndeZeitraum']),]
    championInPatchenAuswahl <- championsInPatchen[championsInPatchen[['Patch']] %in% relevantePatche[['PatchNummer']], 'Champion']

    # Aufsummieren
    for (typ in c('Bannrate', 'Beliebtheit', 'Gewinnrate', 'Haeufigkeit')) {
        haeufigkeitenUndRatenNachQuartalen[[typ]][[zeitRaeume[i, 'Quartal']]] <- NA
    }

    tageListe <- seq(from = zeitRaeume[i, 'AnfangZeitraum'], to = zeitRaeume[i, 'EndeZeitraum'], by = 'day')
    for (j in 1:length(championListe)) {
        if (championListe[j] %in% championsVorhanden[['Champion']]) {
            haeufigkeitenUndRatenNachQuartalen[['Haeufigkeit']][j, zeitRaeume[i, 'Quartal']] <- sum(championInPatchenAuswahl == championListe[j])

            # Durchschnittliche Raten herauslesen
            for (typ in c('Bannrate', 'Beliebtheit', 'Gewinnrate')) {
                haeufigkeitenUndRatenNachQuartalen[[typ]][j, zeitRaeume[i, 'Quartal']] <- round(mean(approx(ratenSepariert[[typ]][[championListe[j]]][['TagDate']], ratenSepariert[[typ]][[championListe[j]]][['Wert']], tageListe)$y, na.rm = TRUE), 4)
            }
        }
    }
}

for (typ in c('Bannrate', 'Beliebtheit', 'Gewinnrate', 'Haeufigkeit')) {
    haeufigkeitenUndRatenNachQuartalen[[typ]][['Sum']] <- haeufigkeitenUndRatenNachQuartalen[[typ]][['Mean']] <- haeufigkeitenUndRatenNachQuartalen[[typ]][['NumberOfQuartalsPresent']] <- 0

    for (i in 1:dim(haeufigkeitenUndRatenNachQuartalen[[typ]])[1]) {
        haeufigkeitenUndRatenNachQuartalen[[typ]][i, 'Sum'] <- sum(haeufigkeitenUndRatenNachQuartalen[[typ]][i, - c(1, dim(haeufigkeitenUndRatenNachQuartalen[[typ]])[2] - 1, dim(haeufigkeitenUndRatenNachQuartalen[[typ]])[2])], na.rm = TRUE)
        haeufigkeitenUndRatenNachQuartalen[[typ]][i, 'NumberOfQuartalsPresent'] <- sum(!is.na(haeufigkeitenUndRatenNachQuartalen[[typ]][i, - c(1, dim(haeufigkeitenUndRatenNachQuartalen[[typ]])[2] - 1, dim(haeufigkeitenUndRatenNachQuartalen[[typ]])[2])]))
        haeufigkeitenUndRatenNachQuartalen[[typ]][i, 'Mean'] <- round(haeufigkeitenUndRatenNachQuartalen[[typ]][i, 'Sum'] / haeufigkeitenUndRatenNachQuartalen[[typ]][i, 'NumberOfQuartalsPresent'], 2)
    }

    # Abspeichern
    write.csv(haeufigkeitenUndRatenNachQuartalen[[typ]], paste0(parameter[['Ergebnisse']], 'Auswertung/', typ, '.csv'), row.names = FALSE)
}

# Rang-Korrelation zwischen dem Alter und dem Durchschnittserscheinen in den Patchnotizen
testNeuChampionsHaeufigerInPatchen <- cor.test(haeufigkeitenUndRatenNachQuartalen[['Haeufigkeit']][['NumberOfQuartalsPresent']], haeufigkeitenUndRatenNachQuartalen[['Haeufigkeit']][['Mean']])
# -> Alte Champions klar weniger in den Patchnotes als neue

# Rang-Korrelation zwischen der Häufigkeit in den Patchnotizen und den unterschiedlichen Raten und Quartalen
korrelationenFuerRaten <- list()
quartale <- names(haeufigkeitenUndRatenNachQuartalen[['Haeufigkeit']])[2:(dim(zeitRaeume)[1] + 1)]
for (typ in c('Bannrate', 'Beliebtheit', 'Gewinnrate')) {
    korrelation <- pWerteNachQuartil <- vector('numeric', length(quartale))

    for (i in 1:length(quartale)) {
        corTest <- cor.test(haeufigkeitenUndRatenNachQuartalen[[typ]][[quartale[i]]], haeufigkeitenUndRatenNachQuartalen[['Haeufigkeit']][[quartale[i]]])
        pWerteNachQuartil[i] <- corTest$p.value
        korrelation[i] <- corTest$estimate
    }
    pWerteNachQuantilKorrigiert <- p.adjust(pWerteNachQuartil, method = 'fdr')

    korrelationenFuerRaten[[typ]] <- data.frame(list('Quartal' = quartale, 'pWerte' = pWerteNachQuartil, 'pWerteKorrigiert' = pWerteNachQuantilKorrigiert, 'Korrelation' = korrelation))

    # Abspeichern
    write.csv(korrelationenFuerRaten[[typ]], paste0(parameter[['Ergebnisse']], 'Auswertung/Korrelation_p_Werte_', typ, '.csv'), row.names = FALSE)
}

# Plot nach Quartalen
farben <- c('firebrick2', 'dodgerblue2', 'forestgreen')

png(paste0(parameter[['Bilder']], '/KorrelationRatenZuHaeufigkeitInPatchen.png'), width = 1200, height = 800, res = 100)
par(mfrow = c(2,1))

# p-Werte
par(mar = c(5,4,1,1))
plot(1:length(quartale), korrelationenFuerRaten[['Bannrate']][['pWerteKorrigiert']], ylim = c(0, 1), axes = FALSE, type = 'n', xlab = 'Quartal', ylab = 'p-Value')
box()
axis(2)
axis(1, at = (0:5) * 4 + 1, labels = quartale[(0:5) * 4 + 1])

lines(1:length(quartale), korrelationenFuerRaten[['Bannrate']][['pWerteKorrigiert']], col = farben[1], lwd = 2)
lines(1:length(quartale), korrelationenFuerRaten[['Beliebtheit']][['pWerteKorrigiert']], col = farben[2], lwd = 2)
lines(1:length(quartale), korrelationenFuerRaten[['Gewinnrate']][['pWerteKorrigiert']], col = farben[3], lwd = 2)

legend('topright', legend = c('Ban rate', 'Popularity', 'Win rate'), col = farben, lwd = 2)

# Korrelationen
par(mar = c(5, 4, 1, 1))
plot(1:length(quartale), korrelationenFuerRaten[['Bannrate']][['Korrelation']], ylim = c(-0.5, 0.5), axes = FALSE, type = 'n', xlab = 'Quartal', ylab = 'Rank-correlation')
box()
axis(2)
axis(1, at = (0:5) * 4 + 1, labels = quartale[(0:5) * 4 + 1])

abline(h = 0, col = 'grey50')
lines(1:length(quartale), korrelationenFuerRaten[['Bannrate']][['Korrelation']], col = farben[1], lwd = 2)
lines(1:length(quartale), korrelationenFuerRaten[['Beliebtheit']][['Korrelation']], col = farben[2], lwd = 2)
lines(1:length(quartale), korrelationenFuerRaten[['Gewinnrate']][['Korrelation']], col = farben[3], lwd = 2)

legend('topright', legend = c('Ban rate', 'Popularity', 'Win rate'), col = farben, lwd = 2)

dev.off()


###
# Korrelation zwischen #Skins/Zeit und Beliebtheit
erscheinungenDf[['AlterChampionInTagen']] <- as.integer(as.Date(parameter[['EndeBetrachtung']], format = parameter[['ZeitFormatTag']]) - erscheinungenDf[['TagDate']])

testSkinsZuBeliebtheit <- cor.test(erscheinungenDf[['AnzahlSkins']] / erscheinungenDf[['AlterChampionInTagen']], haeufigkeitenUndRatenNachQuartalen[['Beliebtheit']][['Mean']])
# -> Populäre Champions haben mehr Skins


###
# Für alle Skins die Gewinnrate vor und nach dem Erscheinen auslesen
for (typ in c('Bannrate', 'Beliebtheit', 'Gewinnrate')) {
    championSkinErscheinungen[[paste0(typ, 'Vorher')]] <- championSkinErscheinungen[[paste0(typ, 'Nachher')]] <- NA
}

for (i in 1:dim(championSkinErscheinungen)[1]) {
    champion <- championSkinErscheinungen[i, 'Champion']
    erscheinungstag <- championSkinErscheinungen[i, 'TagDate']

    # Zeitachsen für die lineare Interpolation erstellen
    tageVorher <- seq(from = as.Date(erscheinungstag - parameter[['TageVorHerauskommenStart']]), to = as.Date(erscheinungstag - parameter[['TageVorHerauskommenEnde']]), by = 'day')
    tageNachher <- seq(from = as.Date(erscheinungstag + parameter[['TageNachHerauskommenStart']]), to = as.Date(erscheinungstag + parameter[['TageNachHerauskommenEnde']]), by = 'day')

    # Interpolation
    for (typ in c('Bannrate', 'Beliebtheit', 'Gewinnrate')) {
        rateVorher <- round(mean(approx(ratenSepariert[[typ]][[champion]][['TagDate']], ratenSepariert[[typ]][[champion]][['Wert']], tageVorher)$y, na.rm = TRUE), 4)
        rateNachher <- round(mean(approx(ratenSepariert[[typ]][[champion]][['TagDate']], ratenSepariert[[typ]][[champion]][['Wert']], tageNachher)$y, na.rm = TRUE), 4)

        championSkinErscheinungen[i, paste0(typ, 'Vorher')] <- rateVorher
        championSkinErscheinungen[i, paste0(typ, 'Nachher')] <- rateNachher
    }
}

for (typ in c('Bannrate', 'Beliebtheit', 'Gewinnrate')) {
    championSkinErscheinungen[[paste0('Delta', typ)]] <- championSkinErscheinungen[[paste0(typ, 'Nachher')]] - championSkinErscheinungen[[paste0(typ, 'Vorher')]]
}
write.csv(championSkinErscheinungen, paste0(parameter[['Ergebnisse']], 'Auswertung/DeltaGewinnrateSkins.csv'), row.names = FALSE)

# Ohne NAs
championSkinsDeltaGewinnrate <- championSkinErscheinungen[complete.cases(championSkinErscheinungen),]

# Duplikate (goldene Chromas) entfernen
championSkinsDeltaGewinnrate <- championSkinsDeltaGewinnrate[!duplicated(championSkinsDeltaGewinnrate[, c('Champion', 'TagDate')]),]

# Bild erstellen
png(paste0(parameter[['Bilder']], '/RatenaenderungenNachSkinerscheinungen.png'), width = 1200, height = 1500, res = 170)
par(mfrow = c(3,2))

par(mar = c(5,4,1,1))
hist(championSkinsDeltaGewinnrate[['DeltaGewinnrate']], xlab = 'Change in win rate after skin release', main = '')

par(mar = c(5, 4, 1, 1))
boxplot(championSkinsDeltaGewinnrate[['DeltaGewinnrate']], xlab = '', ylab = 'Change in win rate after skin release')

par(mar = c(5, 4, 1, 1))
hist(championSkinsDeltaGewinnrate[['DeltaBannrate']], xlab = 'Change in ban rate after skin release', main = '')

par(mar = c(5, 4, 1, 1))
boxplot(championSkinsDeltaGewinnrate[['DeltaBannrate']], xlab = '', ylab = 'Change in ban rate after skin release')

par(mar = c(5, 4, 1, 1))
hist(championSkinsDeltaGewinnrate[['DeltaBeliebtheit']], xlab = 'Change in popularity after skin release', main = '')

par(mar = c(5, 4, 1, 1))
boxplot(championSkinsDeltaGewinnrate[['DeltaBeliebtheit']], xlab = '', ylab = 'Change in popularity after skin release')

dev.off()

#hist(championSkinsDeltaGewinnrate[['DeltaGewinnrate']])
#vioplot(championSkinsDeltaGewinnrate[['DeltaGewinnrate']], col = 'gold', xlab = '', ylab = 'Change in winrate after skin release')

# Mann-Whitney U-Test, ob signifikant unterschiedlich von 0
uTestsRatenaenderung <- list()
mittelwerteRatenaenderung <- list()
for (typ in c('Bannrate', 'Beliebtheit', 'Gewinnrate')) {
    uTestsRatenaenderung[[typ]] <- wilcox.test(championSkinsDeltaGewinnrate[[paste0('Delta', typ)]])
    mittelwerteRatenaenderung[[typ]] <- mean(championSkinsDeltaGewinnrate[[paste0('Delta', typ)]])
}
pWerteRatenaenderung <- p.adjust(unname(sapply(uTestsRatenaenderung, function(x) x[['p.value']])), method = 'fdr')
names(pWerteRatenaenderung) <- c('Bannrate', 'Beliebtheit', 'Gewinnrate')

ratenAenderungDf <- data.frame(list('Typ' = c('Bannrate', 'Beliebtheit', 'Gewinnrate'), 'MittelwertAenderung' = unlist(mittelwerteRatenaenderung), 'pWert' = pWerteRatenaenderung))

##
# Nun das ganze noch nach Quartalen
championSkinsDeltaGewinnrate[['Quartal']] <- 0
for (i in 1:dim(zeitRaeume)[1]) {
    championSkinsDeltaGewinnrate[(championSkinsDeltaGewinnrate[['TagDate']] >= zeitRaeume[i, 'AnfangZeitraum']) & (championSkinsDeltaGewinnrate[['TagDate']] <= zeitRaeume[i, 'EndeZeitraum']), 'Quartal'] <- i
}

championSkinsDeltaGewinnrateInQuartalen <- championSkinsDeltaGewinnrate[championSkinsDeltaGewinnrate[['Quartal']] != 0,]

# Boxplots nach Quartalen
png(paste0(parameter[['Bilder']], '/RatenaenderungenNachQuartalen.png'), width = 1200, height = 1800, res = 170)
par(mfrow = c(3, 1))

par(mar = c(5,4,1,1))
boxplot(DeltaGewinnrate ~ Quartal, data = championSkinsDeltaGewinnrateInQuartalen, xlab = 'Quartal', ylab = 'Change in win rate after skin release', axes = FALSE)
abline(h = 0, col = 'grey50')
box()
axis(2)
axis(1, at = (0:5) * 4 + 1, labels = quartale[(0:5) * 4 + 1])

par(mar = c(5, 4, 1, 1))
boxplot(DeltaBannrate ~ Quartal, data = championSkinsDeltaGewinnrateInQuartalen, xlab = 'Quartal', ylab = 'Change in ban rate after skin release', axes = FALSE)
abline(h = 0, col = 'grey50')
box()
axis(2)
axis(1, at = (0:5) * 4 + 1, labels = quartale[(0:5) * 4 + 1])

par(mar = c(5, 4, 1, 1))
boxplot(DeltaBeliebtheit ~ Quartal, data = championSkinsDeltaGewinnrateInQuartalen, xlab = 'Quartal', ylab = 'Change in popularity after skin release', axes = FALSE)
abline(h = 0, col = 'grey50')
box()
axis(2)
axis(1, at = (0:5) * 4 + 1, labels = quartale[(0:5) * 4 + 1])

dev.off()

# Für die Gewinnrate noch die p-Werte berechnen und darstellen
pWerteQuartal <- numeric(dim(zeitRaeume)[1])
for (i in 1:dim(zeitRaeume)[1]) {
    #datenFuerTest <- championSkinsDeltaGewinnrateInQuartalen[championSkinsDeltaGewinnrateInQuartalen[['Quartal']] == i, 'DeltaGewinnrate']
    #datenFuerTest <- datenFuerTest[datenFuerTest != 0]
    #wilcox.test(datenFuerTest)$p.value
    pWerteQuartal[i] <- wilcox.test(championSkinsDeltaGewinnrateInQuartalen[championSkinsDeltaGewinnrateInQuartalen[['Quartal']] == i, 'DeltaGewinnrate'])$p.value
}

pWerteQuartalKorrigiert <- p.adjust(pWerteQuartal, method = 'fdr')

# Graph
png(paste0(parameter[['Bilder']], '/GewinnrateNachQuartalen.png'), width = 800, height = 500, res = 100)

par(mar = c(5,4,1,1))
plot(1:dim(zeitRaeume)[1], pWerteQuartalKorrigiert, ylim = c(0, 1), lwd = 2, type = 'l', xlab = 'Quartal', ylab = 'p-Values', axes = FALSE)
box()
axis(2)
axis(1, at = (0:5) * 4 + 1, labels = quartale[(0:5) * 4 + 1])

dev.off()


###
# Für alle Skins das Auftreten in den Patchnotizen vor/nach dem Erscheinen auslesen
championSkinsDeltaGewinnrateInQuartalen[['AnzahlInVorherigenPatchen']] <- championSkinsDeltaGewinnrateInQuartalen[['AnzahlInNachfolgendenPatchen']] <- 0
for (i in 1:dim(championSkinsDeltaGewinnrateInQuartalen)[1]) {
    # Finde die vorherigen und nachherigen Patche
    erscheinungstag <- as.Date(championSkinsDeltaGewinnrateInQuartalen[i, 'TagDate'])
    champion <- championSkinsDeltaGewinnrateInQuartalen[i, 'Champion']

    patcheVorherDf <- patchTage[patchTage[['TagDate']] <= erscheinungstag, c('PatchNummer', 'TagDate')]
    patcheVorherDf <- patcheVorherDf[order(patcheVorherDf[['TagDate']], decreasing = TRUE),]

    patcheNachherDf <- patchTage[patchTage[['TagDate']] > erscheinungstag, c('PatchNummer', 'TagDate')]
    patcheNachherDf <- patcheNachherDf[order(patcheNachherDf[['TagDate']], decreasing = FALSE),]

    # Falls keine Informationen vorhanden sind, NAs einfügen
    if ((dim(patcheVorherDf)[1] < parameter[['PatcheVorher']]) | (dim(patcheNachherDf)[1] < parameter[['PatcheNachher']])) {
        championSkinsDeltaGewinnrateInQuartalen[i, 'AnzahlInVorherigenPatchen'] <- championSkinsDeltaGewinnrateInQuartalen[i, 'AnzahlInNachfolgendenPatchen'] <- NA
    } else {
        # Durch die gefundenen Patche gehen und vorhandene Championeinträge heraussuchen
        patcheVorherListe <- patcheVorherDf[1:parameter[['PatcheVorher']], 'PatchNummer']
        patcheNachherListe <- patcheVorherDf[1:parameter[['PatcheNachher']], 'PatchNummer']

        championSkinsDeltaGewinnrateInQuartalen[i, 'AnzahlInVorherigenPatchen'] <- dim(championsInPatchen[(championsInPatchen[['Champion']] == champion) & (championsInPatchen[['Patch']] %in% patcheVorherListe),])[1]
        championSkinsDeltaGewinnrateInQuartalen[i, 'AnzahlInNachfolgendenPatchen'] <- dim(championsInPatchen[(championsInPatchen[['Champion']] == champion) & (championsInPatchen[['Patch']] %in% patcheNachherListe),])[1]
    }
}

# Tabelle abspeichern
write.csv(championSkinsDeltaGewinnrateInQuartalen, paste0(parameter[['Ergebnisse']], 'Auswertung/ChampionSkinerscheinungenAlleInformationen.csv'), row.names = FALSE)

# NAs entfernen
championsSkinsHaeufigkeitInPatchen <- championSkinsDeltaGewinnrateInQuartalen[complete.cases(championSkinsDeltaGewinnrateInQuartalen),]

# Häufigkeit der Patcheinträge mit der Basishäufigkeit eines Champions vergleichen
championsSkinsHaeufigkeitInPatchen[['RelativeHaeufigkeitInPatch']] <- (championsSkinsHaeufigkeitInPatchen[['AnzahlInNachfolgendenPatchen']] + championsSkinsHaeufigkeitInPatchen[['AnzahlInNachfolgendenPatchen']]) / (parameter[['PatcheVorher']] + parameter[['PatcheNachher']])

# Vergleichsliste erstellen mit den Anzahl betrachteten Patches und dem darin enthaltenen Vorkommen für alle Champions
erscheinungenDf[['AnzahlPatcheVorgekommen']] <- erscheinungenDf[['AnzahlPatcheInsgesamt']] <- 0
for(i in 1:length(championListe)) {
    champion <- championListe[i]
    erscheinungstag <- as.Date(erscheinungenDf[i, 'TagDate'])

    erscheinungenDf[i, 'AnzahlPatcheVorgekommen'] <- sum(championsInPatchen[['Champion']] == champion)
    erscheinungenDf[i, 'AnzahlPatcheInsgesamt'] <- dim(patchTage[patchTage[['TagDate']] >= erscheinungstag,])[1]
}

erscheinungenDf[['RelativeHaeufigkeitGesamt']] <- erscheinungenDf[['AnzahlPatcheVorgekommen']] / erscheinungenDf[['AnzahlPatcheInsgesamt']]

write.csv(erscheinungenDf, paste0(parameter[['Ergebnisse']], 'Auswertung/ChampionErscheinungenInformationen.csv'), row.names = FALSE)

# Mit den vorherigen Daten verbinden
championsSkinsHaeufigkeitInPatchen <- merge(championsSkinsHaeufigkeitInPatchen, erscheinungenDf[, c('Champion', 'RelativeHaeufigkeitGesamt')], by = 'Champion', all.x = TRUE)

championsSkinsHaeufigkeitInPatchen[['DeltaHaeufigkeitInPatchen']] <- championsSkinsHaeufigkeitInPatchen[['RelativeHaeufigkeitInPatch']] - championsSkinsHaeufigkeitInPatchen[['RelativeHaeufigkeitGesamt']]

png(paste0(parameter[['Bilder']], '/HaeufigkeitInDenPatchnotizen.png'), width = 500, height = 500, res = 100)
par(mar = c(1,4,1,1))
boxplot(championsSkinsHaeufigkeitInPatchen[['DeltaHaeufigkeitInPatchen']], ylab = 'Difference in patch note appearances skin release to no release')
dev.off()

uTestPatchHaeufigkeitUmSkinerscheinungen <- wilcox.test(championsSkinsHaeufigkeitInPatchen[['DeltaHaeufigkeitInPatchen']])

