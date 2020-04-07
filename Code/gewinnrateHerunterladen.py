# -*- coding: utf-8 -*-
"""

Herunterladen der Gewinn-, Spiel- und Bannraten für alle Champions von leagueofgraphs

"""

# Pakete importieren
import os
import getpass

if getpass.getuser() == 'SUTY1':
    os.chdir('C:/Users/SUTY1/Documents/SonstigeAufgaben/LolWinrateNachSkins_V2')
    
from selenium import webdriver
import requests

import numpy as np
import pandas as pd

import pickle

from tqdm import tqdm

###
# Funktionen
def stringZuMatrix(graphNurDaten):
    graphSplit = graphNurDaten.split('],[')
    
    # Erster und letzter Eintrag korrigieren
    graphSplit[0] = graphSplit[0].split('[[')[1]
    graphSplit[-1] = graphSplit[-1].split(']]')[0]

    # Auf die 2 Spalten aufteilen    
    spalten = 2
    reihen = len(graphSplit)
    
    matrixOutput = np.zeros((reihen, spalten))
    
    for i in range(0, reihen):
        matrixOutput[i,:] = np.array(graphSplit[i].split(',')).astype(float)
        
    return(matrixOutput)


###
# Hauptteil
if __name__ == "__main__": 
    ###
    # Parameter definieren
    parameter = dict()
    
    # Pfade
    parameter['PfadErgebnisse'] = 'Ergebnisse/'
#    parameter['PfadBilder'] = 'Bilder/'
    
    # Aktuelle Championliste aus Datadragon    
    parameter['ChampionUrl'] = 'http://ddragon.leagueoflegends.com/cdn/10.7.1/data/en_US/champion.json'
    
    # Url von Leagueofgraphs
    parameter['LeagueOfGraphsUrl'] = 'https://www.leagueofgraphs.com/de/champions/stats/'


    ###
    # Liste mit den Championnamen laden
    championJson = requests.get(parameter['ChampionUrl']).json()
    championListe = list(championJson['data'].keys())

    championsDf = pd.DataFrame({'Champion': championListe})
    championsDf.to_csv(parameter['PfadErgebnisse'] + 'ChampionListe.csv', index = None)

    ###
    # Die Daten von Leagueofgraphs über den Browser herunterladen
    browser = webdriver.Firefox()
    
    ausgeleseneDaten = dict()
    for feld in ['Beliebtheit', 'Gewinnrate', 'Bannrate']:
        ausgeleseneDaten[feld] = dict()

    for champ in tqdm(championListe, total = len(championListe)):    # champ = championListe[0]
        browser.get(parameter['LeagueOfGraphsUrl'] + '/' + champ.lower())

        # Html abgreifen
        inneresHTML = browser.execute_script("return document.body.innerHTML")
        
        htmlListe = inneresHTML.splitlines()

        # Skriptteile (-> Graphen) finden
        startSkript = list()
        endeTabelle = dict()
        for i in range(0, len(htmlListe)):
            if '<script language="javascript" type="text/javascript">' in htmlListe[i]:
                startSkript.append(i)

            if 'graphFunctions.push(graphFuncgraphDD5)' in htmlListe[i]:
                endeTabelle['Beliebtheit'] = i

            if 'graphFunctions.push(graphFuncgraphDD6)' in htmlListe[i]:
                endeTabelle['Gewinnrate'] = i

            if 'graphFunctions.push(graphFuncgraphDD7)' in htmlListe[i]:
                endeTabelle['Bannrate'] = i
        
        # Korrekter Skript-Block finden, dann den data: String auslesen und konvertieren
        startSkriptNp = np.array(startSkript)
        for feld in ['Beliebtheit', 'Gewinnrate', 'Bannrate']:
            # feld = 'beliebtheit'
            startTabelle = startSkriptNp[len((endeTabelle[feld] - startSkriptNp)[(endeTabelle[feld] - startSkriptNp) > 0]) - 1]

            for i in range(startTabelle, endeTabelle[feld]):
                if 'data:' in htmlListe[i]:
                    datenString = htmlListe[i]
                    break

            # In eine Matrix umformen
            matrixAusgelesen = stringZuMatrix(datenString)
            
            # Im Dict ablegen
            ausgeleseneDaten[feld][champ] = matrixAusgelesen
    
    browser.close()

    ###
    # Daten abspeichern
    pickleSpeicher = open(parameter['PfadErgebnisse'] + 'DatenVonLeagueofgraphs/DatenVonLeagueofgraphs.pkl', 'wb')
    pickle.dump(ausgeleseneDaten, pickleSpeicher)
    pickleSpeicher.close()
    
    # Daten ebenfalls zu einem csv umwandeln und so abspeichern
    datenInListe = list()
    for feld in tqdm(ausgeleseneDaten.keys(), total = len(ausgeleseneDaten)):
        for champ in ausgeleseneDaten[feld].keys():
            matrixAlsDf = pd.DataFrame({'Zeit': ausgeleseneDaten[feld][champ][:,0], 'Wert': ausgeleseneDaten[feld][champ][:,1] / 100})
            
            # Zeit als Integer
            matrixAlsDf['Zeit'] = matrixAlsDf['Zeit'].astype(np.int64)
            
            # Informationen dazufügen
            matrixAlsDf['Typ'] = feld
            matrixAlsDf['Champion'] = champ
            
            # Ablegen
            datenInListe.append(matrixAlsDf)
            
    # Zusammenfassen und abspeichern
    ausgeleseneDatenDf = pd.concat(datenInListe, ignore_index = True)
    ausgeleseneDatenDf.to_csv(parameter['PfadErgebnisse'] + 'DatenVonLeagueofgraphs/DatenVonLeagueofgraphs.csv', index = None)






