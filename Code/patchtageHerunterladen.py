# -*- coding: utf-8 -*-
"""

Patchtage herunterladen

"""


# Pakete importieren
import os
import getpass

if getpass.getuser() == 'SUTY1':
    os.chdir('C:/Users/SUTY1/Documents/SonstigeAufgaben/LolWinrateNachSkins_V2')
    
from selenium import webdriver

import numpy as np
import pandas as pd

from bs4 import BeautifulSoup

import re

#from datetime import datetime

from tqdm import tqdm


###
# Hauptteil
if __name__ == "__main__": 
    ###
    # Parameter definieren
    parameter = dict()
    
    # Pfade
    parameter['PfadErgebnisse'] = 'Ergebnisse/'
    
    # Saison-Informationen
    parameter['SaisonNummer'] = np.arange(5, 11)
    parameter['SaisonPatchMinimum'] = np.array([1] * len(parameter['SaisonNummer']))
    parameter['SaisonPatchMaximum'] = np.array([24,24,24,24,24,7])
    parameter['SaisonBpatchWeihnachten'] = np.array([False, False, True, True, True, False])

    # Url
    parameter['LolWikiPatchUrl'] = 'https://leagueoflegends.fandom.com/wiki/V'

    ###
    # Browser öffnen
    browser = webdriver.Firefox()
    
    patchTageOriginal = dict()
    for i in tqdm(range(0, len(parameter['SaisonNummer']))):
        for j in tqdm(range(0, parameter['SaisonPatchMaximum'][i] - parameter['SaisonPatchMinimum'][i] + 1)):
            patchString = str(parameter['SaisonNummer'][i]) + '.' + str(parameter['SaisonPatchMinimum'][i] + j)
            
            # Zuerst normale Patche
            browser.get(parameter['LolWikiPatchUrl'] + patchString)

            # Html abgreifen
            inneresHTML = browser.execute_script("return document.body.innerHTML")
            html = BeautifulSoup(inneresHTML)
            
            datumText = html.find(text = 'Release Date (US)').findNext('td').contents
            
            # Ablegen
            patchTageOriginal[patchString] = datumText

            ##            
            # Dann, falls vorhanden, noch b-Patche
            if (j == parameter['SaisonPatchMaximum'][i] - parameter['SaisonPatchMinimum'][i]) & parameter['SaisonBpatchWeihnachten'][i]:
                patchString += 'b'
            
                # Zuerst normale Patche
                browser.get(parameter['LolWikiPatchUrl'] + patchString)
        
                # Html abgreifen
                inneresHTML = browser.execute_script("return document.body.innerHTML")
                html = BeautifulSoup(inneresHTML)
                
                datumText = html.find(text = 'Release Date (US)').findNext('td').contents
                
                # Ablegen
                patchTageOriginal[patchString] = datumText
            
    
    browser.close()

    ###
    # Tage auslesen
    monateDict = {'January': 1, 'February': 2, 'March': 3, 'April': 4, 'May': 5, 'June': 6, 'July': 7, 'August': 8, 'September': 9, 'October': 10, 'November': 11, 'December': 12}
    for monat in monateDict.keys():
        monateDict[monat] = str(monateDict[monat]).rjust(2, '0')
        
    tageListe = list()
        
    for patch in patchTageOriginal.keys():
        # patch = list(patchTageOriginal)[0]
        monatStr = '??'
        tagStr = '??'
        jahrStr = '??'
        
        # Monat finden
        for monat in monateDict.keys():
            if monat in patchTageOriginal[patch][0]:
                monatStr = monateDict[monat]

        # Tag und Jahr finden
        if len(patchTageOriginal[patch]) == 1:  # Kein hochgestelltes th
            suche = re.search('[a-zA-Z]+ (\d+)[a-z,]+ (\d{4})', patchTageOriginal[patch][0])
            tagStr = suche.group(1).rjust(2, '0')
            jahrStr = suche.group(2).rjust(2, '0')
        else:
            sucheTag = re.search('[a-zA-Z]+ (\d+)', patchTageOriginal[patch][0])
            tagStr = sucheTag.group(1).rjust(2, '0')

            sucheJahr = re.search('.* (\d{4})', patchTageOriginal[patch][2])
            jahrStr = sucheJahr.group(1).rjust(2, '0')
            
        # Tag zusammensetzen
        tageListe.append(jahrStr + monatStr + tagStr)

    patchTageDf = pd.DataFrame({'PatchNummer': list(patchTageOriginal.keys()), 'Tag': tageListe})


    ###
    # Abspeichern
    patchTageDf.to_csv(parameter['PfadErgebnisse'] + 'PatchTage.csv', index = None)























