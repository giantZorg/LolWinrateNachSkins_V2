# -*- coding: utf-8 -*-
"""

Herunterladen der Patchhistorie von Lolwiki

"""


# Pakete importieren
import os
import getpass

if getpass.getuser() == 'SUTY1':
    os.chdir('C:/Users/SUTY1/Documents/SonstigeAufgaben/LolWinrateNachSkins_V2')
    
from selenium import webdriver

import pandas as pd
import re

import pickle

import copy

from bs4 import BeautifulSoup

from tqdm import tqdm


###
# Hauptteil
if __name__ == "__main__": 
    ###
    # Parameter definieren
    parameter = dict()
    
    # Pfade
    parameter['PfadErgebnisse'] = 'Ergebnisse/'
    
    # Url von Lolwiki
    parameter['LolwikiUrl'] = 'https://leagueoflegends.fandom.com/wiki/'
    
    
    ###
    # Championliste einlesen
    championsDf = pd.read_csv(parameter['PfadErgebnisse'] + 'ChampionListe.csv')

    # Championnamen für Lolwiki korrigieren
    korrekturDict = {'AurelionSol': 'Aurelion_Sol', 'Chogath': 'Cho\'Gath', 'DrMundo': 'Dr._Mundo', 'JarvanIV': 'Jarvan_IV', 'Kaisa': 'Kai\'Sa', 'Khazix': 'Kha\'Zix', 'KogMaw': 'Kog\'Maw', 'Leblanc': 'LeBlanc', 'LeeSin': 'Lee_Sin', 'MasterYi': 'Master_Yi', 'MissFortune': 'Miss_Fortune', 'MonkeyKing': 'Wukong', 'RekSai': 'Rek\'Sai', 'TahmKench': 'Tahm_Kench', 'TwistedFate': 'Twisted_Fate', 'Velkoz': 'Vel\'Koz', 'XinZhao': 'Xin_Zhao'}
    championsDf['ChampionLolwiki'] = copy.deepcopy(championsDf['Champion'])
    for champ in korrekturDict.keys():
        championsDf.loc[championsDf['Champion'] == champ, 'ChampionLolwiki'] = korrekturDict[champ]
    
    # Browser öffnen
    browser = webdriver.Firefox()
    
    
    ###
    # Durch alle Champions gehen
    alleChampsInPatcheListe = list()
    
    for champ, champLolWiki in tqdm(zip(list(championsDf['Champion']), list(championsDf['ChampionLolwiki'])), total = championsDf.shape[0]):
        # champ = list(championsDf['Champion'])[18]
        browser.get(parameter['LolwikiUrl'] + champLolWiki + '/History')
        
        # Html abgreifen
        inneresHTML = browser.execute_script("return document.body.innerHTML")
        
        # Den Teil Patch-History abgreifen
        html = BeautifulSoup(inneresHTML)
        patchGeschichte = html.find(id="Patch_History").findNext('div').contents
        
        # Durch die Linien gehen und alle vorhandenen Patches auslesen
        patchVorgekommenListe = list()
        for i in range(0, len(patchGeschichte)):
            reSuche = re.search('>V\d{1,2}.\d{1,2}b{0,1}<', str(patchGeschichte[i]))
            
            if reSuche is not None:
                patch = reSuche.group(0)[2:-1]
                patchVorgekommenListe.append(patch)
    
        # Werte als Datenframe ablegen
        if len(patchVorgekommenListe):
            patchVorgekommenSortiert = list(reversed(sorted(set(patchVorgekommenListe))))
            
            champInPatchesDf = pd.DataFrame({'Champion': [champ] * len(patchVorgekommenSortiert), 'Patch': patchVorgekommenSortiert})
            alleChampsInPatcheListe.append(champInPatchesDf)
    
    browser.close()
    
    
    ###
    # Daten zusammenfassen und abspeichern
    alleChampsInPatche = pd.concat(alleChampsInPatcheListe, ignore_index = True)

    pickleSpeicher = open(parameter['PfadErgebnisse'] + 'DatenVonPatchen/DatenVonPatchen.pkl', 'wb')
    pickle.dump(alleChampsInPatche, pickleSpeicher)
    pickleSpeicher.close()

    alleChampsInPatche.to_csv(parameter['PfadErgebnisse'] + 'DatenVonPatchen/DatenVonPatchen.csv', index = None)
           
    
    
    
    
    
    
    
    
    
    
    
    
    