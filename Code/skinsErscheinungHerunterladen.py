# -*- coding: utf-8 -*-
"""

Herunterladen der Skins mit den Erscheinungsdaten

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
    alleSkinInformationen = dict()
    
    for champ, champLolWiki in tqdm(zip(list(championsDf['Champion']), list(championsDf['ChampionLolwiki'])), total = championsDf.shape[0]):
        # champ = list(championsDf['Champion'])[0]
        browser.get(parameter['LolwikiUrl'] + champLolWiki + '/Skins')
        
        # Html abgreifen
        inneresHTML = browser.execute_script("return document.body.innerHTML")
        htmlListe = inneresHTML.splitlines()
        
        # Beginn und Ende der einzelnen Skins heraussuchen
        skinInformationen = dict()
        for feld in ['Champion', 'Name', 'Erscheinungsdatum']:
            skinInformationen[feld] = list()
        
        skinBlock = list()
        for i in range(0, len(htmlListe)):
            if '<div style="display:inline-block; margin:5px; width:342px">' in htmlListe[i]:
                skinBlock.append(i)
        
        for i in range(0, len(skinBlock)):
            skinBlockAufgeteilt = htmlListe[skinBlock[i]].split('><')
            
            beginnSkin = list()
            for j in range(0, len(skinBlockAufgeteilt)):
                if 'data-skin' in skinBlockAufgeteilt[j]:
                    beginnSkin.append(j)
            
            for j, jmax in zip(beginnSkin, beginnSkin[1:] + [len(skinBlockAufgeteilt)]):
                # Name des Skins auslesen
                skinInformationen['Champion'].append(champ)
                skinInformationen['Name'].append(skinBlockAufgeteilt[j].split('data-skin="')[1].split('"')[0])
                
                # Nach dem Erscheinungsdatum suchen
                datumGefunden = False
                for k in range(j, jmax):
                    sucheNachDatum = re.search('\d{2}-[A-Z]{1}[a-z]{2}-\d{4}', skinBlockAufgeteilt[k])
                    if sucheNachDatum is not None:
                        skinInformationen['Erscheinungsdatum'].append(sucheNachDatum.group(0))
                        
                        datumGefunden = True
                        break
                
                # Falls kein Datum gefunden wurde
                if not datumGefunden:
                    skinInformationen['Erscheinungsdatum'].append('???')

        # Erhaltene Skins abspeichern
        alleSkinInformationen[champ] = skinInformationen
    
    browser.close()
    
    # Skins zu einem Datenframe zusammenfassen
    alleSkinInformationenDf = pd.concat([pd.DataFrame(alleSkinInformationen[champ]) for champ in alleSkinInformationen.keys()], ignore_index = True)
    
    ###
    # Daten abspeichern
    pickleSpeicher = open(parameter['PfadErgebnisse'] + 'DatenVonSkins/DatenVonSkins.pkl', 'wb')
    pickle.dump(alleSkinInformationen, pickleSpeicher)
    pickleSpeicher.close()

    alleSkinInformationenDf.to_csv(parameter['PfadErgebnisse'] + 'DatenVonSkins/DatenVonSkins.csv', index = None)
        
        
