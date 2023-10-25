# DAV-Arbeitsgruppe: Anonymisierung und Pseudonymisierung

## GitHub mit RStudio verwenden
### Voraussetzungen
Folgende Voraussetzungen müssen vorliegen um mit RStudio GitHub verwenden zu können. 

1. Eine Installation von Git
2. Ein Account bei GitHub

Liegen diese Voraussetzung vor kann eine Verbindung zwischen RStudio und GitHub erzeugt werden.

###  Verbindung zu GitHub über RStudio erstellen
Durch eine Verbindung zu GitHub können R-Skripte nicht nur versioniert, sondern auch mit anderen Nutzer geteilt werden.

Um eine Verbindung zwischen RStudio und GitHub herstellen zu können, müssen folgende Schritte durchlaufen werden. 

1. Starte RStudio
2. Gehe zu File -> New Project -> Version Control -> Git. Es öffnet sich ein Fenster mit der Bezeichnung "Clone Git Repository" mit den Eingabemöglichkeiten 

    - Repository URL
    - Project directory name
    - Create project as subdirectory of

3. Bei dem Eingabebereich "Repository URL" muss nur die geeignete URL vom GitHub-Repository kopiert werden. Die anderen Eingabemöglichkeiten werden oder sind schon mit einem Default-Wert belegt. Die geeignete URL ist im GitHub-Repository zu finden.
4. Um die geeignete URL aus dem GitHub-Repository zu ermitteln, logge dich in Github ein und gehe auf dieses Repository. Eigentlich solltest du dich schon in dem gewünschten Repository befinden, wenn du diese Anleitung liest.
5. Auf der Startseite dieses Repositorys findest du einen grünen Button mit der Aufschrift "<> Code". 
6. Wähle den grünen Button aus. Es öffnet sich ein kleines Fenster. Hier solltest du auf der Seite "Local" sein und der Reiter "HTTPS" sollte farbig unterstrichen sein. 
7. Kopiere die darunter stehende URL "https://github.com/Hetwin/Anonymisierung_und_Pseudonymisierung.git" und füge sie in das Eingabefeld "Repository URL" in RStudio ein.
8. Gehe auf "Create Project". Nun solltest du einen lokale Klone / Kopie des Repositorys haben.
9. Führe ein Pull-Request aus um das lokale Repository auf den aktuellsten Stand zu bringen, sofern nicht schon erfolgt.

