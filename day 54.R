load('Teams.RData')
setDT(Teams)
Teams
load('Pitching.RData')
setDT(Pitching)
Pitching

Pitching[,.SD]