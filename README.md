### HRF Boardgames App

#### Building
In **scala-js-dom-reduced** dir
```
sbt publishLocal
```

In **haunt-roll-fail** dir
```
sbt fastOptJS
```

In **good-game** dir
```
sbt "run create ../good-game-database ../haunt-roll-fail http://localhost:7070 http://localhost:7070/hrf/ 7070"
sbt "run run ../good-game-database ../haunt-roll-fail http://localhost:7070 http://localhost:7070/hrf/ 7070"
```
