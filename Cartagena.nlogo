extensions [GIS]

globals [
  area-shp
  manzanas-shp
  total-humans
  moved-this-tick
  infra_servicios-raster
  infra_transporte-raster
  flood-raster      ; agregar para el raster del flood
  moved-count
  expansion
  tiempo-setup    ; Solo para ver tiempos
  tiempo-go       ; Solo para ver tiempos
  doc-loaded?     ; flag to avoid loading the same documents each setup
  last-exp-type   ; flag to know the last exp type used
  out_city        ; To know how many didnt find place so the left the city
]

patches-own [
  flood
  uso
  estrato
  isflooded?
  state
  val
  residential-status
  infra_servicios
  infra_transporte
  capacity      ; for how many agents can be in one patch!
  available?     ; To know if it is available!
  turtles_here_count    ; for the raster
]

turtles-own [
  group          ; Group number (estrato) to which the human belongs
  ; diffcount      ; Number of neighbors from a different group
  ; upset?         ; Indicates if the human is uncomfortable in their location
  moved?         ; track if turtle moved this tick

]

to setup
  reset-timer

  if not is-boolean? doc-loaded? [
    set doc-loaded? false
    set last-exp-type ""
  ]

  ifelse (not doc-loaded?) or (Expansion_Type != last-exp-type) [
  clear-all
  ;resize-world -150 150 -150 150
  reset-ticks
  clear-turtles
  set moved-count 0
  cargar-shapefiles
  set_limit_available
  cargar-raster
  createHumans
  set tiempo-setup timer
  set doc-loaded? true
  set last-exp-type Expansion_Type
  print(word "Tardo: " tiempo-setup "segundos.")
  ][reset-pop]
end

to reset-pop
  reset-ticks
  clear-turtles
  createHumans
  ask patches [set turtles_here_count 0]
end

to capacity_assignm
  ask patches [
    let str estrato
    ifelse is-string? str [
      set str read-from-string str
    ][
      ifelse (is-number? str and ((str <= 0) or (str >= 0))) [
      ][
        set str 0
      ]
    ]
    if str != 0 [
      let max-capacity (15 - str)  ;; Revisar si tiene sentido!
      set capacity random  max-capacity + 1
      if capacity < 1 [ set capacity 1 ]
    ]
  ]
end

to set_limit_available
  foreach [1 2 3 4 5 6] [
      [x] ->
      let total_patches patches with [
        (uso = "residencial") or (uso = "expansion") and flood = 0 and estrato = x
      ]
      let total_patches_num count total_patches
      let porcentage not_available * total_patches_num / 100
      ask n-of porcentage total_patches
      [
        set available? false
      ]
    ]
end

to cargar-shapefiles
  set area-shp gis:load-dataset "To_Model/areaproject2.shp"
  ask patches gis:intersecting area-shp [
    set pcolor gray - 2
  ]
  ; Call everything!
  set manzanas-shp gis:load-dataset "To_Model/MANZANA_CAT6.shp"
  set flood-raster gis:load-dataset "To_Model/floodedRast.asc"
  gis:apply-raster flood-raster flood
  gis:apply-coverage manzanas-shp "GRUPO" estrato

  ifelse Expansion_Type = "Sin" [
    gis:apply-coverage manzanas-shp "USO" uso
  ][
    ifelse Expansion_Type = "Normal" [
    gis:apply-coverage manzanas-shp "USOBASE" uso
    ][gis:apply-coverage manzanas-shp "USOEXTEND" uso]
  ]

  ask patches [ ; modificado para eficiencia, evitar preguntar a todos los patches
    set turtles_here_count 0
    if member? flood [0 1] [
      set state flood
      set pcolor item flood [gray cyan]
    ]
    if flood = 0 [if ((uso = "residencial") or (uso = "expansion")) [set available? true]]
    if uso = "expansion" [
      set pcolor pink]
    if is-string? estrato [
      set estrato read-from-string substring estrato 1 2]
  ]
  set-patch-size 2
  gis:set-world-envelope gis:envelope-of area-shp
  capacity_assignm

end

to cargar-raster
  ;; Load and apply raster data
  set infra_servicios-raster gis:load-dataset "To_Model/infra_servicios3.asc"
  ;gis:apply-coverage infra_servicios-raster "band1" infra_servicios
  gis:apply-raster infra_servicios-raster infra_servicios

  set infra_transporte-raster gis:load-dataset "To_Model/infra_transporte.asc"
  gis:apply-raster infra_transporte-raster infra_transporte

end

to createHumans
  clear-turtles
  ;let proportions [0.38 0.32 0.15 0.04 0.01 0.005] ; Proportions for each socio-economic group
  let household-count [976 4873 1528 1089 1554 1892] ; Exact counts for each group (9915 household)


  ask patches with [state = 1] [
    set val random-float 1
  ]

  let total-humans-to-create (count patches with [state = 1])

  let current-human-id 1
  let i 1

  foreach household-count [
    ;let group-proportion item (i - 1) household-count
    ;let num-humans-in-group floor (group-proportion * total-humans-to-create)
    let num-humans-in-group item (i - 1) household-count

    let residents patches with [state = 1]

    repeat num-humans-in-group [
      ask one-of residents [
        sprout 1 [
          set shape "person"
          set size 2
          set group i
          set val [val] of patch-here
        ]
      ]
    ]
    set i i + 1
  ]

  ; Assign a color to each group for visualization
  let colores [red blue green orange brown magenta]
  ask turtles [;1   2     3     4      5      6
    set color item (group - 1) colores
  ]

  ;print (word "Group 1: " count turtles with [group = 1])
  ;print (word "Group 2: " count turtles with [group = 2])
  ;print (word "Group 3: " count turtles with [group = 3])
  ;print (word "Group 4: " count turtles with [group = 4])
  ;print (word "Group 5: " count turtles with [group = 5])
  ;print (word "Group 6: " count turtles with [group = 6])

  set total-humans count turtles
  set out_city [0 0 0 0 0 0]
end

to migrate
  ;if not is-list? out_city [
  ;  user-message (word "⚠ ERROR: out_city ya no es lista. Valor actual = " out_city
  ;                       "  (tipo: " out_city ")")
  ;  stop
  ;]
  ;if (group - 1) >= length out_city [
  ;  user-message (word "⚠ ERROR: índice fuera de rango. group = " group
  ;                       " out_city = " out_city)
  ;  stop
  ;]

  ;let old-value item (group - 1) out_city
  ;set out_city replace-item (group - 1) out_city (old-value + 1)
  ;die
end

to go
  if ticks = 0 [ reset-timer ]
  tick
  ifelse RunMode = "Normal" [move][moveEx]
  show "No more humans can move. Simulation stopped."
  ;export-parameters
  ;export-human-locations
  ;export-matrix-groups
  ;export-raster
  set tiempo-go timer
  print (word "Tiempo de simulación: " tiempo-go " segundos.")
  stop
end

to move
  ask turtles [
    set moved? false
    ; set diffcount count (turtles-on neighbors) with [group != [group] of myself]

    if [flood] of patch-here = 1 [
      let opciones patches with [
        estrato = [group] of myself and
        ((uso = "residencial") or (uso = "expansion")) and
        flood = 0 ;and
        ; available? = true and
        ; count turtles-here < capacity
      ]
      ifelse any? opciones [
        let destino one-of opciones
        move-to destino
        set moved-count moved-count + 1
        set moved? true
        ask destino [
            set turtles_here_count turtles_here_count + 1
          ]
      ] [migrate]
    ]
  ]
end

to moveEx
  ask turtles [
    set moved? false
    ifelse member? group [5 6][
      if [flood] of patch-here = 1 [
      let opciones patches with [
        estrato = [group] of myself and
        ((uso = "residencial") or (uso = "expansion")) and
        flood = 0 and
        ; available? = true and
        count turtles-here < capacity
      ]
      ifelse any? opciones [
        let destino one-of opciones
        move-to destino
        set moved-count moved-count + 1
        set moved? true
        ask destino [
            set turtles_here_count turtles_here_count + 1
          ]
        ][migrate]
     ]
    ][
    if [flood] of patch-here = 1 [
      let priority1 10  ;; for infra_servicio
      let priority2 10  ;; for infra_transport

      while [priority1 >= 1 and not moved?] [
        ;; first filter patches by transport
        let opciones patches with [
          estrato = [group] of myself and
          ((uso = "residencial") or (uso = "expansion")) and
          infra_transporte = priority1 and
          flood = 0 and
          available? = true and
          count turtles-here < capacity
        ]
        while [priority2 >= 1 and not moved?] [
          ;; narrow down opciones by infraestructure priority2
          let disponibles opciones with [
            infra_servicios  = priority2
          ]

          if any? disponibles [
            let destino one-of disponibles
            move-to destino
            set moved? true
          ]
          set priority2 priority2 - 1
        ]

        set priority1 priority1 - 1
        set priority2 10  ;; reset for next infra_servicios priority level
      ]
        if not moved? [migrate]
    ]
   ]
  ]
end

to export-human-locations
    let full-path (word "Outputs/human-locations-run-" behaviorspace-run-number
                 "-type-" Expansion_Type
                 "-mode-" RunMode)

    let SegregationExport gis:turtle-dataset turtle-set turtles
    gis:store-dataset SegregationExport full-path
    print (word "Shapefile exported to ")
end

to export-raster
    let full-path (word "Outputs/human-locations-run-" behaviorspace-run-number
                 "-type-" Expansion_Type
                 "-mode-" RunMode ".asc")

  let test  gis:patch-dataset turtles_here_count
  gis:store-dataset test full-path
  print (word "Raster exported to" full-path)
end

to export-parameters
  let filena word "Outputs/" export-filename
  let filename word filena ".csv"
  file-open filename
  file-print "total-humans,moved-count"
  file-print (word total-humans ","  moved-count)
  file-close
end

to export-matrix-groups
  let world-matrix []
  foreach reverse sort remove-duplicates [ pycor ] of patches [ y_cor ->
    let row []

    foreach sort remove-duplicates [ pxcor ] of patches [ x_cor ->
      let p patch x_cor y_cor

      ;; start a vector of 6 zeros
      let vec (list 0 0 0 0 0 0)

      ask turtles-on p [
        let g group
        set vec replace-item (g - 1) vec (item (g - 1) vec + 1)
      ]
      set row lput vec row
    ]
    set world-matrix lput row world-matrix
  ]

  ;; export as CSV
  let filename (word "Outputs/matrix-groups-run-" behaviorspace-run-number
                     "-type-" Expansion_Type
                     "-mode-" RunMode ".csv")
  file-open filename

  foreach world-matrix [ row ->
    let row-strings map [ vec -> (word "[" (reduce [ [a b] -> (word a " " b) ] vec) "]") ] row
    let line reduce [ [a b] -> (word a ";" b) ] row-strings
    file-print line
  ]

  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
246
10
856
621
-1
-1
2.0
1
1
1
1
1
0
1
1
1
-150
150
-150
150
0
0
0
ticks
30.0

BUTTON
13
10
151
43
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
165
10
233
43
Go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
13
335
196
380
Number of households
total-humans
17
1
11

MONITOR
871
10
1019
55
Households relocated
count turtles with [\n  ((uso = \"residencial\") or (uso = \"expansion\")) \n  and [flood] of patch-here = 0\n]
17
1
11

MONITOR
869
147
939
192
Estrato 3
(count turtles with [\n  ((uso = \"residencial\") or (uso = \"expansion\"))\n  and flood = 0 and\n  group = 3\n] * 100) / count turtles with [group = 3]
1
1
11

MONITOR
870
98
940
143
Estrato 1
(count turtles with [\n  ((uso = \"residencial\") or (uso = \"expansion\"))\n  and flood = 0 and\n  group = 1\n] * 100) / count turtles with [group = 1]
1
1
11

MONITOR
948
98
1018
143
Estrato 2
(count turtles with [\n  ((uso = \"residencial\") or (uso = \"expansion\"))\n  and flood = 0 and\n  group = 2\n] * 100) / count turtles with [group = 2]
1
1
11

MONITOR
949
148
1019
193
Estrato 4
(count turtles with [\n  ((uso = \"residencial\") or (uso = \"expansion\")) \n  and flood = 0 and\n  group = 4\n] * 100) / count turtles with [group = 4]
1
1
11

MONITOR
869
197
939
242
Estrato 5
(count turtles with [\n  ((uso = \"residencial\") or (uso = \"expansion\"))\n  and flood = 0 and\n  group = 5\n] * 100) / count turtles with [group = 5]
1
1
11

MONITOR
949
197
1020
242
Estrato 6
(count turtles with [\n  ((uso = \"residencial\") or (uso = \"expansion\"))\n  and flood = 0 and\n  group = 6\n] * 100) / count turtles with [group = 6]
1
1
11

TEXTBOX
14
299
197
341
Porcentaje de patches deshabilitados (0= ninguno; 100= todos)
11
0.0
1

INPUTBOX
15
655
200
715
export-filename
HousesholdLwithExp-302VH
1
0
String

SWITCH
18
608
202
641
export-enabled
export-enabled
1
1
-1000

TEXTBOX
15
100
167
142
Sin: Sin expansion\nNormal: Expansion moderada\nExtendida: Expansion fuerte
11
0.0
1

TEXTBOX
15
199
214
262
Normal: Sin considerar transporte e infraestructura\nInfra and Transport: Considerando transporte e infraestructura
11
0.0
1

TEXTBOX
23
583
260
611
Do you need to export the result?
11
0.0
1

MONITOR
12
391
98
436
NIL
tiempo-setup
17
1
11

MONITOR
110
392
196
437
NIL
tiempo-go
17
1
11

SLIDER
14
261
196
294
not_available
not_available
0
100
0.0
25
1
NIL
HORIZONTAL

CHOOSER
13
51
152
96
Expansion_Type
Expansion_Type
"Sin" "Normal" "Extendida"
0

TEXTBOX
871
61
1021
89
Porcentaje de agentes reubicados por estrato
11
0.0
1

CHOOSER
13
148
153
193
RunMode
RunMode
"Normal" "Infra and Transport"
0

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="ExperimentTest" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <postRun>export-raster</postRun>
    <enumeratedValueSet variable="Expansion_Type">
      <value value="&quot;Sin&quot;"/>
      <value value="&quot;Normal&quot;"/>
      <value value="&quot;Extendida&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RunMode">
      <value value="&quot;Normal&quot;"/>
      <value value="&quot;Infra and Transport&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="not_available">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
