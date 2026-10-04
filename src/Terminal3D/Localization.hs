module Terminal3D.Localization where

data Local = Local { english :: String, spanish :: String, latin :: String, german :: String } deriving (Show, Eq)

appstatePosition, appstateRotation, appstateProjection, appstateScreenSize, appstateSSAA, appstatePPAA, appstateLanguage :: Local
appstatePosition   = Local "Position"   "Posicion"   "Positio"          "Position"
appstateRotation   = Local "Rotation"   "Rotacion"   "Rotatio"          "Rotation"
appstateProjection = Local "Projection" "Proyeccion" "Proiectio"        "Projektion"
appstateScreenSize = Local "ScreenSize" "Pantalla"   "MagnitudoTabulae" "Bildschirmgroesse"
appstateSSAA       = Local "SSAA"       "SSAA"        "SSAA"             "SSAA"
appstatePPAA       = Local "PPAA"       "PPAA"        "PPAA"             "PPAA"
appstateLanguage   = Local "Language"   "Idioma"      "Lingua"           "Sprache"

commandQuit, commandHelp :: Local
commandQuit = Local "quit" "salir" "exi"      "beenden"
commandHelp = Local "help" "ayuda" "auxilium" "hilfe"

inputTypeGaussian, inputTypeBox, inputTypeNone, inputTypeWidth, inputTypeHeight :: Local
inputTypeGaussian = Local "Gaussian" "Gaussiano" "Gaussianus" "Gauss"
inputTypeBox      = Local "box"      "caja"      "capsa"      "Box"
inputTypeNone     = Local "none"     "ninguno"   "nullus"     "keine"
inputTypeWidth    = Local "width"    "ancho"     "latitudo"   "Breite"
inputTypeHeight   = Local "height"   "alto"      "altitudo"   "Hoehe"

inputTypeProjection, inputTypeAffine :: Local
inputTypeProjection = Local "projection" "proyeccion" "proiectio" "Projektion"
inputTypeAffine     = Local "affine"     "afin"       "affinis"   "affin"

feedbackGoodbye, feedbackUnknownCommand, feedbackHelpUnknownCommand, feedbackStartText, feedbackUsage, feedbackPositiveIntegers, feedbackInteger :: Local
feedbackGoodbye            = Local "Goodbye."                  "Adios."                         "Vale."                               "Auf Wiedersehen."
feedbackUnknownCommand     = Local "Unknown command"           "Comando desconocido"            "Mandatum ignotum"                    "Unbekannter Befehl"
feedbackHelpUnknownCommand = Local "Try '?' for help."         "Prueba '?' para ver la ayuda."  "Tempta '?' ad auxilium."             "Versuche '?' fuer Hilfe."
feedbackStartText          = Local "Command (or help/quit)"    "Comando (o ayuda/salir)"        "Mandatum (vel auxilium/exi)"         "Befehl (oder hilfe/beenden)"
feedbackUsage              = Local "Usage"                     "Uso"                            "Usus"                                "Verwendung"
feedbackPositiveIntegers   = Local "Must be Positive Integers" "Deben ser enteros positivos"    "Debent esse numeri integri positivi" "Muessen positive ganze Zahlen sein"
feedbackInteger            = Local "Not a valid integer"       "No es un entero valido"         "Non est numerus integer validus"     "Keine gueltige ganze Zahl"

commandDoNothing, commandForward, commandBackward, commandLeft, commandRight :: Local
commandDoNothing = Local "do nothing"    "no hacer nada"              "nihil agere"           "nichts tun"
commandForward   = Local "move forward"  "avanzar"                    "progredi"              "vorwaerts bewegen"
commandBackward  = Local "move backward" "retroceder"                 "regredi"               "rueckwaerts bewegen"
commandRight     = Local "strafe right"  "desplazarse a la derecha"   "ad dextram transire"   "seitlich nach rechts bewegen"
commandLeft      = Local "strafe left"   "desplazarse a la izquierda" "ad sinistram transire" "seitlich nach links bewegen"

commandTurnLeft, commandTurnRight, commandTurnUp, commandTurnDown :: Local
commandTurnLeft  = Local "turn left"  "girar a la izquierda" "ad sinistram vertere" "nach links drehen"
commandTurnRight = Local "turn right" "girar a la derecha"   "ad dextram vertere"   "nach rechts drehen"
commandTurnUp    = Local "turn up"    "girar hacia arriba"   "sursum vertere"       "nach oben drehen"
commandTurnDown  = Local "turn down"  "girar hacia abajo"    "deorsum vertere"      "nach unten drehen"

data Language = Language { langName :: Local, langPick :: Local -> String }

langEnglish, langSpanish, langLatin, langGerman :: Language
langEnglish = Language (Local "English" "Ingles"  "Anglica"   "Englisch") english
langSpanish = Language (Local "Spanish" "Espanol" "Hispanica" "Spanisch") spanish
langLatin   = Language (Local "Latin"   "Latin"   "Latina"    "Latein")   latin
langGerman  = Language (Local "German"  "Aleman"  "Germanica" "Deutsch")  german

allLanguages :: [Language]
allLanguages = [ langEnglish, langSpanish, langLatin, langGerman ]