return require("funclu").modules(_ENV)

(defn "makeLuaOptions" (newTbl
  "marshallToLua" (true)
  "marshallFromLua" (true)
  "returnSeq" (false)
  "hasSideEffects" (true)
))

(defn "makeLuaFunction" (luaf2 (f.makeLuaOptions)))

--

(defn "setTextColor_" (args "a") (f.makeLuaFunction (1) (term.setTextColor) (a.a)))
(defn "getTextColor_" (f.makeLuaFunction (0) (term.getTextColor) ()))

(defn "setTextColor" (args "color") (block
  (bind "orig" (f.getTextColor_))
  (f.setTextColor_ (a.color))
  (asIO (a.orig))
))

(defn "lookupColor" (args "color") (member (a.color) (colors)))

(exportsf "setTextColor" "lookupColor")