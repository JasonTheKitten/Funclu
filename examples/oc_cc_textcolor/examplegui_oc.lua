local gpu = require("component").gpu
gpu.setDepth(4)
local colors = require("colors")

return require("funclu").modules(_ENV)

(defn "makeLuaOptions" (newTbl
  "marshallToLua" (true)
  "marshallFromLua" (true)
  "returnSeq" (true)
  "hasSideEffects" (true)
))

(defn "makeLuaFunction" (luaf2 (f.makeLuaOptions)))

--

(newtype "colorval" "color" "isPaletteIndex")

(defn "setForeground_" (args "a" "b") (f.makeLuaFunction (2) (gpu.setForeground) (a.a) (a.b)))
(defn "setForeground" (args"a") (match (a.a)
  (t.colorval (a.b) (a.c)) (f.setForeground_ (a.b) (a.c))
  (f.setForeground_ (a.a) (true))))
(defn "getForeground" (f"builtin.>>="
  (f.makeLuaFunction (0) (gpu.getForeground))
  (defn "" (args "a")
    (t.colorval (applySeq (a.a))))))

(defn "setTextColor" (args "color") (block
  (bind "orig" (f.getForeground))
  (f.setForeground (a.color))
  (asIO (a.orig))
))

(defn "lookupColor" (args "color") (member (a.color) (colors)))

(exportsf "setTextColor" "lookupColor")