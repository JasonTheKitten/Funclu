require("funclu").enableDebug().install(_ENV, { ... })

(loadprovider "graphics" (or_
  (and_(nilToVoid(term)) "examplegui_cc")
  "examplegui_oc"
))

(using "graphics")
(defn "main" (block
  (prints "Here is some text")
  (bind "orig" (f.setTextColor (f.lookupColor "red")))
  (prints "This text is red!")
  (f.setTextColor (f.lookupColor "green"))
  (prints "And this text is green!")
  (f.setTextColor (a.orig))
  (prints "And back to the original color")
))