BEGIN {
   main=""
   stanza=""
}
/main/ {
   if ($0 != main) {
      # first or different stanza
      print stanza$0
      main=$0
      stanza=""
   } else {
      # duplicate stanza
      main=$0
      stanza=""
   }
   next
}

{ 
   # other lines
   stanza = stanza $0 "\n"
}
