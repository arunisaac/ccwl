(command #:inputs (number #:type int)
         #:run "echo" "$(1 + inputs.number)"
         #:requirements ((InlineJavascriptRequirement)))
