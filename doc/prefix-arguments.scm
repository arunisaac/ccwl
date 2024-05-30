(command #:inputs (source #:type File) (output_filename #:type string)
         #:run "gcc" source ("-o" output_filename)
         #:outputs (executable
                    #:type File
                    #:binding ((glob . "$(inputs.output_filename)"))))
