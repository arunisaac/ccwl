(command #:inputs (source #:type File) (output_filename #:type string)
         #:run "gcc" source ("-o" output_filename #:separate? #f))
