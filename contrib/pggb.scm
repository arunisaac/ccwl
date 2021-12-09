;; An implementation of the pangenome graph builder shell script at
;; https://github.com/pangenome/pggb in ccwl

(define wfmash
  (command #:inputs
           (input-fasta #:label "input FASTA/FASTQ file")
           (segment-length #:type int)
           (block-length #:type int)
           (no-merge-segments #:type boolean?)
           (no-split #:type boolean?)
           (map-pct-id #:type float)
           (n-secondary #:type int)
           (mash-kmer #:type int #:default 16)
           (threads #:type int #:default 1)
           #:run "wfmash" ("-s" segment-length) ("-l" block-length) ("-M" no-merge-segments)
           ("-N" no-split) ("-p" map-pct-id) ("-n" n-secondary) ("-k" mash-kmer) ("-t" threads)
           input-fasta input-fasta
           #:outputs (mapper-out #:type stdout)))

(define seqwish
  (command #:inputs
           (input-fasta #:label "input FASTA/FASTQ file")
           mapper-out
           (min-match-len #:type int #:default 19)
           (transclose-batch #:type int #:default 1000000)
           (threads #:type int #:default 1)
           #:run "seqwish" "-t" threads "-s" input-fasta "-p" mapper-out "-k" min-match-len
           "-g" "seqwish.gfa" "-B" transclose-batch "-P"
           #:outputs (seqwish-out #:binding '((glob . "seqwish.gfa")))))

(define smoothxg
  (command #:inputs
           seqwish-out
           (block-weight-max #:type int #:default 10000)
           (block-id-min #:type int #:default 0)
           (ratio-contain #:type int #:default 0)
           (path-jump-max #:type int #:default 5000)
           (edge-jump-max #:type int #:default 5000)
           (poa-length-max #:type int #:default 10000)
           (poa-params #:type string #:default "1,4,6,2,26,1")
           (consensus-spec #:type string #:default "10,100,1000,10000")
           (threads #:type int #:default 1)
           #:run "smoothxg" ("-t" threads) ("-g" seqwish-out) ("-w" block-weight-max) "-M"
           "-J" "0.7" "-K" "-G" "150" ("-I" block-id-min) ("-j" path-jump-max)
           ("-e" edge-jump-max) ("-l" poa-length-max) ("-p" poa-params)
           #:outputs
           (smoothxg-graph #:binding '((glob . "smoothxg_out.gfa")))
           (smoothxg-consensus #:binding '((glob . "smoothxg_out.consensus@10__y_0_1000000.gfa")))
           (smoothxg-alignment #:binding '((glob . "smoothxg_out.maf")))))

(define odgi-build
  (command #:inputs
           (threads #:type int #:default 1)
           smoothxg-graph
           #:run "odgi" "build" ("-t" threads) "-P" ("-g" smoothxg-graph) "-o" "odgi_out.og"
           #:outputs (odgi-out #:binding '((glob . "odgi_out.og")))))

(workflow ((segment-length #:type int
                           #:label "segment length for mapping")
           (block-length #:type int
                         #:label "minimum block length filter for mapping")
           (no-merge-segments #:type boolean?
                              #:label "do not merge successive mappings")
           (no-split #:type boolean?
                     #:label "disable splitting of input sequences during mapping")
           (map-pct-id #:type float
                       #:label "percent identity in the wfmash or edyeet mashmap step")
           (n-secondary #:type int
                        #:label "number of secondary mappings to retain in 'map' filter mode")
           (mash-kmer #:type int
                      #:label "kmer size for mashmap"
                      #:default 16)
           (input-fasta #:label "input FASTA/FASTQ file")
           (min-match-len #:type int
                          #:label "ignore exact matches below this length"
                          #:default 19)
           (transclose-batch #:type int
                             #:label "number of bp to use for transitive closure batch"
                             #:default 1000000)
           (block-weight-max #:type int
                             #:label "maximum seed sequence in block"
                             #:default 10000)
           (block-id-min #:type int
                         #:label "split blocks into groups connected by this identity threshold"
                         #:default 0)
           (ratio-contain #:type int
                          #:label "minimum short length / long length ratio to compare sequences for the containment metric in the clustering"
                          #:default 0)
           (path-jump-max #:type int
                          #:label "maximum path jump to include in block"
                          #:default 5000)
           (edge-jump-max #:type int
                          #:label "maximum edge jump before breaking"
                          #:default 5000)
           (poa-length-max #:type int
                           #:label "maximum sequence length to put into POA"
                           #:default 10000)
           (poa-params #:type string
                       #:label "score parameters for POA in the form of match,mismatch,gap1,ext1,gap2,ext2"
                       #:default "1,4,6,2,26,1")
           (consensus-spec #:type string
                           #:label "consensus graph specification: write the consensus graph to BASENAME.cons_[spec].gfa; where each spec contains at least a min_len parameter (which defines the length of divergences from consensus paths to preserve in the output), optionally a file containing reference paths to preserve in the output, a flag (y/n) indicating whether we should also use the POA consensus paths, a minimum coverage of consensus paths to retain (min_cov), and a maximum allele length (max_len, defaults to 1e6); implies -a; example: cons,100,1000:refs1.txt:n,1000:refs2.txt:y:2.3:1000000,10000"
                           #:default "10,100,1000,10000")
           (threads #:type int
                    #:label "number of compute threads to use in parallel steps"
                    #:default 1))
  (pipe (wfmash #:input-fasta input-fasta
                #:segment-length segment-length
                #:block-length block-length
                #:no-merge-segments no-merge-segments
                #:no-split no-split
                #:map-pct-id map-pct-id
                #:n-secondary n-secondary
                #:mash-kmer mash-kmer
                #:threads threads)
        (seqwish #:input-fasta input-fasta
                 #:mapper-out mapper-out
                 #:min-match-len min-match-len
                 #:transclose-batch transclose-batch
                 #:threads threads)
        (smoothxg #:seqwish-out seqwish-out
                  #:block-weight-max block-weight-max
                  #:block-id-min block-id-min
                  #:ratio-contain ratio-contain
                  #:path-jump-max path-jump-max
                  #:edge-jump-max edge-jump-max
                  #:poa-length-max poa-length-max
                  #:poa-params poa-params
                  #:consensus-spec consensus-spec
                  #:threads threads)
        (odgi-build #:smoothxg-graph smoothxg-graph
                    #:threads threads)))
