#lang racket

(require "../../pagerank.rkt")

(with-output-to-file "output"
  (lambda ()
    (print (rank-pages '#hash((node0 . 339/31250)
       (node1 . 131103/1000000)
       (node2 . 144693/500000)
       (node3 . 15689/125000)
       (node4 . 131709/500000)
       (node5 . 179733/1000000))))))
