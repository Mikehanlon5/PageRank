#lang racket

;; Assignment 1: Implementing PageRank
;;
;; PageRank is a popular graph algorithm used for information
;; retrieval and was first popularized as an algorithm powering
;; the Google search engine.

(provide graph?
         pagerank?
         num-pages
         num-links
         get-backlinks
         mk-initial-pagerank
         step-pagerank
         iterate-pagerank-until
         rank-pages)

(define g0 '((n2 n0)
             (n1 n4)
             (n4 n0)
             (n1 n3)
             (n2 n1)
             (n0 n1)
             (n3 n4)
             (n0 n4)
             (n4 n1)
             (n4 n2)
             (n1 n0)))

(define (is-in? val lst)
    (cond
      [(empty? lst) #f]
      [(equal? val (car lst)) #t]
      [else (is-in? val (cdr lst))]
      )
    )
  (define (pages graph lst)
    (cond
      [(empty? graph) lst]
      [(is-in? (caar graph) lst)
       (cond
         [(is-in? (car (cdar graph)) lst) (pages (cdr graph) lst)]
         [else (pages (cdr graph) (cons (car (cdar graph)) lst))]
         )]
      [(is-in? (car (cdar graph)) lst) (pages (cdr graph) (cons (caar graph) lst))]
      [else (pages (cdr graph) (cons (caar graph) (cons (car (cdar graph)) lst)))]
    )
   )

;; This program accepts graphs as input. Graphs are represented as a
;; list of links, where each link is a list `(,src ,dst) that signals
;; page src links to page dst.
;; (-> any? boolean?)
(define (graph? glst)
  (and (list? glst)
       (andmap
        (lambda (element)
          (match element
                 [`(,(? symbol? src) ,(? symbol? dst)) #t]
                 [else #f]))
        glst)))

;; Our implementation takes input graphs and turns them into
;; PageRanks. A PageRank is a Racket hash-map that maps pages (each 
;; represented as a Racket symbol) to their corresponding weights,
;; where those weights must sum to 1 (over the whole map).
;; A PageRank encodes a discrete probability distribution over pages.
;;
;; The test graphs for this assignment adhere to several constraints:
;; + There are no "terminal" nodes. All nodes link to at least one
;; other node.
;; + There are no "self-edges," i.e., there will never be an edge `(n0
;; n0).
;; + To maintain consistenty with the last two facts, each graph will
;; have at least two nodes.
;; + There will be no "repeat" edges. I.e., if `(n0 n1) appears once
;; in the graph, it will not appear a second time.
;;
;; (-> any? boolean?)
(define (pagerank? pr)
  (and (hash? pr)
       (andmap symbol? (hash-keys pr))
       (andmap rational? (hash-values pr))
       ;; All the values in the PageRank must sum to 1. I.e., the
       ;; PageRank forms a probability distribution.
       (= 1 (foldl + 0 (hash-values pr)))))

;; Takes some input graph and computes the number of pages in the
;; graph. For example, the graph '((n0 n1) (n1 n2)) has 3 pages, n0,
;; n1, and n2.
;;
;; (-> graph? nonnegative-integer?)
(define (num-pages graph)
  (length (pages graph '()))
 )

;; Takes some input graph and computes the number of links emanating
;; from page. For example, (num-links '((n0 n1) (n1 n0) (n0 n2)) 'n0)
;; should return 2, as 'n0 links to 'n1 and 'n2.
;;
;; (-> graph? symbol? nonnegative-integer?)
(define (num-links graph page)
     (cond
       [(empty? graph) 0]
       [(equal? (caar graph) page) (+ 1 (num-links (cdr graph) page))]
       [else (+ 0 (num-links (cdr graph) page))]
      )
)



;; Calculates a set of pages that link to page within graph. For
;; example, (get-backlinks '((n0 n1) (n1 n2) (n0 n2)) n2) should
;; return (set 'n0 'n1).
;;
;; (-> graph? symbol? (set/c symbol?))
(define (get-backlinks graph page)
  (define link-pages (map (lambda (x) (car x)) graph))
  (define linked-pages (map (lambda (x) (cadr x)) graph))
  (define result (map (lambda (ele0 ele1) (if (equal? ele1 page) ele0 #f)) link-pages linked-pages))
  (define removef (filter (lambda (ele) (not (equal? #f ele))) result))
  ;;(define rev (reverse removef))
  ;;define (element-adder lst s)
  ;;  (cond
  ;;    [(empty? lst) s]
  ;;    [else (element-adder (cdr lst) (set-add s (car lst)))]
  ;;  )
  ;; )
  removef
  )

;; Generate an initial pagerank for the input graph g. The returned
;; PageRank must satisfy pagerank?, and each value of the hash must be
;; equal to (/ 1 N), where N is the number of pages in the given
;; graph.
;; (-> graph? pagerank?)
(define (mk-initial-pagerank graph)
  (define N (num-pages graph))
  (define lst-pages (pages graph '()))
  (foldl (lambda (next-key hash-accum) (hash-set hash-accum next-key (/ 1 N))) (hash) lst-pages)
  )

;; Perform one step of PageRank on the specified graph. Return a new
;; PageRank with updated values after running the PageRank
;; calculation. The next iteration's PageRank is calculated as
;;
;; NextPageRank(page-i) = (1 - d) / N + d * S
;;
;; Where:
;;  + d is a specified "dampening factor." in range [0,1]; e.g., 0.85
;;  + N is the number of pages in the graph
;;  + S is the sum of P(page-j) for all page-j.
;;  + P(page-j) is CurrentPageRank(page-j)/NumLinks(page-j)
;;  + NumLinks(page-j) is the number of outbound links of page-j
;;  (i.e., the number of pages to which page-j has links).
;;
;; (-> pagerank? rational? graph? pagerank?)
(define (step-pagerank pr d graph)
  (define N (num-pages graph))
  (define list-pages (pages graph '()))
  (define (step pg pr)                   ;;step applies the calculation to a given page and returns its updated val
    (define M (get-backlinks graph pg))
    (define lhs (/ (- 1 d) N)) ;; the lhs is the left hand side of the formula
    (define (sum m)
      (cond
        [(empty? m) 0]
        [else (+ (/ (hash-ref pr (car m)) (num-links graph (car m))) (sum (cdr m)))]
        )
      )
    (define result (+ lhs (* d (sum M)))) ;; perform the remaining calculation to the value
    result
   )
  (define (driver pgrk list-pages) ;;applies step to each page individually and provides an updated hash
    (if (empty? list-pages) pgrk
        (let
            ([val (step (car list-pages) pr)])
        (driver (hash-set pgrk (car list-pages) val) (cdr list-pages))
          )
        )
    )
  (driver pr list-pages)
  )

;; Iterate PageRank until the largest change in any page's rank is
;; smaller than a specified delta.
;;
;; (-> pagerank? rational? graph? rational? pagerank?)
(define (iterate-pagerank-until pr d graph delta)
  (define (lstpr pr)
    (hash->list pr)
    )
  (define (get-key-vals lst new-lst)
    (cond
      [(empty? lst) new-lst]
      [else (get-key-vals (cdr lst) (cons (cdar lst) new-lst))]
      )
    )
  (define key-vals (get-key-vals (lstpr pr) '()))
  (define (checker pgrk)
    (let*
        ([step-pr (step-pagerank pgrk d graph)]
         [step-vals (get-key-vals (lstpr step-pr) '())]
         [ogvals (get-key-vals (lstpr pgrk) '())]
         [changes (map (lambda (pr-1 pr) (abs (- pr-1 pr))) ogvals step-vals)]
         [sort-changes (sort changes >)])
      (cond
        [(< (car sort-changes) delta) step-pr]
        [else (checker step-pr)]
        )
      )
    )
  (checker pr)
  )

;; Given a PageRank, returns the list of pages it contains in ranked
;; order (from least-popular to most-popular) as a list. You may
;; assume that the none of the pages in the pagerank have the same
;; value (i.e., there will be no ambiguity in ranking)
;;
;; (-> pagerank? (listof symbol?))
(define (rank-pages pr)
  (define list (hash->list pr))
  (define (get-cdrs lst)
    (cond
      [(empty? lst) '()]
      [else (cons (cdar lst) (get-cdrs (cdr lst)))]
      )
    )
  (define vals (get-cdrs list))
  (define sorted-vals (sort vals <))
  (define (relister list sorted-vals res)
    (define (finder list val)
      (cond
        [(equal? (cdar list) val) (caar list)]
        [else (finder (cdr list) val)]
        )
      )
    (cond
      [(empty? sorted-vals) res]
      [else (cons (finder list (car sorted-vals)) (relister list (cdr sorted-vals) res))]
      )
    )
  (relister list sorted-vals '())
  )
