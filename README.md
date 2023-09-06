# Implementing PageRank

PageRank is a popular graph algorithm used for information
retrieval and was first popularized as an algorithm powering
the Google search engine. Details of the PageRank algorithm will be
discussed in class. Here are several functions that
implement the PageRank algorithm in Racket.

# (graph? g)
(-> any? boolean?)

This program accepts graphs as input. Graphs are represented as a list
of links, where each link is a list `(,src ,dst) that signals page src
links to page dst.  This function checks whether a value is a valid
graph.

# (pagerank? p).
(-> any? boolean?)

Implementation takes input graphs and turns them into PageRanks. A
PageRank is a Racket hash-map that maps pages (each represented as a
Racket symbol) to their corresponding weights, where those weights
must sum to 1 (over the whole map).  A PageRank encodes a discrete
probability distribution over pages.

The test graphs adhere to several constraints:
+ There are no "terminal" nodes. All nodes link to at least one
other node.
+ There are no "self-edges," i.e., there will never be an edge `(n0
n0).
+ To maintain consistenty with the last two facts, each graph will
have at least two nodes.
+ There will be no "repeat" edges. I.e., if `(n0 n1) appears once
in the graph, it will not appear a second time.

# (num-pages graph)

(-> graph? nonnegative-integer?)

Takes some input graph and computes the number of pages in the
graph. For example, the graph '((n0 n1) (n1 n2)) has 3 pages, n0, n1,
and n2.

# (get-backlinks graph)
(-> graph? symbol? (set/c symbol?))

Calculates a set of pages that link to page within graph. For
example, (get-backlinks '((n0 n1) (n1 n2) (n0 n2)) n2) should
return (set 'n0 'n1).


# (mk-initial-pagerank graph)
(-> graph? pagerank?)

Generate an initial pagerank for the input graph g. The returned
PageRank must satisfy pagerank?, and each value of the hash must be
equal to (/ 1 N), where N is the number of pages in the given graph.

# (step-pagerank pr d graph)
(-> pagerank? rational? graph? pagerank?)

Perform one step of PageRank on the specified graph. Return a new
PageRank with updated values after running the PageRank
calculation. The next iteration's PageRank is calculated as:

NextPageRank(page-i) = (1 - d) / N + d * SumOfWeightedLinks

Where:
 + d is a specified "dampening factor." in range [0,1]; e.g., 0.85
 + N is the number of pages in the graph
 + SumOfWeightedLinks is the sum of P(page-j) for all page-j.
 + P(page-j) is CurrentPageRank(page-j)/NumLinks(page-j)
 + NumLinks(page-j) is the number of outbound links of page-j
 (i.e., the number of pages to which page-j has links).

# (iterate-pagerank-until pr d graph delta)
(-> pagerank? rational? graph? rational? pagerank?)
Iterate PageRank until the largest change in any page's rank is
smaller than a specified delta.

# (rank-pages pr)
(-> pagerank? (listof symbol?))

Given a PageRank, returns the list of pages it contains in ranked
order (from least-popular to most-popular) as a list. You may assume
that the none of the pages in the pagerank have the same value (i.e.,
there will be no ambiguity in ranking).


