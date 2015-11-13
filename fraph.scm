;; Fraph --- Functional Graph Library
;; Copyright (C) 2015  Andrew "Drew" Dudash

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

(define-module (fraph)
  :export (make-graph
	   make-edge
	   graph-vertices
	   graph-edges
	   add-vertex
	   add-edge
	   vertex-neighbors))

(define (make-graph vertices edges)
  "Make a graph with the given list of VERTICES
and the list of EDGES."
  (list vertices edges))

(define (make-edge vertex-1 vertex-2)
  "Make a new edge between VERTEX-1 and VERTEX-2."
  (list vertex-1 vertex-2))

(define (edge-contains? vertex edge)
  "True if EDGE contains VERTEX."
  (or (equal? vertex (car edge))
      (equal? vertex (cadr edge))))

(define (edge-other-vertex vertex edge)
  "Returns the vertex in EDGE that is not VERTEX.
If the edge points to itself, returns VERTEX.
If VERTEX is not in EDGE, returns false."
  (cond ((equal? vertex (car edge)) (cadr edge))
	((equal? vertex (cadr edge)) (car edge))
	(else #f)))

(define (graph-vertices graph)
  "Returns a list of the vertices in GRAPH."
  (car graph))

(define (graph-edges graph)
  "Returns a list of the edges in GRAPH."
  (cadr graph))

(define my-graph
  (make-graph (list 0 1 2 3 4 5 6 7 8)
	      (list (make-edge 0 1)
		    (make-edge 0 2)
		    (make-edge 1 7)
		    (make-edge 7 8)
		    (make-edge 1 8)
		    (make-edge 1 4)
		    (make-edge 4 6)
		    (make-edge 4 5)
		    (make-edge 4 2)
		    (make-edge 2 3))))

(define (vertex-neighbors vertex graph)
  "Returns all vertices adjacent to VERTEX in GRAPH."
  (map (lambda (edge)
	 (edge-other-vertex vertex edge))
       (filter (lambda (edge)
		 (edge-contains? vertex edge))
	       (graph-edges graph))))

(define (list-difference list-1 list-2)
  "Returns every element in LIST-1, but not in LIST-2."
  (filter (lambda (item)
	    (not (member item list-2)))
	  list-1))
    
(define (add-vertex graph new-vertex)
  "Adds a NEW-VERTEX to GRAPH."
  (make-graph (cons new-vertex (graph-vertices graph))
	      (graph-edges graph)))

(define (add-edge graph new-edge)
  "Adds a NEW-EDGE to GRAPH."
  (make-graph (graph-vertices graph)
	      (cons new-edge (graph-edges graph))))
