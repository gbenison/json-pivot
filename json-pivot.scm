; 
;  Copyright (C) 2011 Greg Benison
; 
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
; 
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
; 
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
; 
; 

; ----------------------------
;
; Transform JSON input stream into a tree;
; re-root the tree and re-output as JSON
;
; ----------------------------
(define-module (json-pivot)
  #:export (json:pivot
	    node:key?))
 
(use-modules (json reader)
	     (json writer)
	     (srfi srfi-1))

(define (json:pivot predicate infile outfile)
  (json:dump-to-port
   (tree->json
    (reroot
     (json->tree (json:read-value infile) #f)
     predicate))
   outfile))

;; like 'map', but supplies a second
;; argument to 'op' consisting of
;; "the other elements of set" in their
;; original order.
(define (map-partitions op set)
  (let loop ((first-part '())
	     (result '())
	     (rest set))
    (if (null? rest)
	(reverse result)
	(let ((this-element   (car rest))
	      (other-elements (append (reverse first-part)(cdr rest))))
	  (loop (cons this-element first-part)
		(cons (op this-element other-elements) result)
		(cdr rest))))))

(define (json:dump-to-port json port)
  (display (json:dump json) port))

; ---------------- JSON as tree ----------
(define (node type key value children)
  (cons (cons type key)
	(cons value children)))

(define node:type     caar)
(define node:key      cdar)
(define node:value    cadr)
(define node:children cddr)

(define (node:replace-children N new-children)
  (node (node:type  N)
	(node:key   N)
	(node:value N)
	new-children))

(define (node:replace-key N new-key)
  (node (node:type N)
	new-key
	(node:value N)
	(node:children N)))

(define (node:replace-missing-key N new-key)
  (if (node:key N) N (node:replace-key N new-key)))

(define (node:key? key)
  (lambda (N)(equal? key (node:key N))))

(define (node:take N limit)
  (node:replace-children N (take (node:children N) limit)))

(define (json->tree json key)
  (cond ((hash-table? json)
	 (node 'object
	       key
	       #f
	       (hash-fold
		(lambda (k v p)
		  (cons (json->tree v k) p))
		'()
		json)))
	((list? json)
	 (node 'array
	       key
	       #f
	       (map (lambda (v)(json->tree v #f)) json)))
	(else (node 'scalar key json '()))))

(define (tree->json tree)
  (case (node:type tree)
    ((scalar)(node:value tree))
    ((array)(map tree->json (node:children tree)))
    ((object)
     (let ((table (make-hash-table)))
       (for-each
	(lambda (node)
	  (hash-set! table (node:key node) (tree->json node)))
	(node:children tree))
       table))))

;; Rebuild tree rooted at T, but appending links following 'path'
(define (rebuild T path level)
  (if (null? path)
      T
      (node 'object
	    (node:key T)
	    #f
	    (list
	     (node:replace-missing-key T "new_root")
	     (rebuild (node:replace-key
		       (car path)
		       (format #f "xpath-~a-~a"
			       level
			       (or (node:key (car path)) "NA")))
		      (cdr path)
		      (+ level 1))))))


;; Return an array node,
;; whose members are trees T' which are T re-rooted
;; at nodes N matching (pred N)
(define (reroot T pred)
  (node 'array
	#f
	#f
	(let loop ((T T)
		   (path '()))
	  (if (pred T)
	      (list (rebuild T path 1))
	      (apply append
		     (map-partitions
		      (lambda (child other-children)
			(loop child
			      (cons
			       (node:replace-children T other-children)
			       path)))
		      (node:children T)))))))


