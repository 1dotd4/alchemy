;; This file is part of Alchemy.
;; Copyright (c) 2022 unpx.net
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-library
  (alchemy rewrite)
  (export make-sequent
          sequent->string
          rewrite-tree
          rtree->string
          )
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (alchemy language)
          )
  (begin
    (define (logic? symb)
      (member (car symb) '(and or not then)))

    (define make-sequent 
      (case-lambda
        ((post) (make-sequent '() post))
        ((lhs rhs) (list 'sequent 
                         (if (and (not (null? lhs)) (logic? lhs)) (list lhs) lhs)
                         (if (and (not (null? rhs)) (logic? rhs)) (list rhs) rhs)))))

    (define (sequent->lhs seq) (cadr seq))
    (define (sequent->rhs seq) (car (cddr seq)))

    (define (sequent-cons-lhs seq lhs)
      (make-sequent (append lhs (sequent->lhs seq)) (sequent->rhs seq)))
    (define (sequent-cons-rhs seq rhs)
      (make-sequent (sequent->lhs seq) (append rhs (sequent->rhs seq))))

    (define (sequent? s) (eq? 'sequent (car s)))

    (define (logic->string symb)
      (cond
        ((eq? symb 'and) " ∧ ")
        ((eq? symb 'or) " ∨ ")
        ((eq? symb 'then) " -> ")
        ((eq? symb 'not) "¬")))

    (define (prop->string prop)
      (cond
        ((null? prop) "_|_")
        ((and (list? prop) (logic? prop))
         (if (= 1 (length (cdr prop)))
           (string-join (list (logic->string (car prop)) (prop->string (cadr prop))) "")
           (string-join (list "(" (string-join (map prop->string (cdr prop)) (logic->string (car prop))) ")") "")))
        ((list? prop)
         (string-join (list "(" (string-join (map prop->string prop) ", ") ")") ""))
        (else
          (symbol->string prop))))

    (define (sequent->string seq)
      (string-join (list (prop->string (cadr seq)) " |- " (prop->string (cadr (cdr seq))) "\n")))


    ;;;;;;;; RULES

    ; now instead of searching each time, we already know here that (car lhs) is an and
    (define (rule-and-lhs lhs rhs)
      (let ((lhs-and (car lhs)))
        (list
          'and-lhs
          (make-sequent (append (cdr lhs-and) (cdr lhs))
                        rhs)
          '())))

    (define (rule-not-lhs lhs rhs)
      (let ((lhs-not (car lhs)))
        (list
          'not-lhs
          (make-sequent (cdr lhs) (append (cdr lhs-not) rhs))
          '())))

    (define (rule-not-rhs lhs rhs)
      (let ((rhs-not (car rhs)))
        (list
          'not-rhs
          (make-sequent (append (cdr rhs-not) lhs) (cdr rhs))
          '())))

    (define (rule-or-rhs lhs rhs)
      (let ((rhs-or (car rhs)))
        (list
          'or-rhs
          (make-sequent lhs (append (cdr rhs-or) (cdr rhs)))
          '())))

    (define (rule-then-rhs lhs rhs)
      (let ((then-rhs (car rhs)))
        (list
          'then-rhs
          (make-sequent (append lhs (cadr then-rhs)) (append (cddr then-rhs) (cdr rhs)))
          '())))

    (define (rule-and-rhs lhs rhs)
      (let ((and-rhs (car rhs)))
        (list
          'and-rhs-main-branch
          (make-sequent lhs (append (cdr rhs) (list (cadr and-rhs))))
          (map (lambda (x) (list 'and-rhs-branch (make-sequent lhs (append (cdr rhs) (list x))) '())) (cddr and-rhs)))))
    (define (rule-or-lhs lhs rhs)
      (let ((or-lhs (car lhs)))
        (list
          'and-rhs-main-branch
          (make-sequent (append (cdr lhs) (list (cadr or-lhs))) rhs)
          (map (lambda (x) (list 'and-rhs-branch (make-sequent (append (cdr lhs) (list x)) rhs) '())) (cddr or-lhs)))))



    ;;;;;;;;;;;;;;;;;; RULE SELECTION

    (define *some-rules-left*
      (list
        ;; TODO: terminations
        ;; compareList a = not . null . intersect a => ax-id
        ;; ax-tt (if tt on rhs)
        ;; ax-_|_ (if _|_ on lhs)
        (cons 'and rule-and-lhs)
        (cons 'not rule-not-lhs)
        ;; branches
        (cons 'or rule-or-lhs)
        ))

    (define *some-rules-right*
      (list
        (cons 'or rule-or-rhs)
        (cons 'not rule-not-rhs)
        (cons 'then rule-then-rhs)
        ;; branches
        (cons 'and rule-and-rhs)
        ))

    ;;;;;;;;;;;;; REWRITE LOOP

    (define (try-rewrite-left sequent)
      (let ((rhs (sequent->rhs sequent)))
        (let scan-try-loop ((lhs (sequent->lhs sequent)) (seen '()) (rules-to-try *some-rules-left*))
          (if (null? lhs)
            '()
            (if (null? rules-to-try)
              (scan-try-loop (cdr lhs) (append seen (list (car lhs))) *some-rules-left*)
              (if (and (list? (car lhs)) (eq? (caar rules-to-try) (caar lhs)))
                (let ((result (apply (cdar rules-to-try) (list lhs rhs))))
                  (list
                    (car result)
                    (sequent-cons-lhs (cadr result) seen)
                    (map (lambda (x) (list (car x) (sequent-cons-lhs (cadr x) seen) (car (cddr x)))) (car (cddr result)))))
                (scan-try-loop lhs seen (cdr rules-to-try))))))))

    (define (try-rewrite-right sequent)
      (let ((lhs (sequent->lhs sequent)))
        (let scan-try-loop ((rhs (sequent->rhs sequent)) (seen '()) (rules-to-try *some-rules-right*))
          (if (null? rhs)
            '()
            (if (null? rules-to-try)
              (scan-try-loop (cdr rhs) (append seen (list (car rhs))) *some-rules-right*)
              (if (and (list? (car rhs)) (eq? (caar rules-to-try) (caar rhs)))
                (let ((result (apply (cdar rules-to-try) (list lhs rhs))))
                  (list
                    (car result)
                    (sequent-cons-rhs (cadr result) seen)
                    (map (lambda (x) (list (car x) (sequent-cons-rhs (cadr x) seen) (car (cddr x)))) (car (cddr result)))))
                (scan-try-loop rhs seen (cdr rules-to-try))))))))


    (define (try-rewrite sequent)
      ;; Note this is an "apply-any" loop
      (if (not (sequent? sequent)) (error "What am I looking at?" sequent))
      (let ((maybe-left (try-rewrite-left sequent)))
        (if (null? maybe-left)
          (let ((maybe-right (try-rewrite-right sequent)))
            (if (null? maybe-right)
              (list 'nothing sequent '())
              maybe-right))
          maybe-left)))

    (define rewrite-tree
      (case-lambda
        ; TODO: history should be a tree
        ((target) (rewrite-tree target (list) (list (list 'root target))))
        ((target other-targets history)
         (let ((result (try-rewrite target)))
           ; (display result)
           ; (display " -- result \n")
           ; (display other-targets)
           ; (display " -- other-targets \n")
           (if (eq? (car result) 'nothing)
             (if (null? other-targets)
               history
               ;; TODO: manage branches
               (rewrite-tree (car (cdar other-targets)) (cdr other-targets) (cons (car other-targets) history)))
             (rewrite-tree (cadr result) (append other-targets (car (cddr result))) (cons result history)))))))

    (define (rtree->string rtree)
      (define (rnode->string node)
        ; (display node)
        ; (display "\n")
        (string-append
          ((compose sequent->string cadr) node)
          (string-append "------------------ " (symbol->string (car node)))))
      ; (display rtree)
      ; (display "\n")
      (string-append
        "\n"
        (string-join (map rnode->string rtree) "\n")
        "\n\n"))
          
    ))
