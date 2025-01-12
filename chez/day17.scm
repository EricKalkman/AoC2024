(library (day17)
         (export part-1 part-2)
         (import (rnrs)
                 (rnrs mutable-pairs)
                 (util)
                 (prefix (parsecomb) p:))

  (define (pregister c)
    (->> (p:seq (p:skip (p:str "Register ")) (p:skip (p:char c))
                (p:skip (p:str ": ")) p:parse-int)
         (p:pmap car)))

  ; indices of registers in register state (itself represented as a vector)
  (define ra 0)
  (define rb 1)
  (define rc 2)

  ; converts a combo value into a more useful symbolic representation;
  ; the combo value is tagged as a literal (lit), register (reg), or
  ; ignored (combo value 7). reg values are converted to register indices
  (define (combo-op op)
    (case op
      [(0 1 2 3) `(lit . ,op)]
      [(4 5 6) `(reg . ,(- op 4))]
      [else '(ignored . #f)]))

  ; conversion of instruction codes to symbols; not strictly necessary,
  ; but easier for debugging
  (define (inst->op inst)
    (define opcode (car inst))
    (define operand (cadr inst))
    (case opcode
      [(0) `(adv . ,(combo-op operand))]
      [(1) `(bxl . (lit . ,operand))]
      [(2) `(bst . ,(combo-op operand))]
      [(3) `(jnz . (lit . ,operand))]
      [(4) `(bxc . (ignored . #f))]
      [(5) `(out . ,(combo-op operand))]
      [(6) `(bdv . ,(combo-op operand))]
      [(7) `(cdv . ,(combo-op operand))]
      [else (error 'inst->op "invalid op" opcode)]))

  (define pprogram (->> (p:seq (p:skip (p:str "Program: "))
                               (p:list-of p:parse-int (p:char #\,)))
                        (p:pmap
                          (λ (lst)
                             ; alist of 'prog (parsed operation/operand pairs) and
                             ; 'ints (the raw digits from the input)
                             (list (cons 'prog (->> (car lst) (groups-of 2)
                                                    (map inst->op) (list->vector)))
                                   (cons 'ints (car lst)))))))

  (define pinput
    (->> (p:seq (pregister #\A) p:skip-nl
                (pregister #\B) p:skip-nl
                (pregister #\C) p:skip-nl p:skip-nl
                pprogram)
         (p:pmap
           (λ (lst)
              (let ([a (car lst)]
                    [b (cadr lst)]
                    [c (caddr lst)]
                    [prog (cadddr lst)])
                ; returns alist of pprogram + initial register settings as
                ; a vector keyed by 'regs
                (cons `(regs . #(,a ,b ,c)) prog))))))

  ;; operation functions
  ; operation functions take register values as a 3-vector and the curren output
  ; (a list in reverse order) and return the new output. most operation functions
  ; also take an operand parameter y, which is the actual value on which to operate
  ; (e.g., the value of a register if specified to be a combo value, or a literal
  ; value otherwise).
  ; note that these functions mutate regs
  (define (adv regs out y)
    (vector-update! regs ra (λ (x) (sr x y)))
    out)
  (define (bxl regs out y)
    (vector-update! regs rb (λ (x) (bitwise-xor x y)))
    out)
  (define (bst regs out y)
    (vector-set! regs rb (bitwise-and 7 y))
    out)
  (define (bxc regs out)
    (vector-update! regs rb (λ (b) (bitwise-xor (vector-ref regs rc) b)))
    out)
  (define (out _regs outstack y)
    (cons (bitwise-and 7 y) outstack))
  (define (bdv regs out y)
    (vector-set! regs rb (sr (vector-ref regs ra) y))
    out)
  (define (cdv regs out y)
    (vector-set! regs rc (sr (vector-ref regs ra) y))
    out)

  ; some of the fastest CPU instructions known to mankind have some of the longest
  ; function names in R6RS
  (define sl bitwise-arithmetic-shift-left)
  (define sr bitwise-arithmetic-shift-right)

  ; lookup table taking parsed instruction symbol to the corresponding function
  (define fn-lookup
    `((adv . ,adv) (bxl . ,bxl) (bst . ,bst) (bxc . ,bxc)
                   (out . ,out) (bdv . ,bdv) (cdv . ,cdv)))

  ; compile the parsed commands cmds into One Giant Lambda to which registers and the initial
  ; value of the output stack ('()) are passed.
  (define (compile-program cmds)
    ; stores jump targets (i.e., each instruction) as boxed values (lists).
    ; when compile-aux compiles the instruction at a given instruction pointer (ip), the
    ; corresponding box in cache is modified with set-car!, allowing 'jnz to reference
    ; a jump point that has not finished compiling yet ((compile-aux ip) always
    ; calls (compile-aux (+ 1 ip)) before building the return value for ip, so 'jnz
    ; can't know the λ corresponding to its jump target in advance).
    (define cache (->> (make-vector (vector-length cmds) #f)
                       (vector-map list)))
    ; recurses through each command (incrementing instruction pointer) returning a function
    ; that takes registers and an output stack and returns a new output stack.
    ; the returned lambda calls the (compiled) next operation in the tail position, so Chez
    ; ends up compiling the program effectively down to a string of goto's.
    (define (compile-aux ip)
      (if (>= ip (vector-length cmds))
        ; at the end of the program, the output is in reverse order due to being built by
        ; cons'ing; reverse it before returning it
        (λ (_regs out) (reverse out))
        (let* ([inst (vector-ref cmds ip)]
               [fn (car inst)]  ; symbol corresponding to the operation
               [operand (cdr inst)] ; (operand-kind . operand-value)
               [after (compile-aux (+ ip 1))]) ; rest of program
          (if (eqv? fn 'jnz) ; handle jump separately from other kinds of instructions
            (let* ([jmp-pos (cdr operand)] ; ip to jump to
                   [jmp-entry (vector-ref cache jmp-pos)] ; box containing the jmp target
                   [cont (λ (regs out) ; lambda to return
                            (if (zero? (vector-ref regs ra))
                              (after regs out)
                              ((car jmp-entry) regs out)))])
              ; fill the box corresponding to the current ip in the cache with the compiled
              ; function
              (set-car! (vector-ref cache ip) cont)
              cont)
            ; dispatch on opfn
            (let ([opfn (assv-get fn fn-lookup)])
              (case (car operand)
                [(ignored)
                 (set-car! (vector-ref cache ip)
                           (λ (regs out) (after regs (opfn regs out))))
                 (car (vector-ref cache ip))]
                [(lit)
                 (set-car! (vector-ref cache ip)
                           (λ (regs out) (after regs (opfn regs out (cdr operand)))))
                 (car (vector-ref cache ip))]
                [(reg)
                 (set-car! (vector-ref cache ip)
                           (λ (regs out) (after regs (opfn regs out (vector-ref regs (cdr operand))))))
                 (car (vector-ref cache ip))]
                [else (error 'compile "invalid operand kind" (car operand))]))))))
    (compile-aux 0))

  (define (part-1 s)
    (define parsed (p:parse-result-unwrap (p:parse pinput s)))
    (define instructions (assv-get 'prog parsed))
    (define regs (assv-get 'regs parsed))
    (define program (compile-program instructions))
    (define output (program regs '()))
    (fold-left
      (λ (s n) (string-append s "," (number->string n)))
      (number->string (car output))
      (cdr output)))

  ; Recursively solves for the appropriate value of the A register one 3-bit word at a time,
  ; starting from the most significant word and successively appending less significant
  ; words. Before appending less significant words, the program is run with the current
  ; set of words, checking that the corresponding output digit matches the corresponding
  ; digit in the problem input.
  ; The output length grows by 1 for each additional word, and the "current" (least-
  ; significant) word is always outputted first.
  (define (solve-quinoid program rev-target a)
    (if (null? rev-target)
      a
      (let loop ([i 0])
        (if (>= i 8)
          #f
          (let* ([new-a (bitwise-ior i (sl a 3))]
                 [output (program (vector new-a 0 0) '())])
            (if (= (car output) (car rev-target))
              (or (solve-quinoid program (cdr rev-target) new-a)
                  (loop (+ i 1)))
              (loop (+ i 1))))))))

  (define (part-2 s)
    (define parsed (p:parse-result-unwrap (p:parse pinput s)))
    (define instructions (assv-get 'prog parsed))
    (define rev-target (reverse (assv-get 'ints parsed)))
    (define program (compile-program instructions))
    (solve-quinoid program rev-target 0))
)
#|
(import (rnrs) (util) (day17))
(define test-inp (file->string "../inputs/day17.test"))
(define test-inp2 (file->string "../inputs/day17.test2"))
(define real-inp (file->string "../inputs/day17.inp"))

(part-1 test-inp) ; "4,6,3,5,6,3,5,2,1,0"
(part-1 real-inp) ; "5,1,4,0,5,1,0,2,6"
(part-2 test-inp2) ; 117440
(part-2 real-inp) ; 202322936867370
|#
