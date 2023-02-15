(declare-fun ADDR!1 () Int)
(declare-fun ADDR!2 () Int)
(declare-fun ADDR!3 () Int)
(declare-fun ADDR!4 () Int)
(declare-fun ADDR!5 () Int)
(declare-fun ADDR!6 () Int)
(declare-fun ADDR!7 () Int)
(declare-fun ADDR!8 () Int)
(declare-fun MEM!1 () Int)
(declare-fun MEM!2 () Int)
(declare-fun MEM!3 () Int)
(declare-fun MEM!4 () Int)
(declare-fun MEM!5 () Int)
(declare-fun MEM!6 () Int)
(declare-fun MEM!7 () Int)
(declare-fun MEM!8 () Int)
(declare-fun ap!<root>@0 () Int)
(declare-fun fp!<root> () Int)
(declare-fun prime () Int)
(assert (and (<= 0 ADDR!1) (< ADDR!1 prime)))
(assert (and (<= 0 ADDR!2) (< ADDR!2 prime)))
(assert (and (<= 0 ADDR!3) (< ADDR!3 prime)))
(assert (and (<= 0 ADDR!4) (< ADDR!4 prime)))
(assert (and (<= 0 ADDR!5) (< ADDR!5 prime)))
(assert (and (<= 0 ADDR!6) (< ADDR!6 prime)))
(assert (and (<= 0 ADDR!7) (< ADDR!7 prime)))
(assert (and (<= 0 ADDR!8) (< ADDR!8 prime)))
(assert (and (<= 0 MEM!1) (< MEM!1 prime)))
(assert (and (<= 0 MEM!2) (< MEM!2 prime)))
(assert (and (<= 0 MEM!3) (< MEM!3 prime)))
(assert (and (<= 0 MEM!4) (< MEM!4 prime)))
(assert (and (<= 0 MEM!5) (< MEM!5 prime)))
(assert (and (<= 0 MEM!6) (< MEM!6 prime)))
(assert (and (<= 0 MEM!7) (< MEM!7 prime)))
(assert (and (<= 0 MEM!8) (< MEM!8 prime)))
(assert (and (<= 0 ap!<root>@0) (< ap!<root>@0 prime)))
(assert (and (<= 0 fp!<root>) (< fp!<root> prime)))
(assert (= prime 3618502788666131213697322783095070105623107215331596699973092056135872020481))
(assert (=> (= ADDR!1 ADDR!2) (= MEM!1 MEM!2)))
(assert (=> (= ADDR!1 ADDR!3) (= MEM!1 MEM!3)))
(assert (=> (= ADDR!1 ADDR!4) (= MEM!1 MEM!4)))
(assert (=> (= ADDR!1 ADDR!5) (= MEM!1 MEM!5)))
(assert (=> (= ADDR!1 ADDR!6) (= MEM!1 MEM!6)))
(assert (=> (= ADDR!1 ADDR!7) (= MEM!1 MEM!7)))
(assert (=> (= ADDR!1 ADDR!8) (= MEM!1 MEM!8)))
(assert (=> (= ADDR!2 ADDR!3) (= MEM!2 MEM!3)))
(assert (=> (= ADDR!2 ADDR!4) (= MEM!2 MEM!4)))
(assert (=> (= ADDR!2 ADDR!5) (= MEM!2 MEM!5)))
(assert (=> (= ADDR!2 ADDR!6) (= MEM!2 MEM!6)))
(assert (=> (= ADDR!2 ADDR!7) (= MEM!2 MEM!7)))
(assert (=> (= ADDR!2 ADDR!8) (= MEM!2 MEM!8)))
(assert (=> (= ADDR!3 ADDR!4) (= MEM!3 MEM!4)))
(assert (=> (= ADDR!3 ADDR!5) (= MEM!3 MEM!5)))
(assert (=> (= ADDR!3 ADDR!6) (= MEM!3 MEM!6)))
(assert (=> (= ADDR!3 ADDR!7) (= MEM!3 MEM!7)))
(assert (=> (= ADDR!3 ADDR!8) (= MEM!3 MEM!8)))
(assert (=> (= ADDR!4 ADDR!5) (= MEM!4 MEM!5)))
(assert (=> (= ADDR!4 ADDR!6) (= MEM!4 MEM!6)))
(assert (=> (= ADDR!4 ADDR!7) (= MEM!4 MEM!7)))
(assert (=> (= ADDR!4 ADDR!8) (= MEM!4 MEM!8)))
(assert (=> (= ADDR!5 ADDR!6) (= MEM!5 MEM!6)))
(assert (=> (= ADDR!5 ADDR!7) (= MEM!5 MEM!7)))
(assert (=> (= ADDR!5 ADDR!8) (= MEM!5 MEM!8)))
(assert (=> (= ADDR!6 ADDR!7) (= MEM!6 MEM!7)))
(assert (=> (= ADDR!6 ADDR!8) (= MEM!6 MEM!8)))
(assert (=> (= ADDR!7 ADDR!8) (= MEM!7 MEM!8)))
(assert (= ADDR!1 ap!<root>@0))
(assert (= ADDR!2 (mod (+ fp!<root> (- 4)) prime)))
(assert (= ADDR!3 MEM!2))
(assert (= ADDR!4 (mod (+ fp!<root> (- 3)) prime)))
(assert (= ADDR!5 (mod (+ MEM!2 1) prime)))
(assert (= ADDR!6 (mod (+ ap!<root>@0 1) prime)))
(assert (= ADDR!7 (mod (+ ap!<root>@0 2) prime)))
(assert (= ADDR!8 (mod (+ MEM!2 2) prime)))
(assert (<= fp!<root> ap!<root>@0))
(assert (= ap!<root>@0 fp!<root>))
(assert (= 100890693370601760042082660 MEM!1))
(assert (= MEM!3 MEM!1))
(assert (= MEM!5 MEM!4))
(assert (= (mod (+ MEM!2 3) prime) MEM!6))
(assert (= MEM!8 MEM!7))
(assert true)
(assert false)
(check-sat)