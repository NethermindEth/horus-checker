# Memory
MEM!1=[(+ ap!<root>@6 45)]
MEM!2=[ap!<root>@6]
MEM!3=[(+ fp!<root> (- 5))]
MEM!4=[(+ ap!<root>@6 1)]
MEM!5=[(+ fp!<root> (- 4))]
MEM!6=[(+ ap!<root>@6 2)]
MEM!7=[(+ fp!<root> (- 3))]
MEM!8=[(+ ap!<root>@6 3)]
MEM!9=[(+ ap!<root>@6 4)]
MEM!10=[(+ ap!<root>@6 5)]
MEM!11=[(+ fp!<57=write/root> (- 3))]
MEM!12=[(+ ap!<root>@6 22)]
MEM!13=[(+ ap!<root>@6 23)]
MEM!14=[(+ ap!<root>@6 41)]
MEM!15=[(+ ap!<root>@6 42)]
MEM!16=[(+ ap!<root>@6 38)]
MEM!17=[(+ ap!<root>@6 43)]
MEM!18=[(+ ap!<root>@6 39)]
MEM!19=[(+ ap!<root>@6 44)]
MEM!20=[(+ ap!<root>@6 40)]
MEM!21=[(+ fp!<57=write/root> (- 5))]
MEM!22=[(+ ap!<root>@6 20)]
MEM!23=[(+ fp!<59=read/root> (- 4))]
MEM!24=[(+ fp!<57=write/root> (- 4))]
MEM!25=[(+ ap!<root>@6 21)]
MEM!26=[(+ fp!<59=read/root> (- 3))]
# Assert

(<= fp!<root> ap!<root>@6)
(= ap!<root>@6 fp!<root>)
(= MEM!3 MEM!2)
(= MEM!5 MEM!4)
(= MEM!7 MEM!6)
(= 42 MEM!8)

(and (= fp!<57=write/root> (+ ap!<root>@6 6)) (= MEM!9 fp!<root>) (= MEM!10 59))

(and
  (=> 
    (or (<= range-check!start MEM!14) (<= range-check!start 1337))
    (= (+ MEM!14 1337) (mod (+ MEM!14 1337) prime)))

  (=>
    (and
      (<= range-check!start (+ ap!<root>@6 40))
      (= (mod (+ ap!<root>@6 40) 1) 0)
      (< (+ ap!<root>@6 40) range-check!end))
    (<= 0 MEM!20 (+ range-check-bound (- 1))))

  (=>
    (and
      (<= range-check!start (+ ap!<root>@6 39))
      (= (mod (+ ap!<root>@6 39) 1) 0)
      (< (+ ap!<root>@6 39) range-check!end))
    (<= 0 MEM!18 (+ range-check-bound (- 1))))

  (=>
    (and
      (<= range-check!start (+ ap!<root>@6 38))
      (= (mod (+ ap!<root>@6 38) 1) 0)
      (< (+ ap!<root>@6 38) range-check!end))
    (<= 0 MEM!16 (+ range-check-bound (- 1))))

  (<= MEM!26 MEM!20)
  (<= range-check!start MEM!20)
  (= (mod MEM!20 1) 0)
  (<= MEM!24 MEM!25)
  (<= range-check!start MEM!25)
  (= (mod MEM!25 1) 0)

  (=>
    (and
      (<= range-check!start (+ fp!<root> (- 3)))
      (= (mod (+ fp!<root> (- 3)) 1) 0)
      (< (+ fp!<root> (- 3)) range-check!end))
    (<= 0 MEM!7 (+ range-check-bound (- 1))))
  
  (=>
    (and
      (<= range-check!start (+ fp!<root> (- 4)))
      (= (mod (+ fp!<root> (- 4)) 1) 0)
      (< (+ fp!<root> (- 4)) range-check!end))
    (<= 0 MEM!5 (+ range-check-bound (- 1))))
  
  (=>
    (and
      (<= range-check!start (+ fp!<root> (- 5)))
      (= (mod (+ fp!<root> (- 5)) 1) 0)
      (< (+ fp!<root> (- 5)) range-check!end))
    (<= 0 MEM!3 (+ range-check-bound (- 1))))

  (<= range-check!start MEM!7)
  (= (mod MEM!7 1) 0)
  (<= MEM!19 range-check!end)
  
  (=>
    (or (<= pedersen!start MEM!14) (<= pedersen!start 1337))
    (= (+ MEM!14 1337) (mod (+ MEM!14 1337) prime)))
    
  (<= MEM!23 MEM!18)
  (<= pedersen!start MEM!18)
  (= (mod MEM!18 3) 0)
  (<= MEM!21 MEM!22)
  (<= pedersen!start MEM!22)
  (= (mod MEM!22 3) 0)
  (<= pedersen!start MEM!5)
  (= (mod MEM!5 3) 0)
  (<= MEM!17 pedersen!end)
  (= (+ MEM!14 1337) MEM!1)
  (= MEM!20 MEM!19)
  (= MEM!18 MEM!17)
  (= MEM!16 MEM!15)
  (= MEM!14 MEM!11)
  
  (not
    (and
      (= MEM!11 42)
      (= 42 MEM!11)
      (<= range-check!start MEM!26)
      (= (mod MEM!26 1) 0)
      (<= MEM!20 range-check!end)
      (<= range-check!start MEM!24)
      (= (mod MEM!24 1) 0)
      (<= MEM!25 range-check!end)
      (<= MEM!7 MEM!19)
      (<= range-check!start MEM!19)
      (= (mod MEM!19 1) 0)
      (<= pedersen!start MEM!23)
      (= (mod MEM!23 3) 0)
      (<= MEM!18 pedersen!end)
      (<= pedersen!start MEM!21)
      (= (mod MEM!21 3) 0)
      (<= MEM!22 pedersen!end)
      (<= MEM!5 MEM!17)
      (<= pedersen!start MEM!17)
      (= (mod MEM!17 3) 0)
      (= 1379 MEM!1)))
    
  (= fp!<59=read/root> (+ ap!<root>@6 24))
  (= MEM!12 fp!<root>)
  (= MEM!13 61))
# Expect"