# Memory
MEM!1=[(+ ap!<root>@6 50)]
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
MEM!14=[ap!<59=get_balance/root>@5]
MEM!15=[(+ fp!<59=get_balance/root> (- 5))]
MEM!16=[(+ ap!<59=get_balance/root>@5 1)]
MEM!17=[(+ fp!<59=get_balance/root> (- 4))]
MEM!18=[(+ ap!<59=get_balance/root>@5 2)]
MEM!19=[(+ fp!<59=get_balance/root> (- 3))]
MEM!20=[(+ ap!<59=get_balance/root>@5 3)]
MEM!21=[(+ ap!<59=get_balance/root>@5 4)]
MEM!22=[(+ ap!<59=get_balance/root>@5 22)]
MEM!23=[(+ fp!<59=get_balance/root> (- 2))]
MEM!24=[(+ ap!<root>@6 47)]
MEM!25=[(+ ap!<root>@6 43)]
MEM!26=[(+ ap!<root>@6 48)]
MEM!27=[(+ ap!<root>@6 44)]
MEM!28=[(+ ap!<root>@6 49)]
MEM!29=[(+ ap!<root>@6 45)]
MEM!30=[(+ ap!<root>@6 46)]
MEM!31=[(+ fp!<57=write/root> (- 5))]
MEM!32=[(+ ap!<root>@6 20)]
MEM!33=[(+ fp!<49=read/59=get_balance/root> (- 4))]
MEM!34=[(+ ap!<59=get_balance/root>@5 20)]
MEM!35=[(+ ap!<root>@6 (- 3))]
MEM!36=[(+ fp!<57=write/root> (- 4))]
MEM!37=[(+ ap!<root>@6 21)]
MEM!38=[(+ fp!<49=read/59=get_balance/root> (- 3))]
MEM!39=[(+ ap!<59=get_balance/root>@5 21)]
MEM!40=[(+ ap!<root>@6 (- 2))]
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
    (or (<= range-check!start MEM!30) (<= range-check!start 1337))
    (= (+ MEM!30 1337) (mod (+ MEM!30 1337) prime)))

  (=>
    (and
      (<= range-check!start (+ ap!<root>@6 45))
      (= (mod (+ ap!<root>@6 45) 1) 0)
      (< (+ ap!<root>@6 45) range-check!end))
    (<= 0 MEM!29 (+ range-check-bound (- 1))))
  
  (=>
    (and
      (<= range-check!start (+ ap!<root>@6 44))
      (= (mod (+ ap!<root>@6 44) 1) 0)
      (< (+ ap!<root>@6 44) range-check!end))
    (<= 0 MEM!27 (+ range-check-bound (- 1))))
    
  (=>
    (and
      (<= range-check!start (+ ap!<root>@6 43))
      (= (mod (+ ap!<root>@6 43) 1) 0)
      (< (+ ap!<root>@6 43) range-check!end))
    (<= 0 MEM!25 (+ range-check-bound (- 1))))
    
  (<= MEM!19 MEM!40)
  (<= range-check!start MEM!40)
  (= (mod MEM!40 1) 0)
  (<= MEM!38 MEM!39)
  (<= range-check!start MEM!39)
  (= (mod MEM!39 1) 0)
  
  (=>
    (and
      (<= range-check!start (+ fp!<59=get_balance/root> (- 3)))
      (= (mod (+ fp!<59=get_balance/root> (- 3)) 1) 0)
      (< (+ fp!<59=get_balance/root> (- 3)) range-check!end))
    (<= 0 MEM!19 (+ range-check-bound (- 1))))
    
  (=>
    (and
      (<= range-check!start (+ fp!<59=get_balance/root> (- 4)))
      (= (mod (+ fp!<59=get_balance/root> (- 4)) 1) 0)
      (< (+ fp!<59=get_balance/root> (- 4)) range-check!end))
    (<= 0 MEM!17 (+ range-check-bound (- 1))))
  
  (=>
    (and
      (<= range-check!start (+ fp!<59=get_balance/root> (- 5)))
      (= (mod (+ fp!<59=get_balance/root> (- 5)) 1) 0)
      (< (+ fp!<59=get_balance/root> (- 5)) range-check!end))
    (<= 0 MEM!15 (+ range-check-bound (- 1))))
  
  (<= MEM!36 MEM!37)
  (<= range-check!start MEM!37)
  (= (mod MEM!37 1) 0)

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
  (<= MEM!28 range-check!end)

  (=>
    (or (<= pedersen!start MEM!30) (<= pedersen!start 1337))
    (= (+ MEM!30 1337) (mod (+ MEM!30 1337) prime)))
    
  (<= MEM!17 MEM!35)
  (<= pedersen!start MEM!35)
  (= (mod MEM!35 3) 0)
  (<= MEM!33 MEM!34)
  (<= pedersen!start MEM!34)
  (= (mod MEM!34 3) 0)
  (<= MEM!31 MEM!32)
  (<= pedersen!start MEM!32)
  (= (mod MEM!32 3) 0)
  (<= pedersen!start MEM!5)
  (= (mod MEM!5 3) 0)
  (<= MEM!26 pedersen!end)
  (= (+ ap!<59=get_balance/root>@5 23) (+ ap!<root>@6 47))
  (= (+ MEM!30 1337) MEM!1)
  (= MEM!29 MEM!28)
  (= MEM!27 MEM!26)
  (= MEM!25 MEM!24)
  (= MEM!22 MEM!11)

  (not
    (and
      (= MEM!11 42)
      (= 42 MEM!11)
      (<= range-check!start MEM!19)
      (= (mod MEM!19 1) 0)
      (<= MEM!40 range-check!end)
      (<= range-check!start MEM!38)
      (= (mod MEM!38 1) 0)
      (<= MEM!39 range-check!end)
      (<= range-check!start MEM!36)
      (= (mod MEM!36 1) 0)
      (<= MEM!37 range-check!end)
      (<= MEM!7 MEM!28)
      (<= range-check!start MEM!28)
      (= (mod MEM!28 1) 0)
      (<= pedersen!start MEM!17)
      (= (mod MEM!17 3) 0)
      (<= MEM!35 pedersen!end)
      (<= pedersen!start MEM!33)
      (= (mod MEM!33 3) 0)
      (<= MEM!34 pedersen!end)
      (<= pedersen!start MEM!31)
      (= (mod MEM!31 3) 0)
      (<= MEM!32 pedersen!end)
      (<= MEM!5 MEM!26)
      (<= pedersen!start MEM!26)
      (= (mod MEM!26 3) 0)
      (= 1379 MEM!1)))
    
  (= fp!<49=read/59=get_balance/root> (+ ap!<59=get_balance/root>@5 5))
  (= MEM!20 fp!<59=get_balance/root>)
  (= MEM!21 51)
  (= MEM!19 MEM!18)
  (= MEM!17 MEM!16)
  (= MEM!15 MEM!14)
  (= fp!<59=get_balance/root> (+ ap!<root>@6 24))
  (= MEM!12 fp!<root>)
  (= MEM!13 61))
# Expect