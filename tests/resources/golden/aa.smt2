# Memory
MEM!1=[(mod (mod (+ ap!<root>@2 7) prime) prime)]
MEM!2=[(mod (+ ap!<root>@2 0) prime)]
MEM!3=[(mod (+ ap!<root>@2 1) prime)]
MEM!4=[(mod (+ ap!<root>@2 2) prime)]
MEM!5=[(mod (+ fp!<9=succ/root> (- 3)) prime)]
MEM!6=[(mod (+ ap!<9=succ/root>@0 0) prime)]
MEM!7=[(mod (+ ap!<root>@2 4) prime)]
MEM!8=[(mod (+ ap!<root>@2 5) prime)]
MEM!9=[(mod (mod (+ fp!<11=pred/root> (- 3)) prime) prime)]
# Assert
(<= fp!<root> (+ ap!<root>@2 0))
(= (+ ap!<root>@2 0) fp!<root>)
(= 1000 MEM!2)
(and (= fp!<9=succ/root> (+ ap!<root>@2 3)) (= MEM!3 fp!<root>) (= MEM!4 11))
(= (mod (+ MEM!5 1) prime) MEM!6)
(and (= fp!<11=pred/root> (+ ap!<root>@2 6)) (= MEM!7 fp!<root>) (= MEM!8 13))
(=>
  (or (< 254 MEM!9) (= 0 MEM!9))
  (and (< (+ ap!<root>@2 4) (+ ap!<root>@2 8))
       (= (+ ap!<9=succ/root>@0 1) (+ ap!<root>@2 4))
       (or (< 254 MEM!9) (= 0 MEM!9))
       (= MEM!1 (mod (+ MEM!9 (- 1)) prime))
       (not (= 1000 MEM!1))))
# Expect