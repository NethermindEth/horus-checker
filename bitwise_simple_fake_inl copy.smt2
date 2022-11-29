# Memory
MEM!1=[ap!<root>@1]
MEM!2=[(+ ap!<root>@1 1)]
MEM!3=[(+ fp!<root> (- 4))]
MEM!4=[(+ ap!<root>@1 2)]
MEM!5=[(+ fp!<root> (- 3))]
MEM!6=[(+ ap!<root>@1 3)]
MEM!7=[(+ ap!<root>@1 4)]
MEM!8=[(+ fp!<12=apply_bitwise/root> (- 4))]
MEM!9=[(+ fp!<12=apply_bitwise/root> (- 5))]
MEM!10=[MEM!9]
MEM!11=[(+ fp!<12=apply_bitwise/root> (- 3))]
MEM!12=[(+ MEM!9 1)]
MEM!13=[ap!<12=apply_bitwise/root>@0]
MEM!14=[(+ ap!<12=apply_bitwise/root>@0 1)]
MEM!15=[(+ MEM!9 7)]
MEM!16=[(+ ap!<12=apply_bitwise/root>@0 2)]
MEM!17=[(+ MEM!9 8)]
MEM!18=[(+ ap!<12=apply_bitwise/root>@0 3)]
MEM!19=[(+ MEM!9 9)]
MEM!20=[(+ fp!<12=apply_bitwise/root> (- 2))]
MEM!21=[(+ ap!<root>@1 9)]
MEM!22=[(+ fp!<root> (- 5))]
MEM!23=[(+ ap!<root>@1 10)]
MEM!24=[(+ ap!<root>@1 6)]
MEM!25=[(+ ap!<root>@1 11)]
MEM!26=[(+ ap!<root>@1 7)]
MEM!27=[(+ ap!<root>@1 12)]
MEM!28=[(+ ap!<root>@1 8)]
MEM!29=[(+ ap!<root>@1 5)]

# Assert
(<= fp!<root> ap!<root>@1)
(= ap!<root>@1 fp!<root>)
(= 42 MEM!1)
(= MEM!3 MEM!2)
(= MEM!5 MEM!4)

(and (= fp!<12=apply_bitwise/root> (+ ap!<root>@1 5)) (= MEM!6 fp!<root>) (= MEM!7 0))

(= MEM!10 MEM!8)
(= MEM!12 MEM!11)
(= (+ MEM!9 5) MEM!13)
(= MEM!15 MEM!14)
(= MEM!17 MEM!16)
(= MEM!19 MEM!18)
(= MEM!22 MEM!21)
(= MEM!24 MEM!23)
(= MEM!26 MEM!25)
(= MEM!28 MEM!27)

(= (+ ap!<12=apply_bitwise/root>@0 4) (+ ap!<root>@1 9))
(and (<= bitwise!start MEM!22) (= (mod MEM!22 5) 0) (<= MEM!21 bitwise!end))
(=> (or (<= bitwise!start MEM!9) (<= bitwise!start 5)) (= (+ MEM!9 5) (mod (+ MEM!9 5) prime)))
(and (<= MEM!9 MEM!29) (<= bitwise!start MEM!29) (= (mod MEM!29 5) 0))
# Expect
(and (<= MEM!22 MEM!21) (<= bitwise!start MEM!21) (= (mod MEM!21 5) 0))
(and (<= bitwise!start MEM!9) (= (mod MEM!9 5) 0) (<= MEM!29 bitwise!end))