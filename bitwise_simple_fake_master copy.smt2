# Memory
MEM!1=[ap!1]
MEM!2=[(+ ap!1 1)]
MEM!3=[(+ fp! (- 4))]
MEM!4=[(+ ap!1 2)]
MEM!5=[(+ fp! (- 3))]
MEM!6=[(+ ap!1 3)]
MEM!7=[(+ ap!1 4)]
MEM!8=[(+ ap!1 9)]
MEM!9=[(+ fp! (- 5))]
MEM!10=[(+ ap!1 10)]
MEM!11=[(+ ap!1 6)]
MEM!12=[(+ ap!1 11)]
MEM!13=[(+ ap!1 7)]
MEM!14=[(+ ap!1 12)]
MEM!15=[(+ ap!1 8)]
MEM!16=[(+ ap!1 5)]

# Assert
(<= fp! ap!1)
(= ap!1 fp!)
(= 42 MEM!1)
(= MEM!3 MEM!2)
(= MEM!5 MEM!4)

(and (= fp@12 (+ ap!1 5)) (= MEM!6 fp!) (= MEM!7 14))

(and
  (<= MEM!1 MEM!16)
  (<= bitwise!start MEM!16)
  (= (mod MEM!16 5) 0)
  (<= bitwise!start MEM!9)
  (= (mod MEM!9 5) 0)
  (<= MEM!8 bitwise!end)
  (= MEM!15 MEM!14)
  (= MEM!13 MEM!12)
  (= MEM!11 MEM!10)
  (= MEM!9 MEM!8)
  (not 
    (and 
      (<= bitwise!start MEM!1)
      (= (mod MEM!1 5) 0)
      (<= MEM!16 bitwise!end)
      (<= MEM!9 MEM!8)
      (<= bitwise!start MEM!8)
      (= (mod MEM!8 5) 0))))
# Expect