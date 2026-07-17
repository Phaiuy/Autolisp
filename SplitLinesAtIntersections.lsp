;;; ============================================================================
;;; SplitLinesAtIntersections.lsp
;;; ----------------------------------------------------------------------------
;;; Muc dich:
;;;   Chia (break) cac doi tuong LINE tai cac diem giao nhau giua chung
;;;   thanh cac doan LINE nho hon, doc lap.
;;;   Layer (va ca mau, linetype...) cua line goc duoc giu nguyen cho tung
;;;   doan nho tao ra.
;;;
;;; Lenh:  SLI  (Split Lines at Intersections)
;;;
;;; Cach dung:
;;;   1. Go lenh SLI
;;;   2. Chon cac line can xu ly (hoac Enter de chon tat ca line trong ban ve)
;;;   3. Chuong trinh se cat cac line tai moi diem giao va tao cac doan moi.
;;;
;;; Ghi chu:
;;;   - Chi xu ly doi tuong LINE. LWPOLYLINE/ARC... khong duoc xet.
;;;   - Cac line trung nhau (collinear) khong co diem giao don le nen bo qua.
;;;   - Toan bo thao tac nam trong 1 buoc UNDO.
;;; ============================================================================

(defun c:SLI ( / *error* fuzz ss n i j edata p1 p2 ename
                 lines-lst line-i pts qi qj ipt segdata
                 total-in total-out oldcmd oldosmode )

  ;; ---- Trinh xu ly loi -----------------------------------------------------
  (defun *error* (msg)
    (if oldosmode (setvar "OSMODE" oldosmode))
    (if oldcmd    (setvar "CMDECHO" oldcmd))
    (command "_.UNDO" "_End")
    (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
      (princ (strcat "\nLoi: " msg)))
    (princ))

  ;; ---- Ham phu: loai bo diem trung nhau (trong danh sach da sap xep) -------
  (defun rem-dup-pts (lst tol / out)
    (foreach p lst
      (if (or (null out) (> (distance p (car out)) tol))
        (setq out (cons p out))))
    (reverse out))

  ;; ---- Ham phu: tao 1 doan LINE moi, sao chep du lieu line goc -------------
  ;; base = entget cua line goc (da bo -1/5/330), a,b = 2 dau doan moi
  (defun make-seg (base a b / d)
    (setq d (subst (cons 10 a) (assoc 10 base) base))
    (setq d (subst (cons 11 b) (assoc 11 d)    d))
    (entmake d))

  ;; ---- Khoi tao ------------------------------------------------------------
  (setq fuzz 1e-8)                 ; dung sai so sanh diem
  (setq oldcmd    (getvar "CMDECHO"))
  (setq oldosmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (command "_.UNDO" "_Begin")

  ;; ---- Chon doi tuong ------------------------------------------------------
  (princ "\nChon cac LINE can chia (Enter = tat ca line trong ban ve): ")
  (setq ss (ssget '((0 . "LINE"))))
  (if (null ss)
    (setq ss (ssget "_X" '((0 . "LINE")))))

  (if (null ss)
    (progn
      (princ "\nKhong tim thay LINE nao.")
      (*error* nil)
    )
    (progn
      ;; ---- Doc du lieu tat ca line vao danh sach --------------------------
      ;; Moi phan tu: (ename p1 p2 base-edata)
      (setq n (sslength ss) i 0 lines-lst '())
      (repeat n
        (setq ename (ssname ss i))
        (setq edata (entget ename))
        (setq p1 (cdr (assoc 10 edata)))
        (setq p2 (cdr (assoc 11 edata)))
        ;; Loai bo cac ma khong dung cho entmake
        (setq edata (vl-remove-if
                      '(lambda (x) (member (car x) '(-1 5 330 102)))
                      edata))
        (setq lines-lst
              (cons (list ename p1 p2 edata) lines-lst))
        (setq i (1+ i)))
      (setq lines-lst (reverse lines-lst))

      (setq total-in  n
            total-out 0)

      ;; ---- Duyet tung line, tim diem giao & chia --------------------------
      (foreach line-i lines-lst
        (setq p1  (nth 1 line-i)
              p2  (nth 2 line-i)
              pts (list p1 p2))          ; luon co 2 dau mut

        ;; Tim giao voi moi line khac
        (foreach line-j lines-lst
          (if (not (eq (car line-i) (car line-j)))
            (progn
              (setq qi (nth 1 line-j)
                    qj (nth 2 line-j))
              ;; inters voi tham so nil: chi tra ve khi diem nam tren CA 2 doan
              (setq ipt (inters p1 p2 qi qj nil))
              (if ipt
                (setq pts (cons ipt pts))))))

        ;; Sap xep cac diem doc theo line (theo khoang cach tu p1)
        (setq pts (vl-sort pts
                    '(lambda (a b) (< (distance p1 a) (distance p1 b)))))
        ;; Loai diem trung
        (setq pts (rem-dup-pts pts fuzz))

        ;; Neu co nhieu hon 2 diem => co diem giao => chia
        (if (> (length pts) 2)
          (progn
            (setq segdata (nth 3 line-i))
            ;; tao cac doan lien tiep
            (setq j 0)
            (repeat (1- (length pts))
              (make-seg segdata (nth j pts) (nth (1+ j) pts))
              (setq total-out (1+ total-out))
              (setq j (1+ j)))
            ;; xoa line goc
            (entdel (car line-i)))))

      ;; ---- Thong bao ket qua ---------------------------------------------
      (princ (strcat "\nDa xu ly " (itoa total-in) " line."))
      (if (> total-out 0)
        (princ (strcat "\nDa tao " (itoa total-out)
                       " doan line moi tu cac line bi cat."))
        (princ "\nKhong co diem giao nao => khong line nao bi cat."))
    )
  )

  ;; ---- Ket thuc ------------------------------------------------------------
  (setvar "OSMODE" oldosmode)
  (setvar "CMDECHO" oldcmd)
  (command "_.UNDO" "_End")
  (princ))

(princ "\nDa nap SplitLinesAtIntersections.lsp - Go lenh SLI de chay.")
(princ)
