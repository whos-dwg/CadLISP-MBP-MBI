; ============================================================
; MBP + MBI Combined - Stable BREAKATPOINT Edition
;
; MBP: Select one object and specify multiple break points manually
; MBI: Select cutting edges, then target objects, and break at all intersections
;
; Load this file once to use both MBP and MBI.
; SPLINE / POLYLINE use BREAKATPOINT. ELLIPSE is rebuilt by parameter intervals.
; Unused helpers, multiline command strings, and unnecessary error output removed.
; ============================================================

(vl-load-com)


; -------------------- MBP --------------------

(defun c:MBP (/ *error* oldcmdecho ent ed typ pts pt ptw cpt param pdata totaldist)


  (setq oldcmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (defun *error* (msg)
    (if oldcmdecho
      (setvar "CMDECHO" oldcmdecho)
    )
    (if
      (and
        msg
        (not
          (wcmatch
            (strcase msg)
            "*CANCEL*,*QUIT*,*EXIT*"
          )
        )
      )
      (prompt (strcat "\nError: " msg))
    )
    (princ)
  )

  (defun _2pi ()
    (* 2.0 pi)
  )

  (defun _normang (a)
    (while (< a 0.0)
      (setq a (+ a (_2pi)))
    )
    (while (>= a (_2pi))
      (setq a (- a (_2pi)))
    )
    a
  )

  (defun _sameparam-p (p lst)
    (vl-some
      '(lambda (x)
         (equal p (car x) 1e-8)
       )
      lst
    )
  )

  (defun _sameang-p (a lst)
    (vl-some
      '(lambda (x)
         (equal a (car x) 1e-8)
       )
      lst
    )
  )

  (defun _common-props (ed)
    (append
      (if (assoc 8 ed)   (list (assoc 8 ed))   nil)
      (if (assoc 6 ed)   (list (assoc 6 ed))   nil)
      (if (assoc 48 ed)  (list (assoc 48 ed))  nil)
      (if (assoc 62 ed)  (list (assoc 62 ed))  nil)
      (if (assoc 370 ed) (list (assoc 370 ed)) nil)
      (if (assoc 39 ed)  (list (assoc 39 ed))  nil)
    )
  )

  (defun _make-line (p1 p2 ed)
    (if (> (distance p1 p2) 1e-8)
      (entmakex
        (append
          (list
            '(0 . "LINE")
            (cons 10 p1)
            (cons 11 p2)
          )
          (_common-props ed)
        )
      )
    )
  )

  (defun _make-arc (cen rad a1 a2 ed)
    (if (< a2 a1)
      (setq a2 (+ a2 (_2pi)))
    )

    (if (> (abs (- a2 a1)) 1e-8)
      (entmakex
        (append
          (list
            '(0 . "ARC")
            (cons 10 cen)
            (cons 40 rad)
            (cons 50 (_normang a1))
            (cons 51 (_normang a2))
          )
          (_common-props ed)
        )
      )
    )
  )

  (defun _split-line (ent pts ed / p1 p2 sorted all i a b)
    (setq p1 (cdr (assoc 10 ed)))
    (setq p2 (cdr (assoc 11 ed)))

    (setq sorted
      (vl-sort pts
        '(lambda (a b)
           (< (car a) (car b))
         )
      )
    )

    (setq all
      (append
        (list (list 0.0 p1))
        sorted
        (list (list 1.0 p2))
      )
    )

    (setq i 0)
    (while (< i (1- (length all)))
      (setq a (cadr (nth i all)))
      (setq b (cadr (nth (1+ i) all)))
      (_make-line a b ed)
      (setq i (1+ i))
    )

    (entdel ent)
  )

  (defun _split-arc (ent pts ed / cen rad sa ea data sorted all i a1 a2 x a reala)
    (setq cen (cdr (assoc 10 ed)))
    (setq rad (cdr (assoc 40 ed)))
    (setq sa  (cdr (assoc 50 ed)))
    (setq ea  (cdr (assoc 51 ed)))

    (if (< ea sa)
      (setq ea (+ ea (_2pi)))
    )

    (setq data nil)

    (foreach x pts
      (setq a (car x))
      (setq reala a)

      (if (< reala sa)
        (setq reala (+ reala (_2pi)))
      )

      (if (and (> reala (+ sa 1e-8)) (< reala (- ea 1e-8)))
        (setq data (cons (list reala (cadr x)) data))
      )
    )

    (setq sorted
      (vl-sort data
        '(lambda (a b)
           (< (car a) (car b))
         )
      )
    )

    (setq all
      (append
        (list (list sa nil))
        sorted
        (list (list ea nil))
      )
    )

    (setq i 0)
    (while (< i (1- (length all)))
      (setq a1 (car (nth i all)))
      (setq a2 (car (nth (1+ i) all)))
      (_make-arc cen rad a1 a2 ed)
      (setq i (1+ i))
    )

    (entdel ent)
  )

  (defun _split-circle (ent pts ed / cen rad sorted i a1 a2 lastang firstang)
    (setq cen (cdr (assoc 10 ed)))
    (setq rad (cdr (assoc 40 ed)))

    (if (< (length pts) 2)
      (prompt "\nA circle requires at least two break points.")
      (progn
        (setq sorted
          (vl-sort pts
            '(lambda (a b)
               (< (car a) (car b))
             )
          )
        )

        (setq i 0)
        (while (< i (1- (length sorted)))
          (setq a1 (car (nth i sorted)))
          (setq a2 (car (nth (1+ i) sorted)))
          (_make-arc cen rad a1 a2 ed)
          (setq i (1+ i))
        )

        (setq lastang  (car (car (reverse sorted))))
        (setq firstang (car (car sorted)))

        (_make-arc cen rad lastang (+ firstang (_2pi)) ed)

        (entdel ent)
      )
    )
  )

  ;; Read LWPOLYLINE vertex data
  ;; Return format:
  ;; ((point bulge) (point bulge) ...)
  ;; Bulge stores the arc data from the current vertex to the next vertex
  (defun _lw-get-verts (ed / lst item p b result)
    (setq lst ed)
    (setq result nil)
    (setq p nil)
    (setq b 0.0)

    (foreach item lst
      (cond
        ((= (car item) 10)
          (if p
            (setq result (append result (list (list p b))))
          )
          (setq p (cdr item))
          (setq b 0.0)
        )

        ((= (car item) 42)
          (setq b (cdr item))
        )
      )
    )

    (if p
      (setq result (append result (list (list p b))))
    )

    result
  )

  (defun _lw-closed-p (ed)
    (= 1 (logand 1 (cdr (assoc 70 ed))))
  )

  (defun _lw-param-mod (p n)
    (while (< p 0.0)
      (setq p (+ p n))
    )
    (while (>= p n)
      (setq p (- p n))
    )
    p
  )

  (defun _lw-get-point-at-param (ent p n closed / pp)
    (setq pp p)

    (if closed
      (setq pp (_lw-param-mod pp n))
    )

    (vlax-curve-getPointAtParam ent pp)
  )

  (defun _lw-get-vertex-point (verts idx)
    (car (nth idx verts))
  )

  (defun _lw-get-bulge (verts idx)
    (cadr (nth idx verts))
  )

  (defun _lw-sub-bulge (origbulge frac / theta)
    (if (equal origbulge 0.0 1e-12)
      0.0
      (progn
        (setq theta (* 4.0 (atan origbulge)))
        (tan (/ (* theta frac) 4.0))
      )
    )
  )

  ;; Create an open LWPOLYLINE segment
  (defun _lw-make-poly (nodes bulges ed / dxf i)
    (if (> (length nodes) 1)
      (progn
        (setq dxf
          (append
            (list
              '(0 . "LWPOLYLINE")
              '(100 . "AcDbEntity")
            )
            (_common-props ed)
            (list
              '(100 . "AcDbPolyline")
              (cons 90 (length nodes))
              (cons 70 0)
            )
          )
        )

        (setq i 0)
        (while (< i (length nodes))
          (setq dxf
            (append
              dxf
              (list
                (cons 10 (list (car (nth i nodes)) (cadr (nth i nodes))))
              )
            )
          )

          (if (< i (length bulges))
            (if (not (equal (nth i bulges) 0.0 1e-12))
              (setq dxf
                (append
                  dxf
                  (list (cons 42 (nth i bulges)))
                )
              )
            )
          )

          (setq i (1+ i))
        )

        (entmakex dxf)
      )
    )
  )

  ;; Get points and bulges within a parameter interval
  (defun _lw-build-section (ent verts s e n closed / nodes bulges a b k base frac bul p1 p2)

    (setq nodes nil)
    (setq bulges nil)

    (setq nodes
      (append
        nodes
        (list (_lw-get-point-at-param ent s n closed))
      )
    )

    (setq a s)

    (while (< a (- e 1e-8))

      (setq k (fix a))

      (if (and (equal a (float k) 1e-8) (> a s))
        (setq k (fix a))
      )

      (setq b (1+ k))

      (if (> b e)
        (setq b e)
      )

      (if (> (- b a) 1e-8)
        (progn
          (setq base (rem k n))
          (setq frac (- b a))
          (setq bul (_lw-sub-bulge (_lw-get-bulge verts base) frac))

          (setq bulges (append bulges (list bul)))
          (setq nodes
            (append
              nodes
              (list (_lw-get-point-at-param ent b n closed))
            )
          )
        )
      )

      (setq a b)
    )

    (list nodes bulges)
  )

  (defun _split-lwpoly (ent pts ed / verts n closed endp sorted all sections i s e sec nodes bulges p p2)

    (setq verts (_lw-get-verts ed))
    (setq n (length verts))
    (setq closed (_lw-closed-p ed))

    (if (< n 2)
      (prompt "\nThe polyline does not have enough vertices to be split.")
      (progn
        (setq endp (vlax-curve-getEndParam ent))

        ;; Open polyline: exclude the start and end points
        ;; Closed polyline: convert the end parameter back to 0
        (setq sorted nil)

        (foreach p pts
          (setq p2 (car p))

          (if closed
            (progn
              (if (equal p2 endp 1e-8)
                (setq p2 0.0)
              )

              (if (not (_sameparam-p p2 sorted))
                (setq sorted
                  (cons
                    (list p2 (cadr p))
                    sorted
                  )
                )
              )
            )
            (progn
              (if
                (and
                  (> p2 1e-8)
                  (< p2 (- endp 1e-8))
                  (not (_sameparam-p p2 sorted))
                )
                (setq sorted
                  (cons
                    (list p2 (cadr p))
                    sorted
                  )
                )
              )
            )
          )
        )

        (setq sorted
          (vl-sort sorted
            '(lambda (a b)
               (< (car a) (car b))
             )
          )
        )

        (cond
          ((and closed (< (length sorted) 2))
            (prompt "\nA closed polyline requires at least two break points.")
          )

          ((and (not closed) (< (length sorted) 1))
            (prompt "\nNo valid polyline break points were found.")
          )

          (closed
            ;; Closed polyline:
            ;; Create each segment between consecutive break points, wrapping the last segment to the first point
            (setq i 0)
            (while (< i (1- (length sorted)))
              (setq s (car (nth i sorted)))
              (setq e (car (nth (1+ i) sorted)))

              (setq sec (_lw-build-section ent verts s e n closed))
              (setq nodes (car sec))
              (setq bulges (cadr sec))

              (_lw-make-poly nodes bulges ed)

              (setq i (1+ i))
            )

            ;; Final segment: connect the last break point back to the first break point
            (setq s (car (car (reverse sorted))))
            (setq e (+ (car (car sorted)) n))

            (setq sec (_lw-build-section ent verts s e n closed))
            (setq nodes (car sec))
            (setq bulges (cadr sec))

            (_lw-make-poly nodes bulges ed)

            (entdel ent)
          )

          ((not closed)
            ;; Open polyline:
            ;; Start point -> break points -> end point
            (setq all
              (append
                (list (list 0.0 nil))
                sorted
                (list (list endp nil))
              )
            )

            (setq i 0)
            (while (< i (1- (length all)))
              (setq s (car (nth i all)))
              (setq e (car (nth (1+ i) all)))

              (setq sec (_lw-build-section ent verts s e n closed))
              (setq nodes (car sec))
              (setq bulges (cadr sec))

              (_lw-make-poly nodes bulges ed)

              (setq i (1+ i))
            )

            (entdel ent)
          )
        )
      )
    )
  )


  ;; Get the projected point and distance along the curve safely.
  ;; SPLINE parameter ranges are not always suitable for direct validation,
  ;; so distance from the start point is used as the sorting key.
  (defun _curve-data-at-point (ent p / cp dst)
    (setq cp
      (vl-catch-all-apply
        'vlax-curve-getClosestPointTo
        (list ent p)
      )
    )

    (if (vl-catch-all-error-p cp)
      nil
      (progn
        (setq dst
          (vl-catch-all-apply
            'vlax-curve-getDistAtPoint
            (list ent cp)
          )
        )

        (if (vl-catch-all-error-p dst)
          nil
          (list dst cp)
        )
      )
    )
  )




  ;; Split an ELLIPSE by rebuilding its parameter intervals.
  ;; This does not call BREAK, does not change OSMODE, and does not send
  ;; coordinate input through the command line.
  (defun _ellipse-clean-dxf (data / out item)
    (foreach item data
      (if (not (member (car item) '(-1 5 330 360)))
        (setq out (cons item out))
      )
    )
    (reverse out)
  )

  (defun _ellipse-set-dxf (code value data / old)
    (if (setq old (assoc code data))
      (subst (cons code value) old data)
      (append data (list (cons code value)))
    )
  )

  (defun _ellipse-unique-params (ent pts / item prm out)
    (foreach item pts
      (setq prm
        (vl-catch-all-apply
          'vlax-curve-getParamAtPoint
          (list ent (cadr item))
        )
      )
      (if
        (and
          (not (vl-catch-all-error-p prm))
          prm
          (not
            (vl-some
              '(lambda (x) (< (abs (- x prm)) 1e-9))
              out
            )
          )
        )
        (setq out (cons prm out))
      )
    )
    (vl-sort out '<)
  )

  (defun _ellipse-make-arc (base start end / data)
    (setq data (_ellipse-set-dxf 41 start base))
    (setq data (_ellipse-set-dxf 42 end data))
    (entmakex data)
  )

  (defun _split-ellipse-break
    (ent pts / ed base closed params start end bounds made failed a b firstp aent)
    (setq ed (entget ent))
    (setq base (_ellipse-clean-dxf ed))
    (setq params (_ellipse-unique-params ent pts))

    (setq closed
      (vl-catch-all-apply
        'vlax-curve-isClosed
        (list ent)
      )
    )
    (if (vl-catch-all-error-p closed)
      (setq closed nil)
    )

    (setq start (cdr (assoc 41 ed)))
    (setq end   (cdr (assoc 42 ed)))
    (if (null start) (setq start 0.0))
    (if (null end)   (setq end (* 2.0 pi)))

    (cond
      ((null params)
        (prompt "\nNo valid ellipse break points found.")
        nil
      )

      ((and closed (< (length params) 2))
        (prompt "\nA full ellipse requires at least two break points.")
        nil
      )

      (closed
        (setq made nil)
        (setq failed nil)
        (setq firstp (car params))

        ;; Build every consecutive interval.
        (while (cadr params)
          (setq a (car params))
          (setq b (cadr params))
          (if (> (- b a) 1e-10)
            (if (setq aent (_ellipse-make-arc base a b))
              (setq made (cons aent made))
              (setq failed T)
            )
          )
          (setq params (cdr params))
        )

        ;; Build the wrap-around interval.
        (setq a (car params))
        (setq b (+ firstp (* 2.0 pi)))
        (if (> (- b a) 1e-10)
          (if (setq aent (_ellipse-make-arc base a b))
            (setq made (cons aent made))
            (setq failed T)
          )
        )

        (if failed
          (progn
            (foreach aent made
              (if (and aent (entget aent))
                (entdel aent)
              )
            )
            (prompt "\nEllipse split failed. The original ellipse was kept.")
            nil
          )
          (progn
            (entdel ent)
            T
          )
        )
      )

      (T
        (setq bounds (append (list start) params (list end)))
        (setq made nil)
        (setq failed nil)

        (while (cadr bounds)
          (setq a (car bounds))
          (setq b (cadr bounds))
          (if (> (- b a) 1e-10)
            (if (setq aent (_ellipse-make-arc base a b))
              (setq made (cons aent made))
              (setq failed T)
            )
          )
          (setq bounds (cdr bounds))
        )

        (if failed
          (progn
            (foreach aent made
              (if (and aent (entget aent))
                (entdel aent)
              )
            )
            (prompt "\nEllipse split failed. The original ellipse was kept.")
            nil
          )
          (progn
            (entdel ent)
            T
          )
        )
      )
    )
  )

  ;; Use AutoCAD native BREAKATPOINT to split general curves point by point.
  ;; Process from the curve end toward the start so the original entity keeps the front segment,
  ;; allowing earlier break points to remain on the same original entity.
  ;; Supports SPLINE and legacy POLYLINE. ELLIPSE uses parameter-based rebuilding.
  (defun _split-generic-open (ent pts / sorted item p result ok)
    (setq sorted
      (vl-sort pts
        '(lambda (a b)
           (> (car a) (car b))
         )
      )
    )

    (setq ok T)

    (foreach item sorted
      (if (and ent (entget ent))
        (progn
          (setq p (cadr item))
          (setq result
            (vl-catch-all-apply
              'command-s
              (list
                "_.BREAKATPOINT"
                ent
                (trans p 0 1)
              )
            )
          )

          (if (vl-catch-all-error-p result)
            (progn
              (setq ok nil)
              (prompt "\nBREAKATPOINT failed at one point and was skipped.")
            )
          )
        )
        (setq ok nil)
      )
    )

    ok
  )

  ;; Main command
  (prompt "\nSelect an object to break (Line, Arc, Circle, Polyline, Spline, Ellipse):")
  (setq ent (car (entsel)))

  (if ent
    (progn
      (setq ed  (entget ent))
      (setq typ (cdr (assoc 0 ed)))

      (if (not (member typ '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POLYLINE" "SPLINE" "ELLIPSE")))
        (prompt "\nSupported types: LINE, ARC, CIRCLE, LWPOLYLINE, POLYLINE, SPLINE, ELLIPSE.")
        (progn
          (setq pts nil)

          (prompt "\nSpecify break points continuously. Press Enter or Space to finish.")

          (while (setq pt (getpoint "\nSpecify break point:"))
            (setq ptw (trans pt 1 0))
            (setq cpt (vlax-curve-getClosestPointTo ent ptw))

            (cond
              ((= typ "LINE")
                (setq param (vlax-curve-getParamAtPoint ent cpt))

                (if
                  (and
                    param
                    (> param 1e-8)
                    (< param (- (vlax-curve-getEndParam ent) 1e-8))
                    (not (_sameparam-p param pts))
                  )
                  (progn
                    (setq pts (cons (list param cpt) pts))
                    (prompt "\nBreak point added.")
                  )
                  (prompt "\nInvalid or duplicate point skipped.")
                )
              )

              ((= typ "ARC")
                (setq param (_normang (angle (cdr (assoc 10 ed)) cpt)))

                (if (not (_sameang-p param pts))
                  (progn
                    (setq pts (cons (list param cpt) pts))
                    (prompt "\nBreak point added.")
                  )
                  (prompt "\nDuplicate point skipped.")
                )
              )

              ((= typ "CIRCLE")
                (setq param (_normang (angle (cdr (assoc 10 ed)) cpt)))

                (if (not (_sameang-p param pts))
                  (progn
                    (setq pts (cons (list param cpt) pts))
                    (prompt "\nBreak point added.")
                  )
                  (prompt "\nDuplicate point skipped.")
                )
              )

              ((= typ "LWPOLYLINE")
                (setq param (vlax-curve-getParamAtPoint ent cpt))

                (if
                  (and
                    param
                    (not (_sameparam-p param pts))
                  )
                  (progn
                    (setq pts (cons (list param cpt) pts))
                    (prompt "\nBreak point added.")
                  )
                  (prompt "\nDuplicate or invalid point skipped.")
                )
              )

              ((member typ '("POLYLINE" "SPLINE" "ELLIPSE"))
                (setq pdata (_curve-data-at-point ent ptw))
                (setq totaldist
                  (vlax-curve-getDistAtParam
                    ent
                    (vlax-curve-getEndParam ent)
                  )
                )

                (if pdata
                  (progn
                    (setq param (car pdata))
                    (setq cpt   (cadr pdata))
                  )
                  (setq param nil)
                )

                (if
                  (and
                    param
                    (> param 1e-8)
                    (< param (- totaldist 1e-8))
                    (not (_sameparam-p param pts))
                  )
                  (progn
                    (setq pts (cons (list param cpt) pts))
                    (prompt "\nBreak point added.")
                  )
                  (prompt "\nEndpoint, duplicate, or invalid projected point skipped.")
                )
              )
            )
          )

          (if pts
            (cond
              ((= typ "LINE")
                (_split-line ent pts ed)
              )

              ((= typ "ARC")
                (_split-arc ent pts ed)
              )

              ((= typ "CIRCLE")
                (_split-circle ent pts ed)
              )

              ((= typ "LWPOLYLINE")
                (_split-lwpoly ent pts ed)
              )


              ((= typ "ELLIPSE")
                (_split-ellipse-break ent pts)
              )

              ((member typ '("POLYLINE" "SPLINE"))
                (_split-generic-open ent pts)
              )
            )
            (prompt "\nNo break points were specified.")
          )
        )
      )
    )
    (prompt "\nNo object selected.")
  )

  (setvar "CMDECHO" oldcmdecho)
  (princ)
)

; -------------------- MBI --------------------

(defun c:MBI (/ *error* oldcmdecho cutters targets ent ed typ pts i done nohit)


  (setq oldcmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (defun *error* (msg)
    (if oldcmdecho
      (setvar "CMDECHO" oldcmdecho)
    )
    (if
      (and
        msg
        (not
          (wcmatch
            (strcase msg)
            "*CANCEL*,*QUIT*,*EXIT*"
          )
        )
      )
      (prompt (strcat "\nError: " msg))
    )
    (princ)
  )

  (defun _2pi ()
    (* 2.0 pi)
  )

  (defun _normang (a)
    (while (< a 0.0)
      (setq a (+ a (_2pi)))
    )
    (while (>= a (_2pi))
      (setq a (- a (_2pi)))
    )
    a
  )

  (defun _sameparam-p (p lst)
    (vl-some
      '(lambda (x)
         (equal p (car x) 1e-8)
       )
      lst
    )
  )

  (defun _sameang-p (a lst)
    (vl-some
      '(lambda (x)
         (equal a (car x) 1e-8)
       )
      lst
    )
  )

  (defun _common-props (ed)
    (append
      (if (assoc 8 ed)   (list (assoc 8 ed))   nil)
      (if (assoc 6 ed)   (list (assoc 6 ed))   nil)
      (if (assoc 48 ed)  (list (assoc 48 ed))  nil)
      (if (assoc 62 ed)  (list (assoc 62 ed))  nil)
      (if (assoc 370 ed) (list (assoc 370 ed)) nil)
      (if (assoc 39 ed)  (list (assoc 39 ed))  nil)
    )
  )

  (defun _make-line (p1 p2 ed)
    (if (> (distance p1 p2) 1e-8)
      (entmakex
        (append
          (list
            '(0 . "LINE")
            (cons 10 p1)
            (cons 11 p2)
          )
          (_common-props ed)
        )
      )
    )
  )

  (defun _make-arc (cen rad a1 a2 ed)
    (if (< a2 a1)
      (setq a2 (+ a2 (_2pi)))
    )

    (if (> (abs (- a2 a1)) 1e-8)
      (entmakex
        (append
          (list
            '(0 . "ARC")
            (cons 10 cen)
            (cons 40 rad)
            (cons 50 (_normang a1))
            (cons 51 (_normang a2))
          )
          (_common-props ed)
        )
      )
    )
  )

  (defun _split-line (ent pts ed / p1 p2 sorted all i a b)
    (setq p1 (cdr (assoc 10 ed)))
    (setq p2 (cdr (assoc 11 ed)))

    (setq sorted
      (vl-sort pts
        '(lambda (a b)
           (< (car a) (car b))
         )
      )
    )

    (setq all
      (append
        (list (list 0.0 p1))
        sorted
        (list (list 1.0 p2))
      )
    )

    (setq i 0)
    (while (< i (1- (length all)))
      (setq a (cadr (nth i all)))
      (setq b (cadr (nth (1+ i) all)))
      (_make-line a b ed)
      (setq i (1+ i))
    )

    (entdel ent)
  )

  (defun _split-arc (ent pts ed / cen rad sa ea data sorted all i a1 a2 x a reala)
    (setq cen (cdr (assoc 10 ed)))
    (setq rad (cdr (assoc 40 ed)))
    (setq sa  (cdr (assoc 50 ed)))
    (setq ea  (cdr (assoc 51 ed)))

    (if (< ea sa)
      (setq ea (+ ea (_2pi)))
    )

    (setq data nil)

    (foreach x pts
      (setq a (car x))
      (setq reala a)

      (if (< reala sa)
        (setq reala (+ reala (_2pi)))
      )

      (if (and (> reala (+ sa 1e-8)) (< reala (- ea 1e-8)))
        (setq data (cons (list reala (cadr x)) data))
      )
    )

    (setq sorted
      (vl-sort data
        '(lambda (a b)
           (< (car a) (car b))
         )
      )
    )

    (setq all
      (append
        (list (list sa nil))
        sorted
        (list (list ea nil))
      )
    )

    (setq i 0)
    (while (< i (1- (length all)))
      (setq a1 (car (nth i all)))
      (setq a2 (car (nth (1+ i) all)))
      (_make-arc cen rad a1 a2 ed)
      (setq i (1+ i))
    )

    (entdel ent)
  )

  (defun _split-circle (ent pts ed / cen rad sorted i a1 a2 lastang firstang)
    (setq cen (cdr (assoc 10 ed)))
    (setq rad (cdr (assoc 40 ed)))

    (if (< (length pts) 2)
      (prompt "\nA circle requires at least two break points.")
      (progn
        (setq sorted
          (vl-sort pts
            '(lambda (a b)
               (< (car a) (car b))
             )
          )
        )

        (setq i 0)
        (while (< i (1- (length sorted)))
          (setq a1 (car (nth i sorted)))
          (setq a2 (car (nth (1+ i) sorted)))
          (_make-arc cen rad a1 a2 ed)
          (setq i (1+ i))
        )

        (setq lastang  (car (car (reverse sorted))))
        (setq firstang (car (car sorted)))

        (_make-arc cen rad lastang (+ firstang (_2pi)) ed)

        (entdel ent)
      )
    )
  )

  ;; Read LWPOLYLINE vertex data
  ;; Return format:
  ;; ((point bulge) (point bulge) ...)
  ;; Bulge stores the arc data from the current vertex to the next vertex
  (defun _lw-get-verts (ed / lst item p b result)
    (setq lst ed)
    (setq result nil)
    (setq p nil)
    (setq b 0.0)

    (foreach item lst
      (cond
        ((= (car item) 10)
          (if p
            (setq result (append result (list (list p b))))
          )
          (setq p (cdr item))
          (setq b 0.0)
        )

        ((= (car item) 42)
          (setq b (cdr item))
        )
      )
    )

    (if p
      (setq result (append result (list (list p b))))
    )

    result
  )

  (defun _lw-closed-p (ed)
    (= 1 (logand 1 (cdr (assoc 70 ed))))
  )

  (defun _lw-param-mod (p n)
    (while (< p 0.0)
      (setq p (+ p n))
    )
    (while (>= p n)
      (setq p (- p n))
    )
    p
  )

  (defun _lw-get-point-at-param (ent p n closed / pp)
    (setq pp p)

    (if closed
      (setq pp (_lw-param-mod pp n))
    )

    (vlax-curve-getPointAtParam ent pp)
  )

  (defun _lw-get-vertex-point (verts idx)
    (car (nth idx verts))
  )

  (defun _lw-get-bulge (verts idx)
    (cadr (nth idx verts))
  )

  (defun _lw-sub-bulge (origbulge frac / theta)
    (if (equal origbulge 0.0 1e-12)
      0.0
      (progn
        (setq theta (* 4.0 (atan origbulge)))
        (tan (/ (* theta frac) 4.0))
      )
    )
  )

  ;; Create an open LWPOLYLINE segment
  (defun _lw-make-poly (nodes bulges ed / dxf i)
    (if (> (length nodes) 1)
      (progn
        (setq dxf
          (append
            (list
              '(0 . "LWPOLYLINE")
              '(100 . "AcDbEntity")
            )
            (_common-props ed)
            (list
              '(100 . "AcDbPolyline")
              (cons 90 (length nodes))
              (cons 70 0)
            )
          )
        )

        (setq i 0)
        (while (< i (length nodes))
          (setq dxf
            (append
              dxf
              (list
                (cons 10 (list (car (nth i nodes)) (cadr (nth i nodes))))
              )
            )
          )

          (if (< i (length bulges))
            (if (not (equal (nth i bulges) 0.0 1e-12))
              (setq dxf
                (append
                  dxf
                  (list (cons 42 (nth i bulges)))
                )
              )
            )
          )

          (setq i (1+ i))
        )

        (entmakex dxf)
      )
    )
  )

  ;; Get points and bulges within a parameter interval
  (defun _lw-build-section (ent verts s e n closed / nodes bulges a b k base frac bul p1 p2)

    (setq nodes nil)
    (setq bulges nil)

    (setq nodes
      (append
        nodes
        (list (_lw-get-point-at-param ent s n closed))
      )
    )

    (setq a s)

    (while (< a (- e 1e-8))

      (setq k (fix a))

      (if (and (equal a (float k) 1e-8) (> a s))
        (setq k (fix a))
      )

      (setq b (1+ k))

      (if (> b e)
        (setq b e)
      )

      (if (> (- b a) 1e-8)
        (progn
          (setq base (rem k n))
          (setq frac (- b a))
          (setq bul (_lw-sub-bulge (_lw-get-bulge verts base) frac))

          (setq bulges (append bulges (list bul)))
          (setq nodes
            (append
              nodes
              (list (_lw-get-point-at-param ent b n closed))
            )
          )
        )
      )

      (setq a b)
    )

    (list nodes bulges)
  )

  (defun _split-lwpoly (ent pts ed / verts n closed endp sorted all sections i s e sec nodes bulges p p2)

    (setq verts (_lw-get-verts ed))
    (setq n (length verts))
    (setq closed (_lw-closed-p ed))

    (if (< n 2)
      (prompt "\nThe polyline does not have enough vertices to be split.")
      (progn
        (setq endp (vlax-curve-getEndParam ent))

        ;; Open polyline: exclude the start and end points
        ;; Closed polyline: convert the end parameter back to 0
        (setq sorted nil)

        (foreach p pts
          (setq p2 (car p))

          (if closed
            (progn
              (if (equal p2 endp 1e-8)
                (setq p2 0.0)
              )

              (if (not (_sameparam-p p2 sorted))
                (setq sorted
                  (cons
                    (list p2 (cadr p))
                    sorted
                  )
                )
              )
            )
            (progn
              (if
                (and
                  (> p2 1e-8)
                  (< p2 (- endp 1e-8))
                  (not (_sameparam-p p2 sorted))
                )
                (setq sorted
                  (cons
                    (list p2 (cadr p))
                    sorted
                  )
                )
              )
            )
          )
        )

        (setq sorted
          (vl-sort sorted
            '(lambda (a b)
               (< (car a) (car b))
             )
          )
        )

        (cond
          ((and closed (< (length sorted) 2))
            (prompt "\nA closed polyline requires at least two break points.")
          )

          ((and (not closed) (< (length sorted) 1))
            (prompt "\nNo valid polyline break points were found.")
          )

          (closed
            ;; Closed polyline:
            ;; Create each segment between consecutive break points, wrapping the last segment to the first point
            (setq i 0)
            (while (< i (1- (length sorted)))
              (setq s (car (nth i sorted)))
              (setq e (car (nth (1+ i) sorted)))

              (setq sec (_lw-build-section ent verts s e n closed))
              (setq nodes (car sec))
              (setq bulges (cadr sec))

              (_lw-make-poly nodes bulges ed)

              (setq i (1+ i))
            )

            ;; Final segment: connect the last break point back to the first break point
            (setq s (car (car (reverse sorted))))
            (setq e (+ (car (car sorted)) n))

            (setq sec (_lw-build-section ent verts s e n closed))
            (setq nodes (car sec))
            (setq bulges (cadr sec))

            (_lw-make-poly nodes bulges ed)

            (entdel ent)
          )

          ((not closed)
            ;; Open polyline:
            ;; Start point -> break points -> end point
            (setq all
              (append
                (list (list 0.0 nil))
                sorted
                (list (list endp nil))
              )
            )

            (setq i 0)
            (while (< i (1- (length all)))
              (setq s (car (nth i all)))
              (setq e (car (nth (1+ i) all)))

              (setq sec (_lw-build-section ent verts s e n closed))
              (setq nodes (car sec))
              (setq bulges (cadr sec))

              (_lw-make-poly nodes bulges ed)

              (setq i (1+ i))
            )

            (entdel ent)
          )
        )
      )
    )
  )

  ;; Convert IntersectWith return data into a point list
  (defun _intersection-data->points (data / raw out)
    (cond
      ((null data)
        nil
      )

      ((= (type data) 'VARIANT)
        (_intersection-data->points (vlax-variant-value data))
      )

      ((= (type data) 'SAFEARRAY)
        (_intersection-data->points (vlax-safearray->list data))
      )

      ((listp data)
        (setq raw data)
        (while (>= (length raw) 3)
          (setq out
            (cons
              (list (car raw) (cadr raw) (caddr raw))
              out
            )
          )
          (setq raw (cdddr raw))
        )
        (reverse out)
      )

      (T nil)
    )
  )

  ;; Get intersections within actual object extents without extension
  (defun _get-intersections (ent1 ent2 / obj1 obj2 result)
    (setq obj1 (vlax-ename->vla-object ent1))
    (setq obj2 (vlax-ename->vla-object ent2))

    (setq result
      (vl-catch-all-apply
        'vlax-invoke
        (list obj1 'IntersectWith obj2 0)
      )
    )

    (if (vl-catch-all-error-p result)
      nil
      (_intersection-data->points result)
    )
  )

  ;; Convert intersections into (sort-key point) data
  (defun _collect-cut-data (ent typ ed cutters / pts i cut ip cpt param endp pdata totaldist)
    (setq pts nil)
    (setq i 0)
    (setq endp (vlax-curve-getEndParam ent))

    (while (< i (sslength cutters))
      (setq cut (ssname cutters i))

      ;; Do not intersect an object with itself
      (if (and cut (/= cut ent))
        (foreach ip (_get-intersections ent cut)
          ;; Project back to the target curve to remove tiny floating-point errors
          (setq cpt
            (vl-catch-all-apply
              'vlax-curve-getClosestPointTo
              (list ent ip)
            )
          )

          (if (not (vl-catch-all-error-p cpt))
            (cond
              ((= typ "LINE")
                (setq param
                  (vl-catch-all-apply
                    'vlax-curve-getParamAtPoint
                    (list ent cpt)
                  )
                )

                (if
                  (and
                    (not (vl-catch-all-error-p param))
                    param
                    (> param 1e-8)
                    (< param (- endp 1e-8))
                    (not (_sameparam-p param pts))
                  )
                  (setq pts (cons (list param cpt) pts))
                )
              )

              ((= typ "ARC")
                (setq param
                  (_normang
                    (angle (cdr (assoc 10 ed)) cpt)
                  )
                )

                (if (not (_sameang-p param pts))
                  (setq pts (cons (list param cpt) pts))
                )
              )

              ((= typ "CIRCLE")
                (setq param
                  (_normang
                    (angle (cdr (assoc 10 ed)) cpt)
                  )
                )

                (if (not (_sameang-p param pts))
                  (setq pts (cons (list param cpt) pts))
                )
              )

              ((= typ "LWPOLYLINE")
                (setq param
                  (vl-catch-all-apply
                    'vlax-curve-getParamAtPoint
                    (list ent cpt)
                  )
                )

                (if
                  (and
                    (not (vl-catch-all-error-p param))
                    param
                    (not (_sameparam-p param pts))
                  )
                  (setq pts (cons (list param cpt) pts))
                )
              )

              ((member typ '("POLYLINE" "SPLINE" "ELLIPSE"))
                (setq pdata (_curve-data-at-point ent cpt))
                (setq totaldist
                  (vlax-curve-getDistAtParam
                    ent
                    (vlax-curve-getEndParam ent)
                  )
                )

                (if pdata
                  (progn
                    (setq param (car pdata))
                    (setq cpt   (cadr pdata))
                  )
                  (setq param nil)
                )

                (if
                  (and
                    param
                    (> param 1e-8)
                    (< param (- totaldist 1e-8))
                    (not (_sameparam-p param pts))
                  )
                  (setq pts (cons (list param cpt) pts))
                )
              )
            )
          )
        )
      )

      (setq i (1+ i))
    )

    pts
  )


  ;; Get the projected point and distance along the curve safely.
  ;; SPLINE parameter ranges are not always suitable for direct validation,
  ;; so distance from the start point is used as the sorting key.
  (defun _curve-data-at-point (ent p / cp dst)
    (setq cp
      (vl-catch-all-apply
        'vlax-curve-getClosestPointTo
        (list ent p)
      )
    )

    (if (vl-catch-all-error-p cp)
      nil
      (progn
        (setq dst
          (vl-catch-all-apply
            'vlax-curve-getDistAtPoint
            (list ent cp)
          )
        )

        (if (vl-catch-all-error-p dst)
          nil
          (list dst cp)
        )
      )
    )
  )




  ;; Split an ELLIPSE by rebuilding its parameter intervals.
  ;; This does not call BREAK, does not change OSMODE, and does not send
  ;; coordinate input through the command line.
  (defun _ellipse-clean-dxf (data / out item)
    (foreach item data
      (if (not (member (car item) '(-1 5 330 360)))
        (setq out (cons item out))
      )
    )
    (reverse out)
  )

  (defun _ellipse-set-dxf (code value data / old)
    (if (setq old (assoc code data))
      (subst (cons code value) old data)
      (append data (list (cons code value)))
    )
  )

  (defun _ellipse-unique-params (ent pts / item prm out)
    (foreach item pts
      (setq prm
        (vl-catch-all-apply
          'vlax-curve-getParamAtPoint
          (list ent (cadr item))
        )
      )
      (if
        (and
          (not (vl-catch-all-error-p prm))
          prm
          (not
            (vl-some
              '(lambda (x) (< (abs (- x prm)) 1e-9))
              out
            )
          )
        )
        (setq out (cons prm out))
      )
    )
    (vl-sort out '<)
  )

  (defun _ellipse-make-arc (base start end / data)
    (setq data (_ellipse-set-dxf 41 start base))
    (setq data (_ellipse-set-dxf 42 end data))
    (entmakex data)
  )

  (defun _split-ellipse-break
    (ent pts / ed base closed params start end bounds made failed a b firstp aent)
    (setq ed (entget ent))
    (setq base (_ellipse-clean-dxf ed))
    (setq params (_ellipse-unique-params ent pts))

    (setq closed
      (vl-catch-all-apply
        'vlax-curve-isClosed
        (list ent)
      )
    )
    (if (vl-catch-all-error-p closed)
      (setq closed nil)
    )

    (setq start (cdr (assoc 41 ed)))
    (setq end   (cdr (assoc 42 ed)))
    (if (null start) (setq start 0.0))
    (if (null end)   (setq end (* 2.0 pi)))

    (cond
      ((null params)
        (prompt "\nNo valid ellipse break points found.")
        nil
      )

      ((and closed (< (length params) 2))
        (prompt "\nA full ellipse requires at least two break points.")
        nil
      )

      (closed
        (setq made nil)
        (setq failed nil)
        (setq firstp (car params))

        ;; Build every consecutive interval.
        (while (cadr params)
          (setq a (car params))
          (setq b (cadr params))
          (if (> (- b a) 1e-10)
            (if (setq aent (_ellipse-make-arc base a b))
              (setq made (cons aent made))
              (setq failed T)
            )
          )
          (setq params (cdr params))
        )

        ;; Build the wrap-around interval.
        (setq a (car params))
        (setq b (+ firstp (* 2.0 pi)))
        (if (> (- b a) 1e-10)
          (if (setq aent (_ellipse-make-arc base a b))
            (setq made (cons aent made))
            (setq failed T)
          )
        )

        (if failed
          (progn
            (foreach aent made
              (if (and aent (entget aent))
                (entdel aent)
              )
            )
            (prompt "\nEllipse split failed. The original ellipse was kept.")
            nil
          )
          (progn
            (entdel ent)
            T
          )
        )
      )

      (T
        (setq bounds (append (list start) params (list end)))
        (setq made nil)
        (setq failed nil)

        (while (cadr bounds)
          (setq a (car bounds))
          (setq b (cadr bounds))
          (if (> (- b a) 1e-10)
            (if (setq aent (_ellipse-make-arc base a b))
              (setq made (cons aent made))
              (setq failed T)
            )
          )
          (setq bounds (cdr bounds))
        )

        (if failed
          (progn
            (foreach aent made
              (if (and aent (entget aent))
                (entdel aent)
              )
            )
            (prompt "\nEllipse split failed. The original ellipse was kept.")
            nil
          )
          (progn
            (entdel ent)
            T
          )
        )
      )
    )
  )

  ;; Use AutoCAD native BREAKATPOINT to split general curves point by point.
  ;; Process from the curve end toward the start so the original entity keeps the front segment,
  ;; allowing earlier break points to remain on the same original entity.
  ;; Supports SPLINE and legacy POLYLINE. ELLIPSE uses parameter-based rebuilding.
  (defun _split-generic-open (ent pts / sorted item p result ok)
    (setq sorted
      (vl-sort pts
        '(lambda (a b)
           (> (car a) (car b))
         )
      )
    )

    (setq ok T)

    (foreach item sorted
      (if (and ent (entget ent))
        (progn
          (setq p (cadr item))
          (setq result
            (vl-catch-all-apply
              'command-s
              (list
                "_.BREAKATPOINT"
                ent
                (trans p 0 1)
              )
            )
          )

          (if (vl-catch-all-error-p result)
            (progn
              (setq ok nil)
              (prompt "\nBREAKATPOINT failed at one point and was skipped.")
            )
          )
        )
        (setq ok nil)
      )
    )

    ok
  )

  ;; Main command
  (prompt "\nSelect cutting edges (multiple selection allowed):")
  (setq cutters (ssget))

  (if cutters
    (progn
      (prompt "\nSelect objects to break (multiple selection allowed), then press Enter:")
      (setq targets
        (ssget '((0 . "LINE,ARC,CIRCLE,LWPOLYLINE,POLYLINE,SPLINE,ELLIPSE")))
      )

      (if targets
        (progn
          (setq done 0)
          (setq nohit 0)
          (setq i 0)

          (while (< i (sslength targets))
            (setq ent (ssname targets i))

            (if (and ent (entget ent))
              (progn
                (setq ed  (entget ent))
                (setq typ (cdr (assoc 0 ed)))
                (setq pts (_collect-cut-data ent typ ed cutters))

                (if pts
                  (progn
                    (cond
                      ((= typ "LINE")
                        (_split-line ent pts ed)
                      )

                      ((= typ "ARC")
                        (_split-arc ent pts ed)
                      )

                      ((= typ "CIRCLE")
                        (_split-circle ent pts ed)
                      )

                      ((= typ "LWPOLYLINE")
                        (_split-lwpoly ent pts ed)
                      )

                      ((= typ "ELLIPSE")
                        (_split-ellipse-break ent pts)
                      )

                      ((member typ '("POLYLINE" "SPLINE"))
                        (_split-generic-open ent pts)
                      )
                    )
                    (setq done (1+ done))
                  )
                  (setq nohit (1+ nohit))
                )
              )
            )

            (setq i (1+ i))
          )

          (prompt
            (strcat
              "\nDone: processed "
              (itoa done)
              " object(s); "
              (itoa nohit)
              " object(s) had no valid intersections."
            )
          )
        )
        (prompt "\nNo target objects selected.")
      )
    )
    (prompt "\nNo cutting edges selected.")
  )

  (setvar "CMDECHO" oldcmdecho)
  (princ)
)


(prompt "\nMBP + MBI loaded. Type MBP or MBI to start.")
(princ)
