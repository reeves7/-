;;�û����Լ��޸Ľ���
(defun c:ggr ()
  (mai_make_panel
    (list
      (list "lispС����1"
      (list "       ���ඨ��������        " "CMC") ;(list  ��ť��ǩ  ���е�����)
      (list "       ���㻭Բ       " "hyq")
      (list "       �Զ�����������      " "lgg7")
      (list "       ������ϵ����          " "3tj")
      (list "        ��̬�ӳ�       " "saa")
      (list "       ���ƾ��ζԽ���        " "jxdx")
      (list "           ����䶨������       " "twm")
      (list "           �߶εȷ�          " "XBR")
      (list "           ˫�»���          " "under2")
      ) ;_ lispС����1
             (list "lispС����2"
      (list "�����������ƾ��ζԽ���" "draw_rectangle_diagonals") ;(list  ��ť��ǩ  ���е�����)
	(list "����ѡ��" "ksxz") ;(list  ��ť��ǩ  ���е�����)
	(list "      ����ľ����С�㣩   " "ldmf") ;(list  ��ť��ǩ  ���е�����)
	(list "      ���ࡢ���ľ����С�㣩   " "bdm") ;(list  ��ť��ǩ  ���е�����)
	(list "      ���ּӿ�   " "bt") ;(list  ��ť��ǩ  ���е�����)
	(list "      �ؽ����ͼ���߽�   " "TCBJ") ;(list  ��ť��ǩ  ���е�����)
       ) ;_ ����listС����2
    ) ;_ ����list
  ) ;_ ����mai_make_panel
  (princ)
) ;_ ����defun
;�����Ƕ��ඨ�����Զ����ظ���
(defun C:CMC ()
(setq A nil)
(setq OM (getvar "OSMODE"))
(setvar "OSMODE" 33)
(setq PNT1 (getpoint "\n�������:  "))
(setq PNT2 (getpoint "\n�����յ�: " PNT1))(terpri)
(setq ANG (angle PNT1 PNT2))
(setq JJ (getreal "\n ��������: "))
(setq SL (getreal "\n ��������: "))
(setvar "OSMODE" 0)
(setq A (ssget))
(setq INCR 0)
(setq TMS (FIX (+ 0.00001 SL)))
(repeat TMS
(setq INCR (+ INCR JJ))
(setq NEWPT (polar PNT1 ANG INCR))
(command "copy" A "" PNT1 NEWPT)
)
(setvar "OSMODE" OM)
(setq A nil)
)
;�����ǽ��㻭Բ
(vl-load-com)
(defun c:hyq(/ *acad *doc *error* acad acaddocument mspace odlst ptlist ss ssinters)
	(defun *error*(msg)
		(mapcar 'setvar '("cmdecho" "osmode" "peditaccept") odlst)
		(vlax-invoke-method *doc 'EndUndoMark)
		(princ msg)
	)
	(command "ucs" "w")
  (setq odlst (mapcar 'getvar '("cmdecho" "osmode" "peditaccept" "clayer")))
  (mapcar 'setvar '("cmdecho" "osmode" "peditaccept" ) '(0 0 1))
	(setq *acad  (vlax-get-acad-object) *doc   (vla-get-ActiveDocument *acad))
	(vlax-invoke-method *doc 'StartUndoMark)
	(progn 
		(defun ssinters        (ss / i num obj1 obj2 j interpts ptlist)
			(setq        i   0
				num (sslength ss)
			)
			(while (< i (1- num))
				(setq obj1 (ssname ss i)
					obj1 (vlax-ename->vla-object obj1)
					j    (1+ i)
				)
				(while (< j num)
					(setq obj2     (ssname ss j)
						obj2     (vlax-ename->vla-object obj2)
						interpts (vla-intersectwith
											 obj1
											 obj2
											 0
										 )
						interpts (vlax-variant-value interpts)
					)
					(if (> (vlax-safearray-get-u-bound interpts 1) 0)
						(progn
							(setq        interpts
								(vlax-safearray->list interpts)
							)
							(while (> (length interpts) 0)
								(setq ptlist (cons (list (car interpts)
																		 (cadr interpts)
																		 (caddr interpts)
																	 )
															 ptlist
														 )
								)
								(setq interpts (cdddr interpts))
							)
						)
					)
					(setq j (1+ j))
				)
				(setq i (1+ i))
			)
			ptlist
		)
	)
  (princ "\n��ѡ�����ߡ��������ߡ�ֱ�ߡ�Բ��Բ������Բ��")
  (if (not (setq ss (ssget '((0 . "*LINE,ARC,CIRCLE,ELLIPSE")))))
    (progn (princ "\n��ʾ��δѡȡͼԪ�������˳���\n") (exit))
  )
	(setq bj(getreal "\n������Բ�뾶<48>"))
	(if(not bj)(setq bj 48))
  (setq acad (vlax-get-acad-object))
  (setq acaddocument (vla-get-activedocument acad))
  (setq mspace (vla-get-modelspace acaddocument))
  (setq ptlist (ssinters ss))
	(foreach a ptlist
		(entmake (list (cons 0 "CIRCLE")(cons 10 a)(cons 40 bj) (cons 62 4)))
	)
  (mapcar 'setvar '("cmdecho" "osmode" "peditaccept" "clayer") odlst)
	(vlax-invoke-method *doc 'EndUndoMark)
	(princ)
)
;�������Զ�����������
(defun c:lgg7 ()
(setvar "cmdecho" 0)	
(setq en (entsel "ѡ�����ĳߴ��ע:"))	
(setq en_data (entget (car en)))	
(setq liangguige (assoc 1 en_data))
(setq liangchicun(cdr liangguige))
  (setq pos (vl-string-search "x" liangchicun));��300x800�����ﷵ�ؽ������3������ʼ����0��1��2��3����3���ҵ�x
  (setq liangkan(substr liangchicun 1 pos ));��������Ŀ�ȣ���300x800����ӵ�1λ����ʼ��ȡ�ַ�������ȡ�ĳ��ȵ���ֵ�պõ�������searchx����x����λ�õĴ�������
  (setq lianggao(substr liangchicun (+ pos 2) ));��������ĸ߶ȣ���300x800����ӵ�5λ����ʼ��ȡ�ַ�����3+2=5.
  (setq lianggao1(atof lianggao))
  (setq liangkan1(atof liangkan))
  (setq pa (getpoint "�����������Ͻǵ�: "))
  (setq pb (polar pa (* pi 1.5) lianggao1))
  (setq pc (polar pb 0 liangkan1))
  (setq pd (polar pa 0 liangkan1))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 pa)(cons 10 pb)(cons 10 pc)(cons 10 pd) (cons 62 4) (cons 70 1)))
;(command "rectangle" pa  pc )
  ;(command "pline" pa pb pc pd "c")
(prin1)
)
(prompt "*************** << C:CHGRAD >> *****************")
(prin1)

;�����ǻ�����ϵ����
(defun c:3tj (/ os pt1 pt2 pt3 pt4 ag0 ag len ent ct in k)
  (setq os (getvar "osmode")) 
  (while (and (setq pt1 (getpoint "\n�ǵ�1:"))
              (setq ag0  (getangle pt1 "�Ƕ�:"))
       (progn (princ "�Խǵ�:") (setq ct t))
       (progn
  (setq ct t k t ent nil)
         (while ct
    (setq in (grread 1))
    (cond
       (  (= 5 (car in))
                        (setq pt3 (cadr in))
                        (setq len (distance pt1 pt3) ag  (angle pt1 pt3) )
                        (setq pt2 (polar pt1 ag0 (* len (cos (- ag ag0)))))
                        (setq pt4 (polar pt1 (+ (/ pi 2.) ag0) (* len (sin (- ag ag0)))))
          (if ent (command "erase" ent ""))
          (setvar "osmode" 0)
          (command "pline" pt1 pt2 pt3 pt4 "c" )
          (setvar "osmode" os)
          (setq ent (entlast))
                     )
       (  (= 3 (car in))
          (setq pt3 (cadr in))
                        (setq len (distance pt1 pt3) ag  (angle pt1 pt3) )
                        (setq pt2 (polar pt1 ag0 (* len (cos (- ag ag0)))))
                        (setq pt4 (polar pt1 (+ (/ pi 2.) ag0) (* len (sin (- ag ag0)))))
          (if ent (command "erase" ent ""))
          (setvar "osmode" 0)
          (command "pline" pt1 pt2 pt3 pt4 "c" )
          (setvar "osmode" os)
          (setq ent (entlast))
          (setq ct nil)
       )
       (  (equal '(11 0) in)
          (if ent (command "erase" ent ""))
          (setq ct nil k nil)
       )
       (t)
    )  
         )
         k
       ) 
    ) 
       
  )
  (setvar "osmode" os)
  (princ)
)

;�����Ƕ�̬�ӳ�
(defun c:saa()
(command "Lengthen" "dy" "line" Pause))

;�������Զ����ƾ��ζԽ���
(defun c:jxdx(/ a b c)
(setq a (getpoint "\n��ѡ�����ڲ���һ��: "))
(command "boundary" a "")
(setq a (ssget "L")
a (ssname a 0)
a (entget a)
b (assoc 10 a)
)
(while b
(setq c (cons (cdr b) c)
a (cdr (member b a))
b (assoc 10 a)
)
)
(command "line" (nth 0 c) (nth 2 c) ""
"line" (nth 1 c) (nth 3 c) ""
)
)

;����������䶨������
(defun c:twm ( / en xfx pt1 pt2 zljl sl JJ)
(setvar 'cmdecho 0)
(setq en (car(entsel "\nѡ�����:")))
(setq xfx (car(apply 'mapcar(cons '- (bwh en)))))
(setq pt1 (getpoint "\nָ����һ��:"))
(setq pt2 (getpoint "\nָ���ڶ���:"))
(setq zljl (- (distance pt1 pt2) (abs xfx) ) )
(setq JJ (getreal "\n ��������:<300> "))
(if (not JJ)(setq JJ 300) )
(setq sl (fix(/ zljl JJ)  ))
(command "-array" en "" "r" 1 sl (/ zljl (float sl))  )
(setvar 'cmdecho 1)
(princ)
)


(defun bwh (en / p1 p2)
  (vla-getboundingbox (vlax-ename->vla-object en) 'p1 'p2)
  (setq p1 (vlax-safearray->list p1)
        p2 (vlax-safearray->list p2)
  )
  (list p1 p2)
)


;�������߶εȷ��ӳ���
(defun C:XBR (/ OS OE SS VSS N I D LEN E OBJ E)   
	 (princ "\n �ȷִ���߶�")   
	 (defun *ERROR* (MSG)        (setvar "osmode" OS)        (vl-cmdf ".undo" "e")        (setq *ERROR* OE)    )   
	 (vl-cmdf ".undo" "be")    
	 (princ "\n ѡ��Ҫ�ȷֵ����:")   
	  (setq  OE *ERROR*   OS (getvar "osmode")   SS (ssget '((0 . "LINE,*POLYLINE,SPLINE,ARC")))    )   
	   (setvar "osmode" 0)   
	    (or (setq N (getint "\n �����ȷ�<�趨��ȷ�/ֱ�ӻس�>:"))            (setq D (getdist "\n ָ���߶γ���:"))    ) 
	       (setq I 0)   
	        (if SS        (repeat (sslength SS)            
		        (setq E (ssname SS I))            
		        (setq   LEN (vlax-curve-getdistatparam E (vlax-curve-getendparam E))            )            
		        (if N   (setq D  (/ LEN N)              M  N   )   (setq M (1+ (fix (/ LEN D))))            ) 
		                   (repeat (1- M)   
			                   (if (setq PT (vlax-curve-getpointatdist E D))      (vl-cmdf ".break" (list E PT) PT)   )  
			                    (setq E (entlast))            )           
		                    (setq I (1+ I))        )    )    
	        (setvar "osmode" OS)  
	          (vl-cmdf ".undo" "e")  
	           (princ) )

;������˫�»����ӳ���
(defun c:under2 ( )
    (LM:strikethrough:single 
       '(
            (-0.8  0.05)
            (-1.0  0.05)
        )
    )
)
;; Strikthrough Text: Single Selection  -  Lee Mac
;; Prompts the user to select a single text, mtext or attribute object and evaluates the
;; strikethrough function with the supplied parameter list.
;; l - [lst] List of ((<Spacing Factor> <Width Factor>) ... )

(defun LM:strikethrough:single ( l / *error* e )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (while
        (progn
            (setvar 'errno 0)
            (setq e (nentselp "\nSelect text, mtext or attribute: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null e) nil)
                (   (and (= 2 (length e))
                         (wcmatch (cdr (assoc 0 (entget (car e)))) "TEXT,MTEXT,ATTRIB")
                    )
                    (LM:strikethrough (car e) l)
                )
                (   (princ "\nInvalid object selected."))
            )
        )
    )
    (princ)
)

;; Strikthrough Text: Multiple Selection  -  Lee Mac
;; Prompts the user for a selection of text, mtext or attributed blocks and evaluates the
;; strikethrough function with the supplied parameter list.
;; l - [lst] List of ((<Spacing Factor> <Width Factor>) ... )

(defun LM:strikethrough:selection ( l / *error* a e i s x )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if (setq s (ssget '((-4 . "<OR") (0 . "TEXT,MTEXT") (-4 . "<AND") (0 . "INSERT") (66 . 1) (-4 . "AND>") (-4 . "OR>"))))
        (repeat (setq i (sslength s))
            (setq i (1- i)
                  e (ssname s i)
            )
            (if (= "INSERT" (cdr (assoc 0 (entget e))))
                (progn
                    (setq a (entnext e)
                          x (entget  a)
                    )
                    (while (= "ATTRIB" (cdr (assoc 0 x)))
                        (LM:strikethrough a l)
                        (setq a (entnext a)
                              x (entget  a)
                        )
                    )
                )
                (LM:strikethrough e l)
            )
        )
    )
    (princ)
)

;; Strikethrough Text  -  Lee Mac
;; Generates polylines through the supplied text object, with spacing & width given by the supplied parameter list.
;; ent - [ent] Text or MText entity
;; par - [lst] List of ((<Spacing Factor> <Width Factor>) ... ) for each polyline
;; Returns: [lst] List of created polyline entities

(defun LM:strikethrough ( ent par / ang enx hgt lst md1 md2 rtn )
    (if (setq lst (LM:textbox (setq enx (entget ent))))
        (progn
            (setq hgt (cdr (assoc 40 enx))
                  md1 (mid   (car  lst) (last  lst))
                  md2 (mid   (cadr lst) (caddr lst))
                  ang (angle (car  lst) (last  lst))
            )
            (foreach itm par
                (setq rtn
                    (cons
                        (entmakex
                            (append
                               '(   (000 . "LWPOLYLINE")
                                    (100 . "AcDbEntity")
                                    (100 . "AcDbPolyline")
                                    (090 . 2)
                                    (070 . 0)
                                )
                                (LM:defaultprops enx)
                                (list
                                    (cons  043 (* (cadr itm) hgt))
                                    (cons  038 (caddar lst))
                                    (cons  010 (polar md1 ang (* (car itm) hgt)))
                                    (cons  010 (polar md2 ang (* (car itm) hgt)))
                                    (assoc 210 enx)
                                )
                            )
                        )
                        rtn
                    )
                )
            )
        )
    )
    rtn
)

;; Midpoint  -  Lee Mac
;; Returns the midpoint of two points

(defun mid ( a b )
    (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) a b)
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups
 
(defun LM:defaultprops ( enx )
    (mapcar '(lambda ( x ) (cond ((assoc (car x) enx)) ( x )))
       '(
            (006 . "BYLAYER")
            (008 . "0")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Text Box  -  Lee Mac (based on code by gile)
;; Returns the bounding box of a text, mtext, or attribute entity (in OCS)
;; enx - [lst] Text, MText or Attribute DXF data list

(defun LM:textbox ( enx / bpt hgt jus lst ocs org rot wid )
    (cond
        (   (and (= "ATTRIB" (cdr (assoc 000 enx)))
                 (= "Embedded Object" (cdr (assoc 101 enx)))
            )
            (LM:textbox (cons '(000 . "MTEXT") (member '(101 . "Embedded Object") enx)))
        )
        (   (cond
                (   (wcmatch  (cdr (assoc 000 enx)) "ATTRIB,TEXT")
                    (setq bpt (cdr (assoc 010 enx))
                          rot (cdr (assoc 050 enx))
                          lst (textbox enx)
                          lst (list (car lst) (list (caadr lst) (cadar lst)) (cadr lst) (list (caar lst) (cadadr lst)))
                    )
                )
                (   (= "MTEXT" (cdr (assoc 000 enx)))
                    (setq ocs  (cdr (assoc 210 enx))
                          bpt  (trans (cdr (assoc 010 enx)) 0 ocs)
                          rot  (angle '(0.0 0.0) (trans (cdr (assoc 011 enx)) 0 ocs))
                          wid  (cdr (assoc 042 enx))
                          hgt  (cdr (assoc 043 enx))
                          jus  (cdr (assoc 071 enx))
                          org  (list (cond ((member jus '(2 5 8)) (/ wid -2.0)) ((member jus '(3 6 9)) (- wid))      (0.0))
                                     (cond ((member jus '(1 2 3)) (- hgt))      ((member jus '(4 5 6)) (/ hgt -2.0)) (0.0))
                               )
                          lst  (list org (mapcar '+ org (list wid 0)) (mapcar '+ org (list wid hgt)) (mapcar '+ org (list 0 hgt)))
                    )
                )
            )
            (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) bpt)) lst))
                (list
                    (list (cos rot) (sin (- rot)) 0.0)
                    (list (sin rot) (cos rot)     0.0)
                   '(0.0 0.0 1.0)
                )
            )
        )
    )
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

(princ)



  
;�����ǿ����������ƾ��ζԽ���

;*********************************************************************************************************************************************************************

;	komondormrex, feb 2023

;*********************************************************************************************************************************************************************

(defun c:draw_rectangle_diagonals (/ ename_index ename_sset object)
	(vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
	(repeat (sslength (setq ename_index -1
							ignore_empty_sset (while (null (setq ename_sset (vl-catch-all-apply 'ssget (list '((0 . "lwpolyline") (-4 . "&=") (70 . 1) (90 . 4))))
														   )
													  )
											   )
							ename_sset (cond
												(
													(vl-catch-all-error-p ename_sset)
														(princ "\nCommand cancelled")
														(ssadd)
												)
												(
													t
														ename_sset
												)
									   )
					)
			)
				(setq object (vlax-ename->vla-object (ssname ename_sset (setq ename_index (1+ ename_index)))))
				(if
					(and
						 (= "AcDbPolyline" (vla-get-objectname object))
						 (equal (distance (vlax-curve-getpointatparam object 0) (vlax-curve-getpointatparam object 2))
								(distance (vlax-curve-getpointatparam object 1) (vlax-curve-getpointatparam object 3))
								1e-8
						 )
						 (equal (angle (vlax-curve-getpointatparam object 0) (vlax-curve-getpointatparam object 1))
								(angle (vlax-curve-getpointatparam object 3) (vlax-curve-getpointatparam object 2))
								1e-8
						 )
						 (equal (angle (vlax-curve-getpointatparam object 1) (vlax-curve-getpointatparam object 2))
								(angle (vlax-curve-getpointatparam object 0) (vlax-curve-getpointatparam object 3))
								1e-8
						 )
						 (zerop (apply '+ (mapcar 'abs (mapcar 'cdr (vl-remove-if-not '(lambda (group) (= 42 (car group))) (entget (ssname ename_sset ename_index)))))))
					)
						(progn
							(vla-addline (vla-get-block (vla-get-activelayout (vla-get-activedocument (vlax-get-acad-object))))
										 (vlax-3d-point (vlax-curve-getpointatparam object 0))
										 (vlax-3d-point (vlax-curve-getpointatparam object 2))
							)
							(vla-addline (vla-get-block (vla-get-activelayout (vla-get-activedocument (vlax-get-acad-object))))
										 (vlax-3d-point (vlax-curve-getpointatparam object 1))
										 (vlax-3d-point (vlax-curve-getpointatparam object 3))
							)
						)
				)
				(princ)
	)
	(vla-endaundomark (vla-get-activedocument (vlax-get-acad-object)))
	(princ)
)

;*********************************************************************************************************************************************************************

;*********************************************************************************************************************************************************************





;�����ǿ���ѡ��
;;---------------------------------------
;�ɼ������ֱ꣬�߶μ䲻ȡ�������߶ΰ��������ƶ�0.9999����ȡ��
;;;;WZG356 by 20181129
;;����"line,lwpolyline,polyline,spline,circle,arc,ellipse"
;�ٶ��п�, ����ʱ������2��
;��lwpolyline,polyline,circle,arc,ellipse�����ýǶȷ�ȡ���ٶȸ���
;;ʾ��(outcurvept (car(entsel "\nѡ�����: ")))
(defun outcurvept (en / ob n l ls1 ls2 lo po a b p1 p2 p11 p22)
	(setq ob(vlax-ename->vla-object en))
	(setq ls1(list(cons 0.0(vlax-curve-getstartpoint ob))))
	(if	(wcmatch (vla-get-objectname ob) "*Polyline")
		(setq n 0
			  x(while(setq po(vlax-curve-getpointatparam ob(setq n(1+ n))))
				     (setq l  (vlax-curve-getDistAtParam ob n))
				     (setq ls2(append ls2(list(cons l po))));����+����
				) ;x��ƥ��setq��ʽ��
		)
		(setq l  (vlax-curve-getDistAtParam ob(vlax-curve-getendparam ob))
			  ls2(list(cons l(vlax-curve-getendpoint ob)))
		);line,spline,circle,arc,ellipse
	)	
	(while
		(setq p11(last ls1)  p22(car ls2))
		(setq a  (car p11)  b  (- (car p22) a))
		(setq p1 (cdr p11)  p2 (cdr p22))
		(if	(or (equal b(distance p1 p2) 1e-6);ֱ�߶β���
				(and(setq lo (+(* b 0.5)a))
					(setq po(vlax-curve-getPointAtDist ob lo))
					(< (* b 0.499995)(distance p1 po))
				);
			)
			(setq ls2(cdr ls2) ls1(append ls1 (list p22)))
			(setq ls2(cons(cons lo po)ls2))
		)
	);ѭ������Ӧ
	(mapcar 'cdr ls1)
)
;ssget��ѡ/Ȧ��/ȦΧ
;sel����"F" "CP" "WP" ��ѡ/Ȧ��/ȦΧ
;(ps2ssget "wp" nil '((0 . "text")))
(defun ps2ssget (sel ps filters
	 / x2cpwp myerr olderr selm sel1 FUNSR str1 strs str2 en p n)
	(defun myerr(m)
		(cond(m(redraw)(setq *error* olderr)))
	)
	(defun x2cpwp(sel)
		(setq ssgetlb3n(vl-position sel '("F" "C" "W")))
		;�ڿ���ѡ�񹤾����õĹ�������ssgetlb3n
		(if(= sel "F")sel(strcat sel "p"))
	)	
	(setq olderr *error* *error* myerr)
	(setq sel(strcase(substr sel 1 1)))
	(if ps(setq selm sel
			FUNSR getkword
			str1(apply 'strcat '("F " "C " "W "))
			strs2 (list "\nѡ��ʽ" "/F��ѡ" "/CpȦ��" "/WpȦΧ" (strcat  "<" sel ">:"))
		)
		(setq  selm "E"
			FUNSR getpoint
			str1(apply 'strcat (vl-remove (strcat sel " ")'("F " "C " "W "  "E ")))
			n (vl-position sel '("F" "C" "W"))
			sel1(nth n '("��ѡ" "Ȧ��" "ȦΧ"))
			strs (list (strcat "\nָ��" sel1 "��һ���") 
			          "/F��ѡ" "/CpȦ��" "/WpȦΧ" 
			          (strcat "/Eѡ��" sel1) 
			          "<E>:"
			    )
			str2(apply 'strcat(vl-remove (nth (1+ n) strs)strs))
		)		 
	)	
	(initget str1)(setq p(cond((FUNSR str2))(selm)))
	(cond
		((listp p)
			(setq ps(list p))
			(while
				(setq p (getpoint p "\n��һ��"))
				(redraw)
				(setq ps(cons p ps))
				(repeat (1-(setq n(length ps)))
					(grdraw (nth(setq n(1- n))ps) (nth(1- n)ps) 1 1)					
				)
				(if(/= "F" sel)(grdraw (car ps) (last ps) 1 1))
			)
			(redraw)
			(if(>(length ps)1)(ssget (x2cpwp sel) (reverse ps) filters))
		)		
		((= p "E")
			(princ "\n��\"*olyline,line,spline,circle,arc,ellipse\"ȡ1����")
			(princ sel1)
			(if	(setq en(ssget ":E:S" '((0 . "*olyline,line,spline,circle,arc,ellipse"))))
				(if	(setq ss(ssget (x2cpwp sel) (outcurvept (setq en(ssname en 0))) filters))
					(progn(ssdel en ss)(if(> (sslength ss)0)ss nil))
					;ѡ�񼯲�����������ѡ����
				)
				(ps2ssget (x2cpwp sel) ps filters)
			)
		)
		((member p '("F" "C" "W"))(ps2ssget (x2cpwp p) ps filters))
		((> (length ps)1)(ssget (x2cpwp p) ps filters))
		(t nil)
	)	
)
;=========================
;�ռ���������̳
;SSGET��һЩ�ַ�����ͳ���������. # , *�ȵȣ�
;���Ҫ���˵��ַ����а�����Щ������˲������������������ת���¾�OK �ˡ�
;(transsfilter "*s#s")
(defun transsfilter (str / tf)
  (vl-list->string (apply 'append (mapcar
    '(lambda (c)
        (if (or (> c 128) tf)
            (progn(if (not tf)(setq tf t)(setq tf nil))(list c))
            (progn
                (setq tf nil)
                (if (member c '(35 64 46 42 63 126 91 93 45 44))(list 96 c)(list c))
            )
        )
    )
    (vl-string->list str)
    )))
)
;���ر���ĳԪ����������
;(myvl-position "1"(list "1" "0" "1" "0" "1" "0"))
(defun myvl-position (a ls / i is)
	(repeat (setq i(length ls))		
		(if(equal a(nth (setq i(1- i))ls)1e-8)(setq is(cons i is)))				
	)is
)
;��д�Ի������ѡ�����
(defun SSSfilter(lst / lb3n lb3 file f i name id dd key keys key0 keys0 kvs)	
	(if (numberp ssgetlb3n)
		;��������ssgetlb3n���Ժ���ps2ssgetִ��ʱ�ṩ
		(setq lb3n (fix ssgetlb3n))
		(setq lb3n 0)
	)
	(setq lb3(nth lb3n '("��ѡ" "Ȧ��" "ȦΧ")))
	;lb3n= 0 1 2�ֱ�ѡ��"��ѡ" "Ȧ��" "ȦΧ"��ť
	(setq file (vl-filename-mktemp "temp.dcl") f (open file "w"))
	(write-line "MY_SSS: dialog{label=\"����ѡ��\";" f)
	(write-line " :boxed_column{ label=\"����ѡ��   ֵ\";" f)
	(setq I -1)
	(foreach name lst
		(setq keys(cons(setq key(strcat "KEY"  (itoa (setq I (1+ I)))))keys))
		(write-line (strcat ":toggle{label=\"" name "\";key=\"" key "\";value=0;}")f)			
    )
    (write-line "}" f)
    (write-line ":row{label=\"ѡ����˷�ʽ\";" f)    
    (write-line "      : button{label = \"��ѡ\";key = \"bton1\";}" f)
    (write-line "      : button{label = \"ȫͼ\";key = \"bton2\";}" f)
    (if lb3(write-line (strcat "      : button{label = "(vl-prin1-to-string lb3) ";key = \"bton3\";}") f))
    (write-line "}" f)
    (write-line ":row{: button{label = \"��ѡ����\";key = \"bton4\";}" f)
    (write-line "             cancel_button;}" f)
    (write-line ":text {key = \"zzhe\";label = \"by ı�����                        \" ;}}" f)
    (close f)
    (setq keys(reverse keys) key0(car keys)keys0(cddddr(cddr keys)))
    ;ǰ������Ϊ�����͡�ͼ�㡢��ɫ�����͡����ͱ������߿��������ж�������
    ;��6��֮���ֻ�й�ѡ���Ͳ���Ч��Ϊkeys0
    (setq id (load_dialog file))        
    (new_dialog "MY_SSS" id)        
    (set_tile (car keys) "1")(set_tile (cadr keys) "1")(set_tile (caddr keys) "1")
    (mode_tile "button1" 2)(mode_tile "zzhe" 1)
    (foreach key keys
	    (if(= key key0) 
		    (action_tile key
			    (strcat "(if(= $value \"1\")"
				        "(if keys0(mapcar '(lambda(x)(mode_tile x 0)) keys0))"
				        "(if keys0(mapcar '(lambda(x)(set_tile x \"0\")(mode_tile x 1)) keys0)))"
				        "(mode_tile \"button1\" 2)"
			    );ֻ�й�ѡ����keys0����Ч
			)
			(action_tile key "(mode_tile \"button1\" 2)")
		);button1��Ϊ���㣬�س�����
	)
    (action_tile "bton1" "(setq kvs(mapcar 'get_tile keys))(done_dialog 1)")
    (action_tile "bton2" "(setq kvs(mapcar 'get_tile keys))(done_dialog 2)")
    (action_tile "bton3" "(setq kvs(mapcar 'get_tile keys))(done_dialog 3)")
    (action_tile "bton4" "(done_dialog 4)")
    (setq dd(start_dialog))
    (unload_dialog Id)(vl-file-delete file)
    (cond
	    ((= dd 1)(cons "" (myvl-position "1" kvs)))
	    ((= dd 2)(cons "x" (myvl-position "1" kvs)))
	    ((= dd 3)(cons (nth lb3n '("f" "cp" "wp")) (myvl-position "1" kvs)))
	    ;���ع�ѡ�İ�ť����
	    ((= dd 4)(c:myQSELECT))
	    (t nil)
	)
);end -defun
;����ģ��������ù��˱�(outSSfilterls(car(entsel)))
(defun outSSfilterls (e / es e0 l1 l2 l3 str as x y a b v )
    (setq es (entget e) e0 (cdr (assoc 0 es)))
    (if (not (assoc 62 es))(setq es (append es(list '(62 . 256)))))
    (if (not (assoc 6 es))(setq es (append es(list '(6 . "ByLayer")))))        
    (setq l1 '((6 . "���� =")(62 . "��ɫ =")(8 . "ͼ�� =")(0 . "���� =")))    	
    (setq e(vlax-ename->vla-object e))
    (setq as (list
	    (cons 'LinetypeScale "���ͱ��� =")	    
	    (cons 'Lineweight "�߿� =")
	    ;�Ի���ǰ������Ϊ�����͡�ͼ�㡢��ɫ�����͡����ͱ������߿��������ж�������
        ;�ڶԻ����6��֮���ֻ�й�ѡ���Ͳ���Ч��Ϊkeys0	    
	    (cons 'StyleName "��ʽ���� =")	    
	    (cons 'Height (if (wcmatch e0 "*TEXT,ATTDEF") "���ָ߶� =" "�߶� ="))
	    (cons 'HasAttributes "���Կ�? =")
	    (cons 'isdynamicblock "��̬��? =")
	    (cons 'EffectiveName "���� =");�������ַ�ʽ����
	    (cons 'Rotation "��ת�Ƕ� =")
	    (cons 'AttachmentPoint "���� =");��������9��
	    ;(cons 'Alignment "���� =");û�㶮���   
	    (cons 'Closed "�պ�? =")
	    (cons 'ConstantWidth "ȫ�ֿ�� =")	    
	    (cons 'Radius "�뾶 =")
	    (cons 'Arrowhead1Type "��һ��ͷ =")
	    (cons 'Arrowhead2Type "�ڶ���ͷ =")
	    (cons 'TextStyle "������ʽ =")	    
	    (cons 'Measurement "����ֵ =")
	    (cons 'TextOverride "������� =")
	    (cons 'LinearScaleFactor "�������� =")	    	    
	    (cons 'ScaleFactor (if (wcmatch e0 "TEXT") "������� =" "ȫ�ֱ��� ="))
	    (cons 'area "��� =")
	    (cons 'length "���� =")
	    (cons 'TextString "�������� =")	    
	))
    (foreach x as
	    (setq v(vl-catch-all-apply '(lambda(a b)(Vlax-Get a b))(list e (car x))))
	    (if (not(vl-catch-all-error-p v))
		    (setq l1(cons x l1) es(append es(list (cons (car x) v))))
		)	    
    );���ԺϷ���(���� . ֵ)����es,(����. ����)����l1 
    (cond
	    ((= e0 "INSERT")(setq l1 (cons '(43 . "Z���� =")(cons '(42 . "Y���� =")(cons '(41 . "X���� =")l1)))))	    
	    ((= e0 "HATCH")(setq l1(cons '(52 . "���Ƕ� =")(cons '(41 . "������ =")(cons '(2 . "ͼ���� =")l1)))))	    
	    ((= e0 "ATTDEF")(setq l1 (cons '(2 . "���Ա�� =")l1)))
	    ((= e0 "DIMENSION")(setq l1(cons '(70 . "��ע���� =")l1)))
	    (t nil)
	);����ͼԪ�ಹ��l1
    (setq l2 nil l3 nil)
    (foreach x l1
	    ;����L1�޳���Ҫ��Ⱥ��,����
	    ;l2�ǶԻ���չʾ��ֵ��
	    ;l3ԭʼֵ����L2��Ӧ
	    (and(setq y (assoc (car x) es))
		    (setq l3(cons y l3))
		    (setq str(strcat  (cdr x)
			    (if  (member (car x) '(Closed HasAttributes isdynamicblock))
				    (if  (= (cdr y) -1)"��" "��")
				    (vl-princ-to-string(cdr y))
				)		    
		    ))		    
		    (cond
			    ((and(= e0 "MTEXT")(= (car x) 'TextString))(setq l2(cons "�������� =......" l2)))
			    ((and(= e0 "TEXT")(= (car x) 'TextString))(setq l2(cons (substr str 1 30) l2)))
			    ((and(= e0 "DIMENSION")(= (car x) 70))
			        (setq l2(cons(strcat (cdr x)
				        (nth(apply 'max(mapcar '(lambda(i)(logand i (cdr y)))(list 0 1 2 3 4 5 6)))
			    	    (list "ת��" "����" "�Ƕ�" "ֱ��" "�뾶" "����Ƕ�" "����")))l2)
			    	)
			    )
			    (t(setq l2(cons str l2)))
			)
		)
    )      
    (if  (and l2 (setq l2(SSSfilter l2)))
	    ;�Ի��򷵻ص�l2��һ����ssget����ģʽ
	    (progn		    
		    (setq l3(mapcar '(lambda(x)(nth x l3)) (cdr l2)))
		    (if  (setq v(assoc 'EffectiveName l3))
			    ;Ϊ���Ч����Կ�������ʵ�����			    
			    (setq l3(vl-remove(assoc 'isdynamicblock l3)l3)
				    l3(vl-remove(assoc 'HasAttributes l3)l3)
				    l3(subst(cons 2 (cdr v)) v l3)
				    ;���ѡ���������Ƿ����Կ�/��̬���Ƕ����
				    ;(EffectiveName . ����)תΪ(2 . ����)
				    ;����������˱����ssgetЧ��
			    )
			)
		    (cons (car l2) l3)
		)
    )
)

;ѡ�񼯰�����ֵ���� ;ss:ѡ�� prop:���� v:����ֵ fuz���ݲ�	
(defun propss(ss prop v fuz / n ss0 e ob v1)
	(setq ss0(ssadd) n -1)
	(repeat (sslength ss)
		(setq e(ssname ss (setq n(1+ n))) ob(vlax-ename->vla-object e))
		(setq v1(vl-catch-all-apply '(lambda(a b)(Vlax-Get a b))(list ob prop)))
		(if (and(not(vl-catch-all-error-p v1))(equal v1 v fuz))(ssadd e ss0))
	)ss0
)
(defun MYQSELECT(e / fls x l as a ss oldGrips)	
	(if	(setq fls(outSSfilterls e))
		(progn
			(setq x(car fls) fls(cdr fls))
			(setq as(vl-remove-if '(lambda(a)(numberp (car a)))fls))
			;�����((���� . ֵ) ....)��
			(setq fls(vl-remove-if '(lambda(a)(not(numberp (car a))))fls))
			(setq fls(mapcar '(lambda(a)
				(if(= (setq str(cdr a)) 'STR)(cons (car a)(transsfilter str))a))fls)
			)
			;�����((���� . ֵ) ....)��ssget�ô������ַ�����ͨ���ת��
			(cond
				((= x "")(setq ss(ssget fls)))
				((= x "x")(setq ss(ssget "x" fls)))
				(t(setq ss(ps2ssget x nil fls)))
			)
			(if ss(mapcar '(lambda(a)(if a(setq ss(propss ss (car a) (cdr a) 1e-4))))as))
			(if	(and ss (> (sslength ss)0))
				(progn(sssetfirst nil ss)(princ (strcat "\n ��ѡ��" (itoa(sslength ss)) "��������������")))
				(progn(setq ss nil)(princ "\nδѡ�ж���"))
			)
		)
	)ss
)
(vl-load-com)
;����ѡ��
(defun c:ksxz nil(c:myQSELECT))
(defun c:myQSELECT( / e)
	(princ "\n ����ѡ��,ı�����")
	(if(setq e(car(entsel "\nѡ����������")))(MYQSELECT e))
	(princ)	
)



;����������ľ����С�㣩
(defun c:ldmf()
(setvar "cmdecho" 0)	
  (setq pz (getpoint "�����������е�: "))
  (setq pa (polar pz (* pi 1) 1000))
  (setq pb (polar pa (* pi 1.5) 100))
  (setq pc (polar pb 0 2000))
  (setq pd (polar pz 0 1000))
  (command "rectangle" pa pc) ;rectangle ������ž��εĶԽ������
 ; (command "pline" pa pb pc pd "c")
  ;��ִ�����ܶ�,��Ϊ�ĸ��߶λ��ƣ����Գ���ʱ��ٴ�
(prin1)
)
(prompt "̷�� ����ľ��С���Զ�����")
(prin1)


;���������ࡢ���ľ����С�㣩
(defun c:bdm()
(setvar "cmdecho" 0)	
;�����ǰ����ľ����С�㣩
  (setq za (getpoint "�����������󽻵�: "))
  ;(setq pa (getpoint "�����������ҽ���: "))
  (setq lj (getpoint "�����������·��ǵ㣺"))
  ;(setq ljz (getpoint "�����������·��ǵ㣺"))
  (setq pa (list (car lj) (cadr za)) ljz (list (car za) (cadr lj)))
  (setq pb (polar pa (* pi 0) 50))
  (setq pc (polar pb (* pi 1.5) 100))
  (setq pd (polar pc 0 -50))
  (setq ss (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 pa)(cons 10 pb)(cons 10 pc)(cons 10 pd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) ss)
  
  ;���涨������ľ�������߶εĶ˵�
  (setq laa(polar pa (* pi 0) 30))
   (setq lab(polar laa (* pi 1.5) 20))
   (setq lba(polar pa (* pi 0) 10))
   (setq lbb(polar lba (* pi 1.5) 80))
    (setq lca(polar pa (* pi 0) 35))
   (setq lcb(polar lca (* pi 1.5) 42))
   (setq lda(polar pa (* pi 0) 26))
   (setq ldb(polar lda (* pi 1.5) 70))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 lab)(cons 11 lbb) (cons 62 4)))
   (ssadd (entlast) ss)
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 lcb)(cons 11 ldb) (cons 62 4)))
   (ssadd (entlast) ss)
   	
   ;;�������ɿ�
(command "_.-group"  "c" "*"  "" ss "")

;�����ǰ����ľ����С�㣩
 (setq zb (polar za (* pi 1.5) 100))
  (setq zc (polar zb (* pi 1) 50))
  (setq zd (polar zc (* pi 0.5) 100))
   (setq zz (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 za)(cons 10 zb)(cons 10 zc)(cons 10 zd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) zz)

  ;���涨������ľ�������߶εĶ˵�
(setq za1(polar za (* pi 1) 50))
  (setq laaz(polar za1 (* pi 0) 30))
   (setq labz(polar laaz (* pi 1.5) 20))
   (setq lbaz(polar za1 (* pi 0) 10))
   (setq lbbz(polar lbaz (* pi 1.5) 80))
    (setq lcaz(polar za1 (* pi 0) 35))
   (setq lcbz(polar lcaz (* pi 1.5) 42))
   (setq ldaz(polar za1 (* pi 0) 26))
   (setq ldbz(polar ldaz (* pi 1.5) 70))
  
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 labz)(cons 11 lbbz) (cons 62 4)))
   (ssadd (entlast) zz)
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 lcbz)(cons 11 ldbz) (cons 62 4)))
   (ssadd (entlast) zz)
   (command "_.-group"  "c" "*"  "" zz "")
  
   ;���涨���׸ֹܶ˵�
   (setq gg1(polar pa (* pi 1.5) 100))
   (setq gg2 (polar gg1 (* pi 0) 1000))
   (setq gg3 (polar gg1 (* pi 1.5) 48))
   (setq gg4 (polar gg3 (* pi 0) 1000))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 gg1)(cons 11 gg2) (cons 62 4)))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 gg3)(cons 11 gg4) (cons 62 4)))
   ;���涨�����Ҳ�ľ���ǵ�
      (setq cla(polar pa (* pi 1.5) 148))
   (setq clb (polar cla (* pi 0) 100))
  (setq clc (polar lj (* pi 0) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 cla)(cons 10 clb)(cons 10 clc)(cons 10 lj) (cons 62 4) (cons 70 1)))
;���涨�������ľ���ǵ�
 (setq claz(polar za (* pi 1.5) 148))
     (setq clcz (polar ljz (* pi 1) 100))
     (setq cldz (polar claz (* pi 1) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 claz)(cons 10 ljz)(cons 10 clcz)(cons 10 cldz) (cons 62 4) (cons 70 1)))

;���¶������׵�һ��ľ���ǵ�
   (setq la (polar lj (* pi 1) 1000))
  (setq lb (polar la (* pi 1.5) 100))
  (setq lc (polar lb 0 2000))
  (setq ld (polar lj 0 1000))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 la)(cons 10 lb)(cons 10 lc)(cons 10 ld) (cons 62 4) (cons 70 1)))
  ;(command "rectangle" la lc) ;rectangle ������ž��εĶԽ������

      ;(command "rectangle" pa pc) ;rectangle ������ž��εĶԽ������
 ; (command "pline" pa pb pc pd "c")
  ;��ִ�����ܶ�,��Ϊ�ĸ��߶λ��ƣ����Գ���ʱ��ٴ�
  ;(command "line" lab lbb "")
  ;(command "line" lcb ldb "")
  
 ;(command "line" gg1 gg2 "")
  ; (command "line" gg3 gg4 "")
   ;(command "rectangle" clb lj) 
(prin1)
)
(prompt "̷�� �����ľ��С�� ����Զ�����")
(prin1)


;���������ּӿ����

(defun c:bt ( / *error* def enx idx lst off sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (or (not (setq def (getenv "LMac\\boxtext-off")))
            (not (setq def (distof def 2)))
        )
        (setenv "LMac\\boxtext-off" (rtos (setq def 0.35) 2 2))
    )
    (initget 4)
    (if (setq off (getreal (strcat "\nSpecify offset factor <" (rtos def 2 2) ">: ")))
        (setenv "LMac\\boxtext-off" (rtos off 2 2))
        (setq off def)
    )
    
    (LM:startundo (LM:acdoc))
    (if (setq sel (LM:ssget "\nSelect text or mtext <exit>: " '(((0 . "TEXT,MTEXT")))))
        (repeat (setq idx (sslength sel))
            (setq enx (entget (ssname sel (setq idx (1- idx))))
                  lst (text-box-off enx (* off (cdr (assoc 40 enx))))
            )
            (entmake
                (append
                   '(
                        (000 . "LWPOLYLINE")
                        (100 . "AcDbEntity")
                        (100 . "AcDbPolyline")
                        (090 . 4)
                        (070 . 1)
                    )
                    (LM:defaultprops enx)
                    (list (cons  038 (caddar lst)))
                    (mapcar '(lambda ( x ) (cons 10 x)) lst)
                    (list (assoc 210 enx))
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups

(defun LM:defaultprops ( enx )
    (mapcar '(lambda ( x ) (cond ((assoc (car x) enx)) ( x )))
       '(
            (006 . "BYLAYER")
            (008 . "0")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Text Box  -  gile / Lee Mac
;; Returns an OCS point list describing a rectangular frame surrounding
;; the supplied text or mtext entity with optional offset
;; enx - [lst] Text or MText DXF data list
;; off - [rea] offset (may be zero)

(defun text-box-off ( enx off / bpt hgt jus lst ocs org rot wid )
    (cond
        (   (= "TEXT" (cdr (assoc 00 enx)))
            (setq bpt (cdr (assoc 10 enx))
                  rot (cdr (assoc 50 enx))
                  lst (textbox enx)
                  lst
                (list
                    (list (- (caar  lst) off) (- (cadar  lst) off)) (list (+ (caadr lst) off) (- (cadar  lst) off))
                    (list (+ (caadr lst) off) (+ (cadadr lst) off)) (list (- (caar  lst) off) (+ (cadadr lst) off))
                )
            )
        )
        (   (= "MTEXT" (cdr (assoc 00 enx)))
            (setq ocs  (cdr (assoc 210 enx))
                  bpt  (trans (cdr (assoc 10 enx)) 0 ocs)
                  rot  (angle '(0.0 0.0) (trans (cdr (assoc 11 enx)) 0 ocs))
                  wid  (cdr (assoc 42 enx))
                  hgt  (cdr (assoc 43 enx))
                  jus  (cdr (assoc 71 enx))
                  org  (list (cond ((member jus '(2 5 8)) (/ wid -2.0)) ((member jus '(3 6 9)) (- wid))      (0.0))
                             (cond ((member jus '(1 2 3)) (- hgt))      ((member jus '(4 5 6)) (/ hgt -2.0)) (0.0))
                       )
                  lst
                (list
                    (list (- (car org) off)     (- (cadr org) off))     (list (+ (car org) wid off) (- (cadr org) off))
                    (list (+ (car org) wid off) (+ (cadr org) hgt off)) (list (- (car org) off)     (+ (cadr org) hgt off))
                )
            )
        )
    )
    (if lst
        (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) bpt)) lst))
            (list
                (list (cos rot) (sin (- rot)) 0.0)
                (list (sin rot) (cos rot)     0.0)
               '(0.0 0.0 1.0)
            )
        )
    )
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: BoxText.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"bt\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;




;�������ؽ����߽����
(defun C:TCBJ(/ sg i s1) 
  (princ "\n��������߿���ѡ�����ͼ����")
  (setq sg (ssget '((0 . "HATCH"))) i -1)
  (while (setq s1 (ssname sg (setq i (1+ i))))
    (command "-hatchedit" s1 "b" "p" "y")
   )
  (princ)
)





































  



  


;���¼�����������


;���¼�����������

(defun mai_make_panel (menulist / m n nn nnn menuname val_lst tt order_lst)

  (defun add_dclrow (dstr)
    (if  dstr
      (setq val_lst (cons dstr val_lst))
    ) ;_ ����if
  ) ;_ ����defun

  (add_dclrow "curbutton : button{width=10;vertical_margin=none;vertical_margin=none;}")
  (add_dclrow
    "curpanel : dialog{label=\"̷������С���򹤾���\";alignment=centered;vertical_margin=none;horizontal_margin=none;"
  ) ;_ ����add_dclrow
  (add_dclrow ":row{")
  (setq  len    (apply 'max (mapcar 'length menulist))
  order_lst (apply 'append (mapcar 'cdr menulist))
  n    0
  m    0
  ) ;_ ����setq
  (foreach nn menulist
    (add_dclrow
      (strcat " : boxed_column{label=\"" (car nn) "\";vertical_margin=none; horizontal_margin=none;")
    ) ;����б��
    (foreach nnn (cdr nn)
      (setq n  (1+ n)
      tt (car nnn)
      tt (if tt
     tt
     ""
         ) ;_ ����if
      ) ;_ ����setq
      (add_dclrow (strcat " : curbutton{key=\"but" (vl-princ-to-string n) "\";label=\"" tt "\";}"))
    ) ;����б���еİ�ť
    (repeat (- len (length nn))
      (setq m (1+ m))
      (add_dclrow (strcat " : curbutton{key=\"butno" (vl-princ-to-string m) "\";}"))
    ) ;���в����б���еİ�ť
    (add_dclrow "}")
  ) ;_ ����foreach

  (add_dclrow
    "}:button{label=\"�ر�\";key=\"cancel\";is_cancel=true;width=10;fixed_width=true;alignment=centered;}}"
  )


  (setq menuname (vl-filename-mktemp "temp_pannel.dcl"))
  (setq nn (open menuname "w"))
  (foreach n (reverse val_lst) (write-line n nn))
  (close nn)
  (setq nnn (load_dialog menuname))
  (vl-file-delete menuname)

  (if (not (new_dialog "curpanel" nnn))
    (exit)
  ) ;_ ����if
  (setq n 0)
  (repeat m (mode_tile (strcat "butno" (vl-princ-to-string m)) 1) (setq m (1- m))) ;���ò����б���еİ�ť
  (foreach nn menulist
    (foreach nnn (cdr nn)
      (setq n  (1+ n)
      tt (car nnn) ;tt order_lst
      tt (if tt
     tt
     ""
         ) ;_ ����if
      ) ;_ ����setq
      (if (= tt "")
  (mode_tile (strcat "but" (vl-princ-to-string n)) 1)
  (action_tile
    (strcat "but" (vl-princ-to-string n))
    (strcat "(done_dialog " (vl-princ-to-string n) ")")
  ) ;_ ����action_tile
      ) ;_ ����if
    ) ;_ ����foreach
  ) ;_ ����foreach

  (setq nn (start_dialog))
  (unload_dialog nnn)


  (if (> nn 0)
    (progn (setq tt (cadr (nth (1- nn) order_lst))) ;ȡ�����ť��ֵ(����ܹؼ�)
     (if (= (eval (read (strcat "(type c:" tt ")"))) 'SUBR)
       (progn (princ "\n")
        (eval (read (strcat "(c:" tt ")")))
       ) ;_ ����progn
       (progn
         (princ "\n")
         (vla-SendCommand (vla-get-ActiveDocument(vlax-get-acad-object)) (strcat tt "\n"))
       ) ;_ ����progn
     ) ;_ ����if
    ) ;_ ����progn
  ) ;_ ����if
) ;_ ����defun