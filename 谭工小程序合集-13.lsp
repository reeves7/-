;;用户可自己修改界面
(defun c:ggr ()
  (mai_make_panel
    (list
      (list "lisp小程序1"
      (list "       定距定向定数复制        " "CMC") ;(list  按钮标签  运行的命令)
      (list "       交点画圆       " "hyq")
      (list "       自动绘制梁截面      " "lgg7")
      (list "       绘制连系钢梁          " "3tj")
      (list "        动态延长       " "saa")
      (list "       绘制矩形对角线        " "jxdx")
      (list "           两点间定数阵列       " "twm")
      (list "           线段等分          " "XBR")
      (list "           双下划线          " "under2")
      ) ;_ lisp小程序1
             (list "lisp小程序2"
      (list "快速批量绘制矩形对角线" "draw_rectangle_diagonals") ;(list  按钮标签  运行的命令)
	(list "快速选择" "ksxz") ;(list  按钮标签  运行的命令)
	(list "      梁底木方（小楞）   " "ldmf") ;(list  按钮标签  运行的命令)
	(list "      梁侧、板底木方（小楞）   " "bdm") ;(list  按钮标签  运行的命令)
	(list "      文字加框   " "bt") ;(list  按钮标签  运行的命令)
	(list "      重建填充图案边界   " "TCBJ") ;(list  按钮标签  运行的命令)
       ) ;_ 结束list小程序2
    ) ;_ 结束list
  ) ;_ 结束mai_make_panel
  (princ)
) ;_ 结束defun
;以下是定距定向定数自动多重复制
(defun C:CMC ()
(setq A nil)
(setq OM (getvar "OSMODE"))
(setvar "OSMODE" 33)
(setq PNT1 (getpoint "\n方向起点:  "))
(setq PNT2 (getpoint "\n方向终点: " PNT1))(terpri)
(setq ANG (angle PNT1 PNT2))
(setq JJ (getreal "\n 请输入间距: "))
(setq SL (getreal "\n 复制数量: "))
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
;以下是交点画圆
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
  (princ "\n请选择多段线、样条曲线、直线、圆、圆弧或椭圆：")
  (if (not (setq ss (ssget '((0 . "*LINE,ARC,CIRCLE,ELLIPSE")))))
    (progn (princ "\n提示：未选取图元，程序退出。\n") (exit))
  )
	(setq bj(getreal "\n请输入圆半径<48>"))
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
;以下是自动绘制梁截面
(defun c:lgg7 ()
(setvar "cmdecho" 0)	
(setq en (entsel "选择梁的尺寸标注:"))	
(setq en_data (entget (car en)))	
(setq liangguige (assoc 1 en_data))
(setq liangchicun(cdr liangguige))
  (setq pos (vl-string-search "x" liangchicun));如300x800，这里返回结果就是3，从左开始数，0，1，2，3，第3个找到x
  (setq liangkan(substr liangchicun 1 pos ));定义出梁的宽度，如300x800，则从第1位数开始截取字符串，截取的长度的数值刚好等于利用searchx搜索x所在位置的次序数。
  (setq lianggao(substr liangchicun (+ pos 2) ));定义出梁的高度，如300x800，则从第5位数开始截取字符串，3+2=5.
  (setq lianggao1(atof lianggao))
  (setq liangkan1(atof liangkan))
  (setq pa (getpoint "请输入梁左上角点: "))
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

;以下是绘制连系钢梁
(defun c:3tj (/ os pt1 pt2 pt3 pt4 ag0 ag len ent ct in k)
  (setq os (getvar "osmode")) 
  (while (and (setq pt1 (getpoint "\n角点1:"))
              (setq ag0  (getangle pt1 "角度:"))
       (progn (princ "对角点:") (setq ct t))
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

;以下是动态延长
(defun c:saa()
(command "Lengthen" "dy" "line" Pause))

;以下是自动绘制矩形对角线
(defun c:jxdx(/ a b c)
(setq a (getpoint "\n点选矩形内部的一点: "))
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

;以下是两点间定数阵列
(defun c:twm ( / en xfx pt1 pt2 zljl sl JJ)
(setvar 'cmdecho 0)
(setq en (car(entsel "\n选择对象:")))
(setq xfx (car(apply 'mapcar(cons '- (bwh en)))))
(setq pt1 (getpoint "\n指定第一点:"))
(setq pt2 (getpoint "\n指定第二点:"))
(setq zljl (- (distance pt1 pt2) (abs xfx) ) )
(setq JJ (getreal "\n 请输入间距:<300> "))
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


;以下是线段等分子程序
(defun C:XBR (/ OS OE SS VSS N I D LEN E OBJ E)   
	 (princ "\n 等分打断线段")   
	 (defun *ERROR* (MSG)        (setvar "osmode" OS)        (vl-cmdf ".undo" "e")        (setq *ERROR* OE)    )   
	 (vl-cmdf ".undo" "be")    
	 (princ "\n 选择要等分的物件:")   
	  (setq  OE *ERROR*   OS (getvar "osmode")   SS (ssget '((0 . "LINE,*POLYLINE,SPLINE,ARC")))    )   
	   (setvar "osmode" 0)   
	    (or (setq N (getint "\n 定数等分<需定距等分/直接回车>:"))            (setq D (getdist "\n 指定线段长度:"))    ) 
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

;以下是双下划线子程序
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



  
;以下是快速批量绘制矩形对角线

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





;以下是快速选择
;;---------------------------------------
;采集线坐标，直线段间不取样，曲线段按长度相似度0.9999加密取样
;;;;WZG356 by 20181129
;;适用"line,lwpolyline,polyline,spline,circle,arc,ellipse"
;速度尚可, 万点耗时不大于2秒
;但lwpolyline,polyline,circle,arc,ellipse弧段用角度法取点速度更快
;;示例(outcurvept (car(entsel "\n选择对象: ")))
(defun outcurvept (en / ob n l ls1 ls2 lo po a b p1 p2 p11 p22)
	(setq ob(vlax-ename->vla-object en))
	(setq ls1(list(cons 0.0(vlax-curve-getstartpoint ob))))
	(if	(wcmatch (vla-get-objectname ob) "*Polyline")
		(setq n 0
			  x(while(setq po(vlax-curve-getpointatparam ob(setq n(1+ n))))
				     (setq l  (vlax-curve-getDistAtParam ob n))
				     (setq ls2(append ls2(list(cons l po))));距离+坐标
				) ;x仅匹配setq格式用
		)
		(setq l  (vlax-curve-getDistAtParam ob(vlax-curve-getendparam ob))
			  ls2(list(cons l(vlax-curve-getendpoint ob)))
		);line,spline,circle,arc,ellipse
	)	
	(while
		(setq p11(last ls1)  p22(car ls2))
		(setq a  (car p11)  b  (- (car p22) a))
		(setq p1 (cdr p11)  p2 (cdr p22))
		(if	(or (equal b(distance p1 p2) 1e-6);直线段不管
				(and(setq lo (+(* b 0.5)a))
					(setq po(vlax-curve-getPointAtDist ob lo))
					(< (* b 0.499995)(distance p1 po))
				);
			)
			(setq ls2(cdr ls2) ls1(append ls1 (list p22)))
			(setq ls2(cons(cons lo po)ls2))
		)
	);循环自适应
	(mapcar 'cdr ls1)
)
;ssget栏选/圈交/圈围
;sel适用"F" "CP" "WP" 栏选/圈交/圈围
;(ps2ssget "wp" nil '((0 . "text")))
(defun ps2ssget (sel ps filters
	 / x2cpwp myerr olderr selm sel1 FUNSR str1 strs str2 en p n)
	(defun myerr(m)
		(cond(m(redraw)(setq *error* olderr)))
	)
	(defun x2cpwp(sel)
		(setq ssgetlb3n(vl-position sel '("F" "C" "W")))
		;在快速选择工具中用的公共变量ssgetlb3n
		(if(= sel "F")sel(strcat sel "p"))
	)	
	(setq olderr *error* *error* myerr)
	(setq sel(strcase(substr sel 1 1)))
	(if ps(setq selm sel
			FUNSR getkword
			str1(apply 'strcat '("F " "C " "W "))
			strs2 (list "\n选择方式" "/F栏选" "/Cp圈交" "/Wp圈围" (strcat  "<" sel ">:"))
		)
		(setq  selm "E"
			FUNSR getpoint
			str1(apply 'strcat (vl-remove (strcat sel " ")'("F " "C " "W "  "E ")))
			n (vl-position sel '("F" "C" "W"))
			sel1(nth n '("栏选" "圈交" "圈围"))
			strs (list (strcat "\n指定" sel1 "第一点或") 
			          "/F栏选" "/Cp圈交" "/Wp圈围" 
			          (strcat "/E选线" sel1) 
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
				(setq p (getpoint p "\n下一点"))
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
			(princ "\n点\"*olyline,line,spline,circle,arc,ellipse\"取1条线")
			(princ sel1)
			(if	(setq en(ssget ":E:S" '((0 . "*olyline,line,spline,circle,arc,ellipse"))))
				(if	(setq ss(ssget (x2cpwp sel) (outcurvept (setq en(ssname en 0))) filters))
					(progn(ssdel en ss)(if(> (sslength ss)0)ss nil))
					;选择集不包含用于栏选的线
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
;收集自晓东论坛
;SSGET的一些字符串是统配符，比如. # , *等等，
;如果要过滤的字符串中包括这些，会过滤不到，用下面这个函数转义下就OK 了。
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
;返回表中某元素所有索引
;(myvl-position "1"(list "1" "0" "1" "0" "1" "0"))
(defun myvl-position (a ls / i is)
	(repeat (setq i(length ls))		
		(if(equal a(nth (setq i(1- i))ls)1e-8)(setq is(cons i is)))				
	)is
)
;表写对话框输出选项参数
(defun SSSfilter(lst / lb3n lb3 file f i name id dd key keys key0 keys0 kvs)	
	(if (numberp ssgetlb3n)
		;公共变量ssgetlb3n来自函数ps2ssget执行时提供
		(setq lb3n (fix ssgetlb3n))
		(setq lb3n 0)
	)
	(setq lb3(nth lb3n '("栏选" "圈交" "圈围")))
	;lb3n= 0 1 2分别选用"栏选" "圈交" "圈围"按钮
	(setq file (vl-filename-mktemp "temp.dcl") f (open file "w"))
	(write-line "MY_SSS: dialog{label=\"快速选择\";" f)
	(write-line " :boxed_column{ label=\"过滤选项   值\";" f)
	(setq I -1)
	(foreach name lst
		(setq keys(cons(setq key(strcat "KEY"  (itoa (setq I (1+ I)))))keys))
		(write-line (strcat ":toggle{label=\"" name "\";key=\"" key "\";value=0;}")f)			
    )
    (write-line "}" f)
    (write-line ":row{label=\"选择过滤方式\";" f)    
    (write-line "      : button{label = \"框选\";key = \"bton1\";}" f)
    (write-line "      : button{label = \"全图\";key = \"bton2\";}" f)
    (if lb3(write-line (strcat "      : button{label = "(vl-prin1-to-string lb3) ";key = \"bton3\";}") f))
    (write-line "}" f)
    (write-line ":row{: button{label = \"重选样本\";key = \"bton4\";}" f)
    (write-line "             cancel_button;}" f)
    (write-line ":text {key = \"zzhe\";label = \"by 谋哥国际                        \" ;}}" f)
    (close f)
    (setq keys(reverse keys) key0(car keys)keys0(cddddr(cddr keys)))
    ;前六项须为：类型、图层、颜色、线型、线型比例、线宽即适用所有对象类型
    ;第6项之后的只有勾选类型才有效设为keys0
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
			    );只有勾选类型keys0才有效
			)
			(action_tile key "(mode_tile \"button1\" 2)")
		);button1永为焦点，回车可用
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
	    ;返回勾选的按钮索引
	    ((= dd 4)(c:myQSELECT))
	    (t nil)
	)
);end -defun
;根据模板对象设置过滤表(outSSfilterls(car(entsel)))
(defun outSSfilterls (e / es e0 l1 l2 l3 str as x y a b v )
    (setq es (entget e) e0 (cdr (assoc 0 es)))
    (if (not (assoc 62 es))(setq es (append es(list '(62 . 256)))))
    (if (not (assoc 6 es))(setq es (append es(list '(6 . "ByLayer")))))        
    (setq l1 '((6 . "线型 =")(62 . "颜色 =")(8 . "图层 =")(0 . "类型 =")))    	
    (setq e(vlax-ename->vla-object e))
    (setq as (list
	    (cons 'LinetypeScale "线型比例 =")	    
	    (cons 'Lineweight "线宽 =")
	    ;对话框前六项须为：类型、图层、颜色、线型、线型比例、线宽即适用所有对象类型
        ;在对话框第6项之后的只有勾选类型才有效设为keys0	    
	    (cons 'StyleName "样式名称 =")	    
	    (cons 'Height (if (wcmatch e0 "*TEXT,ATTDEF") "文字高度 =" "高度 ="))
	    (cons 'HasAttributes "属性块? =")
	    (cons 'isdynamicblock "动态块? =")
	    (cons 'EffectiveName "块名 =");块用这种方式更好
	    (cons 'Rotation "旋转角度 =")
	    (cons 'AttachmentPoint "对正 =");多行文字9点
	    ;(cons 'Alignment "对正 =");没搞懂这个   
	    (cons 'Closed "闭合? =")
	    (cons 'ConstantWidth "全局宽度 =")	    
	    (cons 'Radius "半径 =")
	    (cons 'Arrowhead1Type "第一箭头 =")
	    (cons 'Arrowhead2Type "第二箭头 =")
	    (cons 'TextStyle "文字样式 =")	    
	    (cons 'Measurement "测量值 =")
	    (cons 'TextOverride "文字替代 =")
	    (cons 'LinearScaleFactor "测量比例 =")	    	    
	    (cons 'ScaleFactor (if (wcmatch e0 "TEXT") "宽度因子 =" "全局比例 ="))
	    (cons 'area "面积 =")
	    (cons 'length "长度 =")
	    (cons 'TextString "文字内容 =")	    
	))
    (foreach x as
	    (setq v(vl-catch-all-apply '(lambda(a b)(Vlax-Get a b))(list e (car x))))
	    (if (not(vl-catch-all-error-p v))
		    (setq l1(cons x l1) es(append es(list (cons (car x) v))))
		)	    
    );属性合法则(属性 . 值)加入es,(属性. 释义)加入l1 
    (cond
	    ((= e0 "INSERT")(setq l1 (cons '(43 . "Z比例 =")(cons '(42 . "Y比例 =")(cons '(41 . "X比例 =")l1)))))	    
	    ((= e0 "HATCH")(setq l1(cons '(52 . "填充角度 =")(cons '(41 . "填充比例 =")(cons '(2 . "图案名 =")l1)))))	    
	    ((= e0 "ATTDEF")(setq l1 (cons '(2 . "属性标记 =")l1)))
	    ((= e0 "DIMENSION")(setq l1(cons '(70 . "标注类型 =")l1)))
	    (t nil)
	);根据图元类补充l1
    (setq l2 nil l3 nil)
    (foreach x l1
	    ;根据L1剔除不要的群码,生成
	    ;l2是对话框展示的值表
	    ;l3原始值表，与L2对应
	    (and(setq y (assoc (car x) es))
		    (setq l3(cons y l3))
		    (setq str(strcat  (cdr x)
			    (if  (member (car x) '(Closed HasAttributes isdynamicblock))
				    (if  (= (cdr y) -1)"是" "否")
				    (vl-princ-to-string(cdr y))
				)		    
		    ))		    
		    (cond
			    ((and(= e0 "MTEXT")(= (car x) 'TextString))(setq l2(cons "文字内容 =......" l2)))
			    ((and(= e0 "TEXT")(= (car x) 'TextString))(setq l2(cons (substr str 1 30) l2)))
			    ((and(= e0 "DIMENSION")(= (car x) 70))
			        (setq l2(cons(strcat (cdr x)
				        (nth(apply 'max(mapcar '(lambda(i)(logand i (cdr y)))(list 0 1 2 3 4 5 6)))
			    	    (list "转角" "对齐" "角度" "直径" "半径" "三点角度" "坐标")))l2)
			    	)
			    )
			    (t(setq l2(cons str l2)))
			)
		)
    )      
    (if  (and l2 (setq l2(SSSfilter l2)))
	    ;对话框返回的l2第一项是ssget操作模式
	    (progn		    
		    (setq l3(mapcar '(lambda(x)(nth x l3)) (cdr l2)))
		    (if  (setq v(assoc 'EffectiveName l3))
			    ;为提高效率针对块过滤项适当处理			    
			    (setq l3(vl-remove(assoc 'isdynamicblock l3)l3)
				    l3(vl-remove(assoc 'HasAttributes l3)l3)
				    l3(subst(cons 2 (cdr v)) v l3)
				    ;如√选块名，则是否属性块/动态块是多余的
				    ;(EffectiveName . 块名)转为(2 . 块名)
				    ;增加组码过滤表提高ssget效率
			    )
			)
		    (cons (car l2) l3)
		)
    )
)

;选择集按属性值过滤 ;ss:选择集 prop:属性 v:属性值 fuz：容差	
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
			;分离出((属性 . 值) ....)表
			(setq fls(vl-remove-if '(lambda(a)(not(numberp (car a))))fls))
			(setq fls(mapcar '(lambda(a)
				(if(= (setq str(cdr a)) 'STR)(cons (car a)(transsfilter str))a))fls)
			)
			;分离出((组码 . 值) ....)表ssget用处理并对字符串含通配符转义
			(cond
				((= x "")(setq ss(ssget fls)))
				((= x "x")(setq ss(ssget "x" fls)))
				(t(setq ss(ps2ssget x nil fls)))
			)
			(if ss(mapcar '(lambda(a)(if a(setq ss(propss ss (car a) (cdr a) 1e-4))))as))
			(if	(and ss (> (sslength ss)0))
				(progn(sssetfirst nil ss)(princ (strcat "\n 共选择" (itoa(sslength ss)) "个符合条件对象")))
				(progn(setq ss nil)(princ "\n未选中对象"))
			)
		)
	)ss
)
(vl-load-com)
;快速选择
(defun c:ksxz nil(c:myQSELECT))
(defun c:myQSELECT( / e)
	(princ "\n 快速选择,谋哥国际")
	(if(setq e(car(entsel "\n选择样本对象：")))(MYQSELECT e))
	(princ)	
)



;以下是梁底木方（小楞）
(defun c:ldmf()
(setvar "cmdecho" 0)	
  (setq pz (getpoint "请输入梁底中点: "))
  (setq pa (polar pz (* pi 1) 1000))
  (setq pb (polar pa (* pi 1.5) 100))
  (setq pc (polar pb 0 2000))
  (setq pd (polar pz 0 1000))
  (command "rectangle" pa pc) ;rectangle 后面跟着矩形的对角坐标点
 ; (command "pline" pa pb pc pd "c")
  ;会执行慢很多,分为四个线段绘制，明显出现时间顿挫
(prin1)
)
(prompt "谭工 梁底木方小楞自动绘制")
(prin1)


;以下是梁侧、板底木方（小楞）
(defun c:bdm()
(setvar "cmdecho" 0)	
;以下是板底右木方（小楞）
  (setq za (getpoint "请输入梁板左交点: "))
  ;(setq pa (getpoint "请输入梁板右交点: "))
  (setq lj (getpoint "请输入梁右下方角点："))
  ;(setq ljz (getpoint "请输入梁左下方角点："))
  (setq pa (list (car lj) (cadr za)) ljz (list (car za) (cadr lj)))
  (setq pb (polar pa (* pi 0) 50))
  (setq pc (polar pb (* pi 1.5) 100))
  (setq pd (polar pc 0 -50))
  (setq ss (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 pa)(cons 10 pb)(cons 10 pc)(cons 10 pd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) ss)
  
  ;下面定义板底右木方框内线段的端点
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
   	
   ;;以下生成块
(command "_.-group"  "c" "*"  "" ss "")

;以下是板底左木方（小楞）
 (setq zb (polar za (* pi 1.5) 100))
  (setq zc (polar zb (* pi 1) 50))
  (setq zd (polar zc (* pi 0.5) 100))
   (setq zz (ssadd))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 za)(cons 10 zb)(cons 10 zc)(cons 10 zd) (cons 62 4) (cons 70 1)))
(ssadd (entlast) zz)

  ;下面定义板底左木方框内线段的端点
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
  
   ;下面定义板底钢管端点
   (setq gg1(polar pa (* pi 1.5) 100))
   (setq gg2 (polar gg1 (* pi 0) 1000))
   (setq gg3 (polar gg1 (* pi 1.5) 48))
   (setq gg4 (polar gg3 (* pi 0) 1000))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 gg1)(cons 11 gg2) (cons 62 4)))
   (entmake (list (cons 0 "LINE")(cons 100 "AcDbEntity")(cons  100 "AcDbline")(cons 10 gg3)(cons 11 gg4) (cons 62 4)))
   ;下面定义梁右侧木方角点
      (setq cla(polar pa (* pi 1.5) 148))
   (setq clb (polar cla (* pi 0) 100))
  (setq clc (polar lj (* pi 0) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 cla)(cons 10 clb)(cons 10 clc)(cons 10 lj) (cons 62 4) (cons 70 1)))
;下面定义梁左侧木方角点
 (setq claz(polar za (* pi 1.5) 148))
     (setq clcz (polar ljz (* pi 1) 100))
     (setq cldz (polar claz (* pi 1) 100))
(entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 claz)(cons 10 ljz)(cons 10 clcz)(cons 10 cldz) (cons 62 4) (cons 70 1)))

;以下定义梁底第一层木方角点
   (setq la (polar lj (* pi 1) 1000))
  (setq lb (polar la (* pi 1.5) 100))
  (setq lc (polar lb 0 2000))
  (setq ld (polar lj 0 1000))
  (entmake (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons  100 "AcDbPolyline")(cons 10 la)(cons 10 lb)(cons 10 lc)(cons 10 ld) (cons 62 4) (cons 70 1)))
  ;(command "rectangle" la lc) ;rectangle 后面跟着矩形的对角坐标点

      ;(command "rectangle" pa pc) ;rectangle 后面跟着矩形的对角坐标点
 ; (command "pline" pa pb pc pd "c")
  ;会执行慢很多,分为四个线段绘制，明显出现时间顿挫
  ;(command "line" lab lbb "")
  ;(command "line" lcb ldb "")
  
 ;(command "line" gg1 gg2 "")
  ; (command "line" gg3 gg4 "")
   ;(command "rectangle" clb lj) 
(prin1)
)
(prompt "谭工 板底右木方小楞 侧愣自动绘制")
(prin1)


;以下是文字加框程序

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




;以下是重建填充边界程序
(defun C:TCBJ(/ sg i s1) 
  (princ "\n填充重生边框，请选择填充图案：")
  (setq sg (ssget '((0 . "HATCH"))) i -1)
  (while (setq s1 (ssname sg (setq i (1+ i))))
    (command "-hatchedit" s1 "b" "p" "y")
   )
  (princ)
)





































  



  


;往下继续是主程序


;往下继续是主程序

(defun mai_make_panel (menulist / m n nn nnn menuname val_lst tt order_lst)

  (defun add_dclrow (dstr)
    (if  dstr
      (setq val_lst (cons dstr val_lst))
    ) ;_ 结束if
  ) ;_ 结束defun

  (add_dclrow "curbutton : button{width=10;vertical_margin=none;vertical_margin=none;}")
  (add_dclrow
    "curpanel : dialog{label=\"谭工技研小程序工具箱\";alignment=centered;vertical_margin=none;horizontal_margin=none;"
  ) ;_ 结束add_dclrow
  (add_dclrow ":row{")
  (setq  len    (apply 'max (mapcar 'length menulist))
  order_lst (apply 'append (mapcar 'cdr menulist))
  n    0
  m    0
  ) ;_ 结束setq
  (foreach nn menulist
    (add_dclrow
      (strcat " : boxed_column{label=\"" (car nn) "\";vertical_margin=none; horizontal_margin=none;")
    ) ;添加列表框
    (foreach nnn (cdr nn)
      (setq n  (1+ n)
      tt (car nnn)
      tt (if tt
     tt
     ""
         ) ;_ 结束if
      ) ;_ 结束setq
      (add_dclrow (strcat " : curbutton{key=\"but" (vl-princ-to-string n) "\";label=\"" tt "\";}"))
    ) ;添加列表框中的按钮
    (repeat (- len (length nn))
      (setq m (1+ m))
      (add_dclrow (strcat " : curbutton{key=\"butno" (vl-princ-to-string m) "\";}"))
    ) ;按列补齐列表框中的按钮
    (add_dclrow "}")
  ) ;_ 结束foreach

  (add_dclrow
    "}:button{label=\"关闭\";key=\"cancel\";is_cancel=true;width=10;fixed_width=true;alignment=centered;}}"
  )


  (setq menuname (vl-filename-mktemp "temp_pannel.dcl"))
  (setq nn (open menuname "w"))
  (foreach n (reverse val_lst) (write-line n nn))
  (close nn)
  (setq nnn (load_dialog menuname))
  (vl-file-delete menuname)

  (if (not (new_dialog "curpanel" nnn))
    (exit)
  ) ;_ 结束if
  (setq n 0)
  (repeat m (mode_tile (strcat "butno" (vl-princ-to-string m)) 1) (setq m (1- m))) ;禁用补齐列表框中的按钮
  (foreach nn menulist
    (foreach nnn (cdr nn)
      (setq n  (1+ n)
      tt (car nnn) ;tt order_lst
      tt (if tt
     tt
     ""
         ) ;_ 结束if
      ) ;_ 结束setq
      (if (= tt "")
  (mode_tile (strcat "but" (vl-princ-to-string n)) 1)
  (action_tile
    (strcat "but" (vl-princ-to-string n))
    (strcat "(done_dialog " (vl-princ-to-string n) ")")
  ) ;_ 结束action_tile
      ) ;_ 结束if
    ) ;_ 结束foreach
  ) ;_ 结束foreach

  (setq nn (start_dialog))
  (unload_dialog nnn)


  (if (> nn 0)
    (progn (setq tt (cadr (nth (1- nn) order_lst))) ;取点击按钮的值(这个很关键)
     (if (= (eval (read (strcat "(type c:" tt ")"))) 'SUBR)
       (progn (princ "\n")
        (eval (read (strcat "(c:" tt ")")))
       ) ;_ 结束progn
       (progn
         (princ "\n")
         (vla-SendCommand (vla-get-ActiveDocument(vlax-get-acad-object)) (strcat tt "\n"))
       ) ;_ 结束progn
     ) ;_ 结束if
    ) ;_ 结束progn
  ) ;_ 结束if
) ;_ 结束defun