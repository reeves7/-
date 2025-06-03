;;用户可自己修改界面
(defun c:xxx ()
  (mai_make_panel
    (list
      (list "lisp小程序1"
      (list "定距定向定数复制" "CMC") ;(list  按钮标签  运行的命令)
      (list "交点画圆" "hyq")
      (list "自动绘制梁截面" "lgg7")
      (list "绘制连系钢梁" "3tj")
      (list "动态延长" "saa")
      (list "绘制矩形对角线" "jxdx")
      (list "两点间定数阵列" "twm")
      (list "线段等分" "XBR")
      ) ;_ 结束list
      (list "谋哥程序2"
      (list " ＋ 型柱" "mini_c_column")
      (list " Ｌ 型柱" "mini_l_column")
      (list " Ｔ 型柱" "mini_t_column")
      (list " ○ 型柱" "mini_O_column")
      (list " □ 型柱" "mini_r_column")
      (list "轴线布柱" "mini_axis_column")
      (list "填 充 柱" "mini_fill_column")
      (list "墙体填充" "mini_fill_wall")
      (list "转PKPM GS柱" "mini_pline_to_beam")
      (list "多线变柱" "mini_pline_to_column")
      ) ;_ 结束list
       (list "谋哥程序3"
      (list " ＋ 型柱" "mini_c_column")
      (list " Ｌ 型柱" "mini_l_column")
      (list " Ｔ 型柱" "mini_t_column")
      (list " ○ 型柱" "mini_O_column")
      (list " □ 型柱" "mini_r_column")
      (list "轴线布柱" "mini_axis_column")
      (list "填 充 柱" "mini_fill_column")
      (list "墙体填充" "mini_fill_wall")
      (list "转PKPM GS柱" "mini_pline_to_beam")
      (list "多线变柱" "mini_pline_to_column")
      ) ;_ 结束list
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
	(setq bj(getreal "\n请输入圆半径<60>"))
	(if(not bj)(setq bj 60))
  (setq acad (vlax-get-acad-object))
  (setq acaddocument (vla-get-activedocument acad))
  (setq mspace (vla-get-modelspace acaddocument))
  (setq ptlist (ssinters ss))
	(foreach a ptlist
		(command "circle" a bj)
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
  (command "pline" pa pb pc pd "c")
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
(setq JJ (getreal "\n 请输入间距: "))
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