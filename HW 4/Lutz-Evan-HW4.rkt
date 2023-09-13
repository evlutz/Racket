;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Lutz-Evan-HW4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #t)))
;HW 4
;ewlutz
;Evan Lutz
;brmarsh
;Ben Marsh


;1 #################################

(define-struct Student(name email))
;Student is a (make-students (STRING STRING))
;name is the name of the student
;email is the students email

(define john(make-Student "john" "john@gmail.com"))
(define liam(make-Student "liam" "liam@gmail.com"))
(define bite(make-Student "bite" "bite@gmail.com"))



;A ListOfStudents is a
;    empty
;    cons Student(ListOfStudents)

(define ListOfStudents1 (list john liam))
(define ListOfStudents2 (list bite liam))


(define-struct projectnode (project-id val advisor students l r))
;; a ProjectNode is a (make-projectnode Number String String ListOfStudent BST BST)
;; project-id is the node key
;; val is the title
;; advisor is the person advising the Proj.
;; Students is a ListOfStudents working on the project
;; l & r are the left and right subtrees



;A PT (Project tree) is one of
; false
; (make-projectnode Number String String ListOfStudents PT PT)
;interp. false means no PT
;; project-id is the node key
;; val is the title
;; advisor is the person advising the Proj.
;; Students is a ListOfStudents working on the project
;; l & r are the left and right subtrees
;INVARIANT: for a given node
;; project-id is all > all project-id in its left child
;; project-id is all < all project-id in its right child
;; same project-id never appears twice



;2 #####################

;A PT (Project tree) is one of
; false
; (make-projectnode Number String String ListOfStudents PT PT)
;interp. false means no PT
;; project-id is the node key
;; val is the title
;; advisor is the person advising the Proj.
;; Students is a ListOfStudents working on the project
;; l & r are the left and right subtrees
;INVARIANT: for a given node
;; project-id is all > all project-id in its left child
;; project-id is all < all project-id in its right child
;; same project-id never appears twice


(define ProjectTree
  (make-projectnode 3.2345 "airobot" "Mr.Smith" ListOfStudents1 
                    (make-projectnode 2.2134 "3dprinter" "Dr.Johannes" ListOfStudents2
                                      (make-projectnode 1.9878 "Rocket" "Mr. Engie" ListOfStudents2 false false) false)
                    (make-projectnode 4.2222 "Joe" "Mr.Blue" ListOfStudents1 false
  (make-projectnode 5.6756 "healingpavement" "Mr. Infrastructure" ListOfStudents1 false false)
                                             )))




;3 ##########################

;; fn-for-student: Student -> 
#;
(define (fn-for-student Student)
  (Student-name ...)
  (Student-email ...))



;; los-fcn:  ListOfStudents -> 
#;
(define (los-fcn alos)
  (cond [(empty? alos)  (...)      ]
        [(cons? alos)   (... (first alos)
                             (los-fcn (rest alos)))]))


;; fn-for-pt: BST ->
#;
(define (fn-for-pt ProjectTree)
  (cond [(false? BinaryTree) (...)]
        [else ....
              (projectnode-project-id t) ; Integer
              (projectnode-val t) ;String
              (projectnode-advisor t) ;String
              (projectnode-students t) ;ListOfStudents
              (fn-for-pt (projectnode-l t)) ;ProjectNode
              (fn-for-pt (projectnode-r t))]))


;4 #############################
;BST NUMBER -> Boolean
;consumes a BST and the project-id for a department, and produces true if any of the projects in the BST are from that department. 
(define (any-in-dept? t deptNumb)
  (cond [(false? t) false]
        [else (cond [(and(< (projectnode-project-id t)(+ 1 deptNumb))(>(projectnode-project-id t) deptNumb)) true]
                    [(> (projectnode-project-id t) deptNumb) (any-in-dept? (projectnode-l t) deptNumb)]
                    [(< (projectnode-project-id t) deptNumb) (any-in-dept? (projectnode-r t) deptNumb)])]))
 
(any-in-dept? ProjectTree 5)
(check-expect (any-in-dept? ProjectTree 5) true)
(check-expect (any-in-dept? ProjectTree 1) true)
(check-expect (any-in-dept? ProjectTree -1) false)
(check-expect (any-in-dept? ProjectTree 20) false)

         

;5 #############################
;BST project-id email -> BST
;consumes a BST, a project ID and student email, the student will be dropped from the project and produce a new binary search tree without the student.
(define (drop-student t id studentEmail)
  (cond [(boolean? t) t]
        [else (cond [(and(< (projectnode-project-id t)(+ 1 id))(>(projectnode-project-id t) id))
                    (make-projectnode
                     (projectnode-project-id t)
                     (projectnode-val t)
                     (projectnode-advisor t)
                     (removeStudent (projectnode-students t) studentEmail)
                     (projectnode-l t)
                     (projectnode-r t))]
                    [(> (projectnode-project-id t) id)
                     (make-projectnode
                     (projectnode-project-id t)
                     (projectnode-val t)
                     (projectnode-advisor t)
                     (projectnode-students t)
                     (drop-student (projectnode-l t) id studentEmail)
                     (projectnode-r t))]
                    [(< (projectnode-project-id t) id)
                     (make-projectnode
                     (projectnode-project-id t)
                     (projectnode-val t)
                     (projectnode-advisor t)
                     (projectnode-students t)
                     (projectnode-l t)
                     (drop-student (projectnode-r t) id studentEmail)
                     )])])) 


;ListOfStudents String -> ListOfStudents
;Consumes a ListOfStudents and a email, th student is removed from the ListOfStudents
(define (removeStudent los email)
  (cond [(empty? los) empty]
        [(cons? los)
         (if (string=? (Student-email(first los)) email)
             (rest los)
             (cons(first los)
             (removeStudent(rest los) email)))]))

(check-expect (removeStudent ListOfStudents1 "john@gmail.com")  (list liam))
(check-expect (removeStudent ListOfStudents1 "liam@gmail.com")  (list john))
 


(check-expect(drop-student ProjectTree 5 "john@gmail.com") 
             (make-projectnode 3.2345 "airobot" "Mr.Smith" ListOfStudents1 
                               (make-projectnode 2.2134 "3dprinter" "Dr.Johannes" ListOfStudents2
                                      (make-projectnode 1.9878 "Rocket" "Mr. Engie" ListOfStudents2 false false) false)
                               (make-projectnode 4.2222 "Joe" "Mr.Blue" ListOfStudents1 false
                                                 (make-projectnode 5.6756 "healingpavement" "Mr. Infrastructure" (list liam) false false)
                                      )))


(check-expect(drop-student ProjectTree 3 "john@gmail.com")
             (make-projectnode 3.2345 "airobot" "Mr.Smith" (list liam) 
                               (make-projectnode 2.2134 "3dprinter" "Dr.Johannes" ListOfStudents2
                                                 (make-projectnode 1.9878 "Rocket" "Mr. Engie" ListOfStudents2 false false) false)
                               (make-projectnode 4.2222 "Joe" "Mr.Blue" ListOfStudents1 false
                                                 (make-projectnode 5.6756 "healingpavement" "Mr. Infrastructure" ListOfStudents1 false false)
                                                 )))


(check-expect(drop-student ProjectTree 1 "hn@gmail.com")
             (make-projectnode 3.2345 "airobot" "Mr.Smith" ListOfStudents1 
                               (make-projectnode 2.2134 "3dprinter" "Dr.Johannes" ListOfStudents2
                                                 (make-projectnode 1.9878 "Rocket" "Mr. Engie" ListOfStudents2 false false) false)
                               (make-projectnode 4.2222 "Joe" "Mr.Blue" ListOfStudents1 false
                                                 (make-projectnode 5.6756 "healingpavement" "Mr. Infrastructure" ListOfStudents1 false false)
                                                 )))

(check-expect(drop-student ProjectTree 5 "liam@gmail.com")
             (make-projectnode 3.2345 "airobot" "Mr.Smith" ListOfStudents1 
                               (make-projectnode 2.2134 "3dprinter" "Dr.Johannes" ListOfStudents2
                                                 (make-projectnode 1.9878 "Rocket" "Mr. Engie" ListOfStudents2 false false) false)
                               (make-projectnode 4.2222 "Joe" "Mr.Blue" ListOfStudents1 false
                                                 (make-projectnode 5.6756 "healingpavement" "Mr. Infrastructure" (list john) false false)
                                                 )))

(check-expect(drop-student ProjectTree 3 "john@gmail.com")
             (make-projectnode 3.2345 "airobot" "Mr.Smith" (list liam) 
                               (make-projectnode 2.2134 "3dprinter" "Dr.Johannes" ListOfStudents2
                                                 (make-projectnode 1.9878 "Rocket" "Mr. Engie" ListOfStudents2 false false) false)
                               (make-projectnode 4.2222 "Joe" "Mr.Blue" ListOfStudents1 false
                                                 (make-projectnode 5.6756 "healingpavement" "Mr. Infrastructure" ListOfStudents1 false false)
                                                 )))


;6 ###################################

; BST -> los
;consumes a binary search tree and produces a list of the titles of the projects, sorted in order by ascending course number
(define (list-projects-in-order-by-id-num bst)
  (cond [(boolean? bst) empty]
        [(projectnode? bst)
         (append (list-projects-in-order-by-id-num (projectnode-l bst))
                 (list (projectnode-val bst))
                 (list-projects-in-order-by-id-num (projectnode-r bst)))]))



(check-expect (list-projects-in-order-by-id-num ProjectTree) (list "Rocket" "3dprinter" "airobot" "Joe" "healingpavement"))

(check-expect (list-projects-in-order-by-id-num (make-projectnode 3.2345 "airobot" "Mr.Smith" ListOfStudents1 
                                                                  (make-projectnode 2.2134 "3dprinter" "Dr.Johannes" ListOfStudents2
                                                                                    (make-projectnode 1.9878 "Rocket" "Mr. Engie" ListOfStudents2 false false) false)false))
              (list "Rocket" "3dprinter" "airobot"))


  

;7 ###################################
;BST project-id val advisor los -> BST
;consumes a Project Tree, project-id val advisor and los to create a new node in the tree and returns the tree with the new node inserted
(define (add-project t id title advisor los)
  (cond [(boolean? t) (make-projectnode
                     id
                     title
                     advisor
                     los
                     false
                     false)]
        [else (cond [(> (projectnode-project-id t) id)
                     (make-projectnode
                     (projectnode-project-id t)
                     (projectnode-val t)
                     (projectnode-advisor t)
                     (projectnode-students t)
                     (add-project (projectnode-l t) id title advisor los)
                     (projectnode-r t))]
                    [(< (projectnode-project-id t) id)
                     (make-projectnode
                     (projectnode-project-id t)
                     (projectnode-val t)
                     (projectnode-advisor t)
                     (projectnode-students t)
                     (projectnode-l t)
                     (add-project (projectnode-r t) id title advisor los)
                     )])]))



(check-expect (add-project ProjectTree 0.1 "Jesus" "Mr.None" ListOfStudents1)
              (make-projectnode 3.2345 "airobot" "Mr.Smith" ListOfStudents1 
                                (make-projectnode 2.2134 "3dprinter" "Dr.Johannes" ListOfStudents2
                                                  (make-projectnode 1.9878 "Rocket" "Mr. Engie" ListOfStudents2
                                                                    (make-projectnode 0.1 "Jesus" "Mr.None" ListOfStudents1 false false) false) false)
                                (make-projectnode 4.2222 "Joe" "Mr.Blue" ListOfStudents1 false
                                                  (make-projectnode 5.6756 "healingpavement" "Mr. Infrastructure" ListOfStudents1 false false)
                                                  )))

(check-expect (add-project ProjectTree 9.10 "Jesus" "Mr.None" ListOfStudents1)
              (make-projectnode 3.2345 "airobot" "Mr.Smith" ListOfStudents1 
                                (make-projectnode 2.2134 "3dprinter" "Dr.Johannes" ListOfStudents2
                                                  (make-projectnode 1.9878 "Rocket" "Mr. Engie" ListOfStudents2 false false) false)
                                (make-projectnode 4.2222 "Joe" "Mr.Blue" ListOfStudents1 false
                                                  (make-projectnode 5.6756 "healingpavement" "Mr. Infrastructure" ListOfStudents1 false
                                                                    (make-projectnode 9.10 "Jesus" "Mr.None" ListOfStudents1 false false))
                                                  )))

;#########################################################







;Constants:
;(define TEXT-SIZE 14)
;(define TEXT-COLOR "white")
;(define KEY-VAL-SEPARATOR":")
;(define VSPACE (rectangle 1 10 "solid" "white"))
;(define HSPACE (rectangle 10 1 "solid" "white"))
;(define MTTREE(square 0 "solid" "white"))



;(define (render-bst t)
;  (cond [(false? t) MTTREE]
;[else
;(above (text (string-append (number->string (projectnode-project-id t)) KEY-VAL-SEPARATOR (projectnode-val t))
;TEXT-SIZE
;TEXT-COLOR)
;VSPACE
;(beside (render-bst (projectnode-l t))
;HSPACE
;(render-bst (projectnode-r t))))]))

;(render-bst ProjectTree)










