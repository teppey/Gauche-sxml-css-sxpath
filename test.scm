(use gauche.test)
(use sxml.sxpath)

(test-start "css-sxpath")
(use sxml.css-sxpath)
(test-module 'sxml.css-sxpath)

(define html
  '(*TOP*
     (html
       (head
         (meta (@ (http-equiv "content") (content "text/html")))
         (title (@ (class "title")) "html"))
       (body
         (h1 (@ (id "header")) "lorem ipsum")
         (address (@ (class "foo")) "boo woo")
         (p (@ (class "hocus") (color "pocus")) "content")
         (div (@ (class "contents"))
              (h2 (@ (class "en-x")) "palo")
              (ul
                (li "a"
                    (h3 (@ (class "red blue green")) "color"))
                (li (@ (class "foo")) "b")
                (li (@ (id "bar")) "c"))
              (pre (@ (lang "fr-be"))))))))

(define (test-selector selector expected)
  (test* selector expected ((css-sxpath selector) html)))

(test-section "Type selector")
(test-selector "h1" '((h1 (@ (id "header")) "lorem ipsum")))
(test-selector "h5" '())
(test-selector "\th2\n" '((h2 (@ (class "en-x")) "palo")))
(test-selector "li" '((li "a"
                          (h3 (@ (class "red blue green")) "color"))
                      (li (@ (class "foo")) "b")
                      (li (@ (id "bar")) "c")))


(test-section "Universal selector")
(test* "div *" '(h2 ul li h3 li li pre)
       (map car ((css-sxpath "div *") html)))
(test-selector "* h2" '((h2 (@ (class "en-x")) "palo")))
(test-selector "*.foo" '((address (@ (class "foo")) "boo woo")
                         (li (@ (class "foo")) "b")))
(test-selector "*#bar" '((li (@ (id "bar")) "c")))


(test-section "Attribute selectors")
(test-selector "title[class]" '((title (@ (class "title")) "html")))
(test-selector "h1[id=header]" '((h1 (@ (id "header")) "lorem ipsum")))
(test-selector "li[id^=\"ba\"]" '((li (@ (id "bar")) "c")))
(test-selector "title[class$='le']" '((title (@ (class "title")) "html")))
(test-selector "h1[id*=ad]" '((h1 (@ (id "header")) "lorem ipsum")))
(test-selector "h2[class='en-x']" '((h2 (@ (class "en-x")) "palo")))
(test-selector "h3[class~=\"blue\"]"
               '((h3 (@ (class "red blue green")) "color")))
(test-selector "h2[class|=en]" '((h2 (@ (class "en-x")) "palo")))
(test-selector "p[class][color]"
               '((p (@ (class "hocus") (color "pocus")) "content")))
(test-selector "p[class='hocus'][color='pocus']"
               '((p (@ (class "hocus") (color "pocus")) "content")))
(test-selector "p[class][color='pocus'][title]" '())



(test-section "Class selectors")
(test-selector "title.title" '((title (@ (class "title")) "html")))
(test-selector ".title" '((title (@ (class "title")) "html")))
(test-selector "h3.blue" '((h3 (@ (class "red blue green")) "color")))
(test-selector "h3.blue.green.red" '((h3 (@ (class "red blue green")) "color")))
(test-selector "body .foo" '((address (@ (class "foo")) "boo woo")
                             (li (@ (class "foo")) "b")))


(test-section "ID selectors")
(test-selector "h1#header" '((h1 (@ (id "header")) "lorem ipsum")))
(test-selector "#header" '((h1 (@ (id "header")) "lorem ipsum")))


(test-section "Pseudo-classes")
(test-selector "ul li:first-child"
               '((li "a" (h3 (@ (class "red blue green")) "color"))))
(test-selector "ul li:last-child" '((li (@ (id "bar")) "c")))
(test-selector "ul li:nth-child(1)"
               '((li "a" (h3 (@ (class "red blue green")) "color"))))
(test-selector "li:not([class])"
               '((li "a" (h3 (@ (class "red blue green")) "color"))
                 (li (@ (id "bar")) "c")))
(test-selector "pre:not([lang|='en'])" '((pre (@ (lang "fr-be")))))
(test-selector "h1:not([id=lorem])" '((h1 (@ (id "header")) "lorem ipsum")))
(test-selector "h3:not([class~='purple'])"
               '((h3 (@ (class "red blue green")) "color")))
(test* "input:enabled"
       '((input (@ (type "text") (enabled "true"))))
       ((css-sxpath "input:enabled")
        '(*TOP* (form (input (@ (type "text") (enabled "true")))))))
(test* "textarea[rows='10']:disabled"
       '((textarea (@ (rows 10) (cols 20) (disabled "disabled"))))
       ((css-sxpath "textarea:disabled")
        '(*TOP*
           (form
             (textarea (@ (rows 10) (cols 20) (disabled "disabled")))))))
(test* "input[type='checkbox']:checked"
       '((input (@ (type "checkbox") (name "foo") (checked "checked")))
         (input (@ (type "checkbox") (name "foo") (checked "true"))))
       ((css-sxpath "input[type='checkbox']:checked")
        '(*TOP*
           (form
             (input (@ (type "checkbox") (name "foo") (checked "checked")))
             (input (@ (type "checkbox") (name "foo")))
             (input (@ (type "checkbox") (name "foo") (checked "true")))))))




(test-section "Combinators")
(test-selector "body h2" '((h2 (@ (class "en-x")) "palo")))
(test-selector "body div address" '())
(test-selector "ul li:first-child + li" '((li (@ (class "foo")) "b")))
(test-selector "ul > li:first-child"
               '((li "a" (h3 (@ (class "red blue green")) "color"))))
(test-selector "ul > li:last-child" '((li (@ (id "bar")) "c")))
(test-selector "h1 + address" '((address (@ (class "foo")) "boo woo")))
(test-selector "h1 + div" '())
(test-selector "html>head>title.title" '((title (@ (class "title")) "html")))
(test-selector "h2 ~pre" '((pre (@ (lang "fr-be")))))


(test-section "Selectors group")
(test-selector "h1, h2"
               '((h1 (@ (id "header")) "lorem ipsum")
                 (h2 (@ (class "en-x")) "palo")))
(test-selector "h1 , h2"
               '((h1 (@ (id "header")) "lorem ipsum")
                 (h2 (@ (class "en-x")) "palo")))
(test-selector "h1 ,h2"
               '((h1 (@ (id "header")) "lorem ipsum")
                 (h2 (@ (class "en-x")) "palo")))
(test-selector "h1,h2"
               '((h1 (@ (id "header")) "lorem ipsum")
                 (h2 (@ (class "en-x")) "palo")))
(test-selector "h1#header, div ul li#bar"
               '((h1 (@ (id "header")) "lorem ipsum")
                 (li (@ (id "bar")) "c")))
(test-selector "head > meta[content='text/html'],address,pre"
               '((meta (@ (http-equiv "content") (content "text/html")))
                 (address (@ (class "foo")) "boo woo")
                 (pre (@ (lang "fr-be")))))


(test-section "Mixed path")
(test-selector '("h2") '((h2 (@ (class "en-x")) "palo")))
(test-selector '(// body div "h2") '((h2 (@ (class "en-x")) "palo")))
(test-selector `(// div ,(lambda (node root vars)
                           ((select-kids (ntype?? 'ul)) node)) "li.foo")
               '((li (@ (class "foo")) "b")))
(test-selector '("body" (div ("li h3")))
               '((h3 (@ (class "red blue green")) "color")))
(test-selector '("div ul li.foo" *text*) '("b"))

(test* "p.x a" '((a "bar"))
       ((css-sxpath "p.x a")
        '(*TOP* (html (body (a "foo") (p (@ (class "x")) (a "bar")))))))


(test-end)
