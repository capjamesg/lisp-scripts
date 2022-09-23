(defmacro fn (name args &rest contents)
    `(defun ,name (,@args) ,@contents))

(ql:quickload "usocket")

(fn response (data)
    (buildrequest
        (buildheaders
            (header "HTTP/1.1 200 OK")
            (header "Content-Type" "text/html")
            (link "https://webmention.io/jamesg.blog/webmention" "webmention")
            (link "https://indieauth.com/auth" "authorization_endpoint"))
        data))

(fn home ()
    (response
        (with-open-file (stream "./templates/index.jinja2")
            (loop for line = (read-line stream nil)))))

(fn checkpath (sourcepath checkagainst)
    (if (string= sourcepath checkagainst) t ()))

(fn router (path)
    (cond
        ((checkpath path "/") (home))
        (t "404")))

(fn parsereq (text)
    (loop for line = (read-line text nil)
        while line
        collect (progn
            (print (subseq line 4 5))
            (if
                (string= (subseq line 0 3) "GET")
                (return (router (subseq line 4 5)))
                (return "404")
            ))))

;; (print (parsereq (make-string-input-stream "HTTP GET 2022")))

;; (load "sockets.lisp")

(fn header (key &optional value newline)
    (concatenate 'string key (if value (concatenate 'string ": " value)) (if (not newline) '(#\Newline))))

(fn buildheaders (&rest headers)
    (apply 'concatenate 'string headers))

(fn link (url rel)
    (header "Link" (concatenate 'string "<" url ">; rel='" rel "'")))

(fn buildrequest (headers data)
    (concatenate 'string headers '(#\Newline) data))

(fn server (port)
    (let* ((socket (usocket:socket-listen "127.0.0.1" port))
        (connection (usocket:socket-accept socket :element-type 'character)))
    (unwind-protect
        (progn
            (force-output (usocket:socket-stream connection)))
        (progn
            (write-line (parsereq (usocket:socket-stream connection)) (usocket:socket-stream connection))
            (usocket:socket-close connection)))
    (usocket:socket-close socket)))
(server 5037)