(use awful server-test test)

(with-test-server
 (lambda ()
   (awful-start
    (lambda ()
      (load-apps (list "server.scm")))))
 (lambda ()
   (load "client.scm")))

(test-exit)
