(defsystem "zimboard"
  :description "Image board"
  :author "Dmytry Strakhal"
  :license "BSD 2-Clause"
  :depends-on (#:hunchentoot
               #:trivial-mimes
               #:trivial-rfc-1123
               #:cl-who #:sqlite #:flexi-streams #:md5 #:zimboard/magick-util)
  :components
  ((:file "main")))

(defsystem "zimboard/magick-util"
  :description "Bindings to ImageMagick utility functions"
  :author "Dmytry Strakhal"
  :license "BSD 2-Clause"
  :depends-on (#:cffi)
  :around-compile (lambda (next &rest args &key &allow-other-keys)
                    (uiop:run-program 
                      "gcc -fPIC -shared -Wall -Wextra -O2 `pkg-config --cflags --libs ImageMagick MagickWand` -o magick_util.so magick_util.c"
                      :output t
                      :error-output t)
                    (apply next args)) 
  :components
  ((:file "magick-util")))
