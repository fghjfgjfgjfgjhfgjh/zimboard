# Zimboard

Zimboard is a minimal imageboard written in Common Lisp (SBCL-compatible) with a small C helper for ImageMagick operations.

This repository contains the web application (main.lisp), a small C ImageMagick helper (magick_util.c), and a simple stylesheet (static/style.css).

Prerequisites

- A Common Lisp implementation (SBCL recommended)
- Quicklisp (to install runtime deps such as hunchentoot, cl-sqlite, cl-who, md5, flexi-streams, etc.)
- Development files for ImageMagick (MagickWand) and libsqlite3 (pkg names vary by distro, e.g. libmagickwand-dev, libsqlite3-dev)
- gcc (to compile magick_util.c)

Building the native helper

The C file magick_util.c implements a tiny ImageMagick wrapper. Build a shared library so the Lisp code can call it (the exact name/load method depends on magick-util.lisp/CFFI usage). A typical build command on Linux is:

    gcc -O2 -shared -fPIC -o magick_util.so magick_util.c `pkg-config --cflags --libs MagickWand`

If your pkg-config is not configured for MagickWand, you may need to point to the correct include and library paths or use -lMagickWand.

Install and load Lisp dependencies

Start a Lisp repl (SBCL) with Quicklisp available and install dependencies:

    ;; inside SBCL
    (ql:quickload :hunchentoot)
    (ql:quickload :sqlite)
    (ql:quickload :cl-who)
    (ql:quickload :md5)
    (ql:quickload :flexi-streams)
    ;; and any other deps referenced by the system file

Loading and starting the server

There is an ASDF system file zimboard.asd in the repo. In SBCL you can load the system and start the server:

    ;; inside SBCL, from the repository root
    (asdf:load-system :zimboard)
    (in-package :zimboard)
    (start-server)

By default the server listens on port 8080 and will create/open a local sqlite database file named sqlite.db in the working directory.

Notes on runtime and configuration

- Static files
  - static/style.css is served from /static/style.css; other static assets (favicon, images) are read from the ./static and ./imgs directories.

- Database and schema
  - The schema is created automatically on first run by init-database. Tables include users, sessions, comments, posts, tags, tags_to_posts, cache_entries, cache_tags, cache_posts, tag_edits and a handful of indices.

- Sessions and cookies
  - Session cookies are generated with generate-session-cookie and stored in the sessions table. *max-session-age* controls cookie lifetime (default 2592000 seconds).

- Development teapot mode
  - *teapot-mode* is enabled by default (set to t). When enabled any non-localhost request receives an HTTP 418 "I'm a teapot" response. To expose the server to other hosts, set *teapot-mode* to nil before starting the server, e.g.:

        (setf zimboard::*teapot-mode* nil)

- Security
  - User passwords are currently stored as MD5 hashes (see password-match-p). This is insecure for production use. Consider replacing MD5 with a stronger hash (bcrypt/argon2/SHA256+salt) before using this in a real environment.

Useful utility functions

- (start-server) / (stop-server) — start and stop the HTTP acceptor
- clear-uncomplete-posts — removes posts where complete=0
- clear-cache — empties query caches

Routes (overview)

The application exposes these primary routes (see main.lisp for full behavior):

- GET  /                 — Home
- GET  /search           — Search page (?s=<tags>&p=<page>)
- GET  /post             — Make a post (form)
- POST /id               — Create a post (multipart form with image + tags)
- GET  /id/<id>          — View a post
- GET  /id/<id>/edit-tags — Edit tags page
- POST /id/<id>/edit-tags — Submit tag edits
- POST /id/<id>/comment  — Post a comment
- GET  /list-tags        — Tag list
- GET  /user/<name>      — User page
- GET  /register         — Register form
- POST /user             — Create user
- GET  /login            — Login form
- POST /session          — Create session (login)
- POST /delete-session   — Log out
- GET  /static/*         — Static files (CSS, etc.)
- GET  /imgs/*           — Served images/previews

Image storage

- Original uploaded images are written under ./imgs/orig/<subdirs>/... using image-orig-path(hash)
- Previews are written under ./imgs/pre/<subdirs>/... using image-preview-path(id)

Caveats and TODOs

- Tag editing code currently has TODOs related to correctness/performance in main.lisp; be careful when editing related queries.
- The cache implementation is basic (LRU-like) and may need improvements for real workloads.
- The code contains multiple TODOs and comments; review main.lisp before modifying behavior in production.

Where to look in the code

- main.lisp — application logic, routes, HTML rendering
- magick_util.c — C ImageMagick helper (cleaning blob, thumbnail creation)
- static/style.css — basic styling for pages

If you encounter issues building the native helper or loading libraries, make sure ImageMagick (MagickWand) development headers/libraries and SQLite development headers are installed on your system, and adjust the gcc link flags accordingly.
