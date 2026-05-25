;;; epub.el --- EPUB reader -*- lexical-binding: t; -*-

;;; Commentary:

;; EPUB reader for Emacs.  Uses shr.el for HTML rendering.

;;; Code:

(require 'shr)
(require 'imenu)
(require 'dom)
(require 'url-util)
(require 'cl-lib)

(defcustom epub-progress-file (locate-user-emacs-file "epub.prog")
  "File used for saving and restoring reading progress."
  :type 'file
  :group 'epub)

(defvar-local epub-unzip-exdir nil
  "Temporary directory to which the epub file extracts.")

(defvar-local epub-root-url nil
  "Base URL of the OCF container.")

(defvar-local epub-container-file-url nil
  "URL of the container.xml file.")

(defvar-local epub-package-doc-url nil
  "URL of the package document.")

(defvar-local epub-publication-id nil
  "Unique identifier for the EPUB publication.")

(defvar-local epub-manifest-table nil
  "A table from content id to url (absolute) and media-type.")

(defvar-local epub-url-table nil
  "A table from content url to id.")

(defvar-local epub-spine-alist nil
  "A list that specifies reading order.")

(defvar-local epub-toc-id nil
  "File id of the TOC document.")

(defvar-local epub-current-content-id nil
  "File id of the document currently reading.")

(defcustom epub-scroll-pct 0.75
  "Percentage of screen height scrolled up and down."
  :type 'float
  :group 'epub)

(defcustom epub-scroll-beyond t
  "When non-nil, scroll beyond page boundaries to change chapters."
  :type 'boolean
  :group 'epub)

(defcustom epub-resume-progress t
  "Resume from the last position when reopening epub files."
  :type 'boolean
  :group 'epub)

(defgroup epub nil
  "EPUB reader for Emacs."
  :group 'applications)

(defcustom epub-font-scale 1.0
  "Font size scaling factor for EPUB content."
  :type 'float
  :group 'epub)


(defun epub--image-type (ext)
  "Return image type symbol for file extension EXT, or nil."
  (and ext
       (pcase (downcase ext)
	 ("jpg" 'image/jpeg)
	 ("jpeg" 'image/jpeg)
	 ("png" 'image/png)
	 ("gif" 'image/gif)
	 ("svg" 'image/svg+xml))))


(defun epub--manifest-media-type (cid)
  "Return media type for content id CID from the manifest."
  (or (nth 1 (gethash cid epub-manifest-table))
      (error "content id not found: %s" cid)))

(defun epub--manifest-url-put (tb k val)
  "Into TB, index the URL from manifest entry VAL by key K."
  (puthash (car val) k tb))


;; Unzip epub file
(defun epub-unzip (fpath exdir)
  (call-process "unzip" nil "*epub unzip*" nil "-o" fpath "-d" exdir)
  ;; Some EPUBS store META-INF with read-only permissions (0444),
  ;; making directories untraversable.  Ensure directories are
  ;; traversable and files readable.
  (call-process "chmod" nil nil nil "-R" "u+rwX" exdir))

;; Cleanup function
(defun epub-cleanup (&optional err)
  ;; save progress unimplemented
  (unless err (epub-save-progress))
  (delete-directory epub-unzip-exdir t))

(defun epub-cleanup-all ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'epub-mode)
	(epub-cleanup)))))

(defun epub-parse-xml (fpath)
  (with-temp-buffer
    (insert-file-contents fpath)
    (libxml-parse-xml-region (point-min) (point-max)))
  )

(defun epub-url-remotep (url)
  "Return url if it is remote, nil otherwise."
  (and (url-type (url-generic-parse-url url)) url))

(defun epub--decode-url (str)
  "URL-decode STR, replacing percent-encoded characters."
  (if str (url-unhex-string str) str))

(defun epub-locate-container-file (dir)
  "Return absolute URL of the container.xml file under DIR."
  (cl-labels ((locate-container (dir-list)
		(or (locate-file "container.xml" dir-list)
		    (locate-container
		     (seq-filter #'file-directory-p
				 (mapcan (lambda (f)
					   (directory-files f t "[^.]"))
					 dir-list))))))
    (locate-container (list dir))))

(defun epub-locate-package-doc (container-file-url)
  "Given the url of the container file `container.xml',
return the absolute url of the package document.
The relative url is found in the `rootfile' element, after parsing
the container file."
  ;; Package document url is relative to the root URL,
  ;; see https://www.w3.org/TR/epub/#sec-parsing-urls-metainf
  (let* ((pt (epub-parse-xml container-file-url))
	 (node (car (dom-by-tag pt 'rootfile))))
    (expand-file-name (dom-attr node 'full-path) epub-root-url)))

;; The package document parse trees includes:
;; - the manifest element,
;; which provides an exhaustive list of publication resources for renderding.
;; - the spine element,
;; which defines an ordered list of manifest item references,
;; that represent the default reading order.
;; - the navigation document used as TOC,
;; not mandatorily included in the spine.
(defun epub-parse-package-doc ()
  "Parse the package document, located at `epub-package-doc-url'."
  (let* ((pt (epub-parse-xml epub-package-doc-url))
	 (pid (epub-get-pub-id pt))
	 (nav-id (epub-get-nav-doc-id pt))
	 (max-lisp-eval-depth 12800) ;; for parsing large epub files
	 )
    (setq epub-publication-id pid)
    (setq epub-toc-id nav-id)
    (let ((manifest (epub-get-manifest-tb pt))
	  (spine (epub-get-spine-alist pt)))
      (message (format "pub id: %s" pid))
      (message (format "toc id: %s" nav-id))
      ;; (message (format "spine: %s" spine))
      (setq epub-manifest-table manifest)
      (setq epub-spine-alist spine)
      (let ((urltb (make-hash-table :test #'equal
				    :size (hash-table-count manifest))))
        (maphash (apply-partially #'epub--manifest-url-put urltb)
		 manifest)
	(setq epub-url-table urltb)))))

(defun epub-get-pub-version (package-doc-pt)
  "Given the package document parse tree package-doc-pt,
return the publication version."
  (dom-attr package-doc-pt 'version))

(defun epub-get-pub-id (package-doc-pt)
  "Given the package document parse tree package-doc-pt,
return the publication unique identifier."
  (let ((node
	 (or (car (dom-search package-doc-pt
			      (lambda (n)
				(and (eq (dom-tag n) 'identifier)
				     (equal (dom-attr n 'scheme) "uuid")))))
	     (car (dom-search package-doc-pt
			      (lambda (n)
				(and (eq (dom-tag n) 'identifier)
				     (let ((id (dom-attr n 'id)))
				       (and id (string-match-p "ISBN$" id))))))))))
    (if node
	(car (dom-children node))
      (let ((title
	     (car (dom-by-tag package-doc-pt 'title))))
	(md5 (car (dom-children title)))))))

;; In EPUB 3.x, navigation document is declared using the "nav property",
;; within the manifest element,
;; see https://www.w3.org/TR/epub/#sec-item-resource-properties.
;; In EPUB 2.x, an NCX document is required for navigation,
;; item id specied by the toc attribute of the spine element,
;; see https://idpf.org/epub/20/spec/OPF_2.0.1_draft.htm#Section2.4.
(defun epub-get-nav-doc-id (pt)
  "Given the package document parse tree pt,
return the item id of the navigation document, as indexed in the manifest."
  (pcase (epub-get-pub-version pt)
    ((and vs (guard (version< vs "3")))
     (let ((spine (car (dom-by-tag pt 'spine))))
       (dom-attr spine 'toc)))
    (_
     (let ((node (car (dom-search pt
				  (lambda (n)
				    (and (eq (dom-tag n) 'item)
					 (let ((props (dom-attr n 'properties)))
					   (and props (string-match-p "nav" props)))))))))
       (dom-attr node 'id)))))

(defun epub-get-manifest-tb (package-doc-pt)
  "Given the package document parse tree PACKAGE-DOC-PT,
return a hashtable mapping item id to absolute url and media type."
  (let* ((records (dom-by-tag package-doc-pt 'item))
	 (tb (make-hash-table :test #'equal
			      :size (length records))))
    ;; the url specified by the href attribute must be an absolute- or
    ;; path-relative-scheme-less-URL string,
    ;; see https://www.w3.org/TR/epub/#sec-item-elem.
    (mapc (lambda (node)
	    (let* ((id (dom-attr node 'id))
		   (href (epub--decode-url (dom-attr node 'href)))
		   (type (dom-attr node 'media-type))
		   ;; base-url is pkg doc directory
		   (base-url (file-name-directory epub-package-doc-url))
		   (abs-href (or (epub-url-remotep href)
				 (expand-file-name href base-url))))
	      (puthash id (list abs-href type) tb)))
	  records)
    tb))

(defun epub-get-spine-alist (package-doc-pt)
  "Given the package document parse tree package-doc-pt,
return an association list representing the spine.
Each element in the array is a pair of item id and the id of the next item,
(id . id-next), with the last element being (last-id . nil)."
  (let* ((max-lisp-eval-depth 12800)
	 (nodes
	  (dom-by-tag package-doc-pt 'itemref))
	 (ids
	  (mapcar (lambda (nd) (dom-attr nd 'idref)) nodes))
	 (ids-shift1 (cdr (nconc ids '(nil)))) ;; note ids is changed due to side effect.
	 (spine (cl-mapcar #'cons ids ids-shift1))
	 ;; since toc is possibly absent in spine,
	 ;; add toc to front if not included already.
	 (spine (if (and epub-toc-id (assoc epub-toc-id spine))
		    spine
		  (cons (cons epub-toc-id (caar spine)) spine))))
    spine))

(defvar-keymap epub-shr-map
  :parent shr-map
  "<mouse-2>" 'epub-browse-url
  "RET" 'epub-browse-url)

;; Custom rendering functions overriding default 'shr-tag-img'.
(defun epub-tag-img (dom)
  (let ((url (epub--decode-url (dom-attr dom 'src))))
    (if (epub-url-remotep url)
	(shr-tag-img dom)
      (let* ((file (expand-file-name url))
	     (width (shr-string-number (dom-attr dom 'width)))
	     (height (shr-string-number (dom-attr dom 'height)))
	     (title (dom-attr dom 'title))
	     (alt (or (dom-attr dom 'alt) "missing alt attr")))
	(when (file-exists-p file)
	  (let* ((start (point-marker))
		 (data (with-temp-buffer
			 (set-buffer-multibyte nil)
			 (insert-file-contents-literally file)
			 (buffer-string)))
		 (content-type (epub--image-type
				(file-name-extension file)))
		 (spec (list data content-type)))
	    (funcall shr-put-image-function spec alt
		     (list :width width :height height))
	    (when (zerop shr-table-depth)
	      (put-text-property start (point) 'shr-alt alt)
	      (put-text-property start (point) 'image-url file)
	      (put-text-property start (point) 'help-echo
				 (shr-fill-text (or title alt))))))))))

;; Override shr-url-transformer in <a> tag such that,
;; the url passed into 'urlify' is absolute.
;; Bind keymap to epub-shr-map (instead of shr-map) inside 'urlify'.
;; 'urlify' is also called in tags including <video> and <audio>,
;; which most definitely do not point to resources inside container.
(defun epub-tag-a (dom)
  ;; unless remote url, expand relative url to absolute url.
  (let ((shr-url-transformer #'(lambda (url)
				 (if (epub-url-remotep url) url
				   (expand-file-name url))))
	(shr-map epub-shr-map))
    (shr-tag-a dom)))

(defun epub-walk-ncx-node (dom)
  "Transform a ncx dom tree to an xml dom tree."
  (pcase (dom-tag dom)
    ('navMap
     (let ((ch
	    (mapcar #'epub-walk-ncx-node (dom-children dom))))
       `(ol nil ,@ch)))
    
    ('navPoint
     (let* ((label (car (dom-by-tag dom 'text)))
            (label (car (dom-children label)))
            (content (car (dom-by-tag dom 'content)))
            (href (dom-attr content 'src))
            (ch (cl-remove-if-not
		 (lambda (n) (eq (dom-tag n) 'navPoint))
		 (dom-children dom)))
	    (ch (mapcar #'epub-walk-ncx-node ch)))
       (unless href
	 (warn "content href missing: %s" label))
       `(li nil (a ((href . ,href)) ,label) ,@ch)))
    (tag (error (format "cannot handle tag: %s" tag)))))

;; keymap
(defvar-keymap epub-mode-map
  "SPC" 'epub-scroll-up
  "S-SPC" 'epub-scroll-down
  "RET" 'epub-scroll-up
  "DEL" 'epub-scroll-down
  "n" 'epub-next-chap
  "p" 'epub-prev-chap
  "j" 'next-line
  "k" 'previous-line
  "t" 'epub-goto-toc
  )

;; Browse url
(defun epub-browse-url (&optional mouse-event)
  (interactive (list last-nonmenu-event))
  (mouse-set-point mouse-event)
  (let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((null url)
      (message "No link under point"))
     ((epub-url-remotep url)
      (browse-url url))
     (t
      (let ((url (url-generic-parse-url url)))
	(epub-goto-content-helper (url-filename url) nil (url-target url)))
      ))))

;; wrapper function for epub-goto-content,
;; which accepts either a valid url or content id.
;; superlinks are usually accessed directly through url
;; while next/previous pages are accessed through id.
(defun epub-goto-content-helper (url-or-id &optional pnt target)
  (pcase (gethash url-or-id epub-manifest-table)
    (`(,url ,_)
     (epub-goto-content url-or-id url pnt target))
    ((and 'nil
	  (let cid (gethash url-or-id epub-url-table)))
     (epub-goto-content cid url-or-id pnt target))
    (_ (error "neither url or id, should not happen"))))

;; The lowest level fucntion for navigating content documents.
(defun epub-goto-content (cid url pnt target)
  "Given content id and url, render content document."
  (let ((tp (epub--manifest-media-type cid)))
    (message (format "goto url: %s" url))
    (epub-render-content url tp)
    ;; update current content id.
    (setq epub-current-content-id cid))
  
  (when pnt
    (message (format "goto pnt: %s" pnt))
    (goto-char pnt))
  
  (when target
    (message (format "goto target: %s" target))
    (text-property-search-forward 'shr-target-id
				  target
				  (lambda (target ids)
				    (member target ids)))))

(defun epub-render-content (url &optional tp)
  "Render the specified content in the current buffer."
  (let ((tp (or tp
		(epub--manifest-media-type
		 (gethash url epub-url-table)))))
    (pcase tp
      ((pred (string-match "dtbncx")) ;; A ncx TOC file, requires translation
       (let* ((pt (epub-parse-xml url))
	      (dom (car (dom-by-tag pt 'navMap)))
	      (dom (epub-walk-ncx-node dom)))
	 (epub-render-html dom epub-root-url)))
      (_
       (let ((dom (epub-parse-xml url)))
	 (epub-render-html dom url))))
    ))

(defun epub--svg-resolve-images (dom)
  "Replace local image hrefs in SVG DOM with inline data URIs."
  (dolist (child (dom-children dom))
    (when (consp child)
      (if (eq (dom-tag child) 'image)
          (let* ((url (epub--decode-url
                        (or (dom-attr child 'href)
                            (dom-attr child 'xlink:href))))
                 (file (and url (expand-file-name url))))
            (when (and file (file-exists-p file))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally file)
                (let* ((mime (or (symbol-name (epub--image-type
                                               (file-name-extension file)))
                                "image/jpeg"))
                       (data (concat "data:" mime ";base64,"
                                     (base64-encode-string (buffer-string) t))))
                  (dom-set-attribute child 'href data)))))
        (epub--svg-resolve-images child)))))

(defun epub-tag-svg (dom)
  "Render inline SVG, extracting local images from simple wrappers."
  (when (and (image-type-available-p 'svg)
             (not shr-inhibit-images))
    (let ((children (dom-children dom))
          (all-image t))
      (dolist (c children)
        (unless (and (consp c) (eq (dom-tag c) 'image))
          (setq all-image nil)))
      (if (and children all-image)
          (let (any-inserted)
            (dolist (child children)
              (let* ((url (epub--decode-url
                           (or (dom-attr child 'href)
                               (dom-attr child 'xlink:href))))
                     (file (and url (expand-file-name url))))
                (when (and file (file-exists-p file))
                  (let* ((data (with-temp-buffer
                                (set-buffer-multibyte nil)
                                (insert-file-contents-literally file)
                                (buffer-string)))
                         (content-type (epub--image-type
                                        (file-name-extension file))))
                    (setq any-inserted t)
                    (funcall shr-put-image-function
                             (list data content-type)
                             (or (dom-attr child 'alt) ""))))))
            (unless any-inserted
              (epub--svg-resolve-images dom)
              (shr-tag-svg dom)))
        (progn
          (epub--svg-resolve-images dom)
          (shr-tag-svg dom))))))

(defun epub-render-html (dom &optional url)
  "Render the html content file in the current buffer.
Url is necessary to resolve dom elements with relative urls to absolute urls."
  (let ((shr-external-rendering-functions '((img . epub-tag-img)
					    (a . epub-tag-a)
					    (svg . epub-tag-svg)))
	(default-directory (if url (file-name-directory url)
			     default-directory))
	buffer-read-only ;; so that buffer can be modified
	)
    (erase-buffer)
    (when (not (= epub-font-scale 1.0))
      (setq-local face-remapping-alist
                  `((default default (:height ,epub-font-scale))
                    (variable-pitch variable-pitch
                                    (:height ,epub-font-scale)))))
    ;; Ensure SHR can constrain images to window dimensions.
    (unless (get-buffer-window (current-buffer) t)
      (set-window-buffer (selected-window) (current-buffer)))
    (shr-insert-document dom)
    (set-buffer-modified-p nil)
    (goto-char (point-min)) ;; start at the front by default
    ))

(defun epub-next-chap ()
  "Go to next content in the spine."
  (interactive)
  ;; save progress on current content if at neither front or end.
  (let ((id (cdr (assoc epub-current-content-id
			epub-spine-alist))))
    (when id
      (epub-goto-content-helper id))))

(defun epub-prev-chap ()
  "Go to previous content in the spine."
  (interactive)
  (let ((id (car (rassoc epub-current-content-id
			 epub-spine-alist))))
    (when id
      (epub-goto-content-helper id))))

(defun epub-goto-toc ()
  "Go to toc content."
  (interactive)
  (epub-goto-content-helper epub-toc-id))

(defun epub-scroll-up (arg)
  (interactive "P")
  (if (and (>= (window-end) (point-max))
	   epub-scroll-beyond)
      (epub-next-chap)
    (let ((wid-lines
	   (count-screen-lines (window-start) (window-end))))
      (scroll-up (or arg
		     (truncate (* epub-scroll-pct wid-lines)))))))

(defun epub-scroll-down (arg)
  (interactive "P")
  (if (and (<= (window-start) (point-min))
	   epub-scroll-beyond)
      (progn
	(epub-prev-chap)
	(goto-char (point-max)))
    (let ((wid-lines
	   (count-screen-lines (window-start) (window-end))))
      (scroll-down (or arg
		     (truncate (* epub-scroll-pct wid-lines)))))))

(defun epub-dquote (str)
  (concat "\"" str "\""))

(defun epub-dquote-pents (ents)
  (mapcar (lambda (ent)
	    (seq-let (pid cid pnt) ent
	      (list (epub-dquote pid)
		    (epub-dquote cid)
		    pnt)))
	  ents))

(defun epub-save-progress ()
  "Save current reading progress to `epub-progress-file'.
Progress data is a list of (publication-id content-id point)."
  (when epub-progress-file
    (let* ((prev-ents (epub-retrive-progress-all))
	   (ent
	    (list epub-publication-id epub-current-content-id (point)))
	   (new-ents
	    (cons ent (assoc-delete-all epub-publication-id prev-ents))))
      ;; (message (format "save progress: %s" new-ents))
      (with-temp-file epub-progress-file ;; progress file is overwritten
	(let ((ents (epub-dquote-pents new-ents)))
	  ;; format strips one layer of quote from string elements
	  ;; while converting list, thus the need to double quote
	  (insert (format "%s" ents)))))))

(defun epub-retrive-progress-all ()
  "Return all progress data (a list), or nil if empty."
  (when (and epub-progress-file
	     (file-exists-p epub-progress-file))
    (with-temp-buffer
      (insert-file-contents epub-progress-file)
      (condition-case err
	  (read (buffer-string))
	(error (format "Failed to retrieve progress: %s" err))))))

(defun epub-retrive-progress (id)
  "Return progress entry for publication ID, or nil if not found."
  ;; read is used when reading from progress file, changing integer strings to integers.
  (assoc id (epub-retrive-progress-all)))

;; major mode
;;;###autoload (add-to-list 'auto-mode-alist '("\\.epub\\'" . epub-mode))
;;;###autoload
(define-derived-mode epub-mode special-mode "EPUB"
  "Major mode for epub files"
  (add-hook 'change-major-mode-hook 'epub-cleanup nil t)
  (add-hook 'kill-buffer-hook 'epub-cleanup nil t)
  (add-hook 'kill-emacs-hook 'epub-cleanup-all)
  (when (null buffer-file-name)
    (error "EPUB file not specified"))

  ;; Extract EPUB file to temporary directory
  (setq epub-unzip-exdir (make-temp-file "epub-" t))
  (pcase (epub-unzip buffer-file-name epub-unzip-exdir)
    ((and status
	  (guard (null (integerp status))))
     (epub-cleanup)
     (error "EPUB extraction failed: %s" status))
    ((and status
	  (guard (> status 1)))
     (epub-cleanup)
     (error "EPUB extraction exited: %s" status))
    (stat (message (format "extraction success: %s" stat))))
  
  ;; Locate container file
  (setq epub-container-file-url
	(epub-locate-container-file epub-unzip-exdir))
  (unless epub-container-file-url
    (error "container.xml not found."))
  
  ;; Locate container root directory
  ;; Root url is the grandparent of 'epub-container-file-url',
  ;; as in root/META-INF/container.xml.
  (setq epub-root-url (file-name-directory
		       (directory-file-name
			(file-name-directory epub-container-file-url))))

  ;; Parse container file
  (setq epub-package-doc-url (epub-locate-package-doc epub-container-file-url))
  (unless (file-exists-p epub-package-doc-url)
    (error "EPUB package document not found."))

  ;; Parse the package document
  (epub-parse-package-doc)

  ;; Misc
  (setq buffer-undo-list t)

  ;; Interops
  (setq imenu-create-index-function #'epub-imenu-create-index-function)
  (setq imenu-default-goto-function #'epub-imenu-goto-function)

  (let* ((prog (and epub-resume-progress
		    (epub-retrive-progress epub-publication-id)))
	 (cid (nth 1 prog))
	 (pt (nth 2 prog))
	 (valid (and cid pt (gethash cid epub-manifest-table))))
    (if valid
	(condition-case nil
	    (epub-goto-content-helper cid pt)
	  ((error) (epub-cleanup t)))
      (condition-case nil
	  (epub-goto-content-helper (cdar epub-spine-alist))
	((error) (epub-cleanup t))))))

;; imenu interop
;; (defvar-local imenu-create-index-function
;;     'imenu-default-create-index-function ...)

;; the imenu utility is incorporated by customizing
;; 'imenu-create-index-function and 'imenu-default-goto-function

(defun epub-imenu-goto-function (_descp url &rest _args)
  (epub-goto-content-helper url))

;; callstack of imenu
;; - imenu
;; - imenu-choose-buffer-index
;; - imenu--make-index-alist
;; - imenu-create-index-function (within save-excursion)
;; index alist elements look like
;; (INDEX-NAME POSITION FUNCTION ARGUMENTS...)

(defun epub-imenu-create-index-function ()
  (seq-let (url tp) (gethash epub-toc-id epub-manifest-table)
    ;; (message (format "imenu src file: %s" url))
    (let ((pt (epub-parse-xml url))
	  (base-dir (file-name-directory url)))
      (pcase tp
	((pred (string-match "dtbncx")) ;; A ncx TOC file, requires translation
	 (epub-imenu-create-index-from-ncx pt base-dir))
	(_
	 (epub-imenu-create-index-from-nav pt base-dir)))
      )))

(defun epub-imenu-create-index-from-ncx (pt base-dir)
  (let ((nl
	 (dom-by-tag pt 'navPoint)))
    (mapcar (lambda (nd)
	      (let ((text
		     (car (dom-by-tag nd 'text)))
		    (content
		     (car (dom-by-tag nd 'content))))
		(cons (dom-inner-text text)
		      (expand-file-name (dom-attr content 'src)
					base-dir))))
	    nl)))

(defun epub-imenu-create-index-from-nav (pt base-dir)
  (let* ((nav (car (dom-search pt
			       (lambda (n)
				 (and (eq (dom-tag n) 'nav)
				      (equal (dom-attr n 'type) "toc"))))))
	 (nd (car (dom-by-tag nav 'ol)))
	 (menu (mapcan (apply-partially #'epub-imenu-walk-li "")
		       (dom-children nd))))
    (message (format "%s" menu))
    (mapcar (lambda (ele)
	      (pcase ele
		(`(,name . ,url)
		 `(,name . ,(expand-file-name url base-dir)))))
	    menu)
    ))

(defun epub-imenu-walk-li (idnt nd)
  "Walk an li tag and return an imenu index alist.
base-dir is the directory of the nav file."
  (pcase nd
    (`(li ,_ ,atag)
     (let* ((url (epub-url-sanitize (dom-attr atag 'href)))
	    (name (epub-imenu-walk-a atag)))
       (and (epub-str-non-null name)
	    `((,(concat idnt name) . ,url)))))
    (`(li ,_ ,atag ,ol . ,_)
     (let* ((url (epub-url-sanitize (dom-attr atag 'href)))
	    (name (epub-imenu-walk-a atag)))
       (nconc (and (epub-str-non-null name)
		   `((,(concat idnt name) . ,url)))
	      (mapcan (apply-partially #'epub-imenu-walk-li
				       (concat (and (epub-str-non-null name) "-")
					       idnt))
		      (dom-children ol)))))
    ))

(defun epub-imenu-walk-a (nd)
  "Walk an a tag and return the embedded text."
  (dom-inner-text nd))

(defun epub-str-non-null (s)
  (and (not (null s))
       (not (string= "" s))
       s))

(defun epub-url-sanitize (url)
  "Return sanitized version of url URL.
Suffixes like colons, hashes, etc., are removed)"
  (let ((url-obj (url-generic-parse-url url)))
    (or (and (url-type url-obj)
	     url)
	(or (url-filename url-obj)))
    ))

(provide 'epub)
;;; epub.el ends here.
