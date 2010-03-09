;;; Java highlighting package
;;; Copyright (C) 1996 Jan Newmarch

;; Author: Jan Newmarch <jan@ise.canberra.edu.au>
;; Home page: http://pandonia.canberra.edu.au/
;; Maintainer: Jan Newmarch
;; Keywords: hilight, Java

;; This file is not part of GNU Emacs but is distributed under
;; the same conditions as GNU Emacs.
;;
;; This package will highlight Java programs when under the Java
;; major mode. A Java major mode package can be found on e.g.
;; ftp://ftp.javasoft.com/pub/java/contrib/emacs.
;; An enhanced version of that package which has Compile/Run
;; functions is obtainable from my Web site.
;;
;; It requires the hilit19 package
;;
;; Install this package by
;;    (load "/pathname-to-this-file/hilit-java")


(require 'hilit19)

(hilit-set-mode-patterns
   'java-mode
    '(
      ;; comments - do the javadoc ones differently
      ("/\\*\\*" "\\*/" red)
      ("/\\*" "\\*/" firebrick-italic)
      ("//.*$" nil firebrick-italic)

      ;; strings
      (hilit-string-find ?' grey40)

      ;; classes - the toplevel stuff
      ("^\\(class\\|interface\\|import\\|package\\|public\\|private\\|protected\\|{\\|}\\).*$" nil orange2-bold)

      ;; key words
      ("[^_]\\<\\(return\\|goto\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|default\\|while\\|do\\|for\\|public\\|protected\\|private\\|instanceof\\|abstract\\|static\\|super\\|synchronized\\|threadsafe\\|synchronised\\|transient\\|final\\|delete\\|new\\|this\\|try\\|catch\\)\\>[^_]"
       0 RoyalBlue)

      ;; datatype -- black magic regular expression
      ("[ \n\t({]\\(\\(const\\|static\\)\\s +\\)*\\(\\(\\w\\|[$_]\\)+_t\\|float\\|double\\|void\\|boolean\\|char\\|byte\\|short\\|int\\|long\\|FILE\\|\\(\\(struct\\|union\\|enum\\|class\\)\\([ \t]+\\(\\w\\|[$_]\\)*\\)\\)\\)\\(\\s +\\*+)?\\|[ \n\t;()]\\)" nil Maroon)
      ;; standard data values - null, true, false and any ASCII char
      ("[ \n\t()]\\(null\\|true\\|false\\|'.'\\)[ \n\t):;,]" nil chocolate)

      ;; all the standard library objects plus their constructors
      ;; and static uses
      ;; java.applet objects
      ("[ \n\t({,]\\(Applet\\|AppletContext\\|AppletStub\\|AudioClip\\)[ \t().]+" nil purple)
      ;; java.awt objects
      ("[ \n\t({,]\\(BorderLayout\\|Button\\|Canvas\\|Checkbox\\|CheckboxGroup\\|CheckboxMenuItem\\|Choice\\|Component\\|Container\\|Color\\|Dialog\\|Dimension\\|Event\\|FileDialog\\|FlowLayout\\|Font\\|FontMetrics\\|Frame\\|Graphics\\|GridBagConstraints\\|GridBagLayout\\|Image\\|Insets\\|Label\\|LayoutManager\\|List\\|MediaTracker\\|Menu\\|MenuComponent\\|MenuContainer\\|MenuItem\\|MenuBar\\|Panel\\|Point\\|Polygon\\|Rectangle\\|Scrollbar\\|TextArea\\|TextField\\|TextComponent\\|Toolkit\\|Window\\)[ \t().]+" nil purple)
      ;; java.awt.image objects
      ("[ \n\t({,]\\(ColorModel\\|CropImageFilter\\|DirectColorModel\\|FilteredImageSource\\|ImageConsumer\\|ImageFilter\\|ImageObserver\\|ImageProducer\\|IndexColorModel\\|MemoryImageSource\\|PixelGrabber\\|RGBImageFilter\\)[ \t().]+" nil purple)
      ;; java.io objects
      ("[ \n\t({,]\\(BufferedInputStream\\|BufferedOutputStream\\|ByteArrayInputStream\\|ByteArrayOutputStream\\|DataInput\\|DataInputStream\\|DataOutput\\|DataOutputStream\\|File\\|FileDescriptor\\|FileInputStream\\|FileNotFoundException\\|FileOutputStream\\|FilenameFilter\\|FilterInputStream\\|FilterOutputStream\\|InputStream\\|LineNumberInputStream\\|OutputStream\\|PipedInputStream\\|PipedOutputStream\\|PrintStream\\|PushbackInputStream\\|RandomAccessFile\\|SequenceInputStream\\|StreamTokenizer\\|StringBufferInputStream\\)[ \t().]+" nil purple)
      ;; java.lang objects
      ("[ \n\t({,]\\(Boolean\\|Character\\|Class\\|Double\\|Float\\|Integer\\|Long\\|Math\\|Number\\|Object\\|Process\\|Runnable\\|String\\|StringBuffer\\|System\\|Throwable\\|Thread\\)[ \t().]+" nil purple)
      ;; java.net objects
      ("[ \n\t({,]\\(ContentHandler\\|ContentHandlerFactory\\|DatagramPacket\\|DatagramSocket\\|InetAddress\\|PlainSocketImpl\\|ServerSocket\\|Socket\\|SocketImpl\\|SocketImplFactory\\|SocketInputStream\\|SocketOutputStrea\\|URL\\|URLConnection\\|URLEncoder\\|URLStreamHandler\\|URLStreamHandlerFactory\\)[ \t().]+" nil purple)
      ;; java.util objects
      ("[ \n\t({,]\\(BitSet\\|Date\\|Dictionary\\|Enumeration\\|Hashtable\\|Observable\\|Observer\\|Properties\\|Random\\|Stack\\|StringTokenizer\\|Vector\\)[ \t().]+" nil purple)
    )
)